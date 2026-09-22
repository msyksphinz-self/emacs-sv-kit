;;; sv-ide.el --- Completion, xref and ElDoc for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Editor services built on the symbol index of `sv-index':
;;
;; * completion-at-point, aware of where point is.  After a dot inside an
;;   instantiation it offers the ports the instantiated module actually has,
;;   minus the ones already connected; after a backquote it offers the
;;   project's macros; after a dollar the system tasks; elsewhere the signals,
;;   parameters and types in scope, then the modules of the project.
;; * an `xref' backend, so M-. jumps to the declaration of a signal, the
;;   definition of a module or the `typedef' of a type, anywhere in the
;;   project, and M-? lists the places a name is used.
;; * an ElDoc function that shows the declaration of the signal at point, or
;;   the header of the module being instantiated.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-index)

(defgroup sv-ide nil
  "Completion, cross-references and documentation for SystemVerilog."
  :group 'tools
  :prefix "sv-ide-")

(defcustom sv-ide-complete-keywords t
  "When non-nil, offer SystemVerilog keywords among the completions."
  :type 'boolean
  :group 'sv-ide)

(defconst sv-ide-system-tasks
  '("$display" "$write" "$strobe" "$monitor" "$fatal" "$error" "$warning"
    "$info" "$time" "$stime" "$realtime" "$random" "$urandom" "$urandom_range"
    "$clog2" "$bits" "$size" "$left" "$right" "$low" "$high" "$increment"
    "$dimensions" "$unpacked_dimensions" "$signed" "$unsigned" "$cast"
    "$isunknown" "$onehot" "$onehot0" "$countones" "$countbits" "$past"
    "$rose" "$fell" "$stable" "$changed" "$sformat" "$sformatf" "$fopen"
    "$fclose" "$fwrite" "$fdisplay" "$readmemb" "$readmemh" "$finish" "$stop"
    "$dumpfile" "$dumpvars" "$value$plusargs" "$test$plusargs" "$assertoff"
    "$asserton" "$error" "$fell" "$typename")
  "System tasks and functions offered for completion.")


;;;; Context around point

(defun sv-ide--symbol-bounds ()
  "Return the bounds of the identifier around point as a cons, or nil."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (cond
     ((and bounds (<= (car bounds) (point))) bounds)
     ((looking-back "[A-Za-z0-9_$]" 1)
      (cons (save-excursion (skip-chars-backward "A-Za-z0-9_$") (point)) (point)))
     (t (cons (point) (point))))))

(defun sv-ide--identifier-before ()
  "Return the identifier ending at point, leaving point at its start.
Point does not move when there is no identifier there."
  (let ((end (point)))
    (skip-chars-backward "A-Za-z0-9_$")
    (if (and (< (point) end)
             (string-match-p "\\`[A-Za-z_]"
                             (buffer-substring-no-properties (point) end)))
        (buffer-substring-no-properties (point) end)
      (goto-char end)
      nil)))

(defun sv-ide-instance-context (&optional position)
  "Return the instantiation whose port list surrounds POSITION.
The value is a plist with `:module', `:instance', `:open' and
`:connected', the last holding the port names already connected."
  (save-excursion
    (when position (goto-char position))
    (let* ((state (syntax-ppss))
           (open (nth 1 state)))
      (when (and open (not (nth 3 state)) (not (nth 4 state)))
        (let ((inside-end (point)) instance module)
          (goto-char open)
          (skip-chars-backward " \t\n")
          ;; An instance array sits between the name and its ports.
          (when (eq (char-before) ?\])
            (condition-case nil (backward-sexp) (error nil))
            (skip-chars-backward " \t\n"))
          (setq instance (sv-ide--identifier-before))
          (when instance
            (skip-chars-backward " \t\n")
            ;; Step over a parameter override.
            (when (eq (char-before) ?\))
              (condition-case nil (backward-sexp) (error nil))
              (skip-chars-backward " \t\n")
              (when (eq (char-before) ?#) (backward-char 1))
              (skip-chars-backward " \t\n"))
            (setq module (sv-ide--identifier-before))
            (when (and module (not (sv-lexer-keyword-p module))
                       (not (sv-lexer-keyword-p instance)))
              ;; Read the whole port list, not just the part before point: a
              ;; port connected further down is connected all the same.
              (let ((close (ignore-errors (scan-sexps open 1))))
                (list :module module
                      :instance instance
                      :open open
                      :close close
                      :connected (sv-ide--connected-ports
                                  open (if close (1- close) inside-end)))))))))))

(defun sv-ide--connected-ports (open limit)
  "Return the port names connected between OPEN and LIMIT."
  (save-excursion
    (goto-char open)
    (let ((names '()))
      (while (re-search-forward "\\.\\([A-Za-z_][A-Za-z0-9_$]*\\)"
                                (min limit (point-max)) t)
        (push (match-string-no-properties 1) names))
      (nreverse names))))

(defun sv-ide--enclosing-unit (&optional position)
  "Return the design unit that contains POSITION, or point."
  (let* ((position (or position (point)))
         (line (line-number-at-pos position))
         (tree (car (sv-index-buffer)))
         (found nil))
    (dolist (unit (plist-get tree :units))
      (let ((vector (plist-get tree :significant))
            (last (1- (or (plist-get unit :end) 0))))
        (when (and (>= line (plist-get unit :line))
                   (or (< last 0)
                       (>= (if (< last (length vector))
                               (sv-token-line (aref vector last))
                             most-positive-fixnum)
                           line)))
          (setq found unit))))
    found))


;;;; Types and their members

(defun sv-ide-type-name (datatype)
  "Return the bare type name DATATYPE mentions, without its qualifiers."
  (when datatype
    (let ((words (split-string datatype "[ \t]+" t)))
      (setq words (cl-remove-if
                   (lambda (word)
                     (member word '("var" "const" "static" "automatic" "signed"
                                    "unsigned" "packed" "virtual" "rand" "randc")))
                   words))
      (let ((name (car (last words))))
        (when name
          ;; `pkg::entry_t' names the type entry_t.
          (if (string-match "\\`\\(?:.*::\\)?\\([A-Za-z_][A-Za-z0-9_$]*\\)\\'" name)
              (match-string 1 name)
            name))))))

(defun sv-ide-typedef (name)
  "Return the typedef called NAME, looking in this buffer before the project."
  (when name
    (let* ((tree (car (sv-index-buffer)))
           (found nil))
      (dolist (unit (plist-get tree :units))
        (dolist (typedef (sv-parse-collect unit 'typedef))
          (when (and (null found) (equal (plist-get typedef :name) name))
            (setq found typedef))))
      (or found (sv-index-type name)))))

(defun sv-ide--signal-datatype (name)
  "Return the data type the signal NAME is declared with, in this buffer."
  (let ((unit (sv-ide--enclosing-unit))
        (found nil))
    (dolist (record (and unit (sv-parse-declarations unit)))
      (when (and (null found) (equal (plist-get record :name) name))
        (let ((declarator (or (plist-get record :decl) (plist-get record :port))))
          (when declarator (setq found (plist-get declarator :datatype))))))
    found))

(defun sv-ide-dotted-prefix (&optional position)
  "Return the chain of names before the dot at POSITION, outermost first.
For `a.b.\=' the answer is the names a and b in that order, and a select
on the way is stepped over."
  (save-excursion
    (when position (goto-char position))
    (let ((names '()) (scanning t))
      (while scanning
        (skip-chars-backward " \t\n")
        (while (and scanning (eq (char-before) ?\]))
          (condition-case nil (backward-sexp) (error (setq scanning nil)))
          (skip-chars-backward " \t\n"))
        (let ((name (and scanning (sv-ide--identifier-before))))
          (if (null name)
              (setq scanning nil)
            (push name names)
            (skip-chars-backward " \t\n")
            (if (eq (char-before) ?.)
                (backward-char 1)
              (setq scanning nil)))))
      names)))

(defun sv-ide-field-type (names)
  "Return the typedef the dotted NAMES end at, or nil."
  (let ((node (sv-ide-typedef
               (sv-ide-type-name (sv-ide--signal-datatype (car names))))))
    (dolist (field (cdr names))
      (setq node
            (when node
              (let ((member (cl-find field (plist-get node :members)
                                     :key (lambda (m) (plist-get m :name))
                                     :test #'equal)))
                (when member
                  (sv-ide-typedef
                   (sv-ide-type-name (plist-get member :datatype))))))))
    node))

(defun sv-ide--field-candidates (names)
  "Return (CANDIDATES . ANNOTATIONS) for the members of the type NAMES reach."
  (let ((node (sv-ide-field-type names))
        (candidates '())
        (annotations (make-hash-table :test #'equal)))
    (when node
      (dolist (member (plist-get node :members))
        (let ((name (plist-get member :name)))
          (when name
            (puthash name (format " %s" (sv-index--declarator-signature member))
                     annotations)
            (push name candidates))))
      (dolist (literal (plist-get node :enum-members))
        (let ((name (plist-get literal :name)))
          (puthash name " enumeration literal" annotations)
          (push name candidates)))
      (cons (nreverse candidates) annotations))))


;;;; Completion

(defun sv-ide--scope-symbols ()
  "Return the symbols visible from point: this unit first, then the file."
  (let* ((symbols (sv-index-buffer-symbols))
         (unit (sv-ide--enclosing-unit))
         (name (and unit (plist-get unit :name))))
    (if name
        (append (cl-remove-if-not
                 (lambda (symbol) (equal (sv-symbol-container symbol) name))
                 symbols)
                (cl-remove-if
                 (lambda (symbol) (equal (sv-symbol-container symbol) name))
                 symbols))
      symbols)))

(defun sv-ide--candidates-from-symbols (symbols)
  "Return (CANDIDATES . ANNOTATIONS) built from SYMBOLS."
  (let ((candidates '()) (annotations (make-hash-table :test #'equal)))
    (dolist (symbol symbols)
      (let ((name (sv-symbol-name symbol)))
        (unless (gethash name annotations)
          (puthash name (format " %s" (sv-symbol-signature symbol)) annotations)
          (push name candidates))))
    (cons (nreverse candidates) annotations)))

(defun sv-ide--port-candidates (context &optional typed)
  "Return the ports of the module CONTEXT instantiates that are still free.
TYPED is the port name being edited, which stays on offer."
  (let* ((unit (sv-index-unit (plist-get context :module)))
         (connected (remove typed (plist-get context :connected)))
         (candidates '())
         (annotations (make-hash-table :test #'equal)))
    (dolist (port (plist-get unit :ports))
      (let ((name (plist-get port :name)))
        (when (and name (not (member name connected)))
          (puthash name (format " %s %s"
                                (or (plist-get port :dir) "")
                                (string-trim
                                 (concat (or (plist-get port :datatype) "") " "
                                         (sv-parse-token-string
                                          (plist-get port :packed)))))
                   annotations)
          (push name candidates))))
    (cons (nreverse candidates) annotations)))

;;;###autoload
(defun sv-ide-completion-at-point ()
  "Complete the SystemVerilog name before point.
Suitable as a member of `completion-at-point-functions'."
  (let* ((bounds (sv-ide--symbol-bounds))
         (start (car bounds))
         (end (cdr bounds))
         (after-dot (eq (char-before start) ?.))
         (after-quote (eq (char-before start) ?`))
         (system-task (eq (char-after start) ?$))
         (context (and after-dot (sv-ide-instance-context start)))
         (pair
          (cond
           (context (sv-ide--port-candidates
                     context (buffer-substring-no-properties start end)))
           (after-quote
            (cons (sv-index-macros) (make-hash-table :test #'equal)))
           (after-dot (sv-ide--field-candidates
                       (sv-ide-dotted-prefix (1- start))))
           (system-task
            (cons sv-ide-system-tasks (make-hash-table :test #'equal)))
           (t
            (let* ((local (sv-ide--candidates-from-symbols (sv-ide--scope-symbols)))
                   (candidates (car local))
                   (annotations (cdr local)))
              (dolist (name (sv-index-names))
                (unless (gethash name annotations)
                  (puthash name " project" annotations)
                  (push name candidates)))
              (when sv-ide-complete-keywords
                (dolist (keyword sv-lexer-keywords)
                  (unless (gethash keyword annotations)
                    (puthash keyword " keyword" annotations)
                    (push keyword candidates))))
              (cons (nreverse candidates) annotations))))))
    (when pair
      (list start end (car pair)
            :exclusive 'no
            :annotation-function
            (lambda (candidate) (gethash candidate (cdr pair)))
            :company-kind
            (lambda (_candidate) (if context 'property 'variable))))))


;;;; Cross references

(defun sv-ide--symbol-location (symbol)
  "Return an `xref' location for SYMBOL."
  (let* ((file (sv-symbol-file symbol))
         (buffer (and file (find-buffer-visiting file))))
    (cond
     ((and (null file) (buffer-file-name)) ; unsaved buffer with a name
      (xref-make-buffer-location (current-buffer)
                                 (sv-ide--line-position (sv-symbol-line symbol)
                                                        (sv-symbol-col symbol))))
     ((null file)
      (xref-make-buffer-location (current-buffer)
                                 (sv-ide--line-position (sv-symbol-line symbol)
                                                        (sv-symbol-col symbol))))
     ((and buffer (buffer-modified-p buffer))
      (xref-make-buffer-location
       buffer (with-current-buffer buffer
                (sv-ide--line-position (sv-symbol-line symbol)
                                       (sv-symbol-col symbol)))))
     (t (xref-make-file-location file (sv-symbol-line symbol)
                                 (sv-symbol-col symbol))))))

(defun sv-ide--line-position (line col)
  "Return the buffer position of COL on LINE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (min (point-max) (+ (point) col))))

(defun sv-ide--definitions (identifier)
  "Return the symbols that declare IDENTIFIER, this buffer first."
  (let* ((local (cl-remove-if-not
                 (lambda (symbol) (equal (sv-symbol-name symbol) identifier))
                 (sv-index-buffer-symbols)))
         (project (cl-remove-if
                   (lambda (symbol)
                     (and (sv-symbol-file symbol)
                          (equal (sv-symbol-file symbol) (buffer-file-name))))
                   (or (sv-index-lookup identifier) '()))))
    (append local project)))

;;;###autoload
(defun sv-ide-xref-backend ()
  "Return the `xref' backend of sv-kit."
  'sv-kit)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql sv-kit)))
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql sv-kit)))
  (delete-dups (append (mapcar #'sv-symbol-name (sv-index-buffer-symbols))
                       (sv-index-names))))

(cl-defmethod xref-backend-definitions ((_backend (eql sv-kit)) identifier)
  (mapcar (lambda (symbol)
            (xref-make (format "%s  [%s]"
                               (sv-symbol-signature symbol)
                               (sv-symbol-kind symbol))
                       (sv-ide--symbol-location symbol)))
          (sv-ide--definitions identifier)))

(cl-defmethod xref-backend-apropos ((_backend (eql sv-kit)) pattern)
  (let ((regexp (xref-apropos-regexp pattern))
        (results '()))
    (maphash (lambda (name symbols)
               (when (string-match-p regexp name)
                 (dolist (symbol symbols)
                   (push (xref-make (format "%s  [%s]"
                                            (sv-symbol-signature symbol)
                                            (sv-symbol-kind symbol))
                                    (sv-ide--symbol-location symbol))
                         results))))
             (plist-get (sv-index-project) :symbols))
    (nreverse results)))

(cl-defmethod xref-backend-references ((_backend (eql sv-kit)) identifier)
  (let ((regexp (concat "\\_<" (regexp-quote identifier) "\\_>"))
        (results '())
        (current (buffer-file-name)))
    (dolist (file (sv-index-project-files))
      (let ((buffer (find-buffer-visiting file)))
        (if (and buffer (buffer-modified-p buffer))
            (with-current-buffer buffer
              (setq results (append results (sv-ide--references-in regexp file))))
          (with-temp-buffer
            (insert-file-contents file)
            (setq results (append results (sv-ide--references-in regexp file)))))))
    (unless (or (null current) (member current (sv-index-project-files)))
      (setq results (append results (sv-ide--references-in regexp current))))
    results))

(defun sv-ide--references-in (regexp file)
  "Return `xref' items for every match of REGEXP in the current buffer.
FILE names the file the buffer holds."
  (save-excursion
    (goto-char (point-min))
    (let ((results '()))
      (while (re-search-forward regexp nil t)
        ;; Read the match out before building the summary: `string-trim\='
        ;; searches, and would leave the match data pointing at its own work.
        (let* ((start (match-beginning 0))
               (line (line-number-at-pos start))
               (column (save-excursion (goto-char start) (current-column)))
               (summary (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position)))))
          (push (xref-make summary (xref-make-file-location file line column))
                results)))
      (nreverse results))))


;;;; ElDoc

(defun sv-ide--port-documentation ()
  "Return documentation for the instance port name at point, when there is one."
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when (and bounds (eq (char-before (car bounds)) ?.))
        (let* ((name (buffer-substring-no-properties (car bounds) (cdr bounds)))
               (context (sv-ide-instance-context (car bounds)))
               (unit (and context (sv-index-unit (plist-get context :module))))
               (port (cl-find name (plist-get unit :ports)
                              :key (lambda (p) (plist-get p :name))
                              :test #'equal)))
          (when port
            (format "%s.%s: %s"
                    (plist-get context :module) name
                    (string-trim
                     (format "%s %s %s"
                             (or (plist-get port :dir) "")
                             (or (plist-get port :datatype) "")
                             (sv-parse-token-string (plist-get port :packed)))))))))))

(defun sv-ide--field-documentation ()
  "Return the declaration of the struct field at point, when there is one."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (and bounds (eq (char-before (car bounds)) ?.))
      (let* ((name (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (node (sv-ide-field-type (sv-ide-dotted-prefix (1- (car bounds)))))
             (member (cl-find name (plist-get node :members)
                              :key (lambda (m) (plist-get m :name))
                              :test #'equal)))
        (when member
          (format "%s.%s: %s" (plist-get node :name) name
                  (sv-index--declarator-signature member)))))))

(defun sv-ide-documentation-at-point ()
  "Return a one-line description of the SystemVerilog name at point."
  (or (sv-ide--port-documentation)
      (sv-ide--field-documentation)
      (let ((identifier (thing-at-point 'symbol t)))
        (when identifier
          (let ((symbol (car (sv-ide--definitions identifier))))
            (when symbol
              (if (sv-symbol-container symbol)
                  (format "%s  [%s in %s]"
                          (sv-symbol-signature symbol)
                          (sv-symbol-kind symbol)
                          (sv-symbol-container symbol))
                (format "%s  [%s]"
                        (sv-symbol-signature symbol)
                        (sv-symbol-kind symbol)))))))))

;;;###autoload
(defun sv-ide-eldoc-function (callback &rest _ignored)
  "Report the declaration of the name at point to CALLBACK."
  (let ((documentation (ignore-errors (sv-ide-documentation-at-point))))
    (when documentation
      (funcall callback documentation)
      documentation)))

(defun sv-ide-eldoc-legacy ()
  "Return the declaration of the name at point, for Emacs 27's ElDoc."
  (ignore-errors (sv-ide-documentation-at-point)))

;;;###autoload
(defun sv-ide-setup ()
  "Turn on completion, cross references and ElDoc in this buffer."
  (add-hook 'completion-at-point-functions #'sv-ide-completion-at-point nil t)
  (add-hook 'xref-backend-functions #'sv-ide-xref-backend nil t)
  (if (boundp 'eldoc-documentation-functions)
      (add-hook 'eldoc-documentation-functions #'sv-ide-eldoc-function nil t)
    (setq-local eldoc-documentation-function #'sv-ide-eldoc-legacy)))

(defun sv-ide-teardown ()
  "Undo what `sv-ide-setup' did in this buffer."
  (remove-hook 'completion-at-point-functions #'sv-ide-completion-at-point t)
  (remove-hook 'xref-backend-functions #'sv-ide-xref-backend t)
  (if (boundp 'eldoc-documentation-functions)
      (remove-hook 'eldoc-documentation-functions #'sv-ide-eldoc-function t)
    (kill-local-variable 'eldoc-documentation-function)))

(provide 'sv-ide)

;;; sv-ide.el ends here
