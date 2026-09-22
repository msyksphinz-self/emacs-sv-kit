;;; sv-refactor.el --- Renaming for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Renames a name and everything that refers to it, using the parse tree
;; rather than a search and replace.  The difference shows up exactly where
;; it matters: the word inside a comment or a string is left alone, a struct
;; field that happens to share the name is left alone, and `.sig (sig)' in
;; an instantiation has its two halves treated separately -- the left one
;; belongs to the module being instantiated, the right one to this file.
;;
;; Three kinds of rename:
;;
;; * a signal, parameter, type or instance inside one design unit, which
;;   stays in this buffer;
;; * a port, which is also spelled in every instantiation of the module, so
;;   those are offered as well;
;; * a design unit, which is spelled wherever the project instantiates it.
;;
;; When a name is declared in more than one scope of the same unit -- the
;; same `localparam' in two generate branches, say -- the rename stops and
;; says so, because it cannot tell from the name alone which one is meant.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-format)
(require 'sv-index)
(require 'sv-ide)

(defgroup sv-refactor nil
  "Renaming for SystemVerilog."
  :group 'tools
  :prefix "sv-refactor-")

(defcustom sv-refactor-realign-after-rename t
  "When non-nil, re-align an instantiation whose port names a rename changed.
A longer or shorter port name leaves the column of `.port (signal)\=' out
of true, and nothing else in the file needs touching."
  :type 'boolean
  :group 'sv-refactor)

(defcustom sv-refactor-save-after-rename nil
  "When non-nil, save the other files a project-wide rename touched.
By default they are left modified so that the change can be reviewed."
  :type 'boolean
  :group 'sv-refactor)


;;;; Editing

(defun sv-refactor--valid-name-p (name)
  "Return non-nil when NAME can be used as an identifier."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_$]*\\'" name)
       (not (sv-lexer-keyword-p name))))

(defun sv-refactor--token-ranges (tokens)
  "Return the buffer ranges TOKENS occupy, as a list of conses."
  (delq nil (mapcar (lambda (token)
                      (when token
                        (cons (sv-token-start token) (sv-token-end token))))
                    tokens)))

(defun sv-refactor--replace (ranges name)
  "Put NAME in each of RANGES, last one first so the rest stay valid."
  (let ((sorted (sort (copy-sequence ranges)
                      (lambda (a b) (> (car a) (car b)))))
        (count 0))
    (save-excursion
      (dolist (range sorted)
        (goto-char (car range))
        (delete-region (car range) (cdr range))
        (insert name)
        (setq count (1+ count))))
    count))


;;;; What a name is

(defun sv-refactor--declarations (unit name)
  "Return the records of UNIT that declare NAME."
  (cl-remove-if-not (lambda (record) (equal (plist-get record :name) name))
                    (sv-parse-declarations unit)))

(defun sv-refactor--scopes (declarations)
  "Return the distinct scopes DECLARATIONS were made in."
  (delete-dups (mapcar (lambda (record) (plist-get record :scope))
                       declarations)))

(defun sv-refactor--port (unit name)
  "Return the port of UNIT called NAME, or nil."
  (cl-find name (plist-get unit :ports)
           :key (lambda (port) (plist-get port :name))
           :test #'equal))

(defun sv-refactor-local-tokens (unit significant name)
  "Return every token of UNIT that spells NAME as a declaration or a use.
SIGNIFICANT is the token vector UNIT was parsed from."
  (let ((tokens '()))
    (dolist (record (sv-refactor--declarations unit name))
      (when (plist-get record :token)
        (push (plist-get record :token) tokens)))
    (dolist (reference (sv-parse-references unit significant))
      (when (equal (car reference) name)
        (push (cdr reference) tokens)))
    (delete-dups tokens)))


;;;; Renaming inside one unit

(defun sv-refactor--rename-local (unit tree old new)
  "Rename OLD to NEW inside UNIT of TREE, and return how many tokens changed."
  (let* ((significant (plist-get tree :significant))
         (declarations (sv-refactor--declarations unit old))
         (scopes (sv-refactor--scopes declarations)))
    (cond
     ((null declarations)
      (user-error "`%s' is not declared in `%s'" old (plist-get unit :name)))
     ((> (length scopes) 1)
      (user-error "`%s' is declared in %d separate scopes (lines %s); rename it by hand"
                  old (length scopes)
                  (mapconcat #'number-to-string
                             (sort (mapcar (lambda (record)
                                             (plist-get record :line))
                                           declarations)
                                   #'<)
                             ", ")))
     ((sv-refactor--declarations unit new)
      (user-error "`%s' is already declared in `%s'" new (plist-get unit :name)))
     (t
      (sv-refactor--replace
       (sv-refactor--token-ranges
        (sv-refactor-local-tokens unit significant old))
       new)))))


;;;; Renaming across the project

(defun sv-refactor--instance-tokens (unit significant module)
  "Return the tokens of UNIT that name MODULE as the type of an instance."
  (delq nil
        (mapcar (lambda (instance)
                  (when (equal (plist-get instance :module) module)
                    (let ((index (plist-get instance :beg)))
                      (when (and index (< index (length significant)))
                        (aref significant index)))))
                (sv-parse-collect unit 'instance))))

(defun sv-refactor--connection-tokens (unit module port)
  "Return the tokens naming PORT in every instantiation of MODULE inside UNIT."
  (let ((tokens '()))
    (dolist (instance (sv-parse-collect unit 'instance))
      (when (equal (plist-get instance :module) module)
        (dolist (sibling (plist-get instance :siblings))
          (dolist (connection (plist-get sibling :connections))
            (when (and (equal (plist-get connection :name) port)
                       (plist-get connection :name-token))
              (push (plist-get connection :name-token) tokens))))))
    tokens))

(defun sv-refactor--in-each-file (files collector new &optional after)
  "Apply COLLECTOR to every file of FILES and rename what it returns to NEW.
COLLECTOR is called in the file's buffer with its parse tree, and returns
the tokens to replace.  AFTER, when given, runs in each buffer that
changed, once the replacements are in.  Returns a list of (FILE . COUNT)
for the files that changed."
  (let ((touched '()))
    (dolist (file files)
      (let ((buffer (or (find-buffer-visiting file)
                        (find-file-noselect file t))))
        (with-current-buffer buffer
          ;; Parse the buffer, not the file: its contents may already differ.
          (let* ((tree (sv-parse-buffer))
                 (tokens (funcall collector tree)))
            (when tokens
              (let ((count (sv-refactor--replace
                            (sv-refactor--token-ranges tokens) new)))
                (when after (funcall after))
                (push (cons file count) touched)
                (when sv-refactor-save-after-rename (save-buffer))))))))
    (nreverse touched)))

(defun sv-refactor--rename-unit (old new)
  "Rename the design unit OLD to NEW across the project."
  (let* ((files (sv-index-project-files))
         (touched
          (sv-refactor--in-each-file
           files
           (lambda (tree)
             (let ((significant (plist-get tree :significant))
                   (tokens '()))
               (dolist (unit (plist-get tree :units))
                 ;; Its own declaration, and the label on its end keyword.
                 (when (equal (plist-get unit :name) old)
                   (push (plist-get unit :name-token) tokens)
                   (when (equal (plist-get unit :end-label) old)
                     (push (plist-get unit :end-label-token) tokens)))
                 (setq tokens
                       (append (sv-refactor--instance-tokens
                                unit significant old)
                               tokens)))
               (delq nil tokens)))
           new)))
    touched))

(defun sv-refactor--instance-extents (module)
  "Return the buffer ranges the instantiations of MODULE occupy."
  (let* ((tree (sv-parse-buffer))
         (significant (plist-get tree :significant))
         (limit (length significant))
         (extents '()))
    (dolist (unit (plist-get tree :units))
      (dolist (instance (sv-parse-collect unit 'instance))
        (when (equal (plist-get instance :module) module)
          (let ((first (plist-get instance :beg))
                (last (1- (or (plist-get instance :end) 0))))
            (when (and first (< first limit) (>= last 0) (< last limit))
              (push (cons (sv-token-start (aref significant first))
                          (sv-token-end (aref significant last)))
                    extents))))))
    extents))

(defun sv-refactor--realign-instances (module)
  "Re-align each instantiation of MODULE in this buffer."
  (when sv-refactor-realign-after-rename
    ;; Last one first: formatting shifts everything after it.
    (dolist (extent (sort (sv-refactor--instance-extents module)
                          (lambda (a b) (> (car a) (car b)))))
      (sv-format-region (car extent) (cdr extent)))))

(defun sv-refactor--rename-port-connections (module port new)
  "Rename PORT to NEW in every instantiation of MODULE in the project."
  (sv-refactor--in-each-file
   (sv-index-project-files)
   (lambda (tree)
     (let ((tokens '()))
       (dolist (unit (plist-get tree :units))
         (setq tokens (append (sv-refactor--connection-tokens unit module port)
                              tokens)))
       tokens))
   new
   (lambda () (sv-refactor--realign-instances module))))


;;;; Entry point

(defun sv-refactor--report (touched)
  "Describe the files TOUCHED by a rename."
  (if (null touched)
      "no other file mentions it"
    (format "%s"
            (mapconcat (lambda (entry)
                         (format "%s (%d)"
                                 (file-name-nondirectory (car entry))
                                 (cdr entry)))
                       touched ", "))))

;;;###autoload
(defun sv-kit-rename (new-name)
  "Rename the name at point to NEW-NAME.

A signal, parameter, type or instance is renamed inside its design unit.
A port is renamed there too, and every instantiation of the module in the
project is offered the same change.  A module, interface or package is
renamed wherever the project instantiates it.

Comments, strings and same-named struct fields are left alone, because
the rename works from the parse tree and not from the text."
  (interactive
   (let ((old (thing-at-point 'symbol t)))
     (unless old (user-error "Point is not on a name"))
     (list (read-string (format "Rename `%s' to: " old) old))))
  (let* ((old (thing-at-point 'symbol t))
         (tree (car (sv-index-buffer)))
         (unit (sv-ide--enclosing-unit)))
    (unless (sv-refactor--valid-name-p new-name)
      (user-error "`%s' is not a usable identifier" new-name))
    (when (equal old new-name)
      (user-error "That is already its name"))
    (cond
     ;; A name declared in the unit around point.
     ((and unit (sv-refactor--declarations unit old))
      (let* ((port (sv-refactor--port unit old))
             (module (plist-get unit :name))
             (count (sv-refactor--rename-local unit tree old new-name))
             (touched nil))
        (when (and port module
                   (y-or-n-p (format "`%s' is a port of `%s'; update its instantiations too? "
                                     old module)))
          (setq touched (sv-refactor--rename-port-connections
                         module old new-name)))
        (message "Renamed %d occurrence%s of `%s' in `%s'%s"
                 count (if (= count 1) "" "s") old module
                 (if touched (concat "; " (sv-refactor--report touched)) ""))))
     ;; A design unit of the project.
     ((sv-index-unit old)
      (if (not (y-or-n-p (format "Rename the design unit `%s' across the project? "
                                 old)))
          (message "Left `%s' alone" old)
        (let ((touched (sv-refactor--rename-unit old new-name)))
          (sv-index-invalidate)
          (message "Renamed `%s' to `%s' in %s"
                   old new-name (sv-refactor--report touched)))))
     (t
      (user-error "`%s' is neither declared here nor a module of this project"
                  old)))))

(provide 'sv-refactor)

;;; sv-refactor.el ends here
