;;; sv-kit.el --- SystemVerilog parser, linter and formatter -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, tools, verilog
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; sv-kit glues the tokenizer, parser, linter and formatter together and
;; hands them to Emacs: a Flymake backend, an imenu index, an instantiation
;; template generator and a batch entry point for continuous integration.
;;
;; It is a minor mode, so it layers on top of whatever major mode you
;; already use for Verilog -- `verilog-mode', `verilog-ts-mode' or plain
;; `prog-mode':
;;
;;   (add-hook 'verilog-mode-hook #'sv-kit-mode)
;;
;; Inside a buffer with `sv-kit-mode' on:
;;
;;   C-c C-f   format the buffer          `sv-format-buffer'
;;   C-c C-r   format the region          `sv-format-region'
;;   C-c C-l   list the lint findings     `sv-kit-lint'
;;   C-c C-i   insert an instantiation    `sv-kit-insert-instance'
;;   C-c C-u   jump to a design unit      `sv-kit-goto-unit'

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'flymake)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-lint)
(require 'sv-format)

(defgroup sv-kit nil
  "SystemVerilog tooling: parser, linter and formatter."
  :group 'languages
  :prefix "sv-kit-")

(defcustom sv-kit-file-extensions '("sv" "svh" "v" "vh")
  "File extensions searched when building the project module table."
  :type '(repeat string)
  :group 'sv-kit)

(defcustom sv-kit-project-root-markers '(".git" "Makefile" "filelist.f")
  "Files or directories that mark the root of a hardware project."
  :type '(repeat string)
  :group 'sv-kit)

(defcustom sv-kit-format-on-save nil
  "When non-nil, `sv-kit-mode' formats the buffer before saving it."
  :type 'boolean
  :group 'sv-kit)

(defcustom sv-kit-use-indent-function t
  "When non-nil, `sv-kit-mode' takes over TAB with `sv-format-indent-line'."
  :type 'boolean
  :group 'sv-kit)

(defvar-local sv-kit--module-table nil
  "Cached table of the design units visible from this buffer.")

(defvar-local sv-kit--module-table-time nil
  "When `sv-kit--module-table' was last built.")

(defcustom sv-kit-module-table-ttl 30
  "Seconds a cached project module table stays fresh."
  :type 'integer
  :group 'sv-kit)


;;;; Project scanning

(defun sv-kit-project-root (&optional directory)
  "Return the project root above DIRECTORY, or DIRECTORY itself."
  (let ((directory (or directory default-directory)))
    (or (cl-some (lambda (marker) (locate-dominating-file directory marker))
                 sv-kit-project-root-markers)
        directory)))

(defun sv-kit-project-files (&optional root)
  "Return every Verilog source under ROOT."
  (let* ((root (or root (sv-kit-project-root)))
         (regexp (concat "\\.\\(?:"
                         (mapconcat #'regexp-quote sv-kit-file-extensions "\\|")
                         "\\)\\'")))
    (ignore-errors (directory-files-recursively root regexp))))

(defun sv-kit-module-table (&optional force)
  "Return the design units of the current project, caching the result.
With FORCE non-nil, rebuild the table even when the cache is fresh."
  (if (and (not force)
           sv-kit--module-table
           sv-kit--module-table-time
           (< (float-time (time-subtract (current-time)
                                         sv-kit--module-table-time))
              sv-kit-module-table-ttl))
      sv-kit--module-table
    (setq sv-kit--module-table-time (current-time))
    (setq sv-kit--module-table
          (sv-lint-build-module-table (sv-kit-project-files)))))


;;;; Linting

(defun sv-kit-diagnostics (&optional buffer)
  "Return the lint findings of BUFFER, the current one by default."
  (with-current-buffer (or buffer (current-buffer))
    (sv-lint-buffer nil (sv-kit-module-table))))

;;;###autoload
(defun sv-kit-lint (&optional buffer)
  "Lint BUFFER and show the findings in a `compilation-mode' buffer."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (name (or (buffer-file-name buffer) (buffer-name buffer)))
         (diagnostics (sv-kit-diagnostics buffer))
         (output (get-buffer-create "*sv-lint*")))
    (with-current-buffer output
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Linting %s\n\n" name))
        (dolist (diagnostic diagnostics)
          (insert (sv-diagnostic-format diagnostic) "\n"))
        (insert (format "\n%d finding(s).\n" (length diagnostics)))
        (goto-char (point-min)))
      (compilation-mode)
      (setq-local compilation-error-regexp-alist '(gnu)))
    (display-buffer output)
    (message "%d finding(s)" (length diagnostics))
    diagnostics))

;;;###autoload
(defun sv-kit-lint-project ()
  "Lint every Verilog source of the current project."
  (interactive)
  (let* ((files (sv-kit-project-files))
         (table (sv-lint-build-module-table files))
         (output (get-buffer-create "*sv-lint*"))
         (total 0))
    (with-current-buffer output
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Linting %d file(s) under %s\n\n"
                        (length files) (sv-kit-project-root)))
        (dolist (file files)
          (dolist (diagnostic (sv-lint-file file table))
            (setq total (1+ total))
            (insert (sv-diagnostic-format diagnostic) "\n")))
        (insert (format "\n%d finding(s).\n" total))
        (goto-char (point-min)))
      (compilation-mode)
      (setq-local compilation-error-regexp-alist '(gnu)))
    (display-buffer output)
    (message "%d finding(s) in %d file(s)" total (length files))))

(defun sv-kit--diagnostic-region (diagnostic)
  "Return the buffer region DIAGNOSTIC covers, as a cons of positions."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (sv-diagnostic-line diagnostic)))
    (let* ((start (min (point-max) (+ (point) (sv-diagnostic-col diagnostic))))
           (end (save-excursion
                  (goto-char start)
                  (if (re-search-forward "[A-Za-z0-9_$]+\\|.\\|\n"
                                         (line-end-position) t)
                      (match-end 0)
                    (min (point-max) (1+ start))))))
      (cons start (max end (1+ start))))))

(defun sv-kit-flymake-backend (report-fn &rest _args)
  "Report the SystemVerilog lint findings of this buffer to REPORT-FN."
  (let ((buffer (current-buffer)))
    (funcall
     report-fn
     (mapcar
      (lambda (diagnostic)
        (let ((region (sv-kit--diagnostic-region diagnostic)))
          (flymake-make-diagnostic
           buffer (car region) (cdr region)
           (cl-case (sv-diagnostic-severity diagnostic)
             (error :error)
             (warning :warning)
             (t :note))
           (format "%s [%s]"
                   (sv-diagnostic-message diagnostic)
                   (sv-diagnostic-rule diagnostic)))))
      (sv-kit-diagnostics buffer)))))


;;;; Navigation and templates

(defun sv-kit--unit-position (tree unit)
  "Return the buffer position where UNIT of TREE starts."
  (let ((vector (plist-get tree :significant))
        (index (plist-get unit :beg)))
    (if (and vector index (< index (length vector)))
        (sv-token-start (aref vector index))
      (point-min))))

(defun sv-kit-imenu-index ()
  "Build an imenu index of the design units in this buffer."
  (let* ((tree (sv-parse-buffer))
         (units '()) (instances '()) (blocks '()) (routines '()))
    (dolist (unit (plist-get tree :units))
      (push (cons (format "%s %s" (plist-get unit :type)
                          (or (plist-get unit :name) "?"))
                  (sv-kit--unit-position tree unit))
            units)
      (dolist (instance (sv-parse-collect unit 'instance))
        (push (cons (format "%s : %s"
                            (or (plist-get instance :name) "?")
                            (plist-get instance :module))
                    (sv-kit--unit-position tree instance))
              instances))
      (dolist (type '(function task))
        (dolist (node (sv-parse-collect unit type))
          (push (cons (format "%s %s" type (or (plist-get node :name) "?"))
                      (sv-kit--unit-position tree node))
                routines)))
      (dolist (node (sv-parse-collect unit 'always))
        (push (cons (format "%s @%d" (plist-get node :kind) (plist-get node :line))
                    (sv-kit--unit-position tree node))
              blocks)))
    (cl-remove-if-not
     #'cdr
     (list (cons "Units" (nreverse units))
           (cons "Instances" (nreverse instances))
           (cons "Subprograms" (nreverse routines))
           (cons "Blocks" (nreverse blocks))))))

;;;###autoload
(defun sv-kit-goto-unit ()
  "Jump to a design unit of this buffer, chosen by name."
  (interactive)
  (let* ((tree (sv-parse-buffer))
         (choices (mapcar (lambda (unit)
                            (cons (format "%s %s" (plist-get unit :type)
                                          (or (plist-get unit :name) "?"))
                                  (sv-kit--unit-position tree unit)))
                          (plist-get tree :units))))
    (if (null choices)
        (message "No design unit found")
      (let ((choice (completing-read "Design unit: " choices nil t)))
        (push-mark)
        (goto-char (cdr (assoc choice choices)))))))

(defun sv-kit-instance-template (unit &optional instance-name)
  "Return an instantiation of UNIT named INSTANCE-NAME as a string."
  (let* ((name (plist-get unit :name))
         (instance (or instance-name (concat "u_" name)))
         (params (plist-get unit :params))
         (ports (plist-get unit :ports))
         (width (apply #'max 1 (mapcar (lambda (port)
                                         (length (or (plist-get port :name) "")))
                                       ports)))
         (lines '()))
    (push (if params
              (format "%s #(" name)
            (format "%s %s (" name instance))
          lines)
    (when params
      (let ((param-width (apply #'max 1 (mapcar (lambda (param)
                                                  (length (plist-get param :name)))
                                                params))))
        (let ((rest params))
          (while rest
            (push (format "    .%s%s (%s)%s"
                          (plist-get (car rest) :name)
                          (make-string (- param-width
                                          (length (plist-get (car rest) :name)))
                                       ?\s)
                          (plist-get (car rest) :name)
                          (if (cdr rest) "," ""))
                  lines)
            (setq rest (cdr rest)))))
      (push (format "  ) %s (" instance) lines))
    (let ((rest ports))
      (while rest
        (let ((port (car rest)))
          (push (format "    .%s%s (%s)%s"
                        (plist-get port :name)
                        (make-string (- width (length (plist-get port :name))) ?\s)
                        (plist-get port :name)
                        (if (cdr rest) "," ""))
                lines))
        (setq rest (cdr rest))))
    (push "  );" lines)
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

;;;###autoload
(defun sv-kit-insert-instance (name)
  "Insert an instantiation template for the module NAME.
The module is looked up in the project, so its ports and parameters are
filled in for you."
  (interactive
   (let ((table (sv-kit-module-table)))
     (list (completing-read
            "Instantiate module: "
            (let ((names '()))
              (maphash (lambda (key value)
                         (when (and (listp value)
                                    (memq (plist-get value :type)
                                          '(module interface program)))
                           (push key names)))
                       table)
              (sort names #'string<))
            nil t))))
  (let* ((unit (gethash name (sv-kit-module-table))))
    (if (not (and (listp unit) (plist-get unit :name)))
        (user-error "No module named `%s' in this project" name)
      (let ((start (point)))
        (insert (sv-kit-instance-template unit))
        (indent-region start (point))))))


;;;; Minor mode

(defvar sv-kit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'sv-format-buffer)
    (define-key map (kbd "C-c C-r") #'sv-format-region)
    (define-key map (kbd "C-c C-l") #'sv-kit-lint)
    (define-key map (kbd "C-c C-i") #'sv-kit-insert-instance)
    (define-key map (kbd "C-c C-u") #'sv-kit-goto-unit)
    map)
  "Keymap of `sv-kit-mode'.")

(defun sv-kit--maybe-format ()
  "Format the buffer when `sv-kit-format-on-save' asks for it."
  (when sv-kit-format-on-save
    (sv-format-buffer))
  nil)

;;;###autoload
(define-minor-mode sv-kit-mode
  "Parser-backed linting, formatting and navigation for SystemVerilog.

\\{sv-kit-mode-map}"
  :lighter " SV"
  :keymap sv-kit-mode-map
  :group 'sv-kit
  (if sv-kit-mode
      (progn
        (when sv-kit-use-indent-function
          (setq-local indent-line-function #'sv-format-indent-line))
        (setq-local imenu-create-index-function #'sv-kit-imenu-index)
        (add-hook 'flymake-diagnostic-functions #'sv-kit-flymake-backend nil t)
        (add-hook 'before-save-hook #'sv-kit--maybe-format nil t)
        (when (bound-and-true-p flymake-mode) (flymake-start)))
    (kill-local-variable 'indent-line-function)
    (kill-local-variable 'imenu-create-index-function)
    (remove-hook 'flymake-diagnostic-functions #'sv-kit-flymake-backend t)
    (remove-hook 'before-save-hook #'sv-kit--maybe-format t)))

;;;###autoload
(define-minor-mode sv-kit-format-on-save-mode
  "Format SystemVerilog buffers with `sv-format-buffer' before saving."
  :lighter ""
  :group 'sv-kit
  (setq-local sv-kit-format-on-save sv-kit-format-on-save-mode))


;;;; Batch entry point

(defun sv-kit--batch-fail (message &rest arguments)
  "Print MESSAGE formatted with ARGUMENTS on stderr and exit with status 2."
  (princ (apply #'format (concat message "\n") arguments) #'external-debugging-output)
  (kill-emacs 2))

(defconst sv-kit--batch-usage
  "Usage: sv-kit <command> [options] FILE...

Commands:
  lint      report problems found in FILE...
  format    reformat FILE...
  parse     print a summary of the design units in FILE...

Options:
  --write            rewrite the files in place (format)
  --check            exit non-zero if a file is not formatted (format)
  --diff             print a unified diff instead of the result (format)
  --strict           exit non-zero on any finding, not just errors (lint)
  --enable=RULE      turn a rule on, repeatable (lint)
  --disable=RULE     turn a rule off, repeatable (lint)
  --max-line=N       column limit for line-too-long (lint)
  --indent=N         columns per indentation level (format)
  --no-align         do not align columns (format)
  --list-rules       print every rule with its default severity
  --help             print this message
"
  "Help text of the batch interface.")

(defun sv-kit--batch-option (argument)
  "Split ARGUMENT into its option name and value."
  (if (string-match "\\`--\\([^=]+\\)=\\(.*\\)\\'" argument)
      (cons (match-string 1 argument) (match-string 2 argument))
    (cons (string-remove-prefix "--" argument) nil)))

;;;###autoload
(defun sv-kit-batch ()
  "Run sv-kit from the command line.
Reads the sub-command, the options and the file names from `argv'."
  (let ((arguments (prog1 (or argv '()) (setq argv nil)))
        (command nil) (files '()) (write nil) (check nil) (diff nil)
        (strict nil))
    (when (or (null arguments) (member (car arguments) '("--help" "-h" "help")))
      (princ sv-kit--batch-usage)
      (kill-emacs 0))
    (setq command (pop arguments))
    (dolist (argument arguments)
      (if (not (string-prefix-p "--" argument))
          (push argument files)
        (let* ((option (sv-kit--batch-option argument))
               (name (car option))
               (value (cdr option)))
          (pcase name
            ("write" (setq write t))
            ("check" (setq check t))
            ("diff" (setq diff t))
            ("strict" (setq strict t))
            ("no-align" (setq sv-format-align '()))
            ("enable" (setq sv-lint-disabled-rules
                            (remq (intern value) sv-lint-disabled-rules)))
            ("disable" (push (intern value) sv-lint-disabled-rules))
            ("max-line" (setq sv-lint-max-line-length (string-to-number value)))
            ("indent" (setq sv-format-indent-offset (string-to-number value)))
            ("list-rules"
             (dolist (rule sv-lint-rule-alist)
               (princ (format "%-28s %-8s %s\n" (nth 0 rule) (nth 1 rule)
                              (nth 2 rule))))
             (kill-emacs 0))
            ("help" (princ sv-kit--batch-usage) (kill-emacs 0))
            (_ (sv-kit--batch-fail "sv-kit: unknown option %s" argument))))))
    (setq files (nreverse files))
    (unless files (sv-kit--batch-fail "sv-kit: no input files"))
    (dolist (file files)
      (unless (file-readable-p file)
        (sv-kit--batch-fail "sv-kit: cannot read %s" file)))
    (pcase command
      ("lint" (sv-kit--batch-lint files strict))
      ("format" (sv-kit--batch-format files write check diff))
      ("parse" (sv-kit--batch-parse files))
      (_ (sv-kit--batch-fail "sv-kit: unknown command %s" command)))))

(defun sv-kit--batch-lint (files strict)
  "Lint FILES, exiting non-zero on an error or, when STRICT, any finding."
  (let ((table (sv-lint-build-module-table files))
        (errors 0) (total 0))
    (dolist (file files)
      (dolist (diagnostic (sv-lint-file file table))
        (setq total (1+ total))
        (when (eq (sv-diagnostic-severity diagnostic) 'error)
          (setq errors (1+ errors)))
        (princ (concat (sv-diagnostic-format diagnostic) "\n"))))
    (princ (format "%d finding(s) in %d file(s), %d error(s)\n"
                   total (length files) errors))
    (kill-emacs (if (or (> errors 0) (and strict (> total 0))) 1 0))))

(defun sv-kit--batch-format (files write check diff)
  "Format FILES, honouring the WRITE, CHECK and DIFF flags."
  (let ((changed 0))
    (dolist (file files)
      (let* ((original (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-substring-no-properties (point-min) (point-max))))
             (formatted (sv-format-text original)))
        (cond
         ((equal original formatted) nil)
         (t
          (setq changed (1+ changed))
          (cond
           (check (princ (format "%s: needs formatting\n" file)))
           (write
            (with-temp-file file (insert formatted))
            (princ (format "%s: formatted\n" file)))
           (diff
            (let ((temporary (make-temp-file "sv-kit" nil ".sv" formatted)))
              (call-process "diff" nil t nil "-u" file temporary)
              (delete-file temporary)))
           (t (princ formatted)))))))
    (when (or check write)
      (princ (format "%d of %d file(s) %s\n" changed (length files)
                     (if write "formatted" "need formatting"))))
    (kill-emacs (if (and check (> changed 0)) 1 0))))

(defun sv-kit--batch-parse (files)
  "Print a summary of the design units found in FILES."
  (dolist (file files)
    (let ((tree (sv-parse-file file)))
      (princ (format "%s\n" file))
      (dolist (unit (plist-get tree :units))
        (princ (format "  %s %s  (%d parameter(s), %d port(s), %d item(s))\n"
                       (plist-get unit :type)
                       (or (plist-get unit :name) "?")
                       (length (plist-get unit :params))
                       (length (plist-get unit :ports))
                       (length (plist-get unit :items))))
        (dolist (port (plist-get unit :ports))
          (princ (format "    %-6s %-24s %s\n"
                         (or (plist-get port :dir) "")
                         (string-trim
                          (concat (or (plist-get port :datatype) "") " "
                                  (sv-parse-token-string (plist-get port :packed))))
                         (plist-get port :name)))))))
  (kill-emacs 0))

(provide 'sv-kit)

;;; sv-kit.el ends here
