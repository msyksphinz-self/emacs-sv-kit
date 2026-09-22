;;; sv-hierarchy.el --- Browse the instance tree of a design -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Reads the design out of the project index and shows what instantiates
;; what.  With no argument it works out the tops for itself -- the modules
;; nothing else instantiates -- so opening the tree asks nothing.
;;
;; The tree is built as it is opened, one level at a time.  A design of any
;; size has more instances than anybody wants to read, and some of them
;; appear under a dozen different parents, so drawing the whole thing up
;; front is both slow and useless.  A `+' marks a line that has something
;; under it.
;;
;;   TAB   open or close the line under point
;;   RET   go to the instantiation this line stands for
;;   o     go to the definition of the module it names
;;   c     say what else instantiates that module
;;   *     open everything under point, as far as `sv-hierarchy-max-depth'
;;   g     read the design again

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-index)

(defgroup sv-hierarchy nil
  "Browsing the instance tree of a SystemVerilog design."
  :group 'tools
  :prefix "sv-hierarchy-")

(defcustom sv-hierarchy-max-depth 16
  "How many levels `sv-hierarchy-expand-all' opens at once."
  :type 'integer
  :group 'sv-hierarchy)

(defcustom sv-hierarchy-indent 2
  "Columns each level of the tree is indented by."
  :type 'integer
  :group 'sv-hierarchy)

(defcustom sv-hierarchy-show-files t
  "When non-nil, name the file each module comes from."
  :type 'boolean
  :group 'sv-hierarchy)

(defface sv-hierarchy-instance-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the instance name on a tree line."
  :group 'sv-hierarchy)

(defface sv-hierarchy-module-face
  '((t :inherit font-lock-function-name-face))
  "Face for the module name on a tree line."
  :group 'sv-hierarchy)

(defface sv-hierarchy-note-face
  '((t :inherit shadow))
  "Face for the file name and the notes on a tree line."
  :group 'sv-hierarchy)

(defvar-local sv-hierarchy--roots nil
  "The modules this buffer was asked to show, or nil for the whole design.")


;;;; Reading the design

(defun sv-hierarchy--instantiated ()
  "Return a table of the modules the project instantiates, and by whom."
  (let ((table (make-hash-table :test #'equal))
        (index (sv-index-project)))
    (maphash
     (lambda (name unit)
       (dolist (instance (sv-parse-collect unit 'instance))
         (dolist (sibling (plist-get instance :siblings))
           (push (list :parent name
                       :instance (plist-get sibling :name)
                       :line (plist-get sibling :line)
                       :file (gethash name (plist-get index :unit-files)))
                 (gethash (plist-get instance :module) table)))))
     (plist-get index :units))
    table))

(defun sv-hierarchy-tops ()
  "Return the modules of the project that nothing else instantiates."
  (let ((instantiated (sv-hierarchy--instantiated))
        (tops '()))
    (maphash (lambda (name unit)
               (when (and (memq (plist-get unit :type) '(module program))
                          ;; A module that only instantiates itself is still a
                          ;; top: nothing above it uses it.
                          (null (cl-remove-if
                                 (lambda (site)
                                   (equal (plist-get site :parent) name))
                                 (gethash name instantiated))))
                 (push name tops)))
             (plist-get (sv-index-project) :units))
    (sort tops #'string<)))

(defun sv-hierarchy-callers (module)
  "Return where the project instantiates MODULE."
  (gethash module (sv-hierarchy--instantiated)))

(defun sv-hierarchy--children (module)
  "Return what MODULE instantiates, in source order."
  (let ((unit (sv-index-unit module))
        (file (sv-index-unit-file module))
        (children '()))
    (dolist (instance (sv-parse-collect unit 'instance))
      (dolist (sibling (plist-get instance :siblings))
        (push (list :module (plist-get instance :module)
                    :instance (plist-get sibling :name)
                    :line (plist-get sibling :line)
                    :file file)
              children)))
    (nreverse children)))


;;;; Drawing one line

(defun sv-hierarchy--note (text)
  "Return TEXT as a dimmed note."
  (propertize text 'face 'sv-hierarchy-note-face))

(defun sv-hierarchy--insert-line (child depth path)
  "Insert the tree line for CHILD at DEPTH, below the ancestors in PATH."
  (let* ((module (plist-get child :module))
         (instance (plist-get child :instance))
         (unit (sv-index-unit module))
         (definition-file (sv-index-unit-file module))
         (note (cond ((null unit) "(not in this project)")
                     ((member module path) "(recursive)")))
         (expandable (and (null note) (sv-hierarchy--children module)))
         (start (point)))
    (insert (make-string (* sv-hierarchy-indent depth) ?\s))
    (insert (if expandable "+ " "  "))
    (when instance
      (insert (propertize instance 'face 'sv-hierarchy-instance-face) " : "))
    (insert (propertize module 'face 'sv-hierarchy-module-face))
    (cond
     (note (insert " " (sv-hierarchy--note note)))
     ((and sv-hierarchy-show-files definition-file)
      (insert " " (sv-hierarchy--note
                   (concat "-- " (file-name-nondirectory definition-file))))))
    (add-text-properties
     start (point)
     (list 'sv-hierarchy-module module
           'sv-hierarchy-depth depth
           'sv-hierarchy-path path
           'sv-hierarchy-expandable (and expandable t)
           'sv-hierarchy-expanded nil
           'sv-hierarchy-definition (and definition-file
                                         (cons definition-file
                                               (plist-get unit :line)))
           'sv-hierarchy-instantiation (and (plist-get child :file)
                                            (cons (plist-get child :file)
                                                  (plist-get child :line)))))
    (insert "\n")))

(defun sv-hierarchy--set-marker (position character)
  "Put CHARACTER at POSITION, keeping the text properties that were there."
  (let ((inhibit-read-only t)
        (properties (text-properties-at position)))
    (save-excursion
      (goto-char position)
      (delete-char 1)
      (insert (apply #'propertize (char-to-string character) properties)))))

(defun sv-hierarchy--marker-position (start depth)
  "Return where the marker of the line at START, indented to DEPTH, sits."
  (+ start (* sv-hierarchy-indent depth)))


;;;; Opening and closing

(defun sv-hierarchy--expand ()
  "Insert the children of the line at point.  Return how many were added."
  (let* ((start (line-beginning-position))
         (module (get-text-property start 'sv-hierarchy-module))
         (depth (get-text-property start 'sv-hierarchy-depth))
         (path (get-text-property start 'sv-hierarchy-path))
         (children (and module (sv-hierarchy--children module))))
    (when children
      (sv-hierarchy--set-marker (sv-hierarchy--marker-position start depth) ?-)
      (let ((inhibit-read-only t))
        (put-text-property start (line-end-position)
                           'sv-hierarchy-expanded t)
        (save-excursion
          (forward-line 1)
          (dolist (child children)
            (sv-hierarchy--insert-line child (1+ depth) (cons module path))))))
    (length children)))

(defun sv-hierarchy--collapse ()
  "Remove the sub-tree below the line at point."
  (let* ((start (line-beginning-position))
         (depth (get-text-property start 'sv-hierarchy-depth))
         (inhibit-read-only t)
         (from (save-excursion (forward-line 1) (point)))
         (to from))
    (save-excursion
      (goto-char from)
      (while (and (not (eobp))
                  (let ((child (get-text-property (point) 'sv-hierarchy-depth)))
                    (and child (> child depth))))
        (forward-line 1)
        (setq to (point))))
    (when (> to from) (delete-region from to))
    (sv-hierarchy--set-marker (sv-hierarchy--marker-position start depth) ?+)
    (put-text-property start (line-end-position) 'sv-hierarchy-expanded nil)))

(defun sv-hierarchy-toggle ()
  "Open the line under point, or close it if it is already open."
  (interactive)
  (let ((start (line-beginning-position)))
    (cond
     ((not (get-text-property start 'sv-hierarchy-expandable))
      (message "Nothing under this line"))
     ((get-text-property start 'sv-hierarchy-expanded) (sv-hierarchy--collapse))
     (t (sv-hierarchy--expand)))))

(defun sv-hierarchy-expand-all ()
  "Open everything under point, down to `sv-hierarchy-max-depth' levels."
  (interactive)
  (let* ((start (line-beginning-position))
         (base (or (get-text-property start 'sv-hierarchy-depth) 0))
         (limit (+ base sv-hierarchy-max-depth))
         (added 0)
         (working t))
    (save-excursion
      (while working
        (setq working nil)
        (goto-char start)
        (while (and (not (eobp))
                    (let ((depth (get-text-property (point)
                                                    'sv-hierarchy-depth)))
                      (or (= (point) start) (and depth (> depth base)))))
          (let ((depth (get-text-property (point) 'sv-hierarchy-depth)))
            (when (and (get-text-property (point) 'sv-hierarchy-expandable)
                       (not (get-text-property (point) 'sv-hierarchy-expanded))
                       depth (< depth limit))
              (setq added (+ added (sv-hierarchy--expand)))
              (setq working t)))
          (forward-line 1))))
    (message "Opened %d instance%s" added (if (= added 1) "" "s"))))


;;;; Visiting the source

(defun sv-hierarchy--visit (place)
  "Visit PLACE, a cons of a file and a line."
  (if (null place)
      (user-error "Nothing to go to on this line")
    (find-file-other-window (car place))
    (goto-char (point-min))
    (forward-line (1- (or (cdr place) 1)))
    (back-to-indentation)))

(defun sv-hierarchy-visit-instantiation ()
  "Go to the instantiation the line at point stands for."
  (interactive)
  (sv-hierarchy--visit
   (or (get-text-property (line-beginning-position) 'sv-hierarchy-instantiation)
       (get-text-property (line-beginning-position) 'sv-hierarchy-definition))))

(defun sv-hierarchy-visit-definition ()
  "Go to the definition of the module the line at point names."
  (interactive)
  (sv-hierarchy--visit
   (get-text-property (line-beginning-position) 'sv-hierarchy-definition)))

(defun sv-hierarchy-callers-at-point ()
  "Report what else instantiates the module the line at point names."
  (interactive)
  (let* ((module (get-text-property (line-beginning-position)
                                    'sv-hierarchy-module))
         (sites (and module (sv-hierarchy-callers module))))
    (cond
     ((null module) (user-error "No module on this line"))
     ((null sites) (message "Nothing in this project instantiates `%s'" module))
     (t (message "`%s' is instantiated by %s" module
                 (mapconcat (lambda (site)
                              (format "%s.%s (%s:%d)"
                                      (plist-get site :parent)
                                      (plist-get site :instance)
                                      (file-name-nondirectory
                                       (or (plist-get site :file) "?"))
                                      (or (plist-get site :line) 0)))
                            sites ", "))))))


;;;; The tree buffer

(defun sv-hierarchy--insert-tree (roots)
  "Write one line per module of ROOTS, each closed."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (null roots)
        (insert "No module found under "
                (abbreviate-file-name (sv-index-root)) "\n")
      (dolist (module roots)
        (sv-hierarchy--insert-line (list :module module) 0 '())))
    (goto-char (point-min))))

(defun sv-hierarchy-refresh ()
  "Read the design again and redraw the tree."
  (interactive)
  (sv-index-invalidate)
  (let ((line (line-number-at-pos)))
    (sv-hierarchy--insert-tree (or sv-hierarchy--roots (sv-hierarchy-tops)))
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar sv-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'sv-hierarchy-toggle)
    (define-key map (kbd "RET") #'sv-hierarchy-visit-instantiation)
    (define-key map (kbd "o")   #'sv-hierarchy-visit-definition)
    (define-key map (kbd "c")   #'sv-hierarchy-callers-at-point)
    (define-key map (kbd "*")   #'sv-hierarchy-expand-all)
    (define-key map (kbd "g")   #'sv-hierarchy-refresh)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    map)
  "Keymap of `sv-hierarchy-mode'.")

(define-derived-mode sv-hierarchy-mode special-mode "SV-Hierarchy"
  "Major mode for the design tree of a SystemVerilog project.

\\{sv-hierarchy-mode-map}"
  (setq-local truncate-lines t))

;;;###autoload
(defun sv-hierarchy (&optional module)
  "Show the instance tree of the design around the current buffer.
With no MODULE the tops are worked out -- the modules nothing else
instantiates -- and each is listed.  With a prefix argument, or called
from Lisp with MODULE, only that module is listed.  Either way the tree
opens one level at a time; TAB opens a line and `*' opens everything
under it."
  (interactive
   (list (when current-prefix-arg
           (let ((names '()))
             (maphash (lambda (name unit)
                        (when (memq (plist-get unit :type)
                                    '(module interface program))
                          (push name names)))
                      (plist-get (sv-index-project) :units))
             (completing-read "Tree of module: " (sort names #'string<) nil t)))))
  (let* ((roots (if module (list module) (sv-hierarchy-tops)))
         (buffer (get-buffer-create "*sv-hierarchy*")))
    (with-current-buffer buffer
      (sv-hierarchy-mode)
      (setq sv-hierarchy--roots (and module roots))
      (sv-hierarchy--insert-tree roots))
    (pop-to-buffer buffer)
    (message "%d top-level module%s; TAB opens one" (length roots)
             (if (= (length roots) 1) "" "s"))))

;;;###autoload
(defalias 'sv-kit-hierarchy #'sv-hierarchy
  "Kept for the name this command used to have.")

(provide 'sv-hierarchy)

;;; sv-hierarchy.el ends here
