;;; sv-index.el --- Symbol index for SystemVerilog sources -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Turns the parse trees of a buffer and of the surrounding project into a
;; flat list of symbols -- design units, ports, parameters, signals, types,
;; enumeration literals, subprograms and instances -- each with a location
;; and a one-line signature.
;;
;; Completion, `xref' and ElDoc all read from here.  Results are cached per
;; file against its modification time, and the list of project files is
;; itself cached for a short while, so asking for the index on every
;; keystroke stays cheap.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)

(defgroup sv-index nil
  "Symbol index for SystemVerilog projects."
  :group 'tools
  :prefix "sv-index-")

(defcustom sv-index-file-extensions '("sv" "svh" "v" "vh")
  "File extensions scanned when indexing a project."
  :type '(repeat string)
  :group 'sv-index)

(defcustom sv-index-root-markers '(".git" "Makefile" "filelist.f")
  "Files or directories that mark the root of a hardware project."
  :type '(repeat string)
  :group 'sv-index)

(defcustom sv-index-file-list-ttl 60
  "Seconds a cached listing of the project's source files stays fresh."
  :type 'integer
  :group 'sv-index)

(defcustom sv-index-max-files 2000
  "Most files to index in one project."
  :type 'integer
  :group 'sv-index)

(cl-defstruct (sv-symbol (:constructor sv-symbol-create) (:copier nil))
  "One named thing found in a source file.
LINE is 1-based and COL is 0-based, so both a buffer and a file location
can be rebuilt from them."
  name kind line col file signature container)

(defvar sv-index--file-cache (make-hash-table :test #'equal)
  "Maps a file name to (MODIFICATION-TIME TREE SYMBOLS).")

(defvar sv-index--file-lists (make-hash-table :test #'equal)
  "Maps a project root to (TIMESTAMP . FILES).")

(defvar-local sv-index--buffer-cache nil
  "Cached (TICK TREE SYMBOLS) for this buffer.")


;;;; Signatures

(defun sv-index--join (&rest parts)
  "Join the non-empty PARTS with single spaces."
  (mapconcat #'identity
             (cl-remove-if #'string-empty-p
                           (mapcar (lambda (part) (string-trim (or part ""))) parts))
             " "))

(defun sv-index--dimensions (tokens)
  "Return the dimension TOKENS spelled the way the source had them."
  (if tokens (sv-parse-token-string tokens) ""))

(defun sv-index--declarator-signature (declarator &optional prefix)
  "Render DECLARATOR, a port or variable, with an optional PREFIX."
  (sv-index--join prefix
                  (plist-get declarator :datatype)
                  (sv-index--dimensions (plist-get declarator :packed))
                  (plist-get declarator :name)
                  (sv-index--dimensions (plist-get declarator :unpacked))))

(defun sv-index--unit-signature (unit)
  "Render the header of the design UNIT on one line."
  (sv-index--join
   (symbol-name (plist-get unit :type))
   (plist-get unit :name)
   (when (plist-get unit :params)
     (format "#(%s)" (mapconcat (lambda (param) (plist-get param :name))
                                (plist-get unit :params) ", ")))
   (when (plist-get unit :ports)
     (format "(%s)" (mapconcat (lambda (port) (plist-get port :name))
                               (plist-get unit :ports) ", ")))))

(defun sv-index--subprogram-signature (node)
  "Render the header of the function or task NODE."
  (sv-index--join
   (symbol-name (plist-get node :type))
   (plist-get node :name)
   (format "(%s)" (mapconcat (lambda (arg) (sv-index--declarator-signature arg))
                             (plist-get node :args) ", "))))

(defun sv-index--record-signature (record)
  "Render the declaration RECORD as a single line of source."
  (let ((kind (plist-get record :kind))
        (name (plist-get record :name))
        (node (plist-get record :node))
        (declarator (or (plist-get record :decl) (plist-get record :port))))
    (cl-case kind
      (port (sv-index--declarator-signature
             declarator (symbol-name (or (plist-get record :dir) 'port))))
      ((var net) (sv-index--declarator-signature declarator))
      (param (sv-index--join
              (symbol-name (or (plist-get declarator :kind) 'parameter))
              (plist-get declarator :datatype)
              name
              (when (plist-get declarator :init)
                (concat "= " (sv-parse-token-string (plist-get declarator :init))))))
      (typedef (or (plist-get node :text) (concat "typedef " name)))
      (enum (format "%s (enumeration literal)" name))
      ((task function) (sv-index--subprogram-signature node))
      (instance (sv-index--join (plist-get record :module) name))
      (genvar (concat "genvar " name))
      (loopvar (concat "loop variable " name))
      (label (concat "block label " name))
      (arg (sv-index--declarator-signature declarator))
      (t name))))


;;;; Building the index

(defun sv-index--record-column (record)
  "Return the column RECORD starts at, or 0."
  (let ((token (plist-get record :token)))
    (if token (sv-token-col token) 0)))

(defun sv-index-unit-symbols (unit file)
  "Return every symbol the design UNIT of FILE declares, the unit included."
  (let* ((name (plist-get unit :name))
         (symbols (list (sv-symbol-create
                         :name (or name "")
                         :kind (plist-get unit :type)
                         :line (plist-get unit :line)
                         :col 0
                         :file file
                         :signature (sv-index--unit-signature unit)
                         :container nil))))
    (dolist (record (sv-parse-declarations unit))
      (push (sv-symbol-create :name (plist-get record :name)
                              :kind (plist-get record :kind)
                              :line (plist-get record :line)
                              :col (sv-index--record-column record)
                              :file file
                              :signature (sv-index--record-signature record)
                              :container name)
            symbols))
    (nreverse symbols)))

(defun sv-index-tree-symbols (tree file)
  "Return every symbol of the parse TREE of FILE."
  (let ((symbols '()))
    (dolist (unit (plist-get tree :units))
      (setq symbols (append symbols (sv-index-unit-symbols unit file))))
    (cl-remove-if (lambda (symbol) (string-empty-p (sv-symbol-name symbol)))
                  symbols)))

(defun sv-index-buffer (&optional buffer)
  "Return (TREE . SYMBOLS) for BUFFER, reparsing only when it has changed."
  (with-current-buffer (or buffer (current-buffer))
    (let ((tick (buffer-chars-modified-tick)))
      (unless (and sv-index--buffer-cache
                   (equal (car sv-index--buffer-cache) tick))
        (let ((tree (sv-parse-buffer)))
          (setq sv-index--buffer-cache
                (list tick tree (sv-index-tree-symbols tree (buffer-file-name))))))
      (cons (nth 1 sv-index--buffer-cache) (nth 2 sv-index--buffer-cache)))))

(defun sv-index-buffer-symbols (&optional buffer)
  "Return the symbols declared in BUFFER."
  (cdr (sv-index-buffer buffer)))

(defun sv-index-file (file)
  "Return (TREE . SYMBOLS) for FILE, reparsing it only when it has changed."
  (let* ((attributes (file-attributes file))
         (time (and attributes (file-attribute-modification-time attributes)))
         (cached (gethash file sv-index--file-cache)))
    (if (and cached time (equal (nth 0 cached) time))
        (cons (nth 1 cached) (nth 2 cached))
      (let* ((tree (condition-case nil (sv-parse-file file) (error nil)))
             (symbols (and tree (sv-index-tree-symbols tree file))))
        (puthash file (list time tree symbols) sv-index--file-cache)
        (cons tree symbols)))))

(defun sv-index-root (&optional directory)
  "Return the project root above DIRECTORY, or DIRECTORY itself.
When several markers match, the closest one wins, so a nested project
is not swallowed by the repository around it."
  (let* ((directory (or directory default-directory))
         (candidates (delq nil (mapcar (lambda (marker)
                                         (locate-dominating-file directory marker))
                                       sv-index-root-markers))))
    (expand-file-name
     (or (car (sort candidates (lambda (a b) (> (length a) (length b)))))
         directory))))

(defun sv-index-project-files (&optional root force)
  "Return the Verilog sources under ROOT, caching the listing briefly.
With FORCE non-nil, scan the directory tree again."
  (let* ((root (or root (sv-index-root)))
         (cached (gethash root sv-index--file-lists))
         (fresh (and cached
                     (< (float-time (time-subtract (current-time) (car cached)))
                        sv-index-file-list-ttl))))
    (if (and cached fresh (not force))
        (cdr cached)
      (let* ((regexp (concat "\\.\\(?:"
                             (mapconcat #'regexp-quote sv-index-file-extensions "\\|")
                             "\\)\\'"))
             (files (ignore-errors (directory-files-recursively root regexp))))
        (when (> (length files) sv-index-max-files)
          (setq files (cl-subseq files 0 sv-index-max-files)))
        (puthash root (cons (current-time) files) sv-index--file-lists)
        files))))

(defun sv-index-project (&optional root force)
  "Return the index of the project under ROOT as a plist.
`:units' maps a design unit name to its node, `:unit-files' maps it to the
file it lives in, and `:symbols' maps any name to the symbols that carry
it.  With FORCE non-nil, rescan the file list as well."
  (let ((units (make-hash-table :test #'equal))
        (unit-files (make-hash-table :test #'equal))
        (symbols (make-hash-table :test #'equal))
        (files (sv-index-project-files root force)))
    (dolist (file files)
      (let* ((indexed (sv-index-file file))
             (tree (car indexed)))
        (dolist (unit (plist-get tree :units))
          (when (plist-get unit :name)
            (puthash (plist-get unit :name) unit units)
            (puthash (plist-get unit :name) file unit-files)))
        (dolist (symbol (cdr indexed))
          (push symbol (gethash (sv-symbol-name symbol) symbols)))))
    (list :units units :unit-files unit-files :symbols symbols :files files)))

(defun sv-index-unit (name &optional root)
  "Return the design unit called NAME in the project under ROOT."
  (gethash name (plist-get (sv-index-project root) :units)))

(defun sv-index-unit-file (name &optional root)
  "Return the file that declares the design unit called NAME under ROOT."
  (gethash name (plist-get (sv-index-project root) :unit-files)))

(defun sv-index-lookup (name &optional root)
  "Return the project symbols called NAME under ROOT."
  (gethash name (plist-get (sv-index-project root) :symbols)))

(defun sv-index-names (&optional root)
  "Return every name the project under ROOT declares."
  (let ((names '()))
    (maphash (lambda (name _) (push name names))
             (plist-get (sv-index-project root) :symbols))
    (sort names #'string<)))

(defun sv-index-lint-table (&optional root)
  "Return the table the linter uses to check instances across files."
  (let ((table (make-hash-table :test #'equal))
        (index (sv-index-project root)))
    (maphash (lambda (name unit) (puthash name unit table))
             (plist-get index :units))
    (maphash (lambda (name symbols)
               (unless (gethash name table)
                 (when (cl-some (lambda (symbol)
                                  (memq (sv-symbol-kind symbol)
                                        '(typedef enum function task param)))
                                symbols)
                   (puthash name 'name table))))
             (plist-get index :symbols))
    table))

(defun sv-index-tree-macros (tree)
  "Return the macro names `\=`define\=' introduces in TREE."
  (let ((names '()))
    (dolist (token (plist-get tree :tokens))
      (when (and (eq (sv-token-type token) 'directive)
                 (string-match "\\`\\=`define[ \t]+\\([A-Za-z_][A-Za-z0-9_$]*\\)"
                               (sv-token-text token)))
        (push (match-string 1 (sv-token-text token)) names)))
    (nreverse names)))

(defun sv-index-macros (&optional root)
  "Return every macro the project under ROOT defines."
  (let ((names '()))
    (dolist (file (sv-index-project-files root))
      (setq names (append (sv-index-tree-macros (car (sv-index-file file))) names)))
    (sort (delete-dups names) #'string<)))

(defun sv-index-invalidate (&optional file)
  "Drop the cached index of FILE, or of everything when FILE is nil."
  (interactive)
  (if file
      (remhash file sv-index--file-cache)
    (clrhash sv-index--file-cache)
    (clrhash sv-index--file-lists)))

(provide 'sv-index)

;;; sv-index.el ends here
