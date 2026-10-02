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
  "Files or directories that mark the root-directory of a hardware project."
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

(defcustom sv-index-max-file-size (* 2 1024 1024)
  "Size in bytes beyond which a source file is left out of deep analysis.
A generated netlist can run to tens of megabytes, and parsing one in
Lisp takes long enough to freeze the session.  Above this limit a file
is not indexed, and `sv-mode' keeps the whole-buffer services -- the
lint pass, the user-type highlighting, completion, ElDoc and
parser-driven indentation -- out of its buffer too.  nil means no
limit."
  :type '(choice (integer :tag "Bytes") (const :tag "No limit" nil))
  :group 'sv-index)

(defcustom sv-index-scan-seconds 10
  "Seconds the walk listing a project's files may spend before it stops.
The walk only happens where git cannot list the checkout for us, and on
a networked filesystem it can otherwise churn for minutes with the
session frozen.  When the budget runs out, the files found so far are
what gets indexed.  nil means walk the whole tree however long it
takes."
  :type '(choice (number :tag "Seconds") (const :tag "No limit" nil))
  :group 'sv-index)

(defun sv-index-buffer-large-p (&optional buffer)
  "Return non-nil when BUFFER is too large to parse, lint or index.
BUFFER defaults to the current buffer.  See `sv-index-max-file-size'."
  (and sv-index-max-file-size
       (> (buffer-size (and buffer (get-buffer buffer)))
          sv-index-max-file-size)))

(cl-defstruct (sv-symbol (:constructor sv-symbol-create) (:copier nil))
  "One named thing found in a source file.
LINE is 1-based and COL is 0-based, so both a buffer and a file location
can be rebuilt from them."
  name kind line col file signature container)

(defvar sv-index--file-cache (make-hash-table :test #'equal)
  "Maps a file name to (MODIFICATION-TIME TREE SYMBOLS).")

(defvar sv-index--file-lists (make-hash-table :test #'equal)
  "Maps a project root to (TIMESTAMP . FILES).")

(defvar sv-index--projects (make-hash-table :test #'equal)
  "Maps a project root to (SIGNATURE . INDEX), the assembled index.")

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
  "Return (TREE . SYMBOLS) for BUFFER, reparsing only when it has changed.
A buffer larger than `sv-index-max-file-size' is never parsed and
yields (nil . nil): completion, ElDoc and the rest ask for this on
every pause, and each would otherwise freeze the session."
  (with-current-buffer (or buffer (current-buffer))
    (let ((tick (buffer-chars-modified-tick)))
      (unless (and sv-index--buffer-cache
                   (equal (car sv-index--buffer-cache) tick))
        (setq sv-index--buffer-cache
              (if (sv-index-buffer-large-p)
                  (list tick nil nil)
                (let ((tree (sv-parse-buffer)))
                  (list tick tree
                        (sv-index-tree-symbols tree (buffer-file-name)))))))
      (cons (nth 1 sv-index--buffer-cache) (nth 2 sv-index--buffer-cache)))))

(defun sv-index-buffer-symbols (&optional buffer)
  "Return the symbols declared in BUFFER."
  (cdr (sv-index-buffer buffer)))

(defun sv-index-file (file)
  "Return (TREE . SYMBOLS) for FILE, reparsing it only when it has changed.
A file larger than `sv-index-max-file-size' is never parsed and yields
\(nil . nil), so a generated netlist costs the index one stat instead of
a minutes-long parse."
  (let* ((attributes (file-attributes file))
         (time (and attributes (file-attribute-modification-time attributes)))
         (size (and attributes (file-attribute-size attributes)))
         (cached (gethash file sv-index--file-cache)))
    (cond
     ((and cached time (equal (nth 0 cached) time))
      (cons (nth 1 cached) (nth 2 cached)))
     ((and size sv-index-max-file-size (> size sv-index-max-file-size))
      (puthash file (list time nil nil) sv-index--file-cache)
      (cons nil nil))
     (t
      (let* ((tree (condition-case nil (sv-parse-file file) (error nil)))
             (symbols (and tree (sv-index-tree-symbols tree file))))
        (puthash file (list time tree symbols) sv-index--file-cache)
        (cons tree symbols))))))

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

(defun sv-index--root-marked-p (directory)
  "Return non-nil when DIRECTORY itself holds one of `sv-index-root-markers'."
  (cl-some (lambda (marker)
             (file-exists-p (expand-file-name marker directory)))
           sv-index-root-markers))

(defun sv-index--git-files (root-directory regexp limit)
  "Return up to LIMIT files under ROOT-DIRECTORY that git knows about.
Asking git reads the repository's index in milliseconds where walking a
large tree over NFS takes half a minute, so this is tried before
`sv-index--scan-tree'.  Tracked files come from the checkout and its
submodules, untracked ones from the top checkout alone, and files that
`.gitignore' hides -- usually build products -- are left out.  Return
nil when ROOT-DIRECTORY is not a git checkout or git is missing or
fails, so the caller walks the tree instead."
  (when (and (file-exists-p (expand-file-name ".git" root-directory))
             (executable-find "git"))
    (with-temp-buffer
      (let ((default-directory (file-name-as-directory root-directory)))
        (when (and (or (eql 0 (ignore-errors
                                (call-process "git" nil '(t nil) nil "ls-files"
                                              "-z" "--recurse-submodules")))
                       ;; An old git cannot recurse into submodules; the
                       ;; top checkout alone still beats walking the tree.
                       (progn (erase-buffer)
                              (eql 0 (ignore-errors
                                       (call-process "git" nil '(t nil) nil
                                                     "ls-files" "-z")))))
                   (eql 0 (ignore-errors
                            (call-process "git" nil '(t nil) nil "ls-files"
                                          "-z" "--others"
                                          "--exclude-standard"))))
          (let ((files '()) (count 0))
            (dolist (name (split-string (buffer-string) "\0" t))
              (when (and (< count limit) (string-match-p regexp name))
                (push (expand-file-name name root-directory) files)
                (setq count (1+ count))))
            (nreverse files)))))))

(defun sv-index--scan-tree (root-directory regexp limit)
  "Return up to about LIMIT files under ROOT-DIRECTORY matching REGEXP.
Unlike `directory-files-recursively', a directory that cannot be read is
skipped rather than aborting the walk, and hidden directories are not
entered at all -- `.git' or a container store under `.local' would
otherwise dominate the scan.  Symbolic links are never followed.  The
walk is breadth-first so that when LIMIT cuts it short, the files
nearest the root are the ones that survive; `sv-index-scan-seconds'
cuts it short the same way when a vast tree on a slow filesystem
would stall the session."
  (let ((level (list (directory-file-name (expand-file-name root-directory))))
        (files '())
        (count 0)
        (deadline (and sv-index-scan-seconds
                       (+ (float-time) sv-index-scan-seconds))))
    (while (and level (< count limit)
                (or (null deadline) (< (float-time) deadline)))
      (let ((next '()))
        (dolist (directory level)
          (when (and (< count limit)
                     (or (null deadline) (< (float-time) deadline)))
            (dolist (entry (condition-case nil
                               (directory-files-and-attributes
                                directory t "\\`[^.]")
                             (file-error nil)))
              (if (eq (file-attribute-type (cdr entry)) t)
                  (push (car entry) next)
                (when (string-match-p regexp (car entry))
                  (push (car entry) files)
                  (setq count (1+ count)))))))
        (setq level (nreverse next))))
    (nreverse files)))

(defun sv-index-project-files (&optional root-directory force)
  "Return the Verilog sources under ROOT, caching the listing briefly.
The tree is walked only when ROOT looks like a project, that is when it
holds one of `sv-index-root-markers'.  Anywhere else -- typically a file
opened straight in $HOME -- only the files sitting in ROOT itself are
returned, because recursing through an arbitrary directory can churn a
whole network filesystem before the first buffer is shown.  With FORCE
non-nil, scan the directory tree again."
  (let* ((root-directory (or root-directory (sv-index-root)))
         (cached (gethash root-directory sv-index--file-lists))
         (fresh (and cached
                     (< (float-time (time-subtract (current-time) (car cached)))
                        sv-index-file-list-ttl))))
    (if (and cached fresh (not force))
        (cdr cached)
      (let* ((regexp (concat "\\.\\(?:"
                             (mapconcat #'regexp-quote sv-index-file-extensions "\\|")
                             "\\)\\'"))
             (files (if (sv-index--root-marked-p root-directory)
                        (or (sv-index--git-files root-directory regexp
                                                 sv-index-max-files)
                            (sv-index--scan-tree root-directory regexp
                                                 sv-index-max-files))
                      (condition-case nil
                          (directory-files root-directory t regexp)
                        (file-error nil)))))
        (when (> (length files) sv-index-max-files)
          (setq files (cl-subseq files 0 sv-index-max-files)))
        (puthash root-directory (cons (current-time) files) sv-index--file-lists)
        files))))

(defun sv-index--signature (files)
  "Return a value that changes when any of FILES does."
  (mapcar (lambda (file)
            (cons file (file-attribute-modification-time
                        (file-attributes file))))
          files))

(defun sv-index--build (files)
  "Return the index of FILES as a plist."
  (let ((units (make-hash-table :test #'equal))
        (unit-files (make-hash-table :test #'equal))
        (symbols (make-hash-table :test #'equal))
        (types (make-hash-table :test #'equal)))
    (dolist (file files)
      (let* ((indexed (sv-index-file file))
             (tree (car indexed)))
        (dolist (unit (plist-get tree :units))
          (when (plist-get unit :name)
            (puthash (plist-get unit :name) unit units)
            (puthash (plist-get unit :name) file unit-files))
          (dolist (typedef (sv-parse-collect unit 'typedef))
            (when (plist-get typedef :name)
              (puthash (plist-get typedef :name) typedef types))))
        (dolist (symbol (cdr indexed))
          (push symbol (gethash (sv-symbol-name symbol) symbols)))))
    (list :units units :unit-files unit-files :symbols symbols
          :types types :files files)))

(defun sv-index-project (&optional root-directory force)
  "Return the index of the project under ROOT-DIRECTORY as a plist.
`:units\=' maps a design unit name to its node, `:unit-files\=' maps it to
the file it lives in, `:types\=' maps a type name to its typedef, and
`:symbols\=' maps any name to the symbols that carry it.

The assembled index is cached against the modification times of the
files it was built from, because the editor services ask for it many
times per keystroke and rebuilding it each time is what makes them
feel slow.  With FORCE non-nil, everything is read again."
  (let* ((root (or root-directory (sv-index-root)))
         (files (sv-index-project-files root force))
         (signature (sv-index--signature files))
         (cached (gethash root sv-index--projects)))
    (if (and cached (not force) (equal (car cached) signature))
        (cdr cached)
      (let ((index (sv-index--build files)))
        (puthash root (cons signature index) sv-index--projects)
        index))))

(defvar sv-index--build-state nil
  "State of the background build: (ROOT CALLBACKS FILES).
FILES is the work still to parse, or the symbol `list' before the
project has even been listed.")

(defvar sv-index--build-timer nil
  "Timer driving `sv-index--build-step' while a background build runs.")

(defun sv-index-project-ready-p (&optional root-directory)
  "Return non-nil once the project under ROOT-DIRECTORY has an index.
It may be stale, but `sv-index-project' revalidates that cheaply; what
readiness promises is that the expensive first parse of every source
file is already behind us."
  (and (gethash (or root-directory (sv-index-root)) sv-index--projects) t))

(defun sv-index-build-in-background (&optional root-directory callback)
  "Build the first index of the project under ROOT-DIRECTORY off the keyboard.
A timer lists the project and then parses a few files per tick, backing
off whenever input arrives, so a large design indexes itself while the
buffer stays usable.  CALLBACK, when non-nil, receives the index once it
is complete; when the index already exists it is called right away, and
a call made while a build is running just adds its callback to it."
  (let ((root (or root-directory (sv-index-root))))
    (cond
     ((sv-index-project-ready-p root)
      (when callback (funcall callback (sv-index-project root))))
     (sv-index--build-state
      (when callback (push callback (nth 1 sv-index--build-state))))
     (t
      (setq sv-index--build-state
            (list root (and callback (list callback)) 'list))
      (setq sv-index--build-timer
            (run-with-timer 0.1 0.1 #'sv-index--build-step))))))

(defun sv-index--build-step ()
  "Advance the background build by one bounded slice of work."
  (pcase-let ((`(,root ,callbacks ,files) sv-index--build-state))
    (cond
     ((eq files 'list)
      (setf (nth 2 sv-index--build-state) (sv-index-project-files root)))
     (files
      ;; Parse files for a bounded slice of time, dropping the one being
      ;; read the moment the user types; whatever is left waits for the
      ;; next tick.  The budget is time rather than a file count because
      ;; a single large file would otherwise hold the session for as
      ;; long as it takes to parse.
      (let ((deadline (+ (float-time) 0.5)))
        (while (and files (< (float-time) deadline)
                    (eq 'done (while-no-input (sv-index-file (car files))
                                              'done)))
          (setq files (cdr files))))
      (setf (nth 2 sv-index--build-state) files))
     (t
      (cancel-timer sv-index--build-timer)
      (setq sv-index--build-timer nil)
      (setq sv-index--build-state nil)
      (let ((index (sv-index-project root)))
        (dolist (callback (nreverse callbacks))
          (funcall callback index)))))))

(defun sv-index-unit (name &optional root-directory)
  "Return the design unit called NAME in the project under ROOT."
  (gethash name (plist-get (sv-index-project root-directory) :units)))

(defun sv-index-unit-file (name &optional root-directory)
  "Return the file that declares the design unit called NAME under ROOT."
  (gethash name (plist-get (sv-index-project root-directory) :unit-files)))

(defun sv-index-type (name &optional root-directory)
  "Return the typedef called NAME in the project under ROOT."
  (gethash name (plist-get (sv-index-project root-directory) :types)))

(defun sv-index-lookup (name &optional root-directory)
  "Return the project symbols called NAME under ROOT."
  (gethash name (plist-get (sv-index-project root-directory) :symbols)))

(defun sv-index-names (&optional root-directory)
  "Return every name the project under ROOT declares."
  (let ((names '()))
    (maphash (lambda (name _) (push name names))
             (plist-get (sv-index-project root-directory) :symbols))
    (sort names #'string<)))

(defun sv-index-lint-table (&optional root-directory)
  "Return the table the linter uses to check instances across files."
  (let ((table (make-hash-table :test #'equal))
        (index (sv-index-project root-directory)))
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

(defun sv-index-macros (&optional root-directory)
  "Return every macro the project under ROOT defines."
  (let ((names '()))
    (dolist (file (sv-index-project-files root-directory))
      (setq names (append (sv-index-tree-macros (car (sv-index-file file))) names)))
    (sort (delete-dups names) #'string<)))

(defun sv-index-invalidate (&optional file)
  "Drop the cached index of FILE, or of everything when FILE is nil."
  (interactive)
  (clrhash sv-index--projects)
  (if file
      (remhash file sv-index--file-cache)
    (clrhash sv-index--file-cache)
    (clrhash sv-index--file-lists)))

(provide 'sv-index)

;;; sv-index.el ends here
