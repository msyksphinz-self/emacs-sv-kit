;;; sv-mode.el --- Major mode for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; A major mode for Verilog and SystemVerilog built on the rest of sv-kit:
;; syntax highlighting, parser-driven indentation, on-the-fly linting
;; through Flymake, and an imenu index.  It needs no other Verilog package.
;;
;;   (require 'sv-mode)
;;
;; is enough; `.sv', `.svh', `.v' and `.vh' files then open in `sv-mode'.
;;
;; Highlighting is regexp-driven so that it stays fast while you type, but
;; the names of the types the file declares come from the parser, so a
;; `typedef' of your own is highlighted like a built-in type.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-format)
(require 'sv-kit)

(defgroup sv-mode nil
  "Major mode for editing SystemVerilog."
  :group 'languages
  :prefix "sv-mode-")

(defcustom sv-mode-enable-flymake t
  "When non-nil, turn Flymake on in `sv-mode' buffers so lint findings show up."
  :type 'boolean
  :group 'sv-mode)

(defcustom sv-mode-highlight-user-types t
  "When non-nil, highlight the types and enumeration literals the file declares."
  :type 'boolean
  :group 'sv-mode)

(defcustom sv-mode-electric-keywords t
  "When non-nil, re-indent the line as soon as a block-closing keyword is typed."
  :type 'boolean
  :group 'sv-mode)


;;;; Faces

(defface sv-mode-port-face
  '((t :inherit font-lock-constant-face))
  "Face for the port names of an instantiation, as in `.i_clk (clk)'."
  :group 'sv-mode)

(defface sv-mode-label-face
  '((t :inherit font-lock-constant-face))
  "Face for block labels, as in `begin : name' and `endmodule : name'."
  :group 'sv-mode)

(defface sv-mode-directive-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for compiler directives and macro uses, as in `\\=`ifdef'."
  :group 'sv-mode)

(defface sv-mode-instance-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the name given to a module instance."
  :group 'sv-mode)


;;;; Syntax

(defvar sv-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?$  "_"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?\" "\""    table)
    ;; An apostrophe introduces a literal such as 8'hff; it never quotes.
    (modify-syntax-entry ?'  "."     table)
    (modify-syntax-entry ?`  "."     table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"  table)
    (modify-syntax-entry ?\n "> b"   table)
    (dolist (character '(?+ ?- ?= ?% ?< ?> ?& ?| ?^ ?~ ?! ?? ?: ?\; ?, ?. ?# ?@))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "Syntax table used in `sv-mode'.")


;;;; Font lock

(defconst sv-mode--type-keywords
  (append sv-lexer-data-types sv-lexer-net-types
          '("signed" "unsigned" "struct" "union" "enum" "typedef" "packed"
            "virtual" "const" "static" "automatic" "genvar" "parameter"
            "localparam" "type" "var" "interconnect"))
  "Keywords highlighted as types.")

(defconst sv-mode--keywords
  (cl-set-difference sv-lexer-keywords sv-mode--type-keywords :test #'equal)
  "Keywords highlighted as keywords.")

(defconst sv-mode--identifier "[A-Za-z_][A-Za-z0-9_$]*"
  "Regexp matching a simple SystemVerilog identifier.")

(defconst sv-mode--number-regexp
  (concat "\\(?:\\_<[0-9][0-9_]*\\)?'[sS]?[bBoOdDhH][ \t]*[0-9a-fA-FxXzZ?_]+"
          "\\|'[01xXzZ]\\_>"
          "\\|\\_<[0-9][0-9_]*\\(?:\\.[0-9_]+\\)?\\(?:[eE][-+]?[0-9_]+\\)?"
          "\\(?:fs\\|ps\\|ns\\|us\\|ms\\)?\\_>")
  "Regexp matching a numeric literal.")

(defconst sv-mode--declaration-regexp
  (concat "\\(?:^\\|[(,]\\)[ \t]*"
          "\\(?:\\_<\\(?:input\\|output\\|inout\\|ref\\)\\_>[ \t]+\\)?"
          "\\(?:\\_<\\(?:var\\|const\\|static\\|automatic\\|virtual\\)\\_>[ \t]+\\)?"
          ;; The data type, built in or declared by the user.
          "\\(\\(?:" sv-mode--identifier "[ \t]*::[ \t]*\\)?"
          sv-mode--identifier "\\)"
          "\\(?:[ \t]+\\_<\\(?:signed\\|unsigned\\)\\_>\\)?"
          "\\(?:[ \t]*\\[[^]\n]*\\]\\)*"
          "[ \t]+\\(" sv-mode--identifier "\\)"
          "\\(?:[ \t]*\\[[^]\n]*\\]\\)*"
          ;; An instantiation would have a `(\=' here instead.
          "[ \t]*\\(?:[;,=)]\\|$\\)")
  "Regexp matching a declaration, capturing its data type and the name.")

(defconst sv-mode--parameter-regexp
  (concat "\\_<\\(?:parameter\\|localparam\\|genvar\\)\\_>[^;(\n]*?\\("
          sv-mode--identifier "\\)[ \t]*[=;,]")
  "Regexp matching a parameter declaration and capturing its name.")

(defconst sv-mode--instance-regexp
  (concat "^[ \t]*\\(" sv-mode--identifier "\\)"
          "\\(?:[ \t]*::[ \t]*" sv-mode--identifier "\\)?"
          "[ \t]*\\(?:#[ \t]*([^;]*?)[ \t]*\\)?"
          "[ \t]+\\(" sv-mode--identifier "\\)[ \t]*\\(?:\\[[^]\n]*\\][ \t]*\\)?("
          )
  "Regexp matching an instantiation, capturing the module and instance names.")

(defun sv-mode--in-comment-or-string-p (&optional position)
  "Return non-nil when POSITION, or point, sits in a comment or a string.
Both point and the match data are preserved: font-lock matchers call this
between finding a match and handing it over, and `syntax-ppss' would
otherwise clobber the search position they rely on."
  (save-excursion
    (save-match-data
      (let ((state (syntax-ppss (or position (point)))))
        (or (nth 3 state) (nth 4 state))))))

(defun sv-mode--match-instance (limit)
  "Search up to LIMIT for an instantiation that is not a declaration."
  (let ((found nil))
    (while (and (not found) (re-search-forward sv-mode--instance-regexp limit t))
      (unless (or (sv-mode--in-comment-or-string-p (match-beginning 1))
                  ;; `logic foo (' is not an instance, and neither is any
                  ;; other line that opens with a reserved word.
                  (sv-lexer-keyword-p (match-string 1))
                  (sv-lexer-keyword-p (match-string 2)))
        (setq found t)))
    found))

(defconst sv-mode--builtin-types
  (append sv-lexer-data-types sv-lexer-net-types)
  "Keywords that may open a declaration.")

(defun sv-mode--match-declaration (limit)
  "Search up to LIMIT for a declaration, skipping comments and strings."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward sv-mode--declaration-regexp limit t))
      (let ((type (match-string 1))
            (name (match-string 2))
            (resume (match-end 2)))
        (unless (or (sv-mode--in-comment-or-string-p (match-beginning 0))
                    ;; A reserved word may open a declaration only when it is
                    ;; a data type: `always_comb x = 1;\=' declares nothing.
                    (and (sv-lexer-keyword-p type)
                         (not (member type sv-mode--builtin-types)))
                    (sv-lexer-keyword-p name))
          (setq found t))
        ;; Resume right after the name rather than after the separator that
        ;; closed it: the next declaration of a port list needs that comma as
        ;; its own anchor.
        (save-match-data (goto-char resume))))
    found))

(defun sv-mode--match-parameter (limit)
  "Search up to LIMIT for a parameter or genvar declaration."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward sv-mode--parameter-regexp limit t))
      (unless (or (sv-mode--in-comment-or-string-p (match-beginning 0))
                  (sv-lexer-keyword-p (match-string 1)))
        (setq found t)))
    found))

(defvar-local sv-mode--user-type-keywords nil
  "Regexp matching the type names this buffer declares, or nil.")

(defvar-local sv-mode--user-enum-keywords nil
  "Regexp matching the enumeration literals this buffer declares, or nil.")

(defconst sv-mode-font-lock-keywords-1
  (list
   ;; Directives first: `\\=`else' must not be read as the keyword `else'.
   (list (concat "`" sv-mode--identifier) 0 ''sv-mode-directive-face)
   (list (concat "\\$" sv-mode--identifier) 0 'font-lock-builtin-face)
   (list (concat "\\_<" (regexp-opt sv-mode--keywords) "\\_>")
         0 'font-lock-keyword-face)
   (list (concat "\\_<" (regexp-opt sv-mode--type-keywords) "\\_>")
         0 'font-lock-type-face))
  "Minimal highlighting for `sv-mode'.")

(defconst sv-mode-font-lock-keywords-2
  (append
   (list
    ;; Design units and subprograms.
    (list (concat "\\_<\\(?:macro\\)?module\\_>\\|\\_<"
                  (regexp-opt '("interface" "package" "program" "class"
                                "primitive" "checker" "covergroup"))
                  "\\_>[ \t]+\\(?:\\_<\\(?:automatic\\|static\\|virtual\\)\\_>[ \t]+\\)?"
                  "\\(" sv-mode--identifier "\\)")
          '(1 font-lock-function-name-face nil t))
    (list (concat "\\_<module\\_>[ \t]+\\(" sv-mode--identifier "\\)")
          '(1 font-lock-function-name-face))
    (list (concat "\\_<\\(?:function\\|task\\)\\_>[^;(\n]*?\\("
                  sv-mode--identifier "\\)[ \t]*[(;]")
          '(1 font-lock-function-name-face))
    (list #'sv-mode--match-instance
          '(1 font-lock-type-face) '(2 'sv-mode-instance-face))
    (list #'sv-mode--match-declaration
          '(1 font-lock-type-face) '(2 font-lock-variable-name-face))
    (list #'sv-mode--match-parameter '(1 font-lock-variable-name-face))
    (list sv-mode--number-regexp 0 'font-lock-constant-face))
   sv-mode-font-lock-keywords-1)
  "Highlighting for `sv-mode' including the names a file defines.")

(defconst sv-mode-font-lock-keywords-3
  (append
   (list
    (list (concat "\\(\\.\\(?:" sv-mode--identifier "\\|\\*\\)\\)[ \t\n]*[(,)]")
          '(1 'sv-mode-port-face))
    (list (concat "^[ \t]*\\(\\." sv-mode--identifier "\\)") '(1 'sv-mode-port-face))
    (list (concat "\\_<\\(?:begin\\|end\\|fork\\|join\\|join_any\\|join_none\\|"
                  "endmodule\\|endinterface\\|endpackage\\|endprogram\\|"
                  "endclass\\|endfunction\\|endtask\\|endgenerate\\|endcase\\)"
                  "\\_>[ \t]*:[ \t]*\\(" sv-mode--identifier "\\)")
          '(1 'sv-mode-label-face))
    (list #'sv-mode--fontify-user-types '(0 font-lock-type-face))
    (list #'sv-mode--fontify-user-enums '(0 font-lock-constant-face)))
   sv-mode-font-lock-keywords-2)
  "Full highlighting for `sv-mode'.")

(defun sv-mode--fontify-with (regexp limit)
  "Match REGEXP up to LIMIT, skipping comments and strings."
  (let ((found nil))
    (while (and (not found) regexp (re-search-forward regexp limit t))
      (unless (sv-mode--in-comment-or-string-p (match-beginning 0))
        (setq found t)))
    found))

(defun sv-mode--fontify-user-types (limit)
  "Match up to LIMIT the type names this buffer declares."
  (sv-mode--fontify-with sv-mode--user-type-keywords limit))

(defun sv-mode--fontify-user-enums (limit)
  "Match up to LIMIT the enumeration literals this buffer declares."
  (sv-mode--fontify-with sv-mode--user-enum-keywords limit))

(defun sv-mode--names-regexp (names)
  "Return a symbol-anchored regexp matching NAMES, or nil when there are none."
  (and names (concat "\\_<" (regexp-opt (delete-dups names)) "\\_>")))

(defun sv-mode-update-user-types ()
  "Collect the types and enumeration literals this buffer declares.
They are then highlighted like built-in ones.  This runs when the mode
starts and after each save; call it by hand after adding a `typedef' if
you want the new name highlighted right away."
  (interactive)
  (when sv-mode-highlight-user-types
    (let ((types '()) (literals '()))
      (condition-case nil
          (let ((tree (sv-parse-buffer)))
            (dolist (unit (plist-get tree :units))
              (dolist (typedef (sv-parse-collect unit 'typedef))
                (when (plist-get typedef :name)
                  (push (plist-get typedef :name) types))
                (dolist (member (plist-get typedef :enum-members))
                  (push (plist-get member :name) literals)))))
        (error nil))
      (setq sv-mode--user-type-keywords (sv-mode--names-regexp types))
      (setq sv-mode--user-enum-keywords (sv-mode--names-regexp literals))
      (when (and font-lock-mode (derived-mode-p 'sv-mode))
        (font-lock-flush)))))


;;;; Indentation and movement

(defconst sv-mode--electric-keywords
  '("end" "endcase" "endmodule" "endinterface" "endpackage" "endprogram"
    "endclass" "endfunction" "endtask" "endgenerate" "endspecify" "endgroup"
    "else" "begin" "join" "join_any" "join_none" "default")
  "Keywords that re-indent their line as soon as they are typed.")

(defun sv-mode--electric-keyword ()
  "Re-indent the current line when a block-closing keyword has just been typed."
  (when (and sv-mode-electric-keywords
             electric-indent-mode
             (eq (char-syntax last-command-event) ?w))
    (let ((word (save-excursion
                  (let ((end (point)))
                    (skip-syntax-backward "w_")
                    (buffer-substring-no-properties (point) end)))))
      (when (and (member word sv-mode--electric-keywords)
                 (save-excursion
                   (skip-syntax-backward "w_")
                   (looking-back "^[ \t]*" (line-beginning-position))))
        (indent-according-to-mode)))))

(defconst sv-mode--block-openers
  '("begin" "case" "casex" "casez" "fork" "generate" "function" "task"
    "module" "macromodule" "interface" "package" "program" "class")
  "Keywords that open a block hideshow can fold.")

(defconst sv-mode--block-closers
  '("end" "endcase" "join" "join_any" "join_none" "endgenerate" "endfunction"
    "endtask" "endmodule" "endinterface" "endpackage" "endprogram" "endclass")
  "Keywords that close a foldable block.")

(defconst sv-mode--block-regexp
  (concat "\\_<" (regexp-opt (append sv-mode--block-openers
                                   sv-mode--block-closers))
          "\\_>")
  "Regexp matching either end of a foldable block.")

(defun sv-mode-forward-block (&optional _argument)
  "Move past the block that starts at point.
Used by hideshow, which has no way to know that `end\=' closes `begin\='."
  (let ((depth 0) (searching t))
    (while (and searching (re-search-forward sv-mode--block-regexp nil 'move))
      (unless (sv-mode--in-comment-or-string-p (match-beginning 0))
        (if (member (match-string-no-properties 0) sv-mode--block-openers)
            (setq depth (1+ depth))
          (setq depth (1- depth))
          (when (<= depth 0) (setq searching nil)))))))

(with-eval-after-load 'hideshow
  (unless (assq 'sv-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 (list 'sv-mode
                       (concat "\\_<" (regexp-opt sv-mode--block-openers) "\\_>")
                       (concat "\\_<" (regexp-opt sv-mode--block-closers) "\\_>")
                       "/[*/]"
                       #'sv-mode-forward-block
                       nil))))

(defconst sv-mode--defun-regexp
  (concat "^[ \t]*\\_<"
          (regexp-opt '("module" "macromodule" "interface" "package" "program"
                        "class" "primitive" "function" "task"))
          "\\_>")
  "Regexp matching the start of what `sv-mode' treats as a defun.")

(defun sv-mode-beginning-of-defun (&optional count)
  "Move backwards to the start of a design unit or subprogram, COUNT times."
  (interactive "p")
  (let ((count (or count 1)))
    (dotimes (_ (abs count))
      (if (> count 0)
          (progn (beginning-of-line)
                 (re-search-backward sv-mode--defun-regexp nil 'move))
        (end-of-line)
        (re-search-forward sv-mode--defun-regexp nil 'move)
        (beginning-of-line)))))

(defun sv-mode-end-of-defun (&optional _count)
  "Move forward past the end of the current design unit or subprogram."
  (interactive)
  (re-search-forward
   (concat "\\_<" (regexp-opt '("endmodule" "endinterface" "endpackage"
                                "endprogram" "endclass" "endprimitive"
                                "endfunction" "endtask"))
           "\\_>")
   nil 'move))


;;;; The mode

(defvar sv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'sv-mode-update-user-types)
    map)
  "Keymap of `sv-mode'.  It inherits the bindings of `sv-kit-mode'.")

;;;###autoload
(define-derived-mode sv-mode prog-mode "SystemVerilog"
  "Major mode for editing Verilog and SystemVerilog.

Highlighting, indentation, navigation and linting all come from sv-kit's
own parser, so no external Verilog package or tool is needed.

\\{sv-mode-map}"
  :syntax-table sv-mode-syntax-table
  :group 'sv-mode
  (setq-local font-lock-defaults
              '((sv-mode-font-lock-keywords-1
                 sv-mode-font-lock-keywords-1
                 sv-mode-font-lock-keywords-2
                 sv-mode-font-lock-keywords-3)
                nil nil nil nil))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)[ \t]*")
  (setq-local comment-multi-line t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local indent-line-function #'sv-format-indent-line)
  (setq-local indent-region-function #'sv-format-indent-region)
  (setq-local electric-indent-chars
              (append '(?\; ?\) ?\}) electric-indent-chars))
  (setq-local outline-regexp
              (concat "[ \t]*\\_<"
                      (regexp-opt '("module" "macromodule" "interface" "package"
                                    "program" "class" "function" "task"
                                    "generate" "always" "always_comb" "always_ff"
                                    "always_latch" "initial" "final"))
                      "\\_>"))
  (setq-local outline-level
              (lambda () (1+ (/ (current-indentation)
                                (max 1 sv-format-indent-offset)))))
  (setq-local beginning-of-defun-function #'sv-mode-beginning-of-defun)
  (setq-local end-of-defun-function #'sv-mode-end-of-defun)
  (setq-local add-log-current-defun-function #'sv-mode-current-defun)
  (add-hook 'post-self-insert-hook #'sv-mode--electric-keyword nil t)
  (add-hook 'after-save-hook #'sv-mode-update-user-types nil t)
  (sv-kit-mode 1)
  (sv-mode-update-user-types)
  (when (and sv-mode-enable-flymake (not noninteractive))
    (flymake-mode 1)))

(defun sv-mode-current-defun ()
  "Return the name of the design unit or subprogram around point."
  (save-excursion
    (when (re-search-backward
           (concat sv-mode--defun-regexp "[ \t]+\\(?:\\_<\\(?:automatic\\|static\\)"
                   "\\_>[ \t]+\\)?\\(" sv-mode--identifier "\\)")
           nil t)
      (match-string-no-properties 1))))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (concat "\\.\\(?:sv\\|svh\\|svi\\|v\\|vh\\)\\'") #'sv-mode))

(provide 'sv-mode)

;;; sv-mode.el ends here
