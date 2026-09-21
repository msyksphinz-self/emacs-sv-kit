;;; sv-lexer.el --- Tokenizer for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; A tolerant, single-pass tokenizer for Verilog-2001 / SystemVerilog-2017
;; source.  It never signals on malformed input: anything it cannot classify
;; becomes a token of type `unknown', so that the parser, the linter and the
;; formatter can all keep working on files that contain macros, vendor
;; pragmas or plain syntax errors.
;;
;; Trivia (whitespace, comments, attributes) is kept in the token stream so
;; that the formatter can reproduce a file byte for byte; `sv-lex-significant'
;; strips it for the parser.

;;; Code:

(require 'cl-lib)

(cl-defstruct (sv-token (:constructor sv-token--create) (:copier nil))
  "A single lexical token.
TYPE is one of `ws', `comment', `attribute', `string', `number',
`ident', `keyword', `sysfunc', `directive', `operator', `punct' or
`unknown'.  START and END are buffer positions, LINE is 1-based and
COL is 0-based."
  type text start end line col)

(defconst sv-lexer-keywords
  '("accept_on" "alias" "always" "always_comb" "always_ff" "always_latch" "and"
    "assert" "assign" "assume" "automatic" "before" "begin" "bind" "bins"
    "binsof" "bit" "break" "buf" "bufif0" "bufif1" "byte" "case" "casex"
    "casez" "cell" "chandle" "checker" "class" "clocking" "cmos" "config"
    "const" "constraint" "context" "continue" "cover" "covergroup" "coverpoint"
    "cross" "deassign" "default" "defparam" "design" "disable" "dist" "do"
    "edge" "else" "end" "endcase" "endchecker" "endclass" "endclocking"
    "endconfig" "endfunction" "endgenerate" "endgroup" "endinterface"
    "endmodule" "endpackage" "endprimitive" "endprogram" "endproperty"
    "endsequence" "endspecify" "endtable" "endtask" "enum" "event" "eventually"
    "expect" "export" "extends" "extern" "final" "first_match" "for" "force"
    "foreach" "forever" "fork" "forkjoin" "function" "generate" "genvar"
    "global" "highz0" "highz1" "if" "iff" "ifnone" "ignore_bins"
    "illegal_bins" "implements" "implies" "import" "incdir" "include"
    "initial" "inout" "input" "inside" "instance" "int" "integer"
    "interconnect" "interface" "intersect" "join" "join_any" "join_none"
    "large" "let" "liblist" "library" "local" "localparam" "logic" "longint"
    "macromodule" "matches" "medium" "modport" "module" "nand" "negedge"
    "nettype" "new" "nexttime" "nmos" "nor" "noshowcancelled" "not" "notif0"
    "notif1" "null" "or" "output" "package" "packed" "parameter" "pmos"
    "posedge" "primitive" "priority" "program" "property" "protected" "pull0"
    "pull1" "pulldown" "pullup" "pulsestyle_ondetect" "pulsestyle_onevent"
    "pure" "rand" "randc" "randcase" "randsequence" "rcmos" "real" "realtime"
    "ref" "reg" "reject_on" "release" "repeat" "restrict" "return" "rnmos"
    "rpmos" "rtran" "rtranif0" "rtranif1" "s_always" "s_eventually"
    "s_nexttime" "s_until" "s_until_with" "scalared" "sequence" "shortint"
    "shortreal" "showcancelled" "signed" "small" "soft" "solve" "specify"
    "specparam" "static" "string" "strong" "strong0" "strong1" "struct"
    "super" "supply0" "supply1" "sync_accept_on" "sync_reject_on" "table"
    "tagged" "task" "this" "throughout" "time" "timeprecision" "timeunit"
    "tran" "tranif0" "tranif1" "tri" "tri0" "tri1" "triand" "trior" "trireg"
    "type" "typedef" "union" "unique" "unique0" "unsigned" "until"
    "until_with" "untyped" "use" "uwire" "var" "vectored" "virtual" "void"
    "wait" "wait_order" "wand" "weak" "weak0" "weak1" "while" "wildcard"
    "wire" "with" "within" "wor" "xnor" "xor")
  "SystemVerilog reserved words recognized by the lexer.")

(defconst sv-lexer--keyword-table
  (let ((table (make-hash-table :test #'equal :size 400)))
    (dolist (word sv-lexer-keywords) (puthash word t table))
    table)
  "Hash table for constant-time keyword lookup.")

(defconst sv-lexer-net-types
  '("wire" "tri" "tri0" "tri1" "triand" "trior" "trireg" "wand" "wor" "uwire"
    "supply0" "supply1")
  "Keywords that introduce a net declaration.")

(defconst sv-lexer-data-types
  '("logic" "reg" "bit" "byte" "shortint" "int" "longint" "integer" "time"
    "real" "realtime" "shortreal" "string" "chandle" "event" "void")
  "Keywords that introduce a variable declaration.")

(defconst sv-lexer--number-regexp
  (concat "\\(?:[0-9][0-9_]*\\)?'[sS]?[bBoOdDhH][ \t]*[0-9a-fA-FxXzZ?_]+"
          "\\|'[01xXzZ]\\b"
          "\\|[0-9][0-9_]*\\(?:\\.[0-9_]+\\)?\\(?:[eE][-+]?[0-9_]+\\)?"
          "\\(?:fs\\|ps\\|ns\\|us\\|ms\\|step\\)?")
  "Regexp matching integral, real, sized and time literals.")

(defconst sv-lexer--multi-char-operators
  '("<<<=" ">>>=" "===" "!==" "==?" "!=?" "<->" "<<=" ">>=" "**=" "|->" "|=>"
    "<<<" ">>>" "->>" "==" "!=" "<=" ">=" "&&" "||" "**" "->" "+=" "-=" "*="
    "/=" "%=" "&=" "|=" "^=" "<<" ">>" "::" "++" "--" "~&" "~|" "~^" "^~"
    "+:" "-:" "=>" ".*")
  "Operators longer than one character, longest-match first.
Note that the assignment pattern `\='{...}\=' is deliberately absent: lexing
it as one token would hide its opening brace from everything that counts
brackets, while its closing brace stayed visible.")

(defconst sv-lexer--operator-regexp
  (concat (regexp-opt sv-lexer--multi-char-operators)
          "\\|[-+*/%!~&|^<>=?:.@#']")
  "Regexp matching any operator.")

(defconst sv-lexer--punct-chars "()[]{};,"
  "Characters lexed as punctuation.")

(defun sv-lexer-keyword-p (name)
  "Return non-nil when NAME is a SystemVerilog reserved word."
  (and (stringp name) (gethash name sv-lexer--keyword-table)))

(defun sv-token-trivia-p (token)
  "Return non-nil when TOKEN carries no syntactic meaning."
  (memq (sv-token-type token) '(ws comment attribute)))

(defun sv-token-text= (token text)
  "Return non-nil when TOKEN's spelling is exactly TEXT."
  (and token (equal (sv-token-text token) text)))

(defun sv-token-member (token texts)
  "Return non-nil when TOKEN's spelling is one of TEXTS."
  (and token (member (sv-token-text token) texts)))

(defun sv-token-newlines (token)
  "Return the number of newline characters inside TOKEN."
  (cl-count ?\n (sv-token-text token)))

(defun sv-lexer--scan-define ()
  "Move point past the body of a `define directive, honouring continuations.
Point must be just after the directive name."
  (let ((continue t))
    (while continue
      (end-of-line)
      (if (and (eq (char-before) ?\\) (not (eobp)))
          (forward-char 1)
        (setq continue nil)))))

(defun sv-lex (&optional beg end)
  "Tokenize the current buffer between BEG and END.
Return a list of `sv-token' structures in source order, trivia included."
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (tokens '())
         (line 1)
         (line-start beg))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((start (point))
              (type nil))
          (cond
           ((looking-at "[ \t\f\r\n]+")
            (goto-char (match-end 0))
            (setq type 'ws))
           ((looking-at "//[^\n]*")
            (goto-char (match-end 0))
            (setq type 'comment))
           ((looking-at "/\\*")
            (goto-char (match-end 0))
            (unless (search-forward "*/" end t) (goto-char end))
            (setq type 'comment))
           ((and (looking-at "(\\*") (not (looking-at "(\\*)")))
            (goto-char (match-end 0))
            (unless (search-forward "*)" end t) (goto-char end))
            (setq type 'attribute))
           ((looking-at "\"\\(?:[^\"\\\n]\\|\\\\.\\)*\"")
            (goto-char (match-end 0))
            (setq type 'string))
           ((looking-at sv-lexer--number-regexp)
            (goto-char (match-end 0))
            (setq type 'number))
           ((looking-at "\\$[A-Za-z_][A-Za-z0-9_$]*")
            (goto-char (match-end 0))
            (setq type 'sysfunc))
           ((looking-at "`[A-Za-z_][A-Za-z0-9_$]*")
            (let ((name (match-string-no-properties 0)))
              (goto-char (match-end 0))
              (when (equal name "`define") (sv-lexer--scan-define)))
            (setq type 'directive))
           ((looking-at "\\\\[^ \t\n]+")
            (goto-char (match-end 0))
            (setq type 'ident))
           ((looking-at "[A-Za-z_][A-Za-z0-9_$]*")
            (goto-char (match-end 0))
            (setq type (if (sv-lexer-keyword-p (match-string-no-properties 0))
                           'keyword
                         'ident)))
           ((memq (char-after) (string-to-list sv-lexer--punct-chars))
            (forward-char 1)
            (setq type 'punct))
           ((looking-at sv-lexer--operator-regexp)
            (goto-char (match-end 0))
            (setq type 'operator))
           (t
            (forward-char 1)
            (setq type 'unknown)))
          (when (= (point) start) (forward-char 1)) ; never loop forever
          (let* ((text (buffer-substring-no-properties start (point)))
                 (last-newline (cl-position ?\n text :from-end t)))
            (push (sv-token--create :type type :text text
                                    :start start :end (point)
                                    :line line :col (- start line-start))
                  tokens)
            (when last-newline
              (setq line (+ line (cl-count ?\n text)))
              (setq line-start (+ start last-newline 1)))))))
    (nreverse tokens)))

(defun sv-lex-string (text)
  "Tokenize TEXT and return the resulting token list."
  (with-temp-buffer
    (insert text)
    (sv-lex (point-min) (point-max))))

(defun sv-lex-significant (tokens)
  "Return a vector of the non-trivia tokens of TOKENS."
  (vconcat (cl-remove-if #'sv-token-trivia-p tokens)))

(provide 'sv-lexer)

;;; sv-lexer.el ends here
