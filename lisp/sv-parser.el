;;; sv-parser.el --- Recursive-descent parser for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Builds a coarse syntax tree out of the token stream produced by
;; `sv-lexer'.  The goal is not full IEEE 1800 conformance but a tree that is
;; good enough for linting, navigation and imenu on real-world RTL: design
;; units, their parameters and ports, declarations, continuous assignments,
;; procedural blocks with their statement structure, and module instances
;; with their port connections.
;;
;; The parser never signals.  Anything it does not recognize is skipped up to
;; the next `;' or end keyword, so a syntax error degrades the tree locally
;; instead of losing the whole file.
;;
;; Every node is a plist whose `:type' says what it is.  Nodes carry
;; `:line' plus `:beg' and `:end' indices into the significant-token vector,
;; which lets the linter re-scan the exact token range of any node.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)

(defvar sv-parse--toks nil
  "Vector of significant tokens being parsed.")

(defvar sv-parse--pos 0
  "Index of the next token to consume in `sv-parse--toks'.")

(defconst sv-parse--hard-stops
  '("endmodule" "endinterface" "endpackage" "endprogram" "endclass"
    "endfunction" "endtask" "endgenerate" "endcase")
  "Keywords that always terminate a token scan, whatever the nesting.")

(defconst sv-parse--always-keywords
  '("always" "always_comb" "always_ff" "always_latch")
  "Keywords introducing a procedural block.")


;;;; Token cursor

(defsubst sv-parse--eob-p ()
  "Return non-nil when all tokens have been consumed."
  (>= sv-parse--pos (length sv-parse--toks)))

(defsubst sv-parse--tok (&optional n)
  "Return the token N positions ahead of the cursor (0 by default)."
  (let ((index (+ sv-parse--pos (or n 0))))
    (when (and (>= index 0) (< index (length sv-parse--toks)))
      (aref sv-parse--toks index))))

(defsubst sv-parse--text (&optional n)
  "Return the spelling of the token N positions ahead, or nil."
  (let ((tok (sv-parse--tok n))) (and tok (sv-token-text tok))))

(defsubst sv-parse--type (&optional n)
  "Return the type of the token N positions ahead, or nil."
  (let ((tok (sv-parse--tok n))) (and tok (sv-token-type tok))))

(defsubst sv-parse--line (&optional n)
  "Return the line of the token N positions ahead, or 0."
  (let ((tok (sv-parse--tok n))) (if tok (sv-token-line tok) 0)))

(defsubst sv-parse--adv (&optional n)
  "Advance the cursor by N tokens (1 by default)."
  (setq sv-parse--pos (+ sv-parse--pos (or n 1))))

(defun sv-parse--at (text)
  "Return non-nil when the current token is spelled TEXT."
  (equal (sv-parse--text) text))

(defun sv-parse--at-any (texts)
  "Return non-nil when the current token is spelled like a member of TEXTS."
  (member (sv-parse--text) texts))

(defun sv-parse--accept (text)
  "Consume the current token when it is spelled TEXT, and return it."
  (when (sv-parse--at text)
    (prog1 (sv-parse--tok) (sv-parse--adv))))

(defun sv-parse--collect-until (terminators &optional keep-hard-stops)
  "Collect tokens until one of TERMINATORS appears outside brackets.
The terminator itself is left unconsumed.  Unless KEEP-HARD-STOPS is
non-nil the scan also stops at any of `sv-parse--hard-stops'."
  (let ((depth 0) (acc '()) (done nil))
    (while (and (not done) (not (sv-parse--eob-p)))
      (let ((text (sv-parse--text)))
        (cond
         ((and (= depth 0) (member text terminators)) (setq done t))
         ((and (not keep-hard-stops) (member text sv-parse--hard-stops))
          (setq done t))
         (t
          (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
                ((member text '(")" "]" "}")) (setq depth (max 0 (1- depth)))))
          (push (sv-parse--tok) acc)
          (sv-parse--adv)))))
    (nreverse acc)))

(defun sv-parse--skip-balanced ()
  "Consume a bracketed group starting at point, and return its tokens.
Includes both delimiters.  Returns nil when not sitting on an opener."
  (when (sv-parse--at-any '("(" "[" "{"))
    (let ((depth 0) (acc '()) (done nil))
      (while (and (not done) (not (sv-parse--eob-p)))
        (let ((text (sv-parse--text)))
          (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
                ((member text '(")" "]" "}")) (setq depth (1- depth))))
          (push (sv-parse--tok) acc)
          (sv-parse--adv)
          (when (<= depth 0) (setq done t))))
      (nreverse acc))))

(defun sv-parse--balanced-p (tokens)
  "Return non-nil when every bracket TOKENS opens is also closed in TOKENS."
  (let ((depth 0))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
              ((member text '(")" "]" "}")) (setq depth (1- depth))))))
    (zerop depth)))

(defun sv-parse-unwrap (tokens)
  "Return TOKENS without the brackets wrapping them.
The closing bracket is dropped only when the source really has one.  A
group the buffer cuts off mid-way keeps every token it holds, so a
truncated instance or port list does not silently lose its last name."
  (if (sv-parse--balanced-p tokens)
      (butlast (cdr tokens))
    (cdr tokens)))

(defun sv-parse--inside-balanced ()
  "Consume a bracketed group at point and return the tokens inside it."
  (sv-parse-unwrap (sv-parse--skip-balanced)))

(defconst sv-parse--line-directives
  '("`ifdef" "`ifndef" "`elsif" "`undef" "`include" "`timescale" "`line"
    "`default_nettype" "`pragma" "`begin_keywords" "`end_keywords"
    "`unconnected_drive" "`nounconnected_drive")
  "Directives whose arguments run to the end of their line.")

(defun sv-parse--skip-directive ()
  "Consume a compiler directive together with whatever belongs to it.
The preprocessor directives take their arguments to the end of the line,
and a macro used as a statement -- `SOME_MACRO(a, b)\=' -- carries its
argument list but no semicolon.  Left unconsumed, either would be read as
the start of the next statement and swallow it."
  (let* ((token (sv-parse--tok))
         (line (and token (sv-token-line token))))
    (sv-parse--adv)
    (if (member (and token (sv-token-text token)) sv-parse--line-directives)
        (while (and (not (sv-parse--eob-p))
                    (= (sv-token-line (sv-parse--tok)) line))
          (sv-parse--adv))
      (when (sv-parse--at "(") (sv-parse--skip-balanced))
      (sv-parse--accept ";")))
  nil)

(defmacro sv-parse--gather (into parser)
  "Call PARSER, pushing its node onto INTO, and guarantee progress.
A parser that consumes tokens but produces no node -- a compiler
directive, say -- must not make the caller skip the token after it; only
a parser that consumed nothing gets stepped over."
  `(let* ((before sv-parse--pos)
          (node ,parser))
     (cond (node (push node ,into))
           ((= sv-parse--pos before) (sv-parse--adv)))))

(defun sv-parse--skip-statement ()
  "Skip an unrecognized construct up to and including its closing `;'."
  (sv-parse--collect-until '(";"))
  (sv-parse--accept ";"))


;;;; Token-list utilities

(defun sv-parse-split-on (tokens separator)
  "Split TOKENS on every SEPARATOR that sits outside any bracket.
Return a list of token lists; empty groups are dropped."
  (let ((depth 0) (groups '()) (current '()))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (cond
         ((and (= depth 0) (equal text separator))
          (push (nreverse current) groups)
          (setq current '()))
         (t
          (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
                ((member text '(")" "]" "}")) (setq depth (max 0 (1- depth)))))
          (push tok current)))))
    (push (nreverse current) groups)
    (cl-remove-if #'null (nreverse groups))))

(defun sv-parse-split-commas (tokens)
  "Split TOKENS on commas that sit outside any bracket."
  (sv-parse-split-on tokens ","))

(defun sv-parse-braced-body (tokens)
  "Return the tokens inside the first `{\=' of TOKENS, without the braces."
  (let ((depth 0) (body '()) (started nil) (done nil))
    (dolist (tok tokens)
      (unless done
        (let ((text (sv-token-text tok)))
          (cond
           ((member text '("{" "[" "("))
            (setq depth (1+ depth))
            (if (and (not started) (equal text "{"))
                (setq started t)
              (when started (push tok body))))
           ((member text '("}" "]" ")"))
            (setq depth (1- depth))
            (cond ((and started (= depth 0)) (setq done t))
                  (started (push tok body))))
           (started (push tok body))))))
    (nreverse body)))

(defun sv-parse--struct-members (tokens)
  "Return the members declared by the struct or union in TOKENS."
  (when (sv-parse-find-top tokens '("struct" "union"))
    (let ((members '()))
      (dolist (group (sv-parse-split-on (sv-parse-braced-body tokens) ";"))
        (let ((declarator (sv-parse-declarator group)))
          (when (plist-get declarator :name)
            (push declarator members))))
      (nreverse members))))

(defun sv-parse-find-top (tokens texts)
  "Return the position in TOKENS of the first token in TEXTS at bracket depth 0."
  (let ((depth 0) (index 0) (found nil))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (when (and (null found) (= depth 0) (member text texts))
          (setq found index))
        (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
              ((member text '(")" "]" "}")) (setq depth (max 0 (1- depth))))))
      (setq index (1+ index)))
    found))

(defun sv-parse--strip-brackets (tokens)
  "Return TOKENS with every bracketed group removed."
  (let ((depth 0) (acc '()))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
              ((member text '(")" "]" "}")) (setq depth (max 0 (1- depth))))
              ((= depth 0) (push tok acc)))))
    (nreverse acc)))

(defun sv-parse--last-ident (tokens)
  "Return the last identifier token of TOKENS outside brackets, or nil."
  (car (last (cl-remove-if-not (lambda (tok) (eq (sv-token-type tok) 'ident))
                               (sv-parse--strip-brackets tokens)))))

(defun sv-parse-token-text (tokens)
  "Return the concatenated spelling of TOKENS separated by single spaces."
  (mapconcat #'sv-token-text tokens " "))

(defun sv-parse-token-string (tokens)
  "Return TOKENS spelled out as the source had them.
Two tokens are separated by a space only when the source separated them,
so `[WIDTH-1: 0]\=' comes back unchanged."
  (let ((parts '()) (previous nil))
    (dolist (tok tokens)
      (when (and previous (> (sv-token-start tok) (sv-token-end previous)))
        (push " " parts))
      (push (sv-token-text tok) parts)
      (setq previous tok))
    (apply #'concat (nreverse parts))))


;;;; Declarations and ports

(defconst sv-parse-direction-keywords '("input" "output" "inout" "ref")
  "Port direction keywords.")

(defconst sv-parse--qualifier-keywords
  '("var" "const" "static" "automatic" "signed" "unsigned" "packed" "virtual"
    "interface" "rand" "randc" "local" "protected")
  "Keywords that may decorate a declaration without being its data type.")

(defun sv-parse-declarator (tokens &optional dir)
  "Turn TOKENS describing one declarator into a plist, with direction DIR.
TOKENS covers a single comma-separated element such as
`input logic [7:0] i_data\=' or `mem [0:3] = \='{default:0}\='.  The declared
name is the last bare identifier before any initializer; brackets seen
before it are packed dimensions and those after it unpacked ones."
  (let* ((eq-pos (sv-parse-find-top tokens '("=")))
         (init (when eq-pos (nthcdr (1+ eq-pos) tokens)))
         (head (if eq-pos (cl-subseq tokens 0 eq-pos) tokens))
         (direction dir)
         (explicit nil)
         (name-tok nil))
    (when (and head (member (sv-token-text (car head)) sv-parse-direction-keywords))
      (setq direction (intern (sv-token-text (car head))))
      (setq head (cdr head)))
    (if (and head (equal (sv-token-text (car head)) "."))
        (setq name-tok (cadr head) explicit t)
      (setq name-tok (sv-parse--last-ident head)))
    (let ((datatype '()) (packed '()) (unpacked '()) (depth 0) (after nil))
      (dolist (tok head)
        (cond
         ((eq tok name-tok) (setq after t))
         (after (push tok unpacked))
         (t
          (let ((text (sv-token-text tok)))
            (cond
             ((member text '("(" "[" "{"))
              (setq depth (1+ depth))
              (push tok packed))
             ((member text '(")" "]" "}"))
              (setq depth (max 0 (1- depth)))
              (push tok packed))
             ((> depth 0) (push tok packed))
             (t (push tok datatype)))))))
      (let ((anchor (or name-tok (car tokens) (car head))))
        (list :name (and name-tok (sv-token-text name-tok))
              :dir direction
              :explicit explicit
              :datatype (sv-parse-token-text (nreverse datatype))
              :packed (nreverse packed)
              :unpacked (nreverse unpacked)
              :init init
              :line (if anchor (sv-token-line anchor) 0)
              :col (if anchor (sv-token-col anchor) 0)
              :token anchor)))))

(defun sv-parse--port-list (tokens)
  "Parse the module port list TOKENS, minus its enclosing parentheses.
Directions and types are inherited from the previous port when omitted,
as the ANSI header rules require."
  (let ((dir nil) (datatype nil) (packed nil) (ports '()))
    (dolist (group (sv-parse-split-commas tokens))
      (let* ((port (sv-parse-declarator group))
             (interface (and (null (plist-get port :dir))
                             (string-match-p "\\." (or (plist-get port :datatype) "")))))
        (when interface (setq port (plist-put port :interface t)))
        (cond
         ((plist-get port :dir) (setq dir (plist-get port :dir)))
         (interface nil)
         (t (setq port (plist-put port :dir dir))))
        (if (and (plist-get port :datatype)
                 (not (string-empty-p (plist-get port :datatype))))
            (setq datatype (plist-get port :datatype)
                  packed (plist-get port :packed))
          (setq port (plist-put port :datatype datatype))
          ;; `input logic [7:0] a, b\=' makes b as wide as a.
          (unless (plist-get port :packed)
            (setq port (plist-put port :packed packed))))
        (when (plist-get port :name)
          (push port ports))))
    (nreverse ports)))

(defun sv-parse--param-list (tokens)
  "Parse a parameter port list or parameter declaration from TOKENS."
  (let ((params '()) (kind 'parameter))
    (dolist (group (sv-parse-split-commas tokens))
      (when group
        (when (member (sv-token-text (car group)) '("parameter" "localparam"))
          (setq kind (intern (sv-token-text (car group))))
          (setq group (cdr group)))
        (when group
          (let ((decl (sv-parse-declarator group)))
            (when (plist-get decl :name)
              (push (append (list :kind kind) decl) params))))))
    (nreverse params)))


;;;; Statements

(defun sv-parse--assign-operator (tokens)
  "Return the position of a top-level assignment operator inside TOKENS."
  (sv-parse-find-top tokens '("=" "<=" "+=" "-=" "*=" "/=" "%=" "&=" "|="
                              "^=" "<<=" ">>=" "<<<=" ">>>=")))

(defun sv-parse--decl-start-p ()
  "Return non-nil when the cursor sits on something that declares a name."
  (let ((text (sv-parse--text))
        (type (sv-parse--type)))
    (or (member text sv-lexer-data-types)
        (member text sv-lexer-net-types)
        (member text '("var" "typedef" "localparam" "parameter" "genvar"
                       "const" "struct" "union" "enum" "event"
                       "automatic" "static"))
        ;; `my_type_t foo;' or `pkg::my_type_t foo;'
        (and (eq type 'ident)
             (let ((offset 1))
               (while (and (equal (sv-parse--text offset) "::")
                           (eq (sv-parse--type (1+ offset)) 'ident))
                 (setq offset (+ offset 2)))
               (when (equal (sv-parse--text offset) "#")
                 (setq offset (1+ offset))
                 (when (equal (sv-parse--text offset) "(")
                   (let ((depth 0) (done nil))
                     (while (and (not done) (sv-parse--text offset))
                       (cond ((member (sv-parse--text offset) '("(" "[" "{"))
                              (setq depth (1+ depth)))
                             ((member (sv-parse--text offset) '(")" "]" "}"))
                              (setq depth (1- depth))))
                       (setq offset (1+ offset))
                       (when (<= depth 0) (setq done t))))))
               (while (equal (sv-parse--text offset) "[")
                 (let ((depth 0) (done nil))
                   (while (and (not done) (sv-parse--text offset))
                     (cond ((member (sv-parse--text offset) '("(" "[" "{"))
                            (setq depth (1+ depth)))
                           ((member (sv-parse--text offset) '(")" "]" "}"))
                            (setq depth (1- depth))))
                     (setq offset (1+ offset))
                     (when (<= depth 0) (setq done t)))))
               (and (eq (sv-parse--type offset) 'ident)
                    (member (sv-parse--text (1+ offset)) '(";" "," "=" "["))))))))

(defun sv-parse--block-item ()
  "Parse one declaration or statement inside a procedural block."
  (if (sv-parse--decl-start-p)
      (sv-parse--declaration)
    (sv-parse--statement)))

(defun sv-parse--statement ()
  "Parse a single procedural statement and return its node."
  (let ((line (sv-parse--line))
        (beg sv-parse--pos))
    (cond
     ((sv-parse--eob-p) nil)

     ((sv-parse--accept ";") (list :type 'null :line line))

     ((eq (sv-parse--type) 'directive) (sv-parse--skip-directive))

     ((sv-parse--at-any '("unique" "unique0" "priority"))
      (let ((qualifier (intern (sv-parse--text))))
        (sv-parse--adv)
        (let ((inner (sv-parse--statement)))
          (when inner (plist-put inner :qualifier qualifier))
          inner)))

     ((sv-parse--at "begin")
      (sv-parse--adv)
      (let ((label nil) (stmts '()))
        (when (sv-parse--accept ":")
          (setq label (sv-parse--text))
          (sv-parse--adv))
        (while (and (not (sv-parse--eob-p))
                    (not (sv-parse--at "end"))
                    (not (sv-parse--at-any sv-parse--hard-stops)))
          (sv-parse--gather stmts (sv-parse--block-item)))
        (sv-parse--accept "end")
        (when (sv-parse--accept ":") (sv-parse--adv))
        (list :type 'block :label label :stmts (nreverse stmts)
              :line line :beg beg :end sv-parse--pos)))

     ((sv-parse--at "if")
      (sv-parse--adv)
      (let ((cond-tokens (sv-parse--skip-balanced))
            (then nil) (else nil))
        (setq then (sv-parse--statement))
        (when (sv-parse--accept "else")
          (setq else (sv-parse--statement)))
        (list :type 'if :cond cond-tokens :then then :else else
              :line line :beg beg :end sv-parse--pos)))

     ((sv-parse--at-any '("case" "casex" "casez"))
      (let ((kind (intern (sv-parse--text))) (items '()) (expr nil))
        (sv-parse--adv)
        (setq expr (sv-parse--skip-balanced))
        (when (sv-parse--at-any '("matches" "inside")) (sv-parse--adv))
        (while (and (not (sv-parse--eob-p))
                    (not (sv-parse--at "endcase"))
                    (not (sv-parse--at-any (remove "endcase" sv-parse--hard-stops))))
          (let* ((item-line (sv-parse--line))
                 (labels (sv-parse--collect-until '(":")))
                 (default (and labels
                               (equal (sv-token-text (car labels)) "default"))))
            (if (not (sv-parse--accept ":"))
                (sv-parse--adv)
              (push (list :labels labels :default default :line item-line
                          :stmt (sv-parse--statement))
                    items))))
        (sv-parse--accept "endcase")
        (list :type 'case :kind kind :expr expr :items (nreverse items)
              :line line :beg beg :end sv-parse--pos)))

     ((sv-parse--at-any '("for" "while" "foreach" "repeat"))
      (let ((kind (intern (sv-parse--text))))
        (sv-parse--adv)
        (let ((header (sv-parse--skip-balanced)))
          (list :type 'loop :kind kind :header header
                :body (sv-parse--statement)
                :line line :beg beg :end sv-parse--pos))))

     ((sv-parse--at "forever")
      (sv-parse--adv)
      (list :type 'loop :kind 'forever :body (sv-parse--statement)
            :line line :beg beg :end sv-parse--pos))

     ((sv-parse--at "do")
      (sv-parse--adv)
      (let ((body (sv-parse--statement)))
        (sv-parse--accept "while")
        (sv-parse--skip-balanced)
        (sv-parse--accept ";")
        (list :type 'loop :kind 'do-while :body body
              :line line :beg beg :end sv-parse--pos)))

     ((sv-parse--at "fork")
      (sv-parse--adv)
      (let ((stmts '()))
        (while (and (not (sv-parse--eob-p))
                    (not (sv-parse--at-any '("join" "join_any" "join_none")))
                    (not (sv-parse--at-any sv-parse--hard-stops)))
          (sv-parse--gather stmts (sv-parse--block-item)))
        (sv-parse--adv)
        (list :type 'fork :stmts (nreverse stmts)
              :line line :beg beg :end sv-parse--pos)))

     ((sv-parse--at-any '("@" "#"))
      (sv-parse--adv)
      (if (sv-parse--at-any '("(" "["))
          (sv-parse--skip-balanced)
        (sv-parse--adv))
      (let ((inner (sv-parse--statement)))
        (or inner (list :type 'delay :line line))))

     ((sv-parse--at-any '("assert" "assume" "cover" "restrict" "expect"))
      (sv-parse--collect-until '(";"))
      (sv-parse--accept ";")
      (when (sv-parse--accept "else") (sv-parse--statement))
      (list :type 'assertion :line line :beg beg :end sv-parse--pos))

     ((sv-parse--at-any sv-parse--hard-stops) nil)

     (t
      (let* ((tokens (sv-parse--collect-until '(";")))
             (op-pos (sv-parse--assign-operator tokens)))
        (sv-parse--accept ";")
        (if op-pos
            (let ((lhs (cl-subseq tokens 0 op-pos)))
              (list :type 'assign
                    :op (sv-token-text (nth op-pos tokens))
                    :lhs lhs
                    :target (let ((first (car (sv-parse--strip-brackets lhs))))
                              (and first
                                   (if (equal (sv-token-text first) "{")
                                       nil
                                     (sv-token-text first))))
                    :lhs-targets (sv-parse--lhs-targets lhs)
                    :rhs (nthcdr (1+ op-pos) tokens)
                    :line line :beg beg :end sv-parse--pos))
          (list :type 'expr :tokens tokens
                :line line :beg beg :end sv-parse--pos)))))))

(defun sv-parse-lhs-tokens (tokens)
  "Return the token naming each signal assigned by left-hand side TOKENS.
Handles plain targets, bit selects and concatenations."
  (let ((targets '()) (expect t))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (cond
         ((member text '("{" ",")) (setq expect t))
         ((and expect (eq (sv-token-type tok) 'ident))
          (push tok targets)
          (setq expect nil))
         ((member text '("[" "." "::")) (setq expect nil)))))
    (nreverse targets)))

(defun sv-parse--lhs-targets (tokens)
  "Return every signal name assigned by the left-hand side TOKENS."
  (mapcar #'sv-token-text (sv-parse-lhs-tokens tokens)))


;;;; Declarations, instances and design units

(defun sv-parse--declaration ()
  "Parse a declaration statement and return its node."
  (let* ((line (sv-parse--line))
         (beg sv-parse--pos)
         (first (sv-parse--text)))
    (cond
     ((equal first "typedef")
      (let ((tokens (sv-parse--collect-until '(";"))))
        (sv-parse--accept ";")
        (let ((name (sv-parse--last-ident tokens)))
          (list :type 'typedef
                :name (and name (sv-token-text name))
                :text (sv-parse-token-string tokens)
                :enum-members (sv-parse--enum-members tokens)
                :members (sv-parse--struct-members tokens)
                :line line :beg beg :end sv-parse--pos))))

     ((member first '("parameter" "localparam"))
      (let ((kind (intern first)))
        (sv-parse--adv)
        (let ((tokens (sv-parse--collect-until '(";"))))
          (sv-parse--accept ";")
          (list :type 'param
                :kind kind
                :params (mapcar (lambda (param) (plist-put param :kind kind))
                                (sv-parse--param-list tokens))
                :line line :beg beg :end sv-parse--pos))))

     ((equal first "genvar")
      (sv-parse--adv)
      (let ((tokens (sv-parse--collect-until '(";"))))
        (sv-parse--accept ";")
        (list :type 'genvar
              :names (mapcar (lambda (group)
                               (list :name (sv-token-text (car group))
                                     :line (sv-token-line (car group))))
                             (sv-parse-split-commas tokens))
              :line line :beg beg :end sv-parse--pos)))

     (t
      (let* ((tokens (sv-parse--collect-until '(";")))
             (groups (sv-parse-split-commas tokens))
             (head (car groups))
             (base (sv-parse-declarator (or head '())))
             (names '()))
        (sv-parse--accept ";")
        (when (plist-get base :name) (push base names))
        ;; Later declarators inherit the data type of the first one.
        (dolist (group (cdr groups))
          (let ((decl (sv-parse-declarator group)))
            (unless (and (plist-get decl :datatype)
                         (not (string-empty-p (plist-get decl :datatype))))
              (setq decl (plist-put decl :datatype (plist-get base :datatype)))
              ;; `logic [7:0] a, b;\=' declares two bytes, not a byte and a bit.
              (unless (plist-get decl :packed)
                (setq decl (plist-put decl :packed (plist-get base :packed)))))
            (when (plist-get decl :name) (push decl names))))
        (list :type 'decl
              :enum-members (sv-parse--enum-members tokens)
              :members (sv-parse--struct-members tokens)
              :datatype (plist-get base :datatype)
              :nettype (car (cl-intersection
                             sv-lexer-net-types
                             (split-string (or (plist-get base :datatype) "") " " t)
                             :test #'equal))
              :names (nreverse names)
              :line line :beg beg :end sv-parse--pos))))))

(defun sv-parse--enum-members (tokens)
  "Return the enumeration literal names declared inside TOKENS.
A struct body is braced the same way but names members, not literals, so
TOKENS must actually introduce an enumeration."
  (unless (sv-parse-find-top tokens '("enum"))
    (setq tokens nil))
  (let ((members '()) (expect nil) (depth 0))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (cond
         ((equal text "{") (setq depth (1+ depth)) (when (= depth 1) (setq expect t)))
         ((equal text "}") (setq depth (max 0 (1- depth))) (setq expect nil))
         ((and (= depth 1) (equal text ",")) (setq expect t))
         ((and expect (= depth 1) (eq (sv-token-type tok) 'ident))
          (push (list :name text :line (sv-token-line tok)) members)
          (setq expect nil)))))
    (nreverse members)))

(defun sv-parse--instance-p ()
  "Return non-nil when the cursor sits on a module instantiation."
  (and (eq (sv-parse--type) 'ident)
       (let ((offset 1))
         ;; Optional package qualification.
         (while (and (equal (sv-parse--text offset) "::")
                     (eq (sv-parse--type (1+ offset)) 'ident))
           (setq offset (+ offset 2)))
         ;; Optional parameter override.
         (when (equal (sv-parse--text offset) "#")
           (setq offset (1+ offset))
           (when (equal (sv-parse--text offset) "(")
             (let ((depth 0) (done nil))
               (while (and (not done) (sv-parse--text offset))
                 (cond ((equal (sv-parse--text offset) "(") (setq depth (1+ depth)))
                       ((equal (sv-parse--text offset) ")") (setq depth (1- depth))))
                 (setq offset (1+ offset))
                 (when (<= depth 0) (setq done t))))))
         (and (eq (sv-parse--type offset) 'ident)
              ;; An instance name may carry an array range before its ports.
              (let ((next (1+ offset)))
                (while (equal (sv-parse--text next) "[")
                  (let ((depth 0) (done nil))
                    (while (and (not done) (sv-parse--text next))
                      (cond ((equal (sv-parse--text next) "[") (setq depth (1+ depth)))
                            ((equal (sv-parse--text next) "]") (setq depth (1- depth))))
                      (setq next (1+ next))
                      (when (<= depth 0) (setq done t)))))
                (equal (sv-parse--text next) "("))))))

(defun sv-parse--strip-leading-directives (tokens)
  "Return TOKENS without the compiler directives it starts with.
A port or parameter list may be split by `\=`ifdef\=', which otherwise hides
the connection that follows from the parser."
  (while (and tokens (eq (sv-token-type (car tokens)) 'directive))
    (let ((line (sv-token-line (car tokens)))
          (takes-line (member (sv-token-text (car tokens))
                              sv-parse--line-directives)))
      (setq tokens (cdr tokens))
      (when takes-line
        (while (and tokens (= (sv-token-line (car tokens)) line))
          (setq tokens (cdr tokens))))))
  tokens)

(defun sv-parse--connections (tokens)
  "Parse instance port connections from TOKENS, minus enclosing parentheses."
  (let ((connections '()) (index 0))
    (dolist (group (sv-parse-split-commas tokens))
      (setq group (sv-parse--strip-leading-directives group))
      (when group
        (let ((first (car group)))
          (cond
           ;; A `.' with nothing after it is a connection being typed, not a
           ;; positional one.  Reporting it would make the linter fire on a
           ;; half-written line.
           ((and (equal (sv-token-text first) ".") (null (cdr group))) nil)
           ((equal (sv-token-text first) ".")
              (let* ((name-tok (cadr group))
                     (wildcard (equal (sv-token-text name-tok) "*"))
                     (expr (cddr group)))
                (push (list :name (and (not wildcard) (sv-token-text name-tok))
                            :name-token (and (not wildcard) name-tok)
                            :wildcard wildcard
                            :positional nil
                            :index index
                            :expr (if (and expr (equal (sv-token-text (car expr)) "("))
                                      (sv-parse-unwrap expr)
                                    expr)
                            :implicit (null expr)
                            :line (sv-token-line first)
                            :col (sv-token-col first)
                            :token first)
                      connections)))
           (t
            (push (list :name nil :positional t :index index :expr group
                        :line (sv-token-line first)
                        :col (sv-token-col first)
                        :token first)
                  connections)))))
      (setq index (1+ index)))
    (nreverse connections)))

(defun sv-parse--instance ()
  "Parse a module instantiation and return its node."
  (let* ((line (sv-parse--line))
         (beg sv-parse--pos)
         (module (sv-parse--text))
         (params nil))
    (sv-parse--adv)
    (while (and (equal (sv-parse--text) "::") (eq (sv-parse--type 1) 'ident))
      (setq module (concat module "::" (sv-parse--text 1)))
      (sv-parse--adv 2))
    (when (sv-parse--accept "#")
      (setq params (sv-parse--connections
                    (sv-parse--inside-balanced))))
    (let ((instances '()))
      (while (and (not (sv-parse--eob-p)) (not (sv-parse--at ";")))
        (if (eq (sv-parse--type) 'ident)
            (let ((name (sv-parse--text))
                  (name-line (sv-parse--line)))
              (sv-parse--adv)
              (while (sv-parse--at "[") (sv-parse--skip-balanced))
              (let ((conn (when (sv-parse--at "(")
                            (sv-parse--connections
                             (sv-parse--inside-balanced)))))
                (push (list :name name :line name-line :connections conn)
                      instances)))
          (sv-parse--adv)))
      (sv-parse--accept ";")
      (let ((first (car (last instances))))
        (list :type 'instance
              :module module
              :name (plist-get first :name)
              :params params
              :connections (plist-get first :connections)
              :siblings (nreverse instances)
              :line line :beg beg :end sv-parse--pos)))))

(defun sv-parse--subprogram (kind)
  "Parse a function or task body.  KIND is `function' or `task'."
  (let* ((line (sv-parse--line))
         (beg sv-parse--pos)
         (end-keyword (if (eq kind 'function) "endfunction" "endtask"))
         (header (sv-parse--collect-until '(";" "(") t))
         (args nil)
         (name (let ((tok (sv-parse--last-ident header)))
                 (and tok (sv-token-text tok))))
         (body '()))
    (when (sv-parse--at "(")
      (setq args (sv-parse--port-list (sv-parse--inside-balanced))))
    ;; A prototype (`extern', `pure virtual') has no body.
    (if (cl-some (lambda (tok) (member (sv-token-text tok) '("extern" "pure")))
                 header)
        (progn (sv-parse--accept ";")
               (list :type kind :name name :args args :prototype t
                     :body nil :line line :beg beg :end sv-parse--pos))
      (sv-parse--accept ";")
      (while (and (not (sv-parse--eob-p))
                  (not (sv-parse--at end-keyword))
                  (not (sv-parse--at-any (remove end-keyword sv-parse--hard-stops))))
        (sv-parse--gather body (sv-parse--block-item)))
      (sv-parse--accept end-keyword)
      (when (sv-parse--accept ":") (sv-parse--adv))
      (list :type kind :name name :args args :prototype nil
            :body (nreverse body) :line line :beg beg :end sv-parse--pos))))

(defun sv-parse--module-item ()
  "Parse one item of a module, interface, package or generate body."
  (let ((line (sv-parse--line))
        (beg sv-parse--pos)
        (text (sv-parse--text)))
    (cond
     ((null text) nil)

     ((sv-parse--accept ";") (list :type 'null :line line))

     ((eq (sv-parse--type) 'directive) (sv-parse--skip-directive))

     ((equal text "assign")
      (sv-parse--adv)
      (when (sv-parse--at-any '("#" "(")) (sv-parse--skip-balanced))
      (let* ((tokens (sv-parse--collect-until '(";")))
             (op-pos (sv-parse--assign-operator tokens)))
        (sv-parse--accept ";")
        (let ((lhs (if op-pos (cl-subseq tokens 0 op-pos) tokens)))
          (list :type 'continuous-assign
                :lhs lhs
                :lhs-targets (sv-parse--lhs-targets lhs)
                :rhs (and op-pos (nthcdr (1+ op-pos) tokens))
                :line line :beg beg :end sv-parse--pos))))

     ((member text sv-parse--always-keywords)
      (let ((kind (intern text)) (sensitivity nil) (star nil))
        (sv-parse--adv)
        (when (sv-parse--accept "@")
          (if (sv-parse--at "(")
              (let ((group (sv-parse--skip-balanced)))
                (setq sensitivity (sv-parse-unwrap group))
                (setq star (or (null sensitivity)
                               (and (= (length sensitivity) 1)
                                    (equal (sv-token-text (car sensitivity)) "*")))))
            (setq sensitivity (list (sv-parse--tok)))
            (sv-parse--adv)))
        (list :type 'always :kind kind :sensitivity sensitivity :star star
              :body (sv-parse--statement)
              :line line :beg beg :end sv-parse--pos)))

     ((member text '("initial" "final"))
      (sv-parse--adv)
      (list :type (intern text) :body (sv-parse--statement)
            :line line :beg beg :end sv-parse--pos))

     ((equal text "generate")
      (sv-parse--adv)
      (let ((items '()))
        (while (and (not (sv-parse--eob-p))
                    (not (sv-parse--at "endgenerate"))
                    (not (sv-parse--at-any (remove "endgenerate" sv-parse--hard-stops))))
          (sv-parse--gather items (sv-parse--module-item)))
        (sv-parse--accept "endgenerate")
        (list :type 'generate :items (nreverse items)
              :line line :beg beg :end sv-parse--pos)))

     ((equal text "for")
      (sv-parse--adv)
      (let ((header (sv-parse--skip-balanced)))
        (list :type 'generate-for :header header
              :body (sv-parse--generate-block)
              :line line :beg beg :end sv-parse--pos)))

     ((equal text "if")
      (sv-parse--adv)
      (let ((condition (sv-parse--skip-balanced))
            (then (sv-parse--generate-block))
            (else nil))
        (when (sv-parse--accept "else")
          (setq else (sv-parse--generate-block)))
        (list :type 'generate-if :cond condition :then then :else else
              :line line :beg beg :end sv-parse--pos)))

     ((equal text "case")
      (sv-parse--adv)
      (sv-parse--skip-balanced)
      (let ((items '()))
        (while (and (not (sv-parse--eob-p)) (not (sv-parse--at "endcase")))
          (sv-parse--collect-until '(":"))
          (if (sv-parse--accept ":")
              (push (sv-parse--generate-block) items)
            (sv-parse--adv)))
        (sv-parse--accept "endcase")
        (list :type 'generate-case :items (nreverse items)
              :line line :beg beg :end sv-parse--pos)))

     ((member text '("function" "task"))
      (sv-parse--subprogram (intern text)))

     ((equal text "import")
      (let ((tokens (sv-parse--collect-until '(";"))))
        (sv-parse--accept ";")
        (list :type 'import :tokens tokens :text (sv-parse-token-text tokens)
              :line line :beg beg :end sv-parse--pos)))

     ((member text '("modport" "clocking" "property" "sequence" "covergroup"
                     "specify" "constraint" "defparam" "bind" "table"))
      (sv-parse--skip-block text)
      (list :type 'other :keyword text :line line :beg beg :end sv-parse--pos))

     ((member text '("assert" "assume" "cover" "restrict"))
      (sv-parse--collect-until '(";"))
      (sv-parse--accept ";")
      (list :type 'assertion :line line :beg beg :end sv-parse--pos))

     ((sv-parse--instance-p) (sv-parse--instance))

     ((sv-parse--decl-start-p) (sv-parse--declaration))

     ((sv-parse--at-any sv-parse--hard-stops) nil)

     (t (sv-parse--skip-statement)
        (list :type 'unknown :line line :beg beg :end sv-parse--pos)))))

(defun sv-parse--skip-block (keyword)
  "Skip a construct introduced by KEYWORD up to its matching end keyword."
  (let ((end-keyword (cdr (assoc keyword
                                 '(("modport" . nil) ("clocking" . "endclocking")
                                   ("property" . "endproperty")
                                   ("sequence" . "endsequence")
                                   ("covergroup" . "endgroup")
                                   ("specify" . "endspecify")
                                   ("table" . "endtable")
                                   ("constraint" . nil)
                                   ("defparam" . nil) ("bind" . nil))))))
    (if (null end-keyword)
        (sv-parse--skip-statement)
      (sv-parse--adv)
      (while (and (not (sv-parse--eob-p)) (not (sv-parse--at end-keyword)))
        (sv-parse--adv))
      (sv-parse--accept end-keyword))))

(defun sv-parse--generate-block ()
  "Parse the body of a generate loop or conditional."
  (if (sv-parse--at "begin")
      (let ((line (sv-parse--line)) (beg sv-parse--pos) (label nil) (items '()))
        (sv-parse--adv)
        (when (sv-parse--accept ":")
          (setq label (sv-parse--text))
          (sv-parse--adv))
        (while (and (not (sv-parse--eob-p))
                    (not (sv-parse--at "end"))
                    (not (sv-parse--at-any sv-parse--hard-stops)))
          (sv-parse--gather items (sv-parse--module-item)))
        (sv-parse--accept "end")
        (when (sv-parse--accept ":") (sv-parse--adv))
        (list :type 'generate-block :label label :items (nreverse items)
              :line line :beg beg :end sv-parse--pos))
    (sv-parse--module-item)))

(defun sv-parse--design-unit (kind end-keyword)
  "Parse a design unit of KIND terminated by END-KEYWORD."
  (let ((line (sv-parse--line))
        (beg sv-parse--pos)
        (name nil) (name-token nil) (params nil) (ports nil) (items '())
        (end-label nil) (end-label-token nil))
    (sv-parse--adv)
    (while (sv-parse--at-any '("static" "automatic" "virtual")) (sv-parse--adv))
    (when (eq (sv-parse--type) 'ident)
      (setq name (sv-parse--text))
      (setq name-token (sv-parse--tok))
      (sv-parse--adv))
    ;; A design unit may import packages between its name and its headers.
    (while (sv-parse--at "import")
      (let ((tokens (sv-parse--collect-until '(";"))))
        (sv-parse--accept ";")
        (push (list :type 'import :tokens tokens
                    :text (sv-parse-token-text tokens)
                    :line (if tokens (sv-token-line (car tokens)) line))
              items)))
    (when (sv-parse--at "#")
      (sv-parse--adv)
      (when (sv-parse--at "(")
        (setq params (sv-parse--param-list
                      (sv-parse--inside-balanced)))))
    (when (sv-parse--at "(")
      (setq ports (sv-parse--port-list (sv-parse--inside-balanced))))
    ;; `extends'/`implements' clauses of a class, import lists of a package.
    (sv-parse--collect-until '(";"))
    (sv-parse--accept ";")
    (while (and (not (sv-parse--eob-p)) (not (sv-parse--at end-keyword)))
      (sv-parse--gather items (sv-parse--module-item)))
    (sv-parse--accept end-keyword)
    (when (sv-parse--accept ":")
      (setq end-label (sv-parse--text))
      (setq end-label-token (sv-parse--tok))
      (sv-parse--adv))
    (list :type kind :name name :name-token name-token
          :params params :ports ports
          :items (nreverse items)
          :end-label end-label :end-label-token end-label-token
          :line line :beg beg :end sv-parse--pos)))

(defconst sv-parse--design-units
  '(("module" . module) ("macromodule" . module) ("interface" . interface)
    ("package" . package) ("program" . program) ("class" . class))
  "Top-level keywords and the node type they produce.")

(defconst sv-parse--design-unit-ends
  '((module . "endmodule") (interface . "endinterface")
    (package . "endpackage") (program . "endprogram") (class . "endclass"))
  "End keyword for each design-unit type.")

(defun sv-parse-tokens (tokens)
  "Parse the significant TOKENS of one source file.
TOKENS is the full token list produced by `sv-lex'.  Return a plist
holding the design units together with the token stream they came from."
  (let* ((significant (sv-lex-significant tokens))
         (sv-parse--toks significant)
         (sv-parse--pos 0)
         (units '()))
    (while (not (sv-parse--eob-p))
      (let* ((text (sv-parse--text))
             (kind (cdr (assoc text sv-parse--design-units))))
        (cond
         (kind
          (push (sv-parse--design-unit kind (cdr (assq kind sv-parse--design-unit-ends)))
                units))
         ((equal text "primitive")
          (while (and (not (sv-parse--eob-p)) (not (sv-parse--at "endprimitive")))
            (sv-parse--adv))
          (sv-parse--accept "endprimitive"))
         (t (sv-parse--adv)))))
    (list :type 'file
          :units (nreverse units)
          :tokens tokens
          :significant significant)))

(defun sv-parse-buffer (&optional buffer)
  "Parse BUFFER (the current one by default) and return its syntax tree."
  (with-current-buffer (or buffer (current-buffer))
    (sv-parse-tokens (sv-lex))))

(defun sv-parse-string (text)
  "Parse TEXT and return its syntax tree."
  (with-temp-buffer
    (insert text)
    (sv-parse-tokens (sv-lex))))

(defun sv-parse-file (file)
  "Parse FILE and return its syntax tree."
  (with-temp-buffer
    (insert-file-contents file)
    (sv-parse-tokens (sv-lex))))



;;;; Declaration gathering

(defvar sv-parse--declarations nil
  "Accumulator used while walking a node for the names it declares.")

(defvar sv-parse--scope 0
  "Identifier of the declaration scope being walked.")

(defvar sv-parse--scope-counter 0
  "Source of fresh scope identifiers.")

(defmacro sv-parse--in-new-scope (&rest body)
  "Run BODY with a fresh declaration scope.
Each block, generate branch and subprogram is a scope of its own, so the
same name may be declared in two of them without clashing."
  (declare (indent 0) (debug t))
  `(let ((sv-parse--scope (cl-incf sv-parse--scope-counter)))
     ,@body))

(defun sv-parse--record-declaration (name kind line token &optional extra)
  "Push a declaration record built from NAME, KIND, LINE, TOKEN and EXTRA."
  (when (and name (stringp name))
    (push (append (list :name name :kind kind :line line :token token
                        :scope sv-parse--scope)
                  extra)
          sv-parse--declarations)))

(defun sv-parse-header-names (tokens)
  "Return identifiers declared by a loop header TOKENS.
Handles `for (int unsigned i = 0; ...)\=' as well as `foreach (a[i, j])\='."
  (let ((names '()) (typed nil) (bracket-depth 0) (foreach nil))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok)))
        (cond
         ((equal text "[")
          (setq bracket-depth (1+ bracket-depth))
          (setq foreach t))
         ((equal text "]") (setq bracket-depth (max 0 (1- bracket-depth))))
         ((or (member text sv-lexer-data-types)
              (member text '("genvar" "var")))
          (setq typed t))
         ;; A sign qualifier sits between the type and the name.
         ((member text '("signed" "unsigned")) nil)
         ((and (eq (sv-token-type tok) 'ident)
               (or typed (and foreach (> bracket-depth 0))))
          (push (cons text tok) names)
          (setq typed nil))
         ((member text '(";" "=")) (setq typed nil)))))
    (nreverse names)))

(defun sv-parse--scan-declarations (node)
  "Collect every name NODE declares, recursively."
  (when (and node (listp node) (plist-member node :type))
    (let ((type (plist-get node :type)))
      (cl-case type
        (decl
         (dolist (member (plist-get node :enum-members))
           (sv-parse--record-declaration (plist-get member :name) 'enum
                                         (plist-get member :line) nil))
         (dolist (name (plist-get node :names))
           (sv-parse--record-declaration (plist-get name :name)
                                 (if (plist-get node :nettype) 'net 'var)
                                 (plist-get name :line) (plist-get name :token)
                                 (list :node node :decl name))))
        (param
         (dolist (param (plist-get node :params))
           (sv-parse--record-declaration (plist-get param :name) 'param
                                         (plist-get param :line)
                                         (plist-get param :token)
                                         (list :node node :decl param))))
        (genvar
         (dolist (name (plist-get node :names))
           (sv-parse--record-declaration (plist-get name :name) 'genvar
                                 (plist-get name :line) (plist-get name :token))))
        (typedef
         (sv-parse--record-declaration (plist-get node :name) 'typedef
                                       (plist-get node :line) nil
                                       (list :node node))
         (dolist (member (plist-get node :enum-members))
           (sv-parse--record-declaration (plist-get member :name) 'enum
                                 (plist-get member :line) nil)))
        ((task function)
         (sv-parse--record-declaration (plist-get node :name) type
                                       (plist-get node :line) nil
                                       (list :node node))
         (sv-parse--in-new-scope
           (dolist (arg (plist-get node :args))
             (sv-parse--record-declaration (plist-get arg :name) 'arg
                                           (plist-get arg :line)
                                           (plist-get arg :token)))
           (mapc #'sv-parse--scan-declarations (plist-get node :body))))
        (instance
         (dolist (sibling (plist-get node :siblings))
           (sv-parse--record-declaration (plist-get sibling :name) 'instance
                                         (plist-get sibling :line) nil
                                         (list :node node
                                               :module (plist-get node :module)))))
        ((generate generate-block)
         (sv-parse--record-declaration (plist-get node :label) 'label
                                       (plist-get node :line) nil)
         (sv-parse--in-new-scope
           (mapc #'sv-parse--scan-declarations (plist-get node :items))))
        (generate-for
         (sv-parse--in-new-scope
           (dolist (pair (sv-parse-header-names (plist-get node :header)))
             (sv-parse--record-declaration (car pair) 'genvar
                                           (sv-token-line (cdr pair)) (cdr pair)))
           (sv-parse--scan-declarations (plist-get node :body))))
        (generate-if
         (sv-parse--in-new-scope
           (sv-parse--scan-declarations (plist-get node :then)))
         (sv-parse--in-new-scope
           (sv-parse--scan-declarations (plist-get node :else))))
        (generate-case
         (mapc #'sv-parse--scan-declarations (plist-get node :items)))
        (block
         (sv-parse--record-declaration (plist-get node :label) 'label
                                       (plist-get node :line) nil)
         (sv-parse--in-new-scope
           (mapc #'sv-parse--scan-declarations (plist-get node :stmts))))
        (fork (sv-parse--in-new-scope
                (mapc #'sv-parse--scan-declarations (plist-get node :stmts))))
        ((always initial final)
         (sv-parse--scan-declarations (plist-get node :body)))
        (if
         (sv-parse--scan-declarations (plist-get node :then))
         (sv-parse--scan-declarations (plist-get node :else)))
        (case
         (dolist (item (plist-get node :items))
           (sv-parse--scan-declarations (plist-get item :stmt))))
        (loop
         (sv-parse--in-new-scope
           (dolist (pair (sv-parse-header-names (plist-get node :header)))
             (sv-parse--record-declaration (car pair) 'loopvar
                                           (sv-token-line (cdr pair)) (cdr pair)))
           (sv-parse--scan-declarations (plist-get node :body))))
        (t nil)))))

(defun sv-parse--unit-declarations (unit)
  "Return every declaration made by UNIT, ports and parameters included."
  (let ((sv-parse--declarations '()))
    (dolist (param (plist-get unit :params))
      (sv-parse--record-declaration (plist-get param :name) 'param
                                    (plist-get param :line)
                                    (plist-get param :token)
                                    (list :decl param)))
    (dolist (port (plist-get unit :ports))
      (sv-parse--record-declaration (plist-get port :name) 'port
                            (plist-get port :line) (plist-get port :token)
                            (list :dir (plist-get port :dir) :port port)))
    (mapc #'sv-parse--scan-declarations (plist-get unit :items))
    (nreverse sv-parse--declarations)))


(defun sv-parse-statements (node)
  "Return NODE and every statement nested inside it, depth first."
  (let ((found '()))
    (cl-labels
        ((walk (stmt)
           (when (and stmt (listp stmt) (plist-member stmt :type))
             (push stmt found)
             (cl-case (plist-get stmt :type)
               (block (mapc #'walk (plist-get stmt :stmts)))
               (fork (mapc #'walk (plist-get stmt :stmts)))
               (if (walk (plist-get stmt :then)) (walk (plist-get stmt :else)))
               (case (dolist (item (plist-get stmt :items))
                       (walk (plist-get item :stmt))))
               (loop (walk (plist-get stmt :body)))
               ((always initial final) (walk (plist-get stmt :body)))
               ((module interface program package generate generate-block)
                (mapc #'walk (plist-get stmt :items)))
               (generate-for (walk (plist-get stmt :body)))
               (generate-if (walk (plist-get stmt :then))
                            (walk (plist-get stmt :else)))
               (generate-case (mapc #'walk (plist-get stmt :items)))
               ((task function) (mapc #'walk (plist-get stmt :body)))
               (t nil)))))
      (walk node))
    (nreverse found)))

(defun sv-parse-assigned-names (node)
  "Return every signal NODE assigns to, on any path."
  (let ((names '()))
    (dolist (stmt (sv-parse-statements node))
      (when (memq (plist-get stmt :type) '(assign continuous-assign))
        (setq names (append (plist-get stmt :lhs-targets) names))))
    (delete-dups names)))

(defun sv-parse-declarations (node)
  "Return every name NODE declares, as a list of plists.
Each record carries at least `:name\=', `:kind\=', `:line\=' and `:token\='.
For a design unit the list starts with its parameters and ports; for any
other node it holds whatever that subtree declares."
  (let ((sv-parse--scope 0)
        (sv-parse--scope-counter 0))
    (if (memq (plist-get node :type) '(module interface package program class))
        (sv-parse--unit-declarations node)
      (let ((sv-parse--declarations '()))
        (sv-parse--scan-declarations node)
        (nreverse sv-parse--declarations)))))

(defun sv-parse-declared-names (node)
  "Return the names NODE declares, as a list of strings."
  (mapcar (lambda (record) (plist-get record :name))
          (sv-parse-declarations node)))


;;;; References

(defun sv-parse-non-reference-tokens (unit declarations)
  "Return the tokens of UNIT that declare a name rather than use one.
DECLARATIONS is what `sv-parse-declarations\=' returned for UNIT.  The
result is a hash holding both token objects and, for the names that have
no token of their own, their index into the significant-token vector."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (decl declarations)
      (when (plist-get decl :token) (puthash (plist-get decl :token) t table)))
    (dolist (instance (sv-parse-collect unit 'instance))
      (puthash (plist-get instance :beg) t table))
    ;; The members of a struct are declarations, not uses of a signal that
    ;; happens to share their name.
    (dolist (type '(typedef decl))
      (dolist (node (sv-parse-collect unit type))
        (dolist (member (plist-get node :members))
          (when (plist-get member :token)
            (puthash (plist-get member :token) t table)))))
    ;; `modport m (...)' and friends name a scope, not a signal.
    (dolist (node (sv-parse-collect unit 'other))
      (when (plist-get node :beg)
        (puthash (1+ (plist-get node :beg)) t table)))
    table))

(defun sv-parse-references (unit significant &optional ignored)
  "Return the identifier references of UNIT as (name . token) pairs.
SIGNIFICANT is the vector of non-trivia tokens the unit was parsed from.
IGNORED holds the tokens that declare rather than use a name, as
`sv-parse-non-reference-tokens\=' computes them; it is worked out here when
not supplied.

What this leaves out is what tells a reference from a coincidence: a
field after a dot, a name qualified by a package, the argument of a
compiler directive, a member of a struct, the label on an assertion and
the key of an assignment pattern all spell an identifier without reading
the signal of that name."
  (let* ((vec significant)
         (ignored (or ignored
                      (sv-parse-non-reference-tokens
                       unit (sv-parse-declarations unit))))
         (limit (length vec))
         (beg (or (plist-get unit :beg) 0))
         (end (min (or (plist-get unit :end) limit) limit))
         (refs '()))
    (cl-loop
     for i from beg below end
     for tok = (aref vec i)
     when (and (eq (sv-token-type tok) 'ident)
               (not (gethash tok ignored))
               (not (gethash i ignored))
               (let ((prev (and (> i beg) (aref vec (1- i)))))
                 ;; `\=`ifdef VERILATOR\=' names a macro, not a signal.
                 (not (and prev (eq (sv-token-type prev) 'directive))))
               (let ((prev (and (> i beg) (aref vec (1- i)))))
                 (not (and prev (member (sv-token-text prev)
                                        '("." "::" "module" "macromodule"
                                          "interface" "package" "program"
                                          "class" "function" "task"
                                          "endmodule" "endinterface"
                                          "endpackage" "endprogram" "endclass"
                                          "endfunction" "endtask")))))
               (let ((next (and (< (1+ i) end) (aref vec (1+ i)))))
                 (not (and next (equal (sv-token-text next) "::"))))
               ;; `check_id : assert ...' labels the statement.
               (not (and (< (+ i 2) end)
                         (equal (sv-token-text (aref vec (1+ i))) ":")
                         (member (sv-token-text (aref vec (+ i 2)))
                                 '("assert" "assume" "cover" "restrict" "expect"
                                   "property" "sequence" "always" "always_comb"
                                   "always_ff" "always_latch" "initial" "final"))))
               ;; `\='{id: x, len: y}' names members, it does not read them.
               (not (and (> i beg)
                         (member (sv-token-text (aref vec (1- i))) '("{" ","))
                         (< (1+ i) end)
                         (equal (sv-token-text (aref vec (1+ i))) ":"))))
     do (push (cons (sv-token-text tok) tok) refs))
    (nreverse refs)))


;;;; Tree walking

(defun sv-parse-walk (node function)
  "Call FUNCTION on NODE and, depth first, on every node below it."
  (when (and node (listp node) (plist-member node :type))
    (funcall function node)
    (dolist (key '(:units :items :stmts :body :then :else :names :params))
      (let ((value (plist-get node key)))
        (cond
         ((and (listp value) (plist-member value :type))
          (sv-parse-walk value function))
         ((listp value)
          (dolist (child value)
            (when (and (listp child) (plist-member child :type))
              (sv-parse-walk child function)))))))
    (dolist (item (plist-get node :items))
      (when (and (listp item) (plist-member item :stmt))
        (sv-parse-walk (plist-get item :stmt) function)))))

(defun sv-parse-collect (root type)
  "Return every node of TYPE inside ROOT."
  (let ((found '()))
    (sv-parse-walk root (lambda (node)
                          (when (eq (plist-get node :type) type)
                            (push node found))))
    (nreverse found)))

(provide 'sv-parser)

;;; sv-parser.el ends here
