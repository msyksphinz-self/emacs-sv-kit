;;; sv-format.el --- Formatter for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Re-indents and tidies SystemVerilog sources.  The formatter deliberately
;; never joins or splits lines: it recomputes the indentation of each line
;; from the token stream, collapses runs of spaces, drops trailing
;; whitespace, and then re-aligns the columns that RTL authors care about --
;; declaration names, assignment operators, instance port connections and
;; trailing comments.  Line breaks stay where the author put them, so the
;; diff of a first run stays readable and the result is stable: formatting an
;; already-formatted file is a no-op.
;;
;; `sv-format-indent-line' is suitable as an `indent-line-function', which
;; makes TAB and `indent-region' work inside any major mode for Verilog.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)

(defgroup sv-format nil
  "Formatting for SystemVerilog sources."
  :group 'tools
  :prefix "sv-format-")

(defcustom sv-format-indent-offset 2
  "Number of columns each level of nesting adds."
  :type 'integer
  :group 'sv-format)

(defcustom sv-format-continuation-offset 4
  "Extra columns added to a line that continues an unfinished expression."
  :type 'integer
  :group 'sv-format)

(defcustom sv-format-indent-unit-body t
  "When non-nil, indent the body of a module, interface or package.
Set it to nil to keep design-unit bodies flush with their header, the
style used by some Verilog code bases."
  :type 'boolean
  :group 'sv-format)

(defcustom sv-format-directive-column 'zero
  "Where to put a line that starts with a preprocessor directive.
`zero' puts `\\=`ifdef\\=' and friends in column 0, `code' indents them
like ordinary statements."
  :type '(choice (const zero) (const code))
  :group 'sv-format)

(defcustom sv-format-align
  '(decl-type decl-name case-colon assign-op conn-paren comment)
  "Which columns to align across consecutive similar lines.
`decl-type' lines up the data type of ports after their direction,
`decl-name' lines up declared names after their data type, `case-colon'
lines up the colon of case arms, `assign-op' lines up `=' and `<=',
`conn-paren' lines up the parenthesis of instance port connections, and
`comment' lines up trailing comments."
  :type '(set (const decl-type) (const decl-name) (const case-colon)
              (const assign-op) (const conn-paren) (const comment))
  :group 'sv-format)

(defcustom sv-format-align-max-spread 40
  "Widest column spread an alignment group may have.
A group whose anchors are further apart than this is left alone, so one
very long line cannot push a whole block to the right."
  :type 'integer
  :group 'sv-format)

(defconst sv-format--preprocessor-directives
  '("`ifdef" "`ifndef" "`else" "`elsif" "`endif" "`define" "`undef"
    "`include" "`timescale" "`default_nettype" "`line" "`resetall"
    "`celldefine" "`endcelldefine" "`unconnected_drive" "`nounconnected_drive"
    "`pragma" "`begin_keywords" "`end_keywords")
  "Directives that conventionally sit in column 0.")

(defconst sv-format--closer-alist
  '(("end" . begin) ("endcase" . case)
    ("join" . fork) ("join_any" . fork) ("join_none" . fork)
    (")" . paren) ("]" . bracket) ("}" . brace)
    ("endmodule" . unit) ("endinterface" . unit) ("endpackage" . unit)
    ("endprogram" . unit) ("endclass" . unit) ("endfunction" . unit)
    ("endtask" . unit) ("endgenerate" . unit) ("endgroup" . unit)
    ("endproperty" . unit) ("endsequence" . unit) ("endclocking" . unit)
    ("endspecify" . unit) ("endtable" . unit))
  "Token spellings that close a frame, and the frame kind they close.")

(defconst sv-format--unit-openers
  '("module" "macromodule" "interface" "package" "program" "class" "generate"
    "function" "task" "covergroup" "property" "sequence" "clocking" "specify"
    "table")
  "Token spellings that open a design-unit-like frame.")

(defconst sv-format--body-units
  '("module" "macromodule" "interface" "package" "program" "class" "generate")
  "Design units whose body indentation `sv-format-indent-unit-body' governs.")

(defconst sv-format--dangling-keywords
  '("if" "for" "while" "foreach" "repeat" "forever" "do" "else"
    "always" "always_comb" "always_ff" "always_latch" "initial" "final")
  "Keywords followed by a sub-statement that gains one indentation level.")

(defconst sv-format--no-continuation-operators
  '("." "::" "@" "#" "'" "'{" ":" "++" "--")
  "Operators that do not make the next line a continuation.")


;;;; Indentation engine

;; Each open construct becomes a frame carrying two columns: `:open', the
;; indentation of the line its opening token appeared on, and `:indent', the
;; indentation its contents get.  A closing token therefore lands back on the
;; column of the line that opened it, whatever happened in between, and a
;; line inside a construct simply takes the `:indent' of the innermost frame.

(defsubst sv-format--kind (frame) (plist-get frame :kind))

(defun sv-format--frame (kind open &optional indented)
  "Return a frame of KIND opened on a line indented to OPEN.
Its contents are indented one step further unless INDENTED is `same'."
  (list :kind kind :open open
        :indent (if (eq indented 'same) open (+ open sv-format-indent-offset))))

(defun sv-format--pop-danglings (stack)
  "Return STACK with every dangling frame on top removed."
  (while (and stack (eq (sv-format--kind (car stack)) 'dangling))
    (setq stack (cdr stack)))
  stack)

(defun sv-format--pop-for (stack text)
  "Close whatever frame TEXT terminates inside STACK.
Return a cons of the remaining stack and the column the closing token
belongs on, or nil when TEXT closes nothing."
  (let ((kind (cdr (assoc text sv-format--closer-alist))))
    (cond
     ((equal text "else")
      (let ((probe stack) (column nil))
        (while (and probe (eq (sv-format--kind (car probe)) 'dangling))
          (setq column (plist-get (car probe) :open))
          (setq probe (cdr probe)))
        (cons probe column)))
     (kind
      (let ((probe (sv-format--pop-danglings stack)))
        ;; Find the frame this token closes.  An unbalanced source must not
        ;; be allowed to unwind frames that are still open around it.
        (let ((search probe))
          (while (and search (not (eq (sv-format--kind (car search)) kind)))
            (setq search (cdr search)))
          (if search
              (cons (cdr search) (plist-get (car search) :open))
            (cons stack nil)))))
     (t (cons stack nil)))))

(defun sv-format--push-for (stack text recent column)
  "Return STACK after TEXT, seen on a line indented to COLUMN, opened a frame.
RECENT holds the spellings of the tokens just seen, most recent first."
  (cond
   ((equal text "(") (cons (sv-format--frame 'paren column) stack))
   ((equal text "[") (cons (sv-format--frame 'bracket column) stack))
   ((equal text "{") (cons (sv-format--frame 'brace column) stack))
   ((equal text "begin")
    (let* ((below (sv-format--pop-danglings stack))
           ;; A `begin' takes over the level its dangling statement had.
           ;; The frames just popped are the dangling ones; the deepest of
           ;; them sits at index (- (length stack) (length below) 1), which
           ;; `last' reaches with (1+ (length below)) elements.
           (open (if (eq below stack)
                     column
                   (plist-get (car (last stack (1+ (length below))))
                              :open))))
      (cons (sv-format--frame 'begin (min column (or open column))) below)))
   ((member text '("case" "casex" "casez" "randcase"))
    (cons (sv-format--frame 'case column) stack))
   ((equal text "fork") (cons (sv-format--frame 'fork column) stack))
   ((member text sv-format--unit-openers)
    (cond
     ;; A prototype or an imported subprogram has no body to indent.
     ((and (member text '("function" "task"))
           (cl-intersection recent '("extern" "pure" "import") :test #'equal))
      stack)
     ;; `assert property (...)' is an assertion, not a declaration.
     ((and (member text '("property" "sequence"))
           (member (car recent) '("assert" "assume" "cover" "restrict" "expect")))
      stack)
     (t (cons (sv-format--frame
               'unit column
               (and (member text sv-format--body-units)
                    (not sv-format-indent-unit-body)
                    'same))
              stack))))
   (t stack)))

(defun sv-format--continuation-p (stack text previous)
  "Return non-nil when a line starting with TEXT continues a previous line.
STACK is the frame stack and PREVIOUS the last significant token."
  (and previous
       (not (assoc text sv-format--closer-alist))
       (not (member text sv-format--preprocessor-directives))
       (let ((previous-text (sv-token-text previous)))
         (or (and (eq (sv-token-type previous) 'operator)
                  (not (member previous-text sv-format--no-continuation-operators)))
             (and (equal previous-text ",")
                  (not (memq (sv-format--kind (car stack))
                             '(paren bracket brace))))))))

(defun sv-format--indent-table (tokens)
  "Return a hash mapping each line of TOKENS to the column it should start at."
  (let* ((significant (vconcat (cl-remove-if #'sv-token-trivia-p tokens)))
         (count (length significant))
         (indents (make-hash-table :test #'eq))
         (stack '())
         ;; Columns of the `if\=' statements still waiting for an `else\='.
         (open-ifs '())
         (pending nil)
         (previous nil)
         (recent '())
         (column 0)
         (current-line -1))
    (dotimes (index count)
      (let* ((tok (aref significant index))
             (text (sv-token-text tok))
             (line (sv-token-line tok))
             (next (and (< (1+ index) count)
                        (sv-token-text (aref significant (1+ index)))))
             (else-column nil)
             (closed nil))
        ;; An `else\=' whose `if\=' we remember keeps the frames around it: the
        ;; whole conditional is still one statement of its enclosing block.
        (unless (and (equal text "else") open-ifs)
          (setq closed (sv-format--pop-for stack text))
          (setq stack (car closed)))
        ;; A statement hanging off `if\=', `for\=' or `always\=' gains a level as
        ;; soon as it actually starts.
        (when pending
          (cond
           ((equal text "begin") (setq pending nil))
           ((> (length stack) (plist-get pending :depth)) nil)
           ((member text '("(" "[" "@" "#" ")" "]")) nil)
           (t (setq stack (cons (sv-format--frame
                                 'dangling (plist-get pending :column))
                                stack))
              (setq pending nil))))
        (when (and (equal text "else") open-ifs)
          (setq else-column (car (pop open-ifs))))
        (unless (eq line current-line)
          (setq column
                (cond
                 ((and (eq sv-format-directive-column 'zero)
                       (member text sv-format--preprocessor-directives))
                  0)
                 (else-column else-column)
                 (t (+ (or (cdr closed)
                           (if stack (plist-get (car stack) :indent) 0))
                       (if (sv-format--continuation-p stack text previous)
                           sv-format-continuation-offset
                         0)))))
          (puthash line column indents)
          (setq current-line line))
        (when (equal text "if")
          (push (cons column (length stack)) open-ifs))
        (setq stack (sv-format--push-for stack text recent column))
        (when (member text sv-format--dangling-keywords)
          (setq pending (list :column column :depth (length stack))))
        (cond
         ((equal text ";") (setq stack (sv-format--pop-danglings stack)))
         ((and (equal text ":") (eq (sv-format--kind (car stack)) 'case))
          (setq stack (cons (sv-format--frame 'dangling column) stack))))
        ;; A conditional that no `else\=' follows is finished; forget it, so a
        ;; later `else\=' cannot pair with it.
        (when (and (or (equal text ";")
                       ;; A statement ends; a closing bracket does not.
                       (memq (cdr (assoc text sv-format--closer-alist))
                             '(begin case fork unit)))
                   (not (equal next "else")))
          (let ((depth (length stack)))
            (while (and open-ifs (>= (cdar open-ifs) depth))
              (pop open-ifs))))
        (setq previous tok)
        (setq recent (cons text (cl-subseq recent 0 (min 3 (length recent)))))))
    (sv-format--fill-comment-indents tokens indents)
    indents))

(defun sv-format--fill-comment-indents (tokens indents)
  "Give every comment-only line of TOKENS an entry in INDENTS.
A comment takes the deeper of the code around it, so that it stays with
the block it documents."
  (let ((code-lines (sort (hash-table-keys indents) #'<))
        (comment-lines '()))
    (dolist (tok tokens)
      (when (and (memq (sv-token-type tok) '(comment attribute))
                 (not (gethash (sv-token-line tok) indents)))
        (push (sv-token-line tok) comment-lines)))
    (dolist (line (nreverse comment-lines))
      (let ((before 0) (after nil))
        (dolist (code code-lines)
          (cond ((< code line) (setq before (gethash code indents)))
                ((and (> code line) (null after)) (setq after (gethash code indents)))))
        (puthash line (max before (or after 0)) indents)))))


;;;; Rendering

(defun sv-format--separator (text previous-text had-space)
  "Return the spacing to put before TEXT, which follows PREVIOUS-TEXT.
HAD-SPACE says whether the source had whitespace between the two."
  (cond
   ((member text '("," ";" ")" "]")) "")
   ((member previous-text '("(" "[")) "")
   ((member previous-text '("," ";")) " ")
   (had-space " ")
   (t "")))

(defun sv-format--render (tokens indents)
  "Render TOKENS back to text, indenting each line as INDENTS says."
  (let ((lines '())
        (line (if tokens (sv-token-line (car tokens)) 1))
        (current nil)
        (previous-text nil)
        (space nil))
    (dolist (tok tokens)
      (let ((text (sv-token-text tok))
            (type (sv-token-type tok)))
        (if (eq type 'ws)
            (let ((breaks (cl-count ?\n text)))
              (if (> breaks 0)
                  (progn
                    (push (or current "") lines)
                    (dotimes (_ (1- breaks)) (push "" lines))
                    (setq line (+ line breaks) current nil space nil
                          previous-text nil))
                (setq space t)))
          (if (null current)
              (setq current (make-string (max 0 (gethash line indents 0)) ?\s))
            (setq current (concat current
                                  (sv-format--separator text previous-text space))))
          (setq current (concat current text))
          (setq space nil)
          (setq previous-text text)
          (let ((breaks (cl-count ?\n text)))
            (when (> breaks 0) (setq line (+ line breaks)))))))
    (push (or current "") lines)
    (mapconcat #'identity (nreverse lines) "\n")))


;;;; Column alignment

(defun sv-format--line-tokens (tokens)
  "Group TOKENS into an alist of (LINE . TOKENS), trivia included."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (tok tokens)
      (push tok (gethash (sv-token-line tok) table)))
    (let ((result '()))
      (maphash (lambda (line toks) (push (cons line (nreverse toks)) result)) table)
      (sort result (lambda (a b) (< (car a) (car b)))))))

(defun sv-format--direction-anchor (tokens)
  "Return the token following the direction keyword of a port declaration."
  (let ((significant (cl-remove-if #'sv-token-trivia-p tokens)))
    (when (and (cdr significant)
               (member (sv-token-text (car significant))
                       sv-parse-direction-keywords))
      (cadr significant))))

(defun sv-format--declaration-anchor (tokens)
  "Return the token that starts the declared name of declaration TOKENS."
  (let* ((significant (cl-remove-if #'sv-token-trivia-p tokens))
         (first (car significant)))
    (when (and first
               (or (member (sv-token-text first) sv-parse-direction-keywords)
                   (member (sv-token-text first) sv-lexer-data-types)
                   (member (sv-token-text first) sv-lexer-net-types)
                   ;; A user-defined type: `entry_t e;' or `pkg::entry_t e;'.
                   (and (eq (sv-token-type first) 'ident)
                        (cdr significant)
                        (or (eq (sv-token-type (cadr significant)) 'ident)
                            (equal (sv-token-text (cadr significant)) "::"))))
               ;; Not a module header, an instantiation or a statement.
               (not (sv-parse-find-top significant '("("))))
      (let* ((stop (or (sv-parse-find-top significant '("," ";"))
                       (length significant)))
             (head (cl-subseq significant 0 stop))
             (declarator (sv-parse-declarator head)))
        (when (and (plist-get declarator :name)
                   (not (eq (plist-get declarator :token) (car head))))
          (plist-get declarator :token))))))

(defun sv-format--case-item-p (significant)
  "Return non-nil when SIGNIFICANT looks like the start of a case arm."
  (let ((colon (sv-parse-find-top significant '(":"))))
    (and colon (> colon 0)
         (let ((head (cl-subseq significant 0 colon)))
           (and (null (sv-parse-find-top head '("=" "<=" "?" "(" "begin" "end")))
                (not (member (sv-token-text (car head))
                             '("begin" "end" "fork" "join" "module" "interface"
                               "package" "program" "class" "function" "task"))))))))

(defun sv-format--case-colon-anchor (tokens)
  "Return the colon token of a case arm made of TOKENS."
  (let ((significant (cl-remove-if #'sv-token-trivia-p tokens)))
    (when (sv-format--case-item-p significant)
      (nth (sv-parse-find-top significant '(":")) significant))))

(defun sv-format--assign-anchor (tokens)
  "Return the assignment operator token of statement TOKENS."
  (let* ((significant (cl-remove-if #'sv-token-trivia-p tokens))
         (first (car significant)))
    (when (and significant
               (equal (sv-token-text (car (last significant))) ";")
               (not (sv-format--case-item-p significant))
               (not (member (sv-token-text first)
                            '("for" "while" "if" "case" "casex" "casez"))))
      (let ((position (sv-parse-find-top
                       significant '("=" "<=" "+=" "-=" "|=" "&=" "^="))))
        (when position (nth position significant))))))

(defun sv-format--connection-anchor (tokens)
  "Return the opening parenthesis of a `.port (signal)' connection."
  (let* ((significant (cl-remove-if #'sv-token-trivia-p tokens))
         (first (car significant)))
    (when (and first (equal (sv-token-text first) ".")
               (cdr significant))
      (let ((position (sv-parse-find-top (cddr significant) '("("))))
        (when position (nth position (cddr significant)))))))

(defun sv-format--comment-anchor (tokens)
  "Return a trailing comment token of TOKENS, when the line has code first."
  (let ((code nil) (comment nil))
    (dolist (tok tokens)
      (cond
       ((eq (sv-token-type tok) 'comment) (unless comment (setq comment (and code tok))))
       ((not (sv-token-trivia-p tok)) (setq code t))))
    comment))

(defun sv-format--anchor (kind tokens)
  "Return the token KIND wants to align on the line made of TOKENS."
  (cl-case kind
    (decl-type (sv-format--direction-anchor tokens))
    (decl-name (sv-format--declaration-anchor tokens))
    (case-colon (sv-format--case-colon-anchor tokens))
    (assign-op (sv-format--assign-anchor tokens))
    (conn-paren (sv-format--connection-anchor tokens))
    (comment (sv-format--comment-anchor tokens))
    (t nil)))

(defun sv-format--align-pass (text kind)
  "Return TEXT with the KIND anchors of consecutive similar lines aligned."
  (let* ((tokens (sv-lex-string text))
         (lines (vconcat (split-string text "\n")))
         (groups '())
         (current '()))
    (cl-flet ((flush ()
                (when (> (length current) 1)
                  (push (nreverse current) groups))
                (setq current '())))
      (dolist (entry (sv-format--line-tokens tokens))
        (let* ((line (car entry))
               (anchor (sv-format--anchor kind (cdr entry)))
               (indent (and anchor
                            (string-match "\\`[ \t]*" (aref lines (1- line)))
                            (match-end 0))))
          (if (null anchor)
              (flush)
            (let ((previous (car current)))
              (when (and previous
                         (or (/= (nth 0 previous) (1- line))
                             (/= (nth 2 previous) indent)))
                (flush))
              (push (list line (sv-token-col anchor) indent) current)))))
      (flush))
    (dolist (group groups)
      (let* ((columns (mapcar #'cl-second group))
             (target (apply #'max columns)))
        (when (<= (- target (apply #'min columns)) sv-format-align-max-spread)
          (dolist (entry group)
            (let* ((line (cl-first entry))
                   (column (cl-second entry))
                   (text (aref lines (1- line)))
                   (padding (- target column)))
              (when (> padding 0)
                (aset lines (1- line)
                      (concat (substring text 0 column)
                              (make-string padding ?\s)
                              (substring text column)))))))))
    (mapconcat #'identity (append lines nil) "\n")))

(defun sv-format--align (text)
  "Align every column family `sv-format-align' asks for inside TEXT."
  (dolist (kind '(decl-type decl-name case-colon assign-op conn-paren comment))
    (when (memq kind sv-format-align)
      (setq text (sv-format--align-pass text kind))))
  text)


;;;; Entry points

(defun sv-format-text (text)
  "Return the formatted form of the SystemVerilog source TEXT."
  (let* ((tokens (sv-lex-string text))
         (indents (sv-format--indent-table tokens)))
    (sv-format--align (sv-format--render tokens indents))))

(defun sv-format--replace-lines (beg end new-text)
  "Replace the text between BEG and END with NEW-TEXT, line by line.
Only lines that actually change are touched, which keeps point, markers
and the undo history intact."
  (save-excursion
    (let ((new-lines (split-string new-text "\n"))
          (line-start beg)
          (changed 0))
      (goto-char beg)
      (while (and new-lines (< line-start end))
        (let* ((line-end (min end (line-end-position)))
               (old (buffer-substring-no-properties line-start line-end))
               (new (car new-lines)))
          (unless (equal old new)
            (setq changed (1+ changed))
            (let ((offset (- (point) line-start)))
              (delete-region line-start line-end)
              (goto-char line-start)
              (insert new)
              (setq end (+ end (- (length new) (length old))))
              (ignore offset)))
          (setq new-lines (cdr new-lines))
          (goto-char (min (point-max) (1+ (line-end-position))))
          (setq line-start (point))))
      changed)))

;;;###autoload
(defun sv-format-region (beg end)
  "Format the SystemVerilog source between BEG and END.
The region is widened to whole lines; indentation is computed from the
start of the buffer so that the region's nesting is known."
  (interactive "r")
  (let* ((beg (save-excursion (goto-char beg) (line-beginning-position)))
         (end (save-excursion (goto-char end) (line-end-position)))
         (tokens (sv-lex (point-min) (point-max)))
         (indents (sv-format--indent-table tokens))
         (first-line (line-number-at-pos beg))
         (last-line (line-number-at-pos end))
         (region-tokens (cl-remove-if-not
                         (lambda (tok)
                           (and (>= (sv-token-line tok) first-line)
                                (<= (sv-token-line tok) last-line)))
                         tokens))
         (rendered (sv-format--align
                    (sv-format--render region-tokens indents))))
    (sv-format--replace-lines beg end rendered)))

;;;###autoload
(defun sv-format-buffer ()
  "Format the whole buffer as SystemVerilog."
  (interactive)
  (let ((formatted (sv-format-text
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (sv-format--replace-lines (point-min) (point-max) formatted)))

;;;###autoload
(defun sv-format-indent-region (beg end)
  "Reindent every line between BEG and END, touching only leading whitespace.
Suitable as an `indent-region-function': unlike `sv-format-region' it
leaves the rest of each line alone, and it lexes the buffer once instead
of once per line."
  (interactive "r")
  (let* ((tokens (sv-lex (point-min) (point-max)))
         (indents (sv-format--indent-table tokens))
         (end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end-marker)
        (let ((column (gethash (line-number-at-pos (point)) indents)))
          (when (and column (not (looking-at-p "[ \t]*$")))
            (indent-line-to column)))
        (forward-line 1)))
    (set-marker end-marker nil)))

;;;###autoload
(defun sv-format-indent-line ()
  "Indent the current line as SystemVerilog.
Suitable as an `indent-line-function'."
  (interactive)
  (let* ((line (line-number-at-pos (line-beginning-position)))
         (tokens (sv-lex (point-min) (min (point-max)
                                          (1+ (line-end-position)))))
         (indents (sv-format--indent-table tokens))
         (column (gethash line indents)))
    (when column
      (let ((offset (- (point) (line-beginning-position)))
            (old (current-indentation)))
        (indent-line-to column)
        (when (> offset old)
          (goto-char (min (point-max)
                          (+ (line-beginning-position)
                             (+ offset (- column old))))))))))

(provide 'sv-format)

;;; sv-format.el ends here
