;;; sv-lint.el --- Linter for SystemVerilog -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Static checks over the tree produced by `sv-parser'.  The rules are the
;; ones that matter when writing synthesizable RTL by hand: blocking
;; assignments in clocked blocks, latches inferred from an incomplete
;; `always_comb', case statements without a default, signals that are
;; declared and never used or used and never declared, outputs nobody
;; drives, and positional instance connections.
;;
;; Diagnostics can be silenced from the source:
;;
;;   logic unused;                   // sv-lint: disable=unused-declaration
;;   // sv-lint: disable-next-line=case-without-default
;;   // sv-lint: disable-file=line-too-long
;;   /* verilator lint_off CASEINCOMPLETE */  ... /* verilator lint_on ... */
;;
;; A bare `// sv-lint: disable' silences every rule on its line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-width)

(defgroup sv-lint nil
  "Linting for SystemVerilog sources."
  :group 'tools
  :prefix "sv-lint-")

(cl-defstruct (sv-diagnostic (:constructor sv-diagnostic-create) (:copier nil))
  "One linter finding.  LINE is 1-based, COL is 0-based."
  file line col severity rule message)

(cl-defstruct (sv-lint-context (:constructor sv-lint-context-create) (:copier nil))
  "State shared by every rule during one lint run."
  file text lines tree tokens significant modules conditionals
  suppress-file suppress-line suppress-ranges diagnostics)

(defconst sv-lint-rule-alist
  '((blocking-in-always-ff      error   "Blocking assignment inside always_ff.")
    (nonblocking-in-always-comb warning "Non-blocking assignment in combinational logic.")
    (case-without-default       warning "Case statement without a default arm.")
    (implicit-latch             warning "Combinational block infers a latch.")
    (prefer-always-comb         info    "Use always_comb instead of always @*.")
    (incomplete-sensitivity     warning "Signal read but missing from the sensitivity list.")
    (undeclared-identifier      error   "Identifier used without a declaration.")
    (unused-declaration         warning "Declared signal is never used.")
    (unused-parameter           warning "Declared parameter is never used.")
    (undriven-output            warning "Output port is never driven.")
    (duplicate-declaration      error   "Name declared more than once.")
    (multiple-drivers           error   "Signal is driven from more than one place.")
    (assignment-to-input        error   "Input port is assigned to.")
    (duplicate-case-label       warning "Case label appears twice in one statement.")
    (mixed-assignment-style     warning "Block mixes blocking and non-blocking assignments.")
    (width-truncation           warning "Assignment drops the upper bits of its value.")
    (constant-overflow          warning "Constant does not fit in what it is assigned to.")
    (port-width-mismatch        warning "Instance connects a port to a signal of another width.")
    (positional-port-connection warning "Instance connects ports by position.")
    (unconnected-port           info    "Instance port is left unconnected.")
    (instance-unknown-port      error   "Instance connects a port the module does not have.")
    (instance-missing-port      warning "Instance leaves a module port out.")
    (unlabeled-generate-block   info    "Generate block has no label.")
    (module-filename-mismatch   warning "Module name differs from the file name.")
    (legacy-net-type            info    "Prefer logic over reg/wire.")
    (port-naming                info    "Port name does not follow the naming convention.")
    (line-too-long              info    "Line exceeds the configured width.")
    (trailing-whitespace        info    "Line has trailing whitespace.")
    (tab-indentation            info    "Line is indented with tabs.")
    (missing-final-newline      info    "File does not end with a newline."))
  "Every rule: its identifier, default severity and one-line description.")

(defcustom sv-lint-disabled-rules
  '(port-naming legacy-net-type unconnected-port port-width-mismatch)
  "Rules that are not run at all.
Every identifier of `sv-lint-rule-alist' is accepted."
  :type '(repeat symbol)
  :group 'sv-lint)

(defcustom sv-lint-severity-overrides nil
  "Alist mapping a rule identifier to the severity it should report with."
  :type '(alist :key-type symbol
                :value-type (choice (const error) (const warning) (const info)))
  :group 'sv-lint)

(defcustom sv-lint-max-line-length 100
  "Column beyond which `line-too-long' fires."
  :type 'integer
  :group 'sv-lint)

(defcustom sv-lint-port-prefixes
  '((input . "\\`i_") (output . "\\`o_") (inout . "\\`b_"))
  "Alist mapping a port direction to the regexp its name must match.
Only consulted when the `port-naming' rule is enabled."
  :type '(alist :key-type symbol :value-type regexp)
  :group 'sv-lint)

(defcustom sv-lint-case-default-exempt-qualified t
  "When non-nil, `unique' and `priority' case statements need no default arm."
  :type 'boolean
  :group 'sv-lint)

(defcustom sv-lint-ignored-name-regexp "\\`\\(?:_\\|unused_\\)"
  "Names matching this regexp are exempt from the unused-name rules."
  :type 'regexp
  :group 'sv-lint)

(defcustom sv-lint-verilator-alias
  '(("CASEINCOMPLETE" . case-without-default)
    ("CASEX"          . case-without-default)
    ("LATCH"          . implicit-latch)
    ("UNUSED"         . unused-declaration)
    ("UNUSEDSIGNAL"   . unused-declaration)
    ("UNUSEDPARAM"    . unused-parameter)
    ("UNDRIVEN"       . undriven-output)
    ("DECLFILENAME"   . module-filename-mismatch)
    ("PINCONNECTEMPTY" . unconnected-port)
    ("PINMISSING"     . instance-missing-port)
    ("BLKSEQ"         . blocking-in-always-ff)
    ("COMBDLY"        . nonblocking-in-always-comb)
    ("ALWCOMBORDER"   . incomplete-sensitivity))
  "Mapping from a Verilator warning code to the rule it silences.
Lets existing `/* verilator lint_off CODE */' pragmas suppress the
equivalent finding here."
  :type '(alist :key-type string :value-type symbol)
  :group 'sv-lint)


;;;; Reporting and suppression

(defun sv-lint-rule-severity (rule)
  "Return the severity RULE reports with."
  (or (cdr (assq rule sv-lint-severity-overrides))
      (nth 1 (assq rule sv-lint-rule-alist))
      'warning))

(defun sv-lint-rule-documentation (rule)
  "Return the one-line description of RULE."
  (nth 2 (assq rule sv-lint-rule-alist)))

(defun sv-lint--rule-enabled-p (rule)
  "Return non-nil when RULE should run."
  (not (memq rule sv-lint-disabled-rules)))

(defun sv-lint--suppressed-p (ctx rule line)
  "Return non-nil when RULE is silenced on LINE of CTX."
  (let ((file-set (sv-lint-context-suppress-file ctx))
        (line-set (gethash line (sv-lint-context-suppress-line ctx))))
    (or (memq t file-set)
        (memq rule file-set)
        (memq t line-set)
        (memq rule line-set)
        (cl-some (lambda (range)
                   (and (eq (nth 0 range) rule)
                        (>= line (nth 1 range))
                        (<= line (nth 2 range))))
                 (sv-lint-context-suppress-ranges ctx)))))

(defun sv-lint--report (ctx rule line col message)
  "Record a finding for RULE at LINE and COL of CTX with MESSAGE."
  (when (and (sv-lint--rule-enabled-p rule)
             (not (sv-lint--suppressed-p ctx rule line)))
    (push (sv-diagnostic-create :file (sv-lint-context-file ctx)
                                :line line :col (or col 0)
                                :severity (sv-lint-rule-severity rule)
                                :rule rule :message message)
          (sv-lint-context-diagnostics ctx))))

(defun sv-lint--report-token (ctx rule token message)
  "Record a finding for RULE at TOKEN of CTX with MESSAGE."
  (sv-lint--report ctx rule
                   (if token (sv-token-line token) 1)
                   (if token (sv-token-col token) 0)
                   message))

(defun sv-lint--parse-rule-names (text)
  "Turn the comma-separated rule list TEXT into a list of symbols.
An empty list means every rule, which is spelled t."
  (if (or (null text) (string-empty-p (string-trim text)))
      (list t)
    (mapcar #'intern (split-string text "[, \t]+" t))))

(defun sv-lint--scan-conditionals (ctx)
  "Record, for each line of CTX, which `\=`ifdef\=' branches are active on it.
Two declarations in different branches of the same conditional never
reach the compiler together, so they must not be reported as clashing."
  (let ((table (make-hash-table :test #'eq))
        (stack '())
        (counter 0))
    (dolist (token (sv-lint-context-tokens ctx))
      (when (eq (sv-token-type token) 'directive)
        (let ((text (sv-token-text token)))
          (cond
           ((member text '("`ifdef" "`ifndef"))
            (push (cons (cl-incf counter) 0) stack))
           ((member text '("`else" "`elsif"))
            (when stack (setcdr (car stack) (1+ (cdar stack)))))
           ((equal text "`endif") (pop stack)))))
      (unless (sv-token-trivia-p token)
        (puthash (sv-token-line token) (copy-alist stack) table)))
    (setf (sv-lint-context-conditionals ctx) table)))

(defun sv-lint--exclusive-p (ctx line-a line-b)
  "Return non-nil when LINE-A and LINE-B of CTX cannot both be compiled."
  (let ((path-a (gethash line-a (sv-lint-context-conditionals ctx)))
        (path-b (gethash line-b (sv-lint-context-conditionals ctx))))
    (cl-some (lambda (entry)
               (let ((other (assq (car entry) path-b)))
                 (and other (/= (cdr entry) (cdr other)))))
             path-a)))

(defun sv-lint--macro-argument-names (ctx unit)
  "Return the identifiers UNIT passes to macros.
A macro may well assign to what it is handed -- `\=`FF(q, d, clk)\=' does --
so those names must count as driven."
  (let* ((vec (sv-lint-context-significant ctx))
         (limit (length vec))
         (beg (or (plist-get unit :beg) 0))
         (end (min (or (plist-get unit :end) limit) limit))
         (names '())
         (i beg))
    (while (< i end)
      (if (and (eq (sv-token-type (aref vec i)) 'directive)
               (< (1+ i) end)
               (equal (sv-token-text (aref vec (1+ i))) "("))
          (let ((depth 0) (done nil))
            (setq i (1+ i))
            (while (and (< i end) (not done))
              (let ((text (sv-token-text (aref vec i))))
                (cond ((member text '("(" "[" "{")) (setq depth (1+ depth)))
                      ((member text '(")" "]" "}"))
                       (setq depth (1- depth))
                       (when (<= depth 0) (setq done t)))
                      ((eq (sv-token-type (aref vec i)) 'ident)
                       (push text names))))
              (setq i (1+ i))))
        (setq i (1+ i))))
    (delete-dups names)))

(defun sv-lint--scan-suppressions (ctx)
  "Populate the suppression tables of CTX from its comment tokens."
  (let ((line-table (make-hash-table :test #'eq))
        (file-rules '())
        (ranges '())
        (open (make-hash-table :test #'equal))
        (last-line 1))
    (dolist (token (sv-lint-context-tokens ctx))
      (setq last-line (max last-line (sv-token-line token)))
      (when (eq (sv-token-type token) 'comment)
        (let ((text (sv-token-text token))
              (line (sv-token-line token)))
          (when (string-match
                 "sv-lint:?[ \t]+disable\\(-next-line\\|-file\\)?\\(?:[ \t]*=[ \t]*\\([^*\n]*\\)\\)?"
                 text)
            (let* ((scope (match-string 1 text))
                   (rules (sv-lint--parse-rule-names (match-string 2 text))))
              (cond
               ((equal scope "-file") (setq file-rules (append rules file-rules)))
               ((equal scope "-next-line")
                (puthash (1+ line) (append rules (gethash (1+ line) line-table))
                         line-table))
               (t (puthash line (append rules (gethash line line-table))
                           line-table)))))
          ;; Verilator pragmas, mapped onto the equivalent rule.
          (let ((start 0))
            (while (string-match "verilator[ \t]+lint_\\(off\\|on\\)[ \t]+\\([A-Z_0-9]+\\)"
                                 text start)
              (setq start (match-end 0))
              (let* ((action (match-string 1 text))
                     (code (match-string 2 text))
                     (rule (cdr (assoc code sv-lint-verilator-alias))))
                (when rule
                  (if (equal action "off")
                      (puthash code line open)
                    (let ((from (gethash code open)))
                      (when from
                        (push (list rule from line) ranges)
                        (remhash code open)))))))))))
    (maphash (lambda (code from)
               (let ((rule (cdr (assoc code sv-lint-verilator-alias))))
                 (when rule (push (list rule from last-line) ranges))))
             open)
    (setf (sv-lint-context-suppress-file ctx) file-rules)
    (setf (sv-lint-context-suppress-line ctx) line-table)
    (setf (sv-lint-context-suppress-ranges ctx) ranges)))


;;;; Reference gathering


;;;; Statement analysis helpers

(defun sv-lint--write-tokens (node)
  "Return a hash of the tokens NODE uses as assignment targets.
Writing to a signal is not using it, so these occurrences must not keep
`unused-declaration' quiet."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (stmt (sv-parse-statements node))
      (when (memq (plist-get stmt :type) '(assign continuous-assign))
        (dolist (tok (sv-parse-lhs-tokens (plist-get stmt :lhs)))
          (puthash tok t table))))
    table))

(defun sv-lint--must-assign (stmt)
  "Return the signals STMT assigns on every path through it."
  (when (and stmt (listp stmt) (plist-member stmt :type))
    (cl-case (plist-get stmt :type)
      ((assign continuous-assign) (copy-sequence (plist-get stmt :lhs-targets)))
      (block (let ((names '()))
               (dolist (inner (plist-get stmt :stmts))
                 (setq names (append (sv-lint--must-assign inner) names)))
               (delete-dups names)))
      (if (let ((else (plist-get stmt :else)))
            (when else
              (cl-intersection (sv-lint--must-assign (plist-get stmt :then))
                               (sv-lint--must-assign else)
                               :test #'equal))))
      (case (let ((items (plist-get stmt :items)))
              (when (and items
                         (or (cl-some (lambda (item) (plist-get item :default)) items)
                             (memq (plist-get stmt :qualifier) '(unique unique0))))
                (let ((common (sv-lint--must-assign (plist-get (car items) :stmt))))
                  (dolist (item (cdr items))
                    (setq common (cl-intersection
                                  common (sv-lint--must-assign (plist-get item :stmt))
                                  :test #'equal)))
                  common))))
      (t nil))))

(defun sv-lint--read-tokens (stmt)
  "Return the tokens STMT reads: right-hand sides, conditions and indices."
  (let ((tokens '()))
    (dolist (inner (sv-parse-statements stmt))
      (cl-case (plist-get inner :type)
        ((assign continuous-assign)
         (setq tokens (append (plist-get inner :rhs) tokens))
         ;; Index expressions on the left-hand side are reads too.
         (let ((depth 0))
           (dolist (tok (plist-get inner :lhs))
             (let ((text (sv-token-text tok)))
               (cond ((equal text "[") (setq depth (1+ depth)))
                     ((equal text "]") (setq depth (max 0 (1- depth))))
                     ((> depth 0) (push tok tokens)))))))
        (if (setq tokens (append (plist-get inner :cond) tokens)))
        (case (setq tokens (append (plist-get inner :expr) tokens))
              (dolist (item (plist-get inner :items))
                (setq tokens (append (plist-get item :labels) tokens))))
        (loop (setq tokens (append (plist-get inner :header) tokens)))
        (expr (setq tokens (append (plist-get inner :tokens) tokens)))
        (t nil)))
    tokens))

(defun sv-lint--token-names (tokens)
  "Return the identifier names referenced by TOKENS."
  (let ((names '()) (previous nil))
    (dolist (tok tokens)
      (when (and (eq (sv-token-type tok) 'ident)
                 (not (and previous (member (sv-token-text previous) '("." "::")))))
        (push (sv-token-text tok) names))
      (setq previous tok))
    (delete-dups (nreverse names))))

(defun sv-lint--combinational-p (node)
  "Return non-nil when the procedural block NODE describes combinational logic."
  (let ((kind (plist-get node :kind)))
    (or (eq kind 'always_comb)
        (and (eq kind 'always)
             (or (plist-get node :star)
                 (not (cl-some (lambda (tok)
                                 (member (sv-token-text tok) '("posedge" "negedge")))
                               (plist-get node :sensitivity))))))))


;;;; Rules

(defun sv-lint--rule-procedural (ctx unit)
  "Check the procedural blocks of UNIT for assignment and latch problems."
  (dolist (node (plist-get unit :items))
    (when (eq (plist-get node :type) 'always)
      (let* ((kind (plist-get node :kind))
             (body (plist-get node :body))
             (statements (sv-parse-statements body))
             (local (sv-parse-declared-names body)))
        (when (eq kind 'always_ff)
          (dolist (stmt statements)
            (when (and (eq (plist-get stmt :type) 'assign)
                       (equal (plist-get stmt :op) "=")
                       (not (cl-every (lambda (name) (member name local))
                                      (plist-get stmt :lhs-targets))))
              (sv-lint--report ctx 'blocking-in-always-ff
                               (plist-get stmt :line) nil
                               (format "Blocking assignment to `%s' inside always_ff; use `<='."
                                       (or (plist-get stmt :target) "signal"))))))

        (when (sv-lint--combinational-p node)
          (dolist (stmt statements)
            (when (and (eq (plist-get stmt :type) 'assign)
                       (equal (plist-get stmt :op) "<="))
              (sv-lint--report ctx 'nonblocking-in-always-comb
                               (plist-get stmt :line) nil
                               (format "Non-blocking assignment to `%s' in combinational logic; use `='."
                                       (or (plist-get stmt :target) "signal")))))
          (let* ((assigned (sv-parse-assigned-names body))
                 (guaranteed (sv-lint--must-assign body))
                 (partial (mapcar #'car (cl-remove-if-not
                                         #'cdr (sv-lint--driver-targets body))))
                 (latched (cl-set-difference
                           (cl-set-difference assigned guaranteed :test #'equal)
                           partial :test #'equal)))
            (dolist (name (sort latched #'string<))
              (unless (member name local)
                (sv-lint--report ctx 'implicit-latch (plist-get node :line) nil
                                 (format "`%s' is not assigned on every path; a latch is inferred."
                                         name))))))

        (when (and (eq kind 'always) (plist-get node :star))
          (sv-lint--report ctx 'prefer-always-comb (plist-get node :line) nil
                           "Use `always_comb' instead of `always @(*)'."))

        (when (and (eq kind 'always)
                   (sv-lint--combinational-p node)
                   (not (plist-get node :star))
                   (plist-get node :sensitivity))
          (let* ((listed (sv-lint--token-names (plist-get node :sensitivity)))
                 (assigned (sv-parse-assigned-names body))
                 (read (sv-lint--token-names (sv-lint--read-tokens body)))
                 (missing (cl-set-difference
                           (cl-set-difference read listed :test #'equal)
                           assigned :test #'equal)))
            (dolist (name (sort missing #'string<))
              (unless (member name local)
                (sv-lint--report ctx 'incomplete-sensitivity (plist-get node :line) nil
                                 (format "`%s' is read but missing from the sensitivity list."
                                         name))))))))))

(defun sv-lint--rule-case (ctx unit)
  "Check that every case statement of UNIT has a default arm."
  (dolist (node (sv-parse-collect unit 'always))
    (dolist (stmt (sv-parse-statements (plist-get node :body)))
      (when (eq (plist-get stmt :type) 'case)
        (let ((items (plist-get stmt :items))
              (qualifier (plist-get stmt :qualifier)))
          (unless (or (cl-some (lambda (item) (plist-get item :default)) items)
                      (and sv-lint-case-default-exempt-qualified
                           (memq qualifier '(unique unique0 priority))))
            (sv-lint--report ctx 'case-without-default (plist-get stmt :line) nil
                             (format "`%s' has no default arm."
                                     (plist-get stmt :kind)))))))))

(defun sv-lint--rule-names (ctx unit)
  "Check UNIT for undeclared, unused and duplicated names."
  (let* ((declarations (sv-parse-declarations unit))
         (ignored (sv-parse-non-reference-tokens unit declarations))
         (references (sv-parse-references unit (sv-lint-context-significant ctx) ignored))
         (declared (make-hash-table :test #'equal))
         (used (make-hash-table :test #'equal))
         (file-names (sv-lint-context-modules ctx))
         (has-import (or (cl-some (lambda (item) (eq (plist-get item :type) 'import))
                                  (plist-get unit :items))
                         (cl-some (lambda (tok)
                                    (and (eq (sv-token-type tok) 'directive)
                                         (member (sv-token-text tok)
                                                 '("`include" "`define"))))
                                  (sv-lint-context-tokens ctx)))))
    ;; Duplicate declarations, ignoring the ones a generate block legitimately
    ;; repeats in separate scopes.
    (let ((in-scope (make-hash-table :test #'equal)))
      (dolist (decl declarations)
        (let* ((name (plist-get decl :name))
               (key (format "%s@%s" name (plist-get decl :scope)))
               (previous (gethash key in-scope)))
          (when (and previous
                     (not (memq (plist-get decl :kind) '(label genvar loopvar arg)))
                     (not (memq (plist-get previous :kind)
                                '(label genvar loopvar arg)))
                     (not (sv-lint--exclusive-p ctx (plist-get decl :line)
                                                (plist-get previous :line))))
            (sv-lint--report ctx 'duplicate-declaration (plist-get decl :line) nil
                             (format "`%s' is already declared on line %d."
                                     name (plist-get previous :line))))
          (unless previous (puthash key decl in-scope))
          (unless (gethash name declared) (puthash name decl declared)))))

    (let ((writes (sv-lint--write-tokens unit)))
      (dolist (reference references)
        (unless (gethash (cdr reference) writes)
          (puthash (car reference) t used))))

    (unless has-import
      (dolist (reference references)
        (let ((name (car reference)))
          (unless (or (gethash name declared)
                      (and file-names (gethash name file-names)))
            (sv-lint--report-token ctx 'undeclared-identifier (cdr reference)
                                   (format "`%s' is used but never declared." name))))))

    (dolist (decl declarations)
      (let ((name (plist-get decl :name))
            (kind (plist-get decl :kind)))
        (unless (or (gethash name used)
                    (string-match-p sv-lint-ignored-name-regexp name))
          (cl-case kind
            ((var net)
             (sv-lint--report ctx 'unused-declaration (plist-get decl :line) nil
                              (format "`%s' is declared but never used." name)))
            (param
             (sv-lint--report ctx 'unused-parameter (plist-get decl :line) nil
                              (format "Parameter `%s' is never used." name)))
            (t nil)))))

    ;; Outputs need a driver: a procedural or continuous assignment, or a
    ;; connection to an instance port.
    (let ((driven (append (sv-lint--macro-argument-names ctx unit)
                          (sv-parse-assigned-names unit))))
      (dolist (instance (sv-parse-collect unit 'instance))
        (dolist (sibling (plist-get instance :siblings))
          (dolist (connection (plist-get sibling :connections))
            (setq driven (append (sv-lint--token-names (plist-get connection :expr))
                                 driven))
            (when (plist-get connection :implicit)
              (push (plist-get connection :name) driven)))))
      (dolist (port (plist-get unit :ports))
        (when (and (eq (plist-get port :dir) 'output)
                   (not (member (plist-get port :name) driven)))
          (sv-lint--report ctx 'undriven-output (plist-get port :line)
                           (plist-get port :col)
                           (format "Output `%s' is never driven."
                                   (plist-get port :name))))))

    (when (sv-lint--rule-enabled-p 'port-naming)
      (dolist (port (plist-get unit :ports))
        (let ((regexp (cdr (assq (plist-get port :dir) sv-lint-port-prefixes)))
              (name (plist-get port :name)))
          (when (and regexp name (not (string-match-p regexp name)))
            (sv-lint--report ctx 'port-naming (plist-get port :line)
                             (plist-get port :col)
                             (format "%s port `%s' should match \"%s\"."
                                     (plist-get port :dir) name regexp))))))))

(defun sv-lint--rule-instances (ctx unit)
  "Check the instantiations of UNIT against the known module interfaces."
  (let ((modules (sv-lint-context-modules ctx)))
    (dolist (instance (sv-parse-collect unit 'instance))
      (dolist (sibling (plist-get instance :siblings))
        (let* ((connections (plist-get sibling :connections))
               (target (and modules (gethash (plist-get instance :module) modules)))
               (ports (and (listp target) (plist-get target :ports))))
          (when (cl-some (lambda (connection) (plist-get connection :positional))
                         connections)
            (sv-lint--report ctx 'positional-port-connection
                             (plist-get sibling :line) nil
                             (format "Instance `%s' of `%s' connects ports by position; use named connections."
                                     (plist-get sibling :name)
                                     (plist-get instance :module))))
          (dolist (connection connections)
            (when (and (plist-get connection :name)
                       (not (plist-get connection :implicit))
                       (null (plist-get connection :expr)))
              (sv-lint--report-token ctx 'unconnected-port
                                     (plist-get connection :token)
                                     (format "Port `%s' of instance `%s' is left unconnected."
                                             (plist-get connection :name)
                                             (plist-get sibling :name)))))
          (when ports
            (let ((names (mapcar (lambda (port) (plist-get port :name)) ports))
                  (connected '())
                  (wildcard (cl-some (lambda (connection)
                                       (plist-get connection :wildcard))
                                     connections)))
              (dolist (connection connections)
                (when (plist-get connection :name)
                  (push (plist-get connection :name) connected)
                  (unless (member (plist-get connection :name) names)
                    (sv-lint--report-token
                     ctx 'instance-unknown-port (plist-get connection :token)
                     (format "`%s' has no port `%s'."
                             (plist-get instance :module)
                             (plist-get connection :name))))))
              (unless (or wildcard
                          (cl-some (lambda (connection) (plist-get connection :positional))
                                   connections))
                (dolist (name names)
                  (unless (member name connected)
                    (sv-lint--report ctx 'instance-missing-port
                                     (plist-get sibling :line) nil
                                     (format "Instance `%s' does not connect port `%s' of `%s'."
                                             (plist-get sibling :name) name
                                             (plist-get instance :module)))))))))))))

(defun sv-lint--rule-generate (ctx unit)
  "Check that the generate blocks of UNIT are labelled."
  (dolist (type '(generate-for generate-if))
    (dolist (node (sv-parse-collect unit type))
      (dolist (branch (list (plist-get node :body) (plist-get node :then)
                            (plist-get node :else)))
        (when (and branch (eq (plist-get branch :type) 'generate-block)
                   (null (plist-get branch :label)))
          (sv-lint--report ctx 'unlabeled-generate-block (plist-get branch :line) nil
                           "Generate block has no label; name it `begin : name'."))))))

(defun sv-lint--rule-unit-style (ctx unit)
  "Check UNIT for file-naming and declaration-style problems."
  (let ((file (sv-lint-context-file ctx))
        (name (plist-get unit :name)))
    (when (and file name (eq (plist-get unit :type) 'module)
               (eq unit (car (plist-get (sv-lint-context-tree ctx) :units)))
               (not (equal name (file-name-base file))))
      (sv-lint--report ctx 'module-filename-mismatch (plist-get unit :line) nil
                       (format "Module `%s' lives in %s; the names should match."
                               name (file-name-nondirectory file)))))
  (dolist (node (plist-get unit :items))
    (when (eq (plist-get node :type) 'decl)
      (let ((datatype (or (plist-get node :datatype) "")))
        (when (string-match-p "\\`\\(?:reg\\|wire\\)\\b" datatype)
          (sv-lint--report ctx 'legacy-net-type (plist-get node :line) nil
                           (format "Prefer `logic' over `%s'."
                                   (car (split-string datatype " " t)))))))))

(defun sv-lint--rule-whitespace (ctx)
  "Check the raw lines of CTX for whitespace problems."
  (let ((line 1))
    (dolist (text (sv-lint-context-lines ctx))
      (when (> (length text) sv-lint-max-line-length)
        (sv-lint--report ctx 'line-too-long line sv-lint-max-line-length
                         (format "Line is %d columns wide; the limit is %d."
                                 (length text) sv-lint-max-line-length)))
      (when (string-match "[ \t]+\\'" text)
        (sv-lint--report ctx 'trailing-whitespace line (match-beginning 0)
                         "Line has trailing whitespace."))
      (when (string-match-p "\\`[ \t]*\t" text)
        (sv-lint--report ctx 'tab-indentation line 0
                         "Line is indented with a tab."))
      (setq line (1+ line))))
  (let ((text (sv-lint-context-text ctx)))
    (when (and (> (length text) 0) (not (string-suffix-p "\n" text)))
      (sv-lint--report ctx 'missing-final-newline
                       (length (sv-lint-context-lines ctx)) 0
                       "File does not end with a newline."))))


(defun sv-lint--driver-targets (node)
  "Return what NODE drives, as a list of (NAME . PARTIAL) pairs.
PARTIAL is non-nil when every assignment to NAME went through a bit or
part select, which several blocks may legitimately share."
  (let ((targets '()))
    (dolist (stmt (sv-parse-statements node))
      (when (memq (plist-get stmt :type) '(assign continuous-assign))
        (let* ((lhs (plist-get stmt :lhs))
               (tokens (sv-parse-lhs-tokens lhs)))
          (dolist (tok tokens)
            (let* ((name (sv-token-text tok))
                   (rest (cdr (memq tok lhs)))
                   ;; A bit select or a struct field is a partial drive:
                   ;; different blocks may own different pieces.
                   (partial (and rest (member (sv-token-text (car rest))
                                              '("[" "."))))
                   (entry (assoc name targets)))
              (if entry
                  (unless partial (setcdr entry nil))
                (push (cons name partial) targets)))))))
    (nreverse targets)))

(defun sv-lint--rule-drivers (ctx unit)
  "Check UNIT for signals driven from several places, and for driven inputs."
  (let ((drivers (make-hash-table :test #'equal))
        (inputs (make-hash-table :test #'equal)))
    (dolist (port (plist-get unit :ports))
      (when (eq (plist-get port :dir) 'input)
        (puthash (plist-get port :name) port inputs)))
    (dolist (item (plist-get unit :items))
      ;; Only one branch of a generate is elaborated, and a generate loop
      ;; drives a different slice each time around, so neither counts.
      (when (memq (plist-get item :type) '(always continuous-assign))
        (dolist (target (sv-lint--driver-targets item))
          (let ((name (car target)))
            (when (gethash name inputs)
              (sv-lint--report ctx 'assignment-to-input (plist-get item :line) nil
                               (format "Input port `%s' is assigned to." name)))
            (unless (cdr target)
              (puthash name (cons item (gethash name drivers)) drivers))))))
    (maphash
     (lambda (name items)
       (setq items (cl-remove-duplicates
                    items
                    :test (lambda (a b)
                            (sv-lint--exclusive-p ctx (plist-get a :line)
                                                  (plist-get b :line)))))
       (when (> (length items) 1)
         (let ((sorted (sort (mapcar (lambda (item) (plist-get item :line)) items) #'<)))
           (sv-lint--report
            ctx 'multiple-drivers (car (last sorted)) nil
            (format "`%s' is driven from %d places (lines %s)."
                    name (length sorted)
                    (mapconcat #'number-to-string sorted ", "))))))
     drivers)))

(defun sv-lint--rule-case-labels (ctx unit)
  "Check the case statements of UNIT for labels that appear twice."
  (dolist (node (sv-parse-collect unit 'always))
    (dolist (stmt (sv-parse-statements (plist-get node :body)))
      (when (eq (plist-get stmt :type) 'case)
        (let ((seen (make-hash-table :test #'equal)))
          (dolist (item (plist-get stmt :items))
            (dolist (label (sv-parse-split-commas (plist-get item :labels)))
              (let ((text (sv-parse-token-text label)))
                (if (gethash text seen)
                    (sv-lint--report
                     ctx 'duplicate-case-label (plist-get item :line) nil
                     (format "Label `%s' already appears on line %d; the second arm is dead."
                             text (gethash text seen)))
                  (puthash text (plist-get item :line) seen))))))))))

(defun sv-lint--rule-assignment-style (ctx unit)
  "Warn when a plain `always' block of UNIT mixes assignment styles."
  (dolist (node (sv-parse-collect unit 'always))
    (when (eq (plist-get node :kind) 'always)
      (let ((blocking nil) (nonblocking nil)
            (local (sv-parse-declared-names (plist-get node :body))))
        (dolist (stmt (sv-parse-statements (plist-get node :body)))
          (when (and (eq (plist-get stmt :type) 'assign)
                     (not (cl-every (lambda (name) (member name local))
                                    (plist-get stmt :lhs-targets))))
            (cond ((equal (plist-get stmt :op) "=") (setq blocking t))
                  ((equal (plist-get stmt :op) "<=") (setq nonblocking t)))))
        (when (and blocking nonblocking)
          (sv-lint--report ctx 'mixed-assignment-style (plist-get node :line) nil
                           "This block mixes blocking and non-blocking assignments."))))))

(defun sv-lint--live-statements (node widths)
  "Return the statements of NODE that the parameters really elaborate.
A generate branch whose condition folds to false is not compiled, and one
whose condition cannot be folded might not be, so neither is analysed."
  (let ((found '()))
    (cl-labels
        ((walk (item)
           (when (and item (listp item) (plist-member item :type))
             (if (eq (plist-get item :type) 'generate-if)
                 (let* ((tokens (plist-get item :cond))
                        (inside (if (and tokens
                                         (equal (sv-token-text (car tokens)) "("))
                                    (sv-parse-unwrap tokens)
                                  tokens))
                        (value (sv-width-eval inside widths)))
                   (cond ((null value) nil)
                         ((/= value 0) (walk (plist-get item :then)))
                         (t (walk (plist-get item :else)))))
               (push item found)
               (dolist (key '(:items :stmts :body :then :else))
                 (let ((value (plist-get item key)))
                   (cond
                    ((and (listp value) (plist-member value :type)) (walk value))
                    ((listp value) (mapc #'walk value)))))
               (dolist (arm (plist-get item :items))
                 (when (and (listp arm) (plist-member arm :stmt))
                   (walk (plist-get arm :stmt))))))))
      (walk node))
    (nreverse found)))

(defun sv-lint--assignment-pairs (unit &optional widths)
  "Return the (LHS RHS LINE) triples UNIT assigns, procedural ones included.
With WIDTHS, only the generate branches that are really elaborated count."
  (let ((pairs '()))
    (dolist (stmt (if widths
                      (sv-lint--live-statements unit widths)
                    (sv-parse-statements unit)))
      (when (and (memq (plist-get stmt :type) '(assign continuous-assign))
                 (member (plist-get stmt :op) '(nil "=" "<="))
                 (plist-get stmt :lhs)
                 (plist-get stmt :rhs))
        (push (list (plist-get stmt :lhs) (plist-get stmt :rhs)
                    (plist-get stmt :line))
              pairs)))
    (nreverse pairs)))

(defun sv-lint--rule-width (ctx unit)
  "Check UNIT for assignments and connections that do not fit."
  (let ((widths (sv-width-context unit)))
    (dolist (pair (sv-lint--assignment-pairs unit widths))
      (let* ((lhs (nth 0 pair))
             (rhs (nth 1 pair))
             (line (nth 2 pair))
             (target (sv-width-of lhs widths)))
        (when target
          (let ((value (sv-width-eval rhs widths))
                (source (sv-width-of rhs widths)))
            (cond
             ;; A constant that simply does not fit is always a mistake.
             ((and value (>= value 0) (< target 62) (>= value (ash 1 target)))
              (sv-lint--report ctx 'constant-overflow line nil
                               (format "%d does not fit in the %d bit%s of `%s'."
                                       value target (if (= target 1) "" "s")
                                       (or (car (sv-parse--lhs-targets lhs)) "target"))))
             ;; Otherwise compare the widths, but only when both are known.
             ((and source (> source target) (null value))
              (sv-lint--report ctx 'width-truncation line nil
                               (format "`%s' is %d bit%s wide but is assigned %d bits."
                                       (or (car (sv-parse--lhs-targets lhs)) "target")
                                       target (if (= target 1) "" "s") source))))))))

    (let ((modules (sv-lint-context-modules ctx)))
      (when modules
        (dolist (instance (sv-parse-collect unit 'instance))
          (let ((target (gethash (plist-get instance :module) modules)))
            (when (and (listp target) (plist-get target :ports))
              (let* ((overrides
                      (delq nil
                            (mapcar
                             (lambda (param)
                               (let ((value (sv-width-eval (plist-get param :expr)
                                                           widths)))
                                 (when (and (plist-get param :name) value)
                                   (cons (plist-get param :name) value))))
                             (plist-get instance :params))))
                     (inner (sv-width-context target overrides)))
                (dolist (sibling (plist-get instance :siblings))
                  (dolist (connection (plist-get sibling :connections))
                    (let ((port (cl-find (plist-get connection :name)
                                         (plist-get target :ports)
                                         :key (lambda (p) (plist-get p :name))
                                         :test #'equal)))
                      (when (and port (plist-get connection :expr))
                        (let ((expected (sv-width-declarator port inner))
                              (actual (sv-width-of (plist-get connection :expr)
                                                   widths)))
                          (when (and expected actual (/= expected actual))
                            (sv-lint--report-token
                             ctx 'port-width-mismatch (plist-get connection :token)
                             (format "Port `%s' of `%s' is %d bit%s wide but is connected to %d."
                                     (plist-get connection :name)
                                     (plist-get instance :module)
                                     expected (if (= expected 1) "" "s")
                                     actual))))))))))))))))

;;;; Entry points

(defun sv-lint-module-table (tree &optional table)
  "Add every design unit of TREE to TABLE, creating it when absent.
Returns the table, which maps a unit name to its node."
  (let ((table (or table (make-hash-table :test #'equal))))
    (dolist (unit (plist-get tree :units))
      (when (plist-get unit :name)
        (puthash (plist-get unit :name) unit table))
      ;; Names a unit exports to the rest of the file.
      (dolist (type '(typedef function task param))
        (dolist (node (sv-parse-collect unit type))
          (cl-case type
            (param (dolist (param (plist-get node :params))
                     (puthash (plist-get param :name) 'name table)))
            (typedef (puthash (plist-get node :name) 'name table)
                     (dolist (member (plist-get node :enum-members))
                       (puthash (plist-get member :name) 'name table)))
            (t (puthash (plist-get node :name) 'name table))))))
    table))

(defun sv-lint-build-module-table (files)
  "Build a module table by parsing every file of FILES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file files)
      (condition-case nil
          (sv-lint-module-table (sv-parse-file file) table)
        (error nil)))
    table))

(defun sv-lint-analyze (text &optional file modules)
  "Lint TEXT, said to come from FILE, and return its diagnostics.
MODULES is an optional table of known design units, as built by
`sv-lint-build-module-table', enabling the cross-module rules."
  (let* ((tokens (sv-lex-string text))
         (tree (sv-parse-tokens tokens))
         (ctx (sv-lint-context-create
               :file file
               :text text
               :lines (split-string text "\n")
               :tree tree
               :tokens tokens
               :significant (plist-get tree :significant)
               :modules (or modules (sv-lint-module-table tree))
               :diagnostics '())))
    (sv-lint--scan-suppressions ctx)
    (sv-lint--scan-conditionals ctx)
    (dolist (unit (plist-get tree :units))
      (when (memq (plist-get unit :type) '(module interface program))
        (sv-lint--rule-procedural ctx unit)
        (sv-lint--rule-case ctx unit)
        (sv-lint--rule-names ctx unit)
        (sv-lint--rule-drivers ctx unit)
        (sv-lint--rule-case-labels ctx unit)
        (sv-lint--rule-assignment-style ctx unit)
        (sv-lint--rule-width ctx unit)
        (sv-lint--rule-instances ctx unit)
        (sv-lint--rule-generate ctx unit)
        (sv-lint--rule-unit-style ctx unit)))
    (sv-lint--rule-whitespace ctx)
    (sort (nreverse (sv-lint-context-diagnostics ctx))
          (lambda (a b)
            (if (= (sv-diagnostic-line a) (sv-diagnostic-line b))
                (< (sv-diagnostic-col a) (sv-diagnostic-col b))
              (< (sv-diagnostic-line a) (sv-diagnostic-line b)))))))

(defun sv-lint-buffer (&optional buffer modules)
  "Lint BUFFER, the current one by default, against MODULES."
  (with-current-buffer (or buffer (current-buffer))
    (sv-lint-analyze (buffer-substring-no-properties (point-min) (point-max))
                     (buffer-file-name) modules)))

(defun sv-lint-file (file &optional modules)
  "Lint FILE against the optional MODULES table."
  (with-temp-buffer
    (insert-file-contents file)
    (sv-lint-analyze (buffer-substring-no-properties (point-min) (point-max))
                     file modules)))

(defun sv-diagnostic-format (diagnostic)
  "Render DIAGNOSTIC the way compilers do, for `compilation-mode'."
  (format "%s:%d:%d: %s: %s [%s]"
          (or (sv-diagnostic-file diagnostic) "<buffer>")
          (sv-diagnostic-line diagnostic)
          (1+ (sv-diagnostic-col diagnostic))
          (sv-diagnostic-severity diagnostic)
          (sv-diagnostic-message diagnostic)
          (sv-diagnostic-rule diagnostic)))

(provide 'sv-lint)

;;; sv-lint.el ends here
