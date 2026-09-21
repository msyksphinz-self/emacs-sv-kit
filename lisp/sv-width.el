;;; sv-width.el --- Constant folding and width inference -*- lexical-binding: t; -*-

;; Author: SCARIV project
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Works out how wide an expression is, which is what the width rules of the
;; linter need and what no amount of pattern matching can guess.
;;
;; Two things are computed.  `sv-width-eval' folds a constant expression to
;; an integer, resolving parameters and localparams through each other and
;; understanding the system functions that appear in widths -- `$clog2',
;; `$bits'.  `sv-width-of' then infers the width of an arbitrary expression
;; from the declarations around it, following the self-determined width
;; rules of IEEE 1800: a concatenation adds, a replication multiplies, an
;; arithmetic operator takes the wider side, a comparison or a reduction is
;; one bit, a part select is as wide as it says.
;;
;; Anything it cannot work out is nil rather than a guess, and every caller
;; is expected to stay quiet on nil.  Being silent about a real problem is
;; the right trade here: a width warning that cries wolf is worse than none.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)

(defconst sv-width-builtin-widths
  '(("logic" . 1) ("bit" . 1) ("reg" . 1) ("wire" . 1) ("tri" . 1)
    ("uwire" . 1) ("wand" . 1) ("wor" . 1) ("triand" . 1) ("trior" . 1)
    ("tri0" . 1) ("tri1" . 1) ("trireg" . 1) ("supply0" . 1) ("supply1" . 1)
    ("byte" . 8) ("shortint" . 16) ("int" . 32) ("integer" . 32)
    ("longint" . 64) ("time" . 64))
  "Width of each built-in data type that has one.")

(cl-defstruct (sv-width-context (:constructor sv-width-context--create)
                                (:copier nil))
  "What is known about the names of one design unit."
  params types widths values pending)


;;;; Literals

(defun sv-width-parse-number (text)
  "Return the value and width of the numeric literal TEXT as a cons.
The width is nil when the literal does not state one, which means its
width comes from the context it is used in."
  (cond
   ;; Sized, based literal: 8'hff, 12'd100, 'b1010.
   ((string-match "\\`\\([0-9_]*\\)'\\([sS]?\\)\\([bBoOdDhH]\\)[ \t]*\\(.+\\)\\'" text)
    (let* ((size (match-string 1 text))
           (base (downcase (match-string 3 text)))
           (digits (replace-regexp-in-string "_" "" (match-string 4 text)))
           (radix (cond ((equal base "b") 2) ((equal base "o") 8)
                        ((equal base "d") 10) (t 16))))
      (cons (unless (string-match-p "[xXzZ?]" digits)
              (ignore-errors (string-to-number digits radix)))
            (unless (string-empty-p size)
              (string-to-number (replace-regexp-in-string "_" "" size))))))
   ;; Unsized single-bit literal: '0, '1.
   ((string-match "\\`'\\([01]\\)\\'" text)
    (cons (string-to-number (match-string 1 text)) nil))
   ((string-match-p "\\`'[xXzZ]\\'" text) (cons nil nil))
   ;; Plain decimal, whose width comes from the context.
   ((string-match "\\`\\([0-9][0-9_]*\\)\\'" text)
    (cons (string-to-number (replace-regexp-in-string "_" "" text)) nil))
   (t (cons nil nil))))

(defun sv-width-clog2 (value)
  "Return $clog2 of VALUE: the smallest N with 2**N at least VALUE."
  (when (and value (>= value 0))
    (if (<= value 1)
        0
      (let ((bits 0) (limit 1))
        (while (< limit value) (setq limit (* 2 limit) bits (1+ bits)))
        bits))))


;;;; The context of a design unit

(defun sv-width--collect-types (unit)
  "Return a table of the types UNIT declares."
  (let ((types (make-hash-table :test #'equal)))
    (dolist (typedef (sv-parse-collect unit 'typedef))
      (when (plist-get typedef :name)
        (puthash (plist-get typedef :name) typedef types)))
    types))

(defun sv-width-context (unit &optional overrides)
  "Return what is known about the names of UNIT.
OVERRIDES is an alist of parameter names to values, as an instantiation
supplies them."
  (let ((context (sv-width-context--create
                  :params (make-hash-table :test #'equal)
                  :types (sv-width--collect-types unit)
                  :widths (make-hash-table :test #'equal)
                  :values (make-hash-table :test #'equal)
                  :pending (make-hash-table :test #'equal))))
    (dolist (param (plist-get unit :params))
      (when (plist-get param :name)
        (puthash (plist-get param :name) (plist-get param :init)
                 (sv-width-context-params context))))
    (dolist (node (sv-parse-collect unit 'param))
      (dolist (param (plist-get node :params))
        (when (plist-get param :name)
          (puthash (plist-get param :name) (plist-get param :init)
                   (sv-width-context-params context)))))
    (dolist (pair overrides)
      (puthash (car pair) (cdr pair) (sv-width-context-values context)))
    (dolist (record (sv-parse-declarations unit))
      (when (memq (plist-get record :kind) '(port var net))
        (let ((declarator (or (plist-get record :decl) (plist-get record :port))))
          (when declarator
            (puthash (plist-get record :name) declarator
                     (sv-width-context-widths context))))))
    context))

(defun sv-width--parameter-value (name context)
  "Return the value of the parameter NAME in CONTEXT, or nil."
  (let ((values (sv-width-context-values context)))
    (cond
     ((gethash name values) (gethash name values))
     ((gethash name (sv-width-context-pending context)) nil) ; a cycle
     (t
      (let ((tokens (gethash name (sv-width-context-params context))))
        (when tokens
          (puthash name t (sv-width-context-pending context))
          (let ((value (sv-width-eval tokens context)))
            (remhash name (sv-width-context-pending context))
            (when value (puthash name value values))
            value)))))))


;;;; Constant folding

(defvar sv-width--tokens nil "Vector of tokens being folded.")
(defvar sv-width--index 0 "Cursor into `sv-width--tokens'.")
(defvar sv-width--context nil "Context the fold runs in.")

(defsubst sv-width--peek (&optional n)
  (let ((index (+ sv-width--index (or n 0))))
    (when (< index (length sv-width--tokens)) (aref sv-width--tokens index))))

(defsubst sv-width--text (&optional n)
  (let ((token (sv-width--peek n))) (and token (sv-token-text token))))

(defsubst sv-width--next () (cl-incf sv-width--index))

(defun sv-width--accept (text)
  (when (equal (sv-width--text) text) (sv-width--next) t))

(defun sv-width--binary (operators next)
  "Fold a left-associative run of OPERATORS over the NEXT level."
  (let ((left (funcall next)))
    (while (member (sv-width--text) operators)
      (let ((operator (sv-width--text)))
        (sv-width--next)
        (let ((right (funcall next)))
          (setq left (sv-width--apply operator left right)))))
    left))

(defun sv-width--apply (operator left right)
  "Return the value of LEFT OPERATOR RIGHT, or nil when either is unknown."
  (when (and left right)
    (pcase operator
      ("+" (+ left right)) ("-" (- left right)) ("*" (* left right))
      ("/" (unless (zerop right) (truncate left right)))
      ("%" (unless (zerop right) (mod left right)))
      ("**" (when (and (>= right 0) (< right 64)) (expt left right)))
      ("<<" (ash left right)) ("<<<" (ash left right))
      (">>" (ash left (- right))) (">>>" (ash left (- right)))
      ("&" (logand left right)) ("|" (logior left right))
      ("^" (logxor left right))
      ("<" (if (< left right) 1 0)) (">" (if (> left right) 1 0))
      ("<=" (if (<= left right) 1 0)) (">=" (if (>= left right) 1 0))
      ("==" (if (= left right) 1 0)) ("!=" (if (/= left right) 1 0))
      ("===" (if (= left right) 1 0)) ("!==" (if (/= left right) 1 0))
      ("&&" (if (and (/= left 0) (/= right 0)) 1 0))
      ("||" (if (or (/= left 0) (/= right 0)) 1 0))
      (_ nil))))

(defun sv-width--expression ()
  "Fold a full expression, ternary included."
  (let ((condition (sv-width--logical-or)))
    (if (sv-width--accept "?")
        (let ((then (sv-width--expression)))
          (if (sv-width--accept ":")
              (let ((else (sv-width--expression)))
                (cond ((null condition) nil)
                      ((/= condition 0) then)
                      (t else)))
            nil))
      condition)))

(defun sv-width--logical-or ()
  (sv-width--binary '("||") #'sv-width--logical-and))
(defun sv-width--logical-and ()
  (sv-width--binary '("&&") #'sv-width--bit-or))
(defun sv-width--bit-or ()
  (sv-width--binary '("|") #'sv-width--bit-xor))
(defun sv-width--bit-xor ()
  (sv-width--binary '("^") #'sv-width--bit-and))
(defun sv-width--bit-and ()
  (sv-width--binary '("&") #'sv-width--equality))
(defun sv-width--equality ()
  (sv-width--binary '("==" "!=" "===" "!==") #'sv-width--relational))
(defun sv-width--relational ()
  (sv-width--binary '("<" ">" "<=" ">=") #'sv-width--shift))
(defun sv-width--shift ()
  (sv-width--binary '("<<" ">>" "<<<" ">>>") #'sv-width--additive))
(defun sv-width--additive ()
  (sv-width--binary '("+" "-") #'sv-width--multiplicative))
(defun sv-width--multiplicative ()
  (sv-width--binary '("*" "/" "%") #'sv-width--unary))

(defun sv-width--unary ()
  (let ((operator (sv-width--text)))
    (cond
     ((member operator '("+" "-" "!" "~"))
      (sv-width--next)
      (let ((value (sv-width--unary)))
        (when value
          (pcase operator
            ("+" value) ("-" (- value))
            ("!" (if (zerop value) 1 0)) ("~" (lognot value))))))
     (t (sv-width--power)))))

(defun sv-width--power ()
  (let ((base (sv-width--primary)))
    (if (sv-width--accept "**")
        (sv-width--apply "**" base (sv-width--unary))
      base)))

(defun sv-width--skip-group ()
  "Skip a bracketed group, and return the tokens inside it."
  (let ((depth 0) (inside '()) (done nil))
    (while (and (not done) (sv-width--peek))
      (let ((text (sv-width--text)))
        (cond
         ((member text '("(" "[" "{"))
          (setq depth (1+ depth))
          (when (> depth 1) (push (sv-width--peek) inside)))
         ((member text '(")" "]" "}"))
          (setq depth (1- depth))
          (if (zerop depth) (setq done t) (push (sv-width--peek) inside)))
         (t (push (sv-width--peek) inside))))
      (sv-width--next))
    (nreverse inside)))

(defun sv-width--primary ()
  "Fold a primary expression."
  (let ((token (sv-width--peek)))
    (cond
     ((null token) nil)
     ((eq (sv-token-type token) 'number)
      (sv-width--next)
      (car (sv-width-parse-number (sv-token-text token))))
     ((equal (sv-token-text token) "(")
      (let ((inside (sv-width--skip-group)))
        (sv-width-eval inside sv-width--context)))
     ((eq (sv-token-type token) 'sysfunc)
      (let ((name (sv-token-text token)))
        (sv-width--next)
        (let ((argument (when (equal (sv-width--text) "(")
                          (sv-width--skip-group))))
          (cond
           ((equal name "$clog2")
            (sv-width-clog2 (sv-width-eval argument sv-width--context)))
           ((equal name "$bits")
            (sv-width-of argument sv-width--context))
           ((member name '("$signed" "$unsigned"))
            (sv-width-eval argument sv-width--context))
           (t nil)))))
     ((eq (sv-token-type token) 'ident)
      (let ((name (sv-token-text token)))
        (sv-width--next)
        ;; A package qualification or a cast: pkg::NAME, type'(expr).
        (while (equal (sv-width--text) "::")
          (sv-width--next)
          (setq name (or (sv-width--text) name))
          (sv-width--next))
        (if (equal (sv-width--text) "'")
            (progn (sv-width--next)
                   (sv-width-eval (sv-width--skip-group) sv-width--context))
          (sv-width--parameter-value name sv-width--context))))
     (t (sv-width--next) nil))))

(defun sv-width-eval (tokens context)
  "Fold TOKENS to an integer in CONTEXT, or return nil when it cannot be done."
  (when tokens
    (let ((sv-width--tokens (vconcat tokens))
          (sv-width--index 0)
          (sv-width--context context))
      (sv-width--expression))))


;;;; Width inference

(defun sv-width--dimension-width (tokens context)
  "Return how many bits the packed dimensions TOKENS describe."
  (let ((groups '()) (depth 0) (current '()))
    (dolist (token tokens)
      (let ((text (sv-token-text token)))
        (cond
         ((equal text "[")
          (setq depth (1+ depth))
          (when (= depth 1) (setq current '())))
         ((equal text "]")
          (setq depth (1- depth))
          (when (zerop depth) (push (nreverse current) groups)))
         ((> depth 0) (push token current)))))
    (let ((total 1))
      (dolist (group (nreverse groups))
        (let ((width (sv-width--range-width group context)))
          (setq total (and total width (* total width)))))
      (when groups total))))

(defun sv-width--range-width (tokens context)
  "Return the width of one dimension TOKENS, such as `7:0' or `W-1:0'."
  (let ((colon (sv-parse-find-top tokens '(":")))
        (plus (sv-parse-find-top tokens '("+:" "-:"))))
    (cond
     (plus (sv-width-eval (nthcdr (1+ plus) tokens) context))
     (colon
      (let ((high (sv-width-eval (cl-subseq tokens 0 colon) context))
            (low (sv-width-eval (nthcdr (1+ colon) tokens) context)))
        (when (and high low) (1+ (abs (- high low))))))
     (t 1))))

(defun sv-width--type-width (datatype packed context &optional depth)
  "Return the width of DATATYPE carrying PACKED dimensions, in CONTEXT."
  (let* ((depth (or depth 0))
         (words (split-string (or datatype "") "[ \t]+" t))
         (base (cl-find-if (lambda (word)
                             (or (assoc word sv-width-builtin-widths)
                                 (gethash word (sv-width-context-types context))))
                           words))
         (dimension (sv-width--dimension-width packed context)))
    (cond
     ((> depth 8) nil)
     ;; Dimensions that are there but cannot be folded make the width
     ;; unknown.  Falling back to the element width would claim a
     ;; parameterized vector is one bit, and every comparison against it
     ;; would then be wrong.
     ((and packed (null dimension)) nil)
     ;; With no data type at all the dimensions are the width, as in
     ;; `wire [7:0] bus;\='.  But a type this context cannot resolve -- a type
     ;; parameter, or one from another file -- leaves the element width
     ;; unknown, and the dimensions alone say nothing.
     ((null base) (if (string-empty-p (string-trim (or datatype "")))
                      dimension
                    nil))
     ((assoc base sv-width-builtin-widths)
      (let ((unit (cdr (assoc base sv-width-builtin-widths))))
        (if dimension
            (if (= unit 1) dimension (* unit dimension))
          unit)))
     (t
      (let ((width (sv-width--typedef-width
                    (gethash base (sv-width-context-types context))
                    context (1+ depth))))
        (cond ((null width) nil)
              (dimension (* width dimension))
              (t width)))))))

(defun sv-width--typedef-width (typedef context depth)
  "Return the width the TYPEDEF stands for, in CONTEXT."
  (when typedef
    (let ((members (plist-get typedef :members)))
      (if members
          ;; A packed struct is as wide as its members together.
          (let ((total 0))
            (dolist (member members)
              (let ((width (sv-width--type-width (plist-get member :datatype)
                                                 (plist-get member :packed)
                                                 context depth)))
                (setq total (and total width (+ total width)))))
            total)
        (let* ((tokens (plist-get typedef :tokens))
               (text (plist-get typedef :text)))
          (ignore tokens)
          ;; `typedef logic [7:0] byte_t;' and enums over a base type.
          (when text
            (let* ((lexed (cl-remove-if #'sv-token-trivia-p (sv-lex-string text)))
                   (brace (sv-parse-find-top lexed '("{")))
                   (head (if brace (cl-subseq lexed 0 brace) lexed))
                   (packed (cl-remove-if-not
                            (lambda (token)
                              (ignore token) t)
                            head))
                   (declarator (sv-parse-declarator
                                (cl-remove-if
                                 (lambda (token)
                                   (member (sv-token-text token)
                                           '("typedef" "enum" "packed")))
                                 head))))
              (ignore packed)
              (sv-width--type-width (plist-get declarator :datatype)
                                    (plist-get declarator :packed)
                                    context depth))))))))

(defun sv-width-declarator (declarator context)
  "Return the width of the port or variable DECLARATOR in CONTEXT."
  (when declarator
    (sv-width--type-width (plist-get declarator :datatype)
                          (plist-get declarator :packed)
                          context)))

(defun sv-width-signal (name context)
  "Return the declared width of the signal NAME in CONTEXT."
  (let ((declarator (gethash name (sv-width-context-widths context))))
    (when declarator
      (sv-width--type-width (plist-get declarator :datatype)
                            (plist-get declarator :packed)
                            context))))

(defun sv-width-of (tokens context)
  "Return the width of the expression TOKENS in CONTEXT, or nil.
Follows the self-determined widths of IEEE 1800 closely enough for the
linter: a concatenation adds its parts, a replication multiplies, an
arithmetic or bitwise operator takes its wider side, a comparison or a
reduction is one bit."
  (setq tokens (cl-remove-if #'sv-token-trivia-p tokens))
  (when tokens
    (let ((texts (mapcar #'sv-token-text tokens)))
      (cond
       ;; A ternary is as wide as its wider arm.
       ((sv-parse-find-top tokens '("?"))
        (let* ((mark (sv-parse-find-top tokens '("?")))
               (rest (nthcdr (1+ mark) tokens))
               (colon (sv-parse-find-top rest '(":"))))
          (when colon
            (let ((then (sv-width-of (cl-subseq rest 0 colon) context))
                  (else (sv-width-of (nthcdr (1+ colon) rest) context)))
              (when (and then else) (max then else))))))
       ;; Comparisons, logical operators and reductions yield one bit.
       ((sv-parse-find-top tokens '("==" "!=" "===" "!==" "<" ">" "<=" ">="
                                    "&&" "||" "inside"))
        1)
       ((and (member (car texts) '("!" "&" "|" "^" "~&" "~|" "~^" "^~"))
             (not (sv-parse-find-top (cdr tokens) '("&" "|" "^"))))
        1)
       ;; Arithmetic and bitwise operators take the wider side.  The search
       ;; starts past the first token, which can only be a unary operator.
       ((sv-parse-find-top (cdr tokens) '("+" "-" "*" "/" "%" "&" "|" "^"))
        (let* ((mark (1+ (sv-parse-find-top
                          (cdr tokens) '("+" "-" "*" "/" "%" "&" "|" "^"))))
               (left (sv-width-of (cl-subseq tokens 0 mark) context))
               (right (sv-width-of (nthcdr (1+ mark) tokens) context)))
          (when (and left right) (max left right))))
       ;; A shift is as wide as what is shifted.
       ((sv-parse-find-top (cdr tokens) '("<<" ">>" "<<<" ">>>"))
        (sv-width-of (cl-subseq tokens 0 (1+ (sv-parse-find-top
                                              (cdr tokens)
                                              '("<<" ">>" "<<<" ">>>"))))
                     context))
       ((member (sv-token-text (car tokens)) '("+" "-" "~"))
        (sv-width-of (cdr tokens) context))
       (t (sv-width--primary-width tokens context))))))

(defun sv-width--primary-width (tokens context)
  "Return the width of the primary expression TOKENS in CONTEXT."
  (let* ((first (car tokens))
         (text (sv-token-text first)))
    (cond
     ;; A parenthesised expression.
     ((and (equal text "(") (equal (sv-token-text (car (last tokens))) ")"))
      (sv-width-of (butlast (cdr tokens)) context))
     ;; A concatenation or a replication.
     ((equal text "{")
      (let* ((body (sv-parse-braced-body tokens))
             (inner (and body (equal (sv-token-text (car body)) "{"))))
        (if (and body (not inner)
                 (sv-parse-find-top body '("{")))
            ;; {N{expr}}
            (let* ((mark (sv-parse-find-top body '("{")))
                   (count (sv-width-eval (cl-subseq body 0 mark) context))
                   (width (sv-width-of (sv-parse-braced-body (nthcdr mark body))
                                       context)))
              (when (and count width) (* count width)))
          (let ((total 0))
            (dolist (part (sv-parse-split-commas body))
              (let ((width (sv-width-of part context)))
                (setq total (and total width (+ total width)))))
            total))))
     ((eq (sv-token-type first) 'number)
      (cdr (sv-width-parse-number text)))
     ((eq (sv-token-type first) 'sysfunc)
      (cond
       ((member text '("$clog2" "$bits" "$size" "$countones" "$countbits")) 32)
       ((member text '("$signed" "$unsigned"))
        (sv-width-of (sv-parse-braced-body
                      (cons first (cdr tokens))) context))
       (t nil)))
     ((eq (sv-token-type first) 'ident)
      (let ((rest (cdr tokens)))
        (cond
         ;; A cast, `type'(expr)', is as wide as the type.
         ((and rest (equal (sv-token-text (car rest)) "'"))
          (sv-width--type-width text nil context))
         ;; A select narrows: x[3:0], x[i], x[i+:8].
         ((and rest (equal (sv-token-text (car rest)) "["))
          (sv-width--indexed-width text rest context))
         ;; A field of a struct, or a call: not worth guessing.
         ((and rest (member (sv-token-text (car rest)) '("." "(" "::"))) nil)
         (t (sv-width-signal text context)))))
     (t nil))))

(defun sv-width--select-groups (tokens)
  "Return the contents of each bracketed group TOKENS starts with."
  (let ((groups '()) (current '()) (depth 0) (done nil))
    (dolist (token tokens)
      (unless done
        (let ((text (sv-token-text token)))
          (cond
           ((equal text "[")
            (setq depth (1+ depth))
            (when (> depth 1) (push token current)))
           ((equal text "]")
            (setq depth (1- depth))
            (if (zerop depth)
                (progn (push (nreverse current) groups) (setq current '()))
              (push token current)))
           ((> depth 0) (push token current))
           (t (setq done t))))))
    (nreverse groups)))

(defun sv-width--plain-vector-p (declarator context)
  "Return non-nil when DECLARATOR is a plain vector of single-bit elements.
Only for such a signal does `x[i]\=' select exactly one bit; an array of
structs or a multi-dimensional signal selects something wider, and this
analysis would rather say nothing than guess."
  (and declarator
       (null (plist-get declarator :unpacked))
       (= (length (sv-width--select-groups (plist-get declarator :packed))) 1)
       (let ((words (split-string (or (plist-get declarator :datatype) "")
                                  "[ \t]+" t)))
         (and (cl-notany (lambda (word)
                           (gethash word (sv-width-context-types context)))
                         words)
              (cl-some (lambda (word)
                         (equal (cdr (assoc word sv-width-builtin-widths)) 1))
                       words)))))

(defun sv-width--indexed-width (name rest context)
  "Return the width of NAME with the select REST applied, in CONTEXT."
  (let* ((groups (sv-width--select-groups rest))
         (inside (car groups))
         (ranged (and inside (or (sv-parse-find-top inside '(":"))
                                 (sv-parse-find-top inside '("+:" "-:"))))))
    (cond
     ;; A part select states its own width.
     ((and ranged (= (length groups) 1)) (sv-width--range-width inside context))
     ((/= (length groups) 1) nil)
     ((sv-width--plain-vector-p (gethash name (sv-width-context-widths context))
                                context)
      1)
     (t nil))))

(defun sv-width--select-width (tokens context)
  "Return the width of the select that TOKENS opens with."
  (let ((depth 0) (inside '()) (done nil))
    (dolist (token tokens)
      (unless done
        (let ((text (sv-token-text token)))
          (cond
           ((equal text "[") (setq depth (1+ depth)) (when (> depth 1) (push token inside)))
           ((equal text "]") (setq depth (1- depth))
            (if (zerop depth) (setq done t) (push token inside)))
           ((> depth 0) (push token inside))))))
    (setq inside (nreverse inside))
    (if (or (sv-parse-find-top inside '(":"))
            (sv-parse-find-top inside '("+:" "-:")))
        (sv-width--range-width inside context)
      1)))

(provide 'sv-width)

;;; sv-width.el ends here
