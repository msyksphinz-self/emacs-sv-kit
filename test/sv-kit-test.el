;;; sv-kit-test.el --- Tests for sv-kit -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:

;; Run with `make test', or:
;;   emacs --batch -L lisp -L test -l sv-kit-test -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'sv-lexer)
(require 'sv-parser)
(require 'sv-lint)
(require 'sv-format)
(require 'sv-kit)
(require 'sv-mode)
(require 'sv-index)
(require 'sv-ide)
(require 'sv-width)
(require 'sv-refactor)
(require 'sv-hierarchy)

(defun sv-test-types (text)
  "Return the type of every significant token of TEXT."
  (mapcar #'sv-token-type
          (cl-remove-if #'sv-token-trivia-p (sv-lex-string text))))

(defun sv-test-texts (text)
  "Return the spelling of every significant token of TEXT."
  (mapcar #'sv-token-text
          (cl-remove-if #'sv-token-trivia-p (sv-lex-string text))))

(defun sv-test-unit (text &optional index)
  "Parse TEXT and return its design unit number INDEX, zero by default."
  (nth (or index 0) (plist-get (sv-parse-string text) :units)))

(defun sv-test-rules (text &optional file)
  "Return the rule identifiers reported for TEXT, said to live in FILE."
  (let ((sv-lint-disabled-rules '()))
    (mapcar #'sv-diagnostic-rule (sv-lint-analyze text (or file "test.sv")))))

(defun sv-test-port-names (unit)
  "Return the port names of UNIT."
  (mapcar (lambda (port) (plist-get port :name)) (plist-get unit :ports)))


;;;; Lexer

(ert-deftest sv-lexer-classifies-tokens ()
  (should (equal (sv-test-types "module m; endmodule")
                 '(keyword ident punct keyword)))
  (should (equal (sv-test-types "x = $clog2(8);")
                 '(ident operator sysfunc punct number punct punct)))
  (should (equal (sv-test-types "`include \"a.svh\"")
                 '(directive string))))

(ert-deftest sv-lexer-reads-number-literals ()
  (should (equal (sv-test-texts "8'hFF 'b1010 32'd12_3 1.5e-3 10ns 'x")
                 '("8'hFF" "'b1010" "32'd12_3" "1.5e-3" "10ns" "'x")))
  ;; The brace of an assignment pattern is a brace like any other, so that
  ;; everything which counts brackets stays balanced.
  (should (equal (sv-test-texts "a'{1}") '("a" "'" "{" "1" "}"))))

(ert-deftest sv-parser-keeps-assignment-patterns-balanced ()
  (let ((unit (sv-test-unit "module m;
                               for (genvar i = 0; i < 2; i++) begin : g
                                 `FF(q[i], d, '{default: '0}, clk)
                               end
                               assign x = 1;
                             endmodule")))
    (should (equal (mapcar (lambda (item) (plist-get item :type))
                           (plist-get unit :items))
                   '(generate-for continuous-assign)))))

(ert-deftest sv-lexer-keeps-comments-and-strings-whole ()
  (let ((tokens (sv-lex-string "a /* one\ntwo */ b // trailing\nc")))
    (should (equal (mapcar #'sv-token-text
                           (cl-remove-if-not
                            (lambda (tok) (eq (sv-token-type tok) 'comment))
                            tokens))
                   '("/* one\ntwo */" "// trailing"))))
  (should (equal (sv-test-texts "s = \"a \\\" b\";")
                 '("s" "=" "\"a \\\" b\"" ";"))))

(ert-deftest sv-lexer-tracks-lines-and-columns ()
  (let* ((tokens (cl-remove-if #'sv-token-trivia-p
                               (sv-lex-string "module m;\n  logic x;\n")))
         (logic (nth 3 tokens)))
    (should (equal (sv-token-text logic) "logic"))
    (should (= (sv-token-line logic) 2))
    (should (= (sv-token-col logic) 2))))

(ert-deftest sv-lexer-swallows-define-bodies ()
  (let ((tokens (cl-remove-if #'sv-token-trivia-p
                              (sv-lex-string "`define A(x) \\\n  (x + 1)\nlogic y;"))))
    (should (eq (sv-token-type (car tokens)) 'directive))
    (should (equal (sv-token-text (nth 1 tokens)) "logic"))))

(ert-deftest sv-lexer-never-loops-on-stray-characters ()
  (should (sv-lex-string "module m; ^^^ \x01 endmodule")))


;;;; Parser

(ert-deftest sv-parser-reads-an-ansi-header ()
  (let ((unit (sv-test-unit "module m #(parameter W = 8, parameter D = 2)
                             (input logic clk, input logic [W-1:0] i_a,
                              output logic o_b); endmodule")))
    (should (eq (plist-get unit :type) 'module))
    (should (equal (plist-get unit :name) "m"))
    (should (equal (mapcar (lambda (p) (plist-get p :name))
                           (plist-get unit :params))
                   '("W" "D")))
    (should (equal (sv-test-port-names unit) '("clk" "i_a" "o_b")))
    (should (eq (plist-get (nth 2 (plist-get unit :ports)) :dir) 'output))))

(ert-deftest sv-parser-inherits-direction-and-type ()
  (let ((unit (sv-test-unit "module m (input logic a, b, output c); endmodule")))
    (should (equal (sv-test-port-names unit) '("a" "b" "c")))
    (should (eq (plist-get (nth 1 (plist-get unit :ports)) :dir) 'input))
    (should (equal (plist-get (nth 1 (plist-get unit :ports)) :datatype) "logic"))))

(ert-deftest sv-parser-separates-packed-from-unpacked-dimensions ()
  (let* ((unit (sv-test-unit "module m (input entry_t [1:0] e [4]); endmodule"))
         (port (car (plist-get unit :ports))))
    (should (equal (plist-get port :name) "e"))
    (should (equal (plist-get port :datatype) "entry_t"))
    (should (equal (sv-parse-token-string (plist-get port :packed)) "[1:0]"))
    (should (equal (sv-parse-token-string (plist-get port :unpacked)) "[4]"))))

(ert-deftest sv-parser-accepts-an-import-in-the-header ()
  (let ((unit (sv-test-unit "module m
                             import pkg::*;
                             #(parameter W = 1)
                             (input logic i_a); endmodule")))
    (should (equal (sv-test-port-names unit) '("i_a")))
    (should (equal (mapcar (lambda (p) (plist-get p :name))
                           (plist-get unit :params))
                   '("W")))))

(ert-deftest sv-parser-tells-instances-from-declarations ()
  (let* ((unit (sv-test-unit "module m;
                                my_type_t sig;
                                sub #(.W(2)) u_sub (.a(x), .b(y));
                                other u_other (z);
                              endmodule"))
         (items (plist-get unit :items)))
    (should (eq (plist-get (nth 0 items) :type) 'decl))
    (should (eq (plist-get (nth 1 items) :type) 'instance))
    (should (equal (plist-get (nth 1 items) :module) "sub"))
    (should (equal (plist-get (nth 1 items) :name) "u_sub"))
    (should (equal (mapcar (lambda (c) (plist-get c :name))
                           (plist-get (nth 1 items) :connections))
                   '("a" "b")))
    (should (plist-get (car (plist-get (nth 2 items) :connections)) :positional))))

(ert-deftest sv-parser-records-procedural-structure ()
  (let* ((unit (sv-test-unit "module m;
                                always_ff @(posedge clk, negedge rst_n) begin
                                  if (!rst_n) q <= '0;
                                  else q <= d;
                                end
                              endmodule"))
         (always (car (plist-get unit :items)))
         (body (plist-get always :body)))
    (should (eq (plist-get always :kind) 'always_ff))
    (should (equal (sv-parse-token-text (plist-get always :sensitivity))
                   "posedge clk , negedge rst_n"))
    (should (eq (plist-get body :type) 'block))
    (let ((branch (car (plist-get body :stmts))))
      (should (eq (plist-get branch :type) 'if))
      (should (plist-get branch :else))
      (should (equal (plist-get (plist-get branch :then) :op) "<=")))))

(ert-deftest sv-parser-reads-case-statements ()
  (let* ((unit (sv-test-unit "module m;
                                always_comb begin
                                  unique case (s)
                                    2'b00 : y = 0;
                                    2'b01, 2'b10 : y = 1;
                                    default : y = 2;
                                  endcase
                                end
                              endmodule"))
         (case-node (car (plist-get (plist-get (car (plist-get unit :items)) :body)
                                    :stmts))))
    (should (eq (plist-get case-node :type) 'case))
    (should (eq (plist-get case-node :qualifier) 'unique))
    (should (= (length (plist-get case-node :items)) 3))
    (should (plist-get (nth 2 (plist-get case-node :items)) :default))))

(ert-deftest sv-parser-handles-generate-and-functions ()
  (let ((unit (sv-test-unit "module m;
                               generate for (genvar g = 0; g < 2; g++) begin : g_loop
                                 assign o[g] = i[g];
                               end endgenerate
                               function automatic int f(input int a);
                                 return a + 1;
                               endfunction
                             endmodule")))
    (should (sv-parse-collect unit 'generate-for))
    (should (equal (plist-get (car (sv-parse-collect unit 'function)) :name) "f"))
    (should (equal (plist-get (car (sv-parse-collect unit 'generate-block)) :label)
                   "g_loop"))))

(ert-deftest sv-parser-collects-lhs-targets ()
  (let* ((unit (sv-test-unit "module m;
                                always_comb {a, b[1]} = c;
                              endmodule"))
         (assign (plist-get (car (plist-get unit :items)) :body)))
    (should (equal (plist-get assign :lhs-targets) '("a" "b")))))

(ert-deftest sv-parser-keeps-module-boundaries-across-directives ()
  ;; A directive right before `endmodule' used to make the parser step over
  ;; the end keyword and swallow whatever module came next.
  (let ((tree (sv-parse-string "module a;\n`ifndef SYN\n  initial $display(\"x\");\n`endif\nendmodule\nmodule b;\nendmodule")))
    (should (equal (mapcar (lambda (unit) (plist-get unit :name))
                           (plist-get tree :units))
                   '("a" "b")))))

(ert-deftest sv-parser-consumes-directive-arguments ()
  (let ((unit (sv-test-unit "module m;\n`ifndef SYN\n  if (D < 2) begin : g\n    initial $fatal(1, \"x\");\n  end\n`endif\n  assign x = 1;\nendmodule")))
    (should (equal (mapcar (lambda (item) (plist-get item :type))
                           (plist-get unit :items))
                   '(generate-if continuous-assign)))))

(ert-deftest sv-parser-does-not-let-a-macro-swallow-the-next-statement ()
  (let* ((unit (sv-test-unit "module m;
                                always_comb begin
                                  if (a) begin
                                    `SET_STRUCT(o.b, i[k].b)
                                  end else begin
                                    o.b = '0;
                                  end
                                end
                              endmodule"))
         (branch (car (plist-get (plist-get (car (plist-get unit :items)) :body) :stmts))))
    (should (eq (plist-get branch :type) 'if))
    (should (plist-get branch :else))
    (should (equal (plist-get (car (plist-get (plist-get branch :else) :stmts))
                              :lhs-targets)
                   '("o")))))

(ert-deftest sv-parser-scopes-declarations-per-block ()
  (let* ((unit (sv-test-unit "module m;
                                if (X) begin : a
                                  localparam P = 1;
                                end else begin : b
                                  localparam P = 2;
                                end
                              endmodule"))
         (scopes (mapcar (lambda (record) (plist-get record :scope))
                         (cl-remove-if-not
                          (lambda (record) (equal (plist-get record :name) "P"))
                          (sv-parse-declarations unit)))))
    (should (= (length scopes) 2))
    (should (/= (nth 0 scopes) (nth 1 scopes)))))

(ert-deftest sv-parser-reads-anonymous-enums-and-structs ()
  (let* ((unit (sv-test-unit "module m;
                                enum logic [1:0] {IDLE, RUN} state;
                                struct packed { logic v; logic [7:0] d; } entry;
                              endmodule"))
         (declaration (car (plist-get unit :items))))
    (should (equal (mapcar (lambda (member) (plist-get member :name))
                           (plist-get declaration :enum-members))
                   '("IDLE" "RUN")))
    (should (member "IDLE" (sv-parse-declared-names unit)))
    (should (equal (mapcar (lambda (member) (plist-get member :name))
                           (plist-get (nth 1 (plist-get unit :items)) :members))
                   '("v" "d")))))

(ert-deftest sv-parser-finds-a-typed-loop-variable ()
  (let ((names (sv-parse-header-names
                (cl-remove-if #'sv-token-trivia-p
                              (sv-lex-string "(int unsigned j = 0; j < 3; j++)")))))
    (should (equal (mapcar #'car names) '("j")))))

(ert-deftest sv-parser-survives-a-syntax-error ()
  (let ((tree (sv-parse-string "module m; logic ((( ; endmodule module n; endmodule")))
    (should (member "n" (mapcar (lambda (u) (plist-get u :name))
                                (plist-get tree :units))))))

(ert-deftest sv-parser-reads-packages-and-interfaces ()
  (let ((tree (sv-parse-string "package p;
                                  typedef enum logic [1:0] { A, B } e_t;
                                endpackage
                                interface i_f (input logic clk);
                                  logic x;
                                  modport m (output x);
                                endinterface")))
    (should (equal (mapcar (lambda (u) (plist-get u :type)) (plist-get tree :units))
                   '(package interface)))
    (let ((typedef (car (sv-parse-collect (car (plist-get tree :units)) 'typedef))))
      (should (equal (plist-get typedef :name) "e_t"))
      (should (equal (mapcar (lambda (m) (plist-get m :name))
                             (plist-get typedef :enum-members))
                     '("A" "B"))))))


;;;; Linter

(ert-deftest sv-lint-accepts-clean-code ()
  (should-not
   (sv-test-rules "module test (input logic i_clk, input logic i_d, output logic o_q);
                     logic r_q;
                     always_ff @(posedge i_clk) begin
                       r_q <= i_d;
                     end
                     assign o_q = r_q;
                   endmodule
")))

(ert-deftest sv-lint-flags-blocking-in-always-ff ()
  (should (memq 'blocking-in-always-ff
                (sv-test-rules "module test (input logic i_clk, output logic o_q);
                                  always_ff @(posedge i_clk) o_q = 1'b1;
                                endmodule"))))

(ert-deftest sv-lint-allows-blocking-on-a-block-local-variable ()
  (should-not
   (memq 'blocking-in-always-ff
         (sv-test-rules "module test (input logic i_clk, output logic o_q);
                           always_ff @(posedge i_clk) begin
                             automatic logic tmp;
                             tmp = 1'b1;
                             o_q <= tmp;
                           end
                         endmodule"))))

(ert-deftest sv-lint-flags-nonblocking-in-combinational-logic ()
  (should (memq 'nonblocking-in-always-comb
                (sv-test-rules "module test (input logic i_a, output logic o_q);
                                  always_comb o_q <= i_a;
                                endmodule"))))

(ert-deftest sv-lint-finds-an-inferred-latch ()
  (let ((rules (sv-test-rules "module test (input logic i_s, input logic i_a,
                                            output logic o_q);
                                 always_comb begin
                                   if (i_s) o_q = i_a;
                                 end
                               endmodule")))
    (should (memq 'implicit-latch rules))))

(ert-deftest sv-lint-accepts-a-complete-conditional ()
  (should-not
   (memq 'implicit-latch
         (sv-test-rules "module test (input logic i_s, input logic i_a,
                                      output logic o_q);
                           always_comb begin
                             if (i_s) o_q = i_a;
                             else o_q = 1'b0;
                           end
                         endmodule"))))

(ert-deftest sv-lint-accepts-a-default-assignment-before-a-conditional ()
  (should-not
   (memq 'implicit-latch
         (sv-test-rules "module test (input logic i_s, input logic i_a,
                                      output logic o_q);
                           always_comb begin
                             o_q = 1'b0;
                             if (i_s) o_q = i_a;
                           end
                         endmodule"))))

(ert-deftest sv-lint-requires-a-case-default ()
  (let ((text "module test (input logic [1:0] i_s, output logic o_q);
                 always_comb begin
                   case (i_s)
                     2'b00 : o_q = 1'b1;
                     default : o_q = 1'b0;
                   endcase
                 end
               endmodule"))
    (should-not (memq 'case-without-default (sv-test-rules text)))
    (should (memq 'case-without-default
                  (sv-test-rules (replace-regexp-in-string
                                  "default : o_q = 1'b0;" "" text))))))

(ert-deftest sv-lint-exempts-a-unique-case ()
  (should-not
   (memq 'case-without-default
         (sv-test-rules "module test (input logic [1:0] i_s, output logic o_q);
                           always_comb begin
                             unique case (i_s)
                               2'b00 : o_q = 1'b1;
                               2'b01 : o_q = 1'b0;
                             endcase
                           end
                         endmodule"))))

(ert-deftest sv-lint-finds-undeclared-and-unused-names ()
  (let ((rules (sv-test-rules "module test (output logic o_q);
                                 logic w_never;
                                 assign o_q = w_missing;
                               endmodule")))
    (should (memq 'undeclared-identifier rules))
    (should (memq 'unused-declaration rules))))

(ert-deftest sv-lint-treats-a-write-only-signal-as-unused ()
  (should (memq 'unused-declaration
                (sv-test-rules "module test (input logic i_a, output logic o_q);
                                  logic w_dead;
                                  always_comb w_dead = i_a;
                                  assign o_q = i_a;
                                endmodule"))))

(ert-deftest sv-lint-stays-quiet-about-names-from-an-imported-package ()
  (should-not
   (memq 'undeclared-identifier
         (sv-test-rules "module test
                           import pkg::*;
                           (output logic o_q);
                           assign o_q = SOME_CONSTANT;
                         endmodule"))))

(ert-deftest sv-lint-finds-an-undriven-output ()
  (should (memq 'undriven-output
                (sv-test-rules "module test (input logic i_a, output logic o_q);
                                endmodule"))))

(ert-deftest sv-lint-counts-an-instance-connection-as-a-driver ()
  (should-not
   (memq 'undriven-output
         (sv-test-rules "module test (input logic i_a, output logic o_q);
                           sub u_sub (.a (i_a), .q (o_q));
                         endmodule"))))

(ert-deftest sv-lint-accepts-the-same-name-in-two-scopes ()
  (should-not
   (memq 'duplicate-declaration
         (sv-test-rules "module test (output logic o_q);
                           if (X) begin : a
                             localparam P = 1;
                             assign o_q = P;
                           end else begin : b
                             localparam P = 2;
                             assign o_q = P;
                           end
                         endmodule"))))

(ert-deftest sv-lint-accepts-the-same-name-in-two-ifdef-branches ()
  (should-not
   (memq 'duplicate-declaration
         (sv-test-rules "module test (output logic o_q);
                         `ifdef WIDE
                           typedef logic [7:0] word_t;
                         `else
                           typedef logic [3:0] word_t;
                         `endif
                           word_t w;
                           assign o_q = |w;
                         endmodule"))))

(ert-deftest sv-lint-ignores-struct-members-and-labels ()
  (let ((rules (sv-test-rules "module test (output logic o_q);
                                 typedef struct packed {
                                   logic       id;
                                   logic [7:0] len;
                                 } entry_t;
                                 entry_t e;
                                 assign o_q = e.id;
                                 check_it : assert final (o_q == 1'b1);
                               endmodule")))
    (should-not (memq 'undeclared-identifier rules))))

(ert-deftest sv-lint-counts-a-macro-argument-as-a-driver ()
  (should-not
   (memq 'undriven-output
         (sv-test-rules "module test (input logic i_clk, input logic i_d,
                                      output logic o_q);
                           `FFLARN(o_q, i_d, 1'b1, '0, i_clk, 1'b1)
                         endmodule"))))

(ert-deftest sv-lint-finds-duplicate-declarations ()
  (should (memq 'duplicate-declaration
                (sv-test-rules "module test (output logic o_q);
                                  logic x;
                                  logic x;
                                  assign o_q = x;
                                endmodule"))))

(ert-deftest sv-lint-checks-instance-connections ()
  (let* ((table (sv-lint-module-table
                 (sv-parse-string "module sub (input logic a, output logic q);
                                   endmodule")))
         (rules (let ((sv-lint-disabled-rules '()))
                  (mapcar #'sv-diagnostic-rule
                          (sv-lint-analyze
                           "module test (output logic o_q);
                              sub u_sub (.a (1'b0), .nope (o_q));
                            endmodule"
                           "test.sv" table)))))
    (should (memq 'instance-unknown-port rules))
    (should (memq 'instance-missing-port rules))))

(ert-deftest sv-lint-flags-positional-and-empty-connections ()
  (let ((rules (sv-test-rules "module test (input logic i_a, output logic o_q);
                                 sub u_a (i_a, o_q);
                                 sub u_b (.a (i_a), .q ());
                               endmodule")))
    (should (memq 'positional-port-connection rules))
    (should (memq 'unconnected-port rules))))

(ert-deftest sv-lint-checks-the-file-name ()
  (should (memq 'module-filename-mismatch
                (sv-test-rules "module test (output logic o_q);
                                  assign o_q = 1'b0;
                                endmodule"
                               "other.sv")))
  (should-not (memq 'module-filename-mismatch
                    (sv-test-rules "module test (output logic o_q);
                                      assign o_q = 1'b0;
                                    endmodule"
                                   "/tmp/test.sv"))))

(ert-deftest sv-lint-checks-whitespace ()
  (let ((sv-lint-max-line-length 20))
    (let ((rules (sv-test-rules "module test;\nlogic aaaaaaaaaaaaaaaaaaaaaaa;  \n\tlogic b;\nendmodule")))
      (should (memq 'line-too-long rules))
      (should (memq 'trailing-whitespace rules))
      (should (memq 'tab-indentation rules)))))

(ert-deftest sv-lint-honours-inline-suppressions ()
  (let ((text "module test (output logic o_q);
                 logic w_a; // sv-lint: disable=unused-declaration
                 // sv-lint: disable-next-line=unused-declaration
                 logic w_b;
                 logic w_c;
                 assign o_q = 1'b0;
               endmodule"))
    (should (equal (cl-count 'unused-declaration (sv-test-rules text)) 1))))

(ert-deftest sv-lint-honours-file-wide-suppressions ()
  (should-not
   (memq 'unused-declaration
         (sv-test-rules "// sv-lint: disable-file=unused-declaration
                         module test (output logic o_q);
                           logic w_a;
                           assign o_q = 1'b0;
                         endmodule"))))

(ert-deftest sv-lint-honours-verilator-pragmas ()
  (should-not
   (memq 'case-without-default
         (sv-test-rules "module test (input logic [1:0] i_s, output logic o_q);
                           /* verilator lint_off CASEINCOMPLETE */
                           always_comb begin
                             case (i_s)
                               2'b00 : o_q = 1'b1;
                             endcase
                           end
                           /* verilator lint_on CASEINCOMPLETE */
                         endmodule"))))

(ert-deftest sv-lint-reports-sorted-positions ()
  (let* ((sv-lint-disabled-rules '())
         (diagnostics (sv-lint-analyze
                       "module test (output logic o_q, output logic o_r);
                        endmodule"
                       "test.sv"))
         (lines (mapcar #'sv-diagnostic-line diagnostics)))
    (should (equal lines (sort (copy-sequence lines) #'<)))))


;;;; Formatter

(defun sv-test-format (text) (sv-format-text text))

(ert-deftest sv-format-indents-nested-blocks ()
  (should (equal (sv-test-format "module m;
always_ff @(posedge clk) begin
if (a) begin
x <= 1;
end
end
endmodule")
                 "module m;
  always_ff @(posedge clk) begin
    if (a) begin
      x <= 1;
    end
  end
endmodule")))

(ert-deftest sv-format-indents-an-else-if-chain ()
  "`end else if (...) begin\=' must not lose a level.
The `if\=' hanging off the `else\=' pushes a dangling frame, and the `begin\='
that follows has to inherit that frame's column, not the column of the
block below it."
  (should (equal (sv-test-format "module m;
always_comb begin
if (a) begin
x = 1;
end else if (b) begin
x = 2;
end else if (c) begin
x = 3;
end else begin
x = 4;
end
end
endmodule")
                 "module m;
  always_comb begin
    if (a) begin
      x = 1;
    end else if (b) begin
      x = 2;
    end else if (c) begin
      x = 3;
    end else begin
      x = 4;
    end
  end
endmodule")))

(ert-deftest sv-format-indents-a-hanging-statement ()
  (should (equal (sv-test-format "module m;
always_comb
if (a)
x = 1;
else
x = 2;
endmodule")
                 "module m;
  always_comb
    if (a)
      x = 1;
    else
      x = 2;
endmodule")))

(ert-deftest sv-format-indents-case-arms ()
  (should (equal (sv-test-format "module m;
always_comb
case (s)
0 : x = 1;
default :
x = 2;
endcase
endmodule")
                 "module m;
  always_comb
    case (s)
      0       : x = 1;
      default :
        x = 2;
    endcase
endmodule")))

(ert-deftest sv-format-indents-port-lists ()
  (should (equal (sv-test-format "module m
#(
parameter W = 1
)
(
input logic i_a,
output logic o_b
);
endmodule")
                 "module m
  #(
    parameter W = 1
  )
  (
    input  logic i_a,
    output logic o_b
  );
endmodule")))

(ert-deftest sv-format-keeps-directives-in-column-zero ()
  (should (equal (sv-test-format "`ifndef A\n`define A\nmodule m;\nlogic x;\nendmodule\n`endif")
                 "`ifndef A\n`define A\nmodule m;\n  logic x;\nendmodule\n`endif")))

(ert-deftest sv-format-aligns-declarations-and-assignments ()
  (should (equal (sv-test-format "module m;
logic [7:0] wide;
logic n;
assign wide = 1;
assign n = 0;
endmodule")
                 "module m;
  logic [7:0] wide;
  logic       n;
  assign wide = 1;
  assign n    = 0;
endmodule")))

(ert-deftest sv-format-aligns-instance-connections ()
  (should (equal (sv-test-format "module m;
sub u_sub (
.a (x),
.long_name (y)
);
endmodule")
                 "module m;
  sub u_sub (
    .a         (x),
    .long_name (y)
  );
endmodule")))

(ert-deftest sv-format-aligns-across-blank-lines-and-comments ()
  "A blank line or a comment does not end a column group.
A port list written in paragraphs is still one list, and lining its
paragraphs up separately is what a reader notices."
  (should (equal (sv-test-format "module m
(
input logic i_clk,

// requests
input logic [3:0] i_req,
output logic o_ack
);
endmodule")
                 "module m
  (
    input  logic       i_clk,

    // requests
    input  logic [3:0] i_req,
    output logic       o_ack
  );
endmodule")))

(ert-deftest sv-format-ends-a-column-group-at-code ()
  "Anything other than a blank line or a comment still ends a group.
These two declarations belong to different parts of the module and must
not be lined up with each other."
  (should (equal (sv-test-format "module m;
logic a;

always_comb begin
x = 1;
end

logic bbbb;
endmodule")
                 "module m;
  logic a;

  always_comb begin
    x = 1;
  end

  logic bbbb;
endmodule")))

(ert-deftest sv-format-respects-the-alignment-spread-limit ()
  (let ((sv-format-align-max-spread 2))
    (should (equal (sv-test-format "module m;
logic [7:0] wide;
logic n;
endmodule")
                   "module m;
  logic [7:0] wide;
  logic n;
endmodule"))))

(ert-deftest sv-format-normalizes-spacing ()
  (should (equal (sv-test-format "module m;\nassign  x   =  y ;\nendmodule")
                 "module m;\n  assign x = y;\nendmodule"))
  (should (equal (sv-test-format "module m;\nfoo(a ,b);\nendmodule")
                 "module m;\n  foo(a, b);\nendmodule")))

(ert-deftest sv-format-keeps-blank-lines-and-comments ()
  (should (equal (sv-test-format "module m;\n\n// note\nlogic x;\nendmodule")
                 "module m;\n\n  // note\n  logic x;\nendmodule")))

(ert-deftest sv-format-indents-a-continuation-line ()
  (should (equal (sv-test-format "module m;\nassign x =\ny + z;\nendmodule")
                 "module m;\n  assign x =\n      y + z;\nendmodule")))

(ert-deftest sv-format-aligns-a-continued-assignment ()
  "A continued assignment lines up under its right-hand side.
A break after the `:\=' of a conditional counts, even though `:\=' is not a
continuation operator anywhere else."
  (should (equal (sv-test-format "module m;
assign a = sel ? bbb :
ccc;
assign e = fff +
ggg;
endmodule")
                 "module m;
  assign a = sel ? bbb :
             ccc;
  assign e = fff +
             ggg;
endmodule")))

(ert-deftest sv-format-lines-bracket-contents-up-under-the-bracket ()
  "Contents line up under whatever follows the bracket on its line."
  (should (equal (sv-test-format "module m;
assign a = {p[3:0],
q[2:0],
r[1]};
assign b = foo(xx,
yy);
endmodule")
                 "module m;
  assign a = {p[3:0],
              q[2:0],
              r[1]};
  assign b = foo(xx,
                 yy);
endmodule")))

(ert-deftest sv-format-offsets-contents-of-a-bracket-that-ends-its-line ()
  "A bracket with nothing after it offers no column to line up under.
Its contents fall back to `sv-format-indent-offset', which is what an
instance port list written one connection per line relies on."
  (should (equal (sv-test-format "module m;
assign c = {
p,
q};
sub u (
.a (1),
.bb (2));
assign k =
lll;
endmodule")
                 "module m;
  assign c = {
    p,
    q};
  sub u (
    .a  (1),
    .bb (2));
  assign k =
      lll;
endmodule")))

(ert-deftest sv-format-honours-the-continuation-align-option ()
  (let ((sv-format-align-assign-continuation nil))
    (should (equal (sv-test-format "module m;
assign e = fff +
ggg;
endmodule")
                   "module m;
  assign e = fff +
      ggg;
endmodule"))))

(ert-deftest sv-format-honours-the-unit-body-option ()
  (let ((sv-format-indent-unit-body nil))
    (should (equal (sv-test-format "module m;\nlogic x;\nendmodule")
                   "module m;\nlogic x;\nendmodule"))))

(ert-deftest sv-format-preserves-every-token ()
  (dolist (text (list "module m; logic [3:0] x = 4'hf; endmodule"
                      "module m;\nalways_comb begin\ncase (s)\n0 : x = 1;\nendcase\nend\nendmodule"
                      "`define M(a) (a)\nmodule m;\n/* c */ assign x = `M(1);\nendmodule"))
    (let ((before (sv-test-texts text))
          (after (sv-test-texts (sv-test-format text))))
      (should (equal before after)))))

(ert-deftest sv-format-is-idempotent ()
  (dolist (text (list "module m;\nalways_ff @(posedge c) if (a) x <= 1; else x <= 2;\nendmodule"
                      "module m (input logic a, output logic b);\nassign b = a;\nendmodule"
                      "module m;\nsub u (\n.a (1),\n.bb (2)\n);\nendmodule"
                      "module m;\nassign o = s ? a :\nb;\nassign p = c +\nd;\nendmodule"))
    (let ((once (sv-test-format text)))
      (should (equal once (sv-test-format once))))))

(ert-deftest sv-format-region-only-touches-its-lines ()
  (with-temp-buffer
    (insert "module m;\nlogic x;\n      logic y;\nendmodule\n")
    (let ((start (progn (goto-char (point-min)) (forward-line 2) (point))))
      (sv-format-region start (line-end-position)))
    (should (equal (buffer-string)
                   "module m;\nlogic x;\n  logic y;\nendmodule\n"))))

(ert-deftest sv-format-indent-line-keeps-point-in-the-text ()
  (with-temp-buffer
    (insert "module m;\nlogic x;\nendmodule\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (sv-format-indent-line)
    (should (= (current-indentation) 2))
    (should (equal (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))
                   "  logic x;"))
    (should (= (point) (line-end-position)))))


;;;; Integration

(ert-deftest sv-kit-builds-an-instance-template ()
  (let ((unit (sv-test-unit "module sub #(parameter W = 1)
                             (input logic i_clk, output logic [W-1:0] o_q);
                             endmodule")))
    (should (equal (sv-kit-instance-template unit)
                   "sub #(
    .W (W)
  ) u_sub (
    .i_clk (i_clk),
    .o_q   (o_q)
  );
"))))

(ert-deftest sv-kit-indexes-a-buffer-for-imenu ()
  (with-temp-buffer
    (insert "module m;\n  sub u_sub (.a (b));\n  always_comb x = 1;\nendmodule\n")
    (let* ((index (sv-kit-imenu-index))
           (units (cdr (assoc "Units" index))))
      (should (equal (caar units) "module m"))
      (should (assoc "Instances" index))
      (goto-char (cdar units))
      (should (looking-at "module")))))

(ert-deftest sv-kit-formats-and-lints-the-same-buffer ()
  (with-temp-buffer
    (insert "module test (input logic i_a, output logic o_q);\nassign o_q = i_a;\nendmodule\n")
    (sv-format-buffer)
    (should (equal (buffer-string)
                   "module test (input logic i_a, output logic o_q);\n  assign o_q = i_a;\nendmodule\n"))
    (should-not (sv-lint-buffer))))


;;;; Major mode


(defun sv-test-fontify (text)
  "Return a temporary buffer holding TEXT fontified in `sv-mode'."
  (let ((buffer (generate-new-buffer " *sv-test*")))
    (with-current-buffer buffer
      (insert text)
      (sv-mode)
      (font-lock-mode 1)
      (font-lock-ensure))
    buffer))

(defun sv-test-face (text needle &optional occurrence)
  "Return the face `sv-mode' gives to NEEDLE inside TEXT.
OCCURRENCE selects which match to look at, counting from one."
  (let ((buffer (sv-test-fontify text)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (search-forward needle nil t (or occurrence 1))
            (let ((face (get-text-property (match-beginning 0) 'face)))
              (if (listp face) (car face) face))))
      (kill-buffer buffer))))

(ert-deftest sv-mode-highlights-keywords-types-and-literals ()
  (let ((text "module m;\n  logic [3:0] r_data;\n  assign r_data = 4'hf;\nendmodule\n"))
    (should (eq (sv-test-face text "module") 'font-lock-keyword-face))
    (should (eq (sv-test-face text "logic") 'font-lock-type-face))
    (should (eq (sv-test-face text "r_data") 'font-lock-variable-name-face))
    (should (eq (sv-test-face text "4'hf") 'font-lock-constant-face))
    (should (eq (sv-test-face text "assign") 'font-lock-keyword-face))))

(ert-deftest sv-mode-highlights-directives-rather-than-their-keyword ()
  (let ((text "`ifdef A\n`else\n`endif\n"))
    (should (eq (sv-test-face text "`else") 'sv-mode-directive-face))
    ;; The `else' inside `\=`else' must not be read as the keyword.
    (should (eq (sv-test-face text "else") 'sv-mode-directive-face))))

(ert-deftest sv-mode-highlights-design-unit-names ()
  (let ((text "module top (input logic i_a, output logic o_b);\nendmodule : top\n"))
    (should (eq (sv-test-face text "top") 'font-lock-function-name-face))
    (should (eq (sv-test-face text "top" 2) 'sv-mode-label-face))
    (should (eq (sv-test-face text "i_a") 'font-lock-variable-name-face))
    (should (eq (sv-test-face text "o_b") 'font-lock-variable-name-face))))

(ert-deftest sv-mode-highlights-instances-and-connections ()
  (let ((text "module m;\n  sub #(.W (2)) u_sub (.i_clk (clk), .o_q (q));\nendmodule\n"))
    (should (eq (sv-test-face text "sub") 'font-lock-type-face))
    (should (eq (sv-test-face text "u_sub") 'sv-mode-instance-face))
    (should (eq (sv-test-face text ".i_clk") 'sv-mode-port-face))
    (should (eq (sv-test-face text ".W") 'sv-mode-port-face))))

(ert-deftest sv-mode-highlights-types-the-file-declares ()
  (let ((text "module m;\n  typedef enum logic [1:0] { IDLE, RUN } state_t;\n  state_t r_state;\n  always_comb r_state = IDLE;\nendmodule\n"))
    (should (eq (sv-test-face text "state_t" 2) 'font-lock-type-face))
    (should (eq (sv-test-face text "r_state") 'font-lock-variable-name-face))
    (should (eq (sv-test-face text "IDLE" 2) 'font-lock-constant-face))))

(ert-deftest sv-mode-leaves-comments-and-strings-alone ()
  (let ((text "module m;\n  // module logic assign\n  initial $display(\"module logic\");\nendmodule\n"))
    (should (eq (sv-test-face text "module logic assign") 'font-lock-comment-face))
    (should (eq (sv-test-face text "\"module logic\"") 'font-lock-string-face))
    (should (eq (sv-test-face text "$display") 'font-lock-builtin-face))))

(ert-deftest sv-mode-treats-an-apostrophe-as-punctuation ()
  ;; A literal such as 4'b0 must not open a string that swallows the rest.
  (let ((text "module m;\n  assign x = 4'b0;\n  // plain comment\nendmodule\n"))
    (should (eq (sv-test-face text "plain comment") 'font-lock-comment-face))
    (should (eq (sv-test-face text "endmodule") 'font-lock-keyword-face))))

(ert-deftest sv-mode-matchers-always-advance ()
  ;; A matcher that rejects a match must still move point, or font-lock spins.
  (let ((buffer (generate-new-buffer " *sv-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "module top;\n  logic [3:0] r_data;\n  always @(posedge c or negedge r) q <= 1;\nendmodule\n")
          (sv-mode)
          (dolist (matcher (list #'sv-mode--match-declaration
                                 #'sv-mode--match-instance
                                 #'sv-mode--match-parameter))
            (goto-char (point-min))
            (let ((previous -1) (steps 0))
              (while (and (< steps 200) (funcall matcher (point-max)))
                (should (> (point) previous))
                (setq previous (point))
                (setq steps (1+ steps)))
              (should (< steps 200)))))
      (kill-buffer buffer))))

(ert-deftest sv-mode-indents-with-tab-and-indent-region ()
  (with-temp-buffer
    (insert "module m;\nalways_comb begin\nx = 1;\nend\nendmodule\n")
    (sv-mode)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "module m;\n  always_comb begin\n    x = 1;\n  end\nendmodule\n"))))

(ert-deftest sv-mode-navigates-to-the-enclosing-unit ()
  (with-temp-buffer
    (insert "module first;\nendmodule\nmodule second;\n  logic x;\nendmodule\n")
    (sv-mode)
    (goto-char (point-min))
    (search-forward "logic x")
    (should (equal (sv-mode-current-defun) "second"))
    (beginning-of-defun)
    (should (looking-at-p "module second"))))

(ert-deftest sv-mode-claims-verilog-file-names ()
  (dolist (name '("foo.sv" "foo.svh" "foo.v" "foo.vh"))
    (should (eq (assoc-default name auto-mode-alist #'string-match-p) #'sv-mode))))


;;;; Constant folding and width inference

(defun sv-test-width-context (source)
  "Return a width context for the first design unit of SOURCE."
  (sv-width-context (sv-test-unit source)))

(defun sv-test-expression (text)
  "Return the significant tokens of the expression TEXT."
  (cl-remove-if #'sv-token-trivia-p (sv-lex-string text)))

(defconst sv-test-width-source
  "module m #(parameter int W = 8, parameter int D = 4) ();
     localparam int PTR = $clog2(D);
     typedef logic [W-1:0] word_t;
     typedef struct packed { logic v; logic [7:0] d; } entry_t;
     logic [W-1:0] a, a2;
     logic [3:0]   b;
     word_t        c;
     entry_t       e;
     logic         x;
     logic [3:0]   mem [8];
   endmodule"
  "A module exercising the shapes the width engine has to handle.")

(ert-deftest sv-width-folds-constant-expressions ()
  (let ((context (sv-test-width-context sv-test-width-source)))
    (should (= (sv-width-eval (sv-test-expression "W") context) 8))
    (should (= (sv-width-eval (sv-test-expression "PTR") context) 2))
    (should (= (sv-width-eval (sv-test-expression "$clog2(D)") context) 2))
    (should (= (sv-width-eval (sv-test-expression "W*2+1") context) 17))
    (should (= (sv-width-eval (sv-test-expression "(W > 4) ? 10 : 20") context) 10))
    (should (= (sv-width-eval (sv-test-expression "8'hff") context) 255))
    (should-not (sv-width-eval (sv-test-expression "unknown_name") context))))

(ert-deftest sv-width-reads-declared-widths ()
  (let ((context (sv-test-width-context sv-test-width-source)))
    (should (= (sv-width-signal "a" context) 8))
    ;; A later declarator inherits the dimensions of the first.
    (should (= (sv-width-signal "a2" context) 8))
    (should (= (sv-width-signal "b" context) 4))
    (should (= (sv-width-signal "c" context) 8))
    ;; A packed struct is as wide as its members together.
    (should (= (sv-width-signal "e" context) 9))
    (should (= (sv-width-signal "x" context) 1))))

(ert-deftest sv-width-infers-expression-widths ()
  (let* ((context (sv-test-width-context sv-test-width-source))
         (width (lambda (text)
                  (sv-width-of (sv-test-expression text) context))))
    (should (= (funcall width "a + b") 8))
    (should (= (funcall width "{a, b}") 12))
    (should (= (funcall width "{4{b}}") 16))
    (should (= (funcall width "a[3:0]") 4))
    (should (= (funcall width "a[2]") 1))
    (should (= (funcall width "a[1+:3]") 3))
    (should (= (funcall width "a == b") 1))
    (should (= (funcall width "|a") 1))
    (should (= (funcall width "|a | x") 1))
    (should (= (funcall width "-b") 4))
    (should (= (funcall width "a << 2") 8))
    (should (= (funcall width "x ? a : c") 8))
    (should (= (funcall width "word_t'(b)") 8))
    ;; An unsized literal takes its width from the context, so it has none.
    (should-not (funcall width "42"))
    (should-not (funcall width "'0"))
    ;; An element of an array is not one bit, and its width is not guessed.
    (should-not (funcall width "mem[2]"))))

(ert-deftest sv-lint-finds-a-truncating-assignment ()
  (let ((rules (sv-test-rules "module test (input logic [7:0] i_a, output logic [3:0] o_q);
                                 assign o_q = i_a;
                               endmodule")))
    (should (memq 'width-truncation rules)))
  (should-not
   (memq 'width-truncation
         (sv-test-rules "module test (input logic [3:0] i_a, output logic [7:0] o_q);
                           assign o_q = i_a;
                         endmodule"))))

(ert-deftest sv-lint-finds-a-constant-that-does-not-fit ()
  (should (memq 'constant-overflow
                (sv-test-rules "module test (output logic [3:0] o_q);
                                  assign o_q = 5'd20;
                                endmodule")))
  (should-not
   (memq 'constant-overflow
         (sv-test-rules "module test (output logic [3:0] o_q);
                           assign o_q = 4'hf;
                         endmodule"))))

(ert-deftest sv-lint-respects-the-generate-branch-that-is-elaborated ()
  ;; With Width at 1 the taken branch assigns one bit to one bit; the branch
  ;; that is not elaborated must not be judged.
  (should-not
   (memq 'width-truncation
         (sv-test-rules "module test #(parameter int Width = 1)
                                     (input logic [Width-1:0] i_d, output logic o_q);
                           if (Width == 1) begin : gen_one
                             assign o_q = i_d;
                           end else begin : gen_many
                             assign o_q = |i_d;
                           end
                         endmodule"))))

(ert-deftest sv-lint-checks-port-widths-with-overrides ()
  (let* ((table (sv-lint-module-table
                 (sv-parse-string "module sub #(parameter int N = 8)
                                     (input logic [N-1:0] i_d);
                                   endmodule")))
         (sv-lint-disabled-rules '())
         (rules (mapcar #'sv-diagnostic-rule
                        (sv-lint-analyze
                         "module test (output logic o_q);
                            logic [7:0] w_wide;
                            sub #(.N(4)) u_sub (.i_d (w_wide));
                            assign o_q = |w_wide;
                          endmodule"
                         "test.sv" table))))
    (should (memq 'port-width-mismatch rules))))

;;;; Project index and editor services

(defconst sv-test-fixtures
  (expand-file-name "fixtures"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "Directory holding the small project the index tests run against.")

(defmacro sv-test-with-project (&rest body)
  "Run BODY with the fixture directory as the current project."
  (declare (indent 0) (debug t))
  `(let ((sv-index-root-markers '("filelist.f"))
         (default-directory (file-name-as-directory sv-test-fixtures)))
     (sv-index-invalidate)
     (unwind-protect (progn ,@body) (sv-index-invalidate))))

(defmacro sv-test-with-source (text &rest body)
  "Run BODY in a `sv-mode' buffer of the fixture project holding TEXT."
  (declare (indent 1) (debug t))
  `(sv-test-with-project
     (let ((buffer (generate-new-buffer " *sv-source*")))
       (unwind-protect
           (with-current-buffer buffer
             (setq default-directory (file-name-as-directory sv-test-fixtures))
             (insert ,text)
             (sv-mode)
             (goto-char (point-min))
             ,@body)
         (kill-buffer buffer)))))

(ert-deftest sv-index-indexes-a-project ()
  (sv-test-with-project
    (let ((index (sv-index-project)))
      (should (gethash "sub_block" (plist-get index :units)))
      (should (gethash "types_pkg" (plist-get index :units)))
      (should (equal (sv-symbol-signature (car (sv-index-lookup "i_data")))
                     "input logic [W-1:0] i_data"))
      (should (equal (sv-symbol-kind (car (sv-index-lookup "ST_RUN"))) 'enum))
      (should (member "state_t" (sv-index-names))))))

(ert-deftest sv-index-reuses-a-cached-file ()
  (sv-test-with-project
    (let* ((file (expand-file-name "sub_block.sv" sv-test-fixtures))
           (first (car (sv-index-file file)))
           (second (car (sv-index-file file))))
      (should (eq first second))
      (sv-index-invalidate file)
      (should-not (eq first (car (sv-index-file file)))))))

(ert-deftest sv-index-picks-the-closest-root ()
  (sv-test-with-project
    ;; The repository above the fixtures also carries a marker; the nearer
    ;; one must win.
    (let ((sv-index-root-markers '(".git" "filelist.f")))
      (should (equal (file-name-as-directory (sv-index-root))
                     (file-name-as-directory sv-test-fixtures))))))

(ert-deftest sv-ide-finds-the-instance-around-point ()
  (sv-test-with-source
      "module probe;\n  sub_block #(.W (4)) u_sub (.i_clk (clk), .i_data (d));\nendmodule\n"
    (search-forward ".i_data")
    (let ((context (sv-ide-instance-context)))
      (should (equal (plist-get context :module) "sub_block"))
      (should (equal (plist-get context :instance) "u_sub"))
      (should (equal (sort (copy-sequence (plist-get context :connected)) #'string<)
                     '("i_clk" "i_data"))))))

(ert-deftest sv-ide-completes-the-ports-still-free ()
  (sv-test-with-source
      "module probe;\n  sub_block u_sub (.i_clk (clk), ."
    (goto-char (point-max))
    (let ((completion (sv-ide-completion-at-point)))
      (should (equal (nth 2 completion) '("i_data" "o_data")))
      (should (string-match-p "input"
                              (funcall (plist-get (nthcdr 3 completion)
                                                  :annotation-function)
                                       "i_data"))))))

(ert-deftest sv-ide-completes-names-in-scope-before-the-project ()
  (sv-test-with-source
      "module probe;\n  logic w_local;\n  assign w_l"
    (goto-char (point-max))
    (let ((candidates (nth 2 (sv-ide-completion-at-point))))
      (should (member "w_local" candidates))
      (should (member "sub_block" candidates))
      (should (< (cl-position "w_local" candidates :test #'equal)
                 (cl-position "sub_block" candidates :test #'equal))))))

(ert-deftest sv-ide-completes-system-tasks ()
  (sv-test-with-source
      "module probe;\n  initial $disp"
    (goto-char (point-max))
    (should (member "$display" (nth 2 (sv-ide-completion-at-point))))))

(ert-deftest sv-ide-jumps-to-a-module-definition ()
  (sv-test-with-source
      "module probe;\n  sub_block u_sub (.i_clk (clk));\nendmodule\n"
    (search-forward "sub_block")
    (backward-char 2)
    (let ((definitions (xref-backend-definitions
                        'sv-kit (xref-backend-identifier-at-point 'sv-kit))))
      (should definitions)
      (should (string-match-p "module sub_block"
                              (xref-item-summary (car definitions))))
      (should (equal (file-name-nondirectory
                      (xref-location-group (xref-item-location (car definitions))))
                     "sub_block.sv")))))

(ert-deftest sv-ide-jumps-to-a-local-declaration ()
  (sv-test-with-source
      "module probe;\n  logic [3:0] w_sum;\n  assign w_sum = 4'h0;\nendmodule\n"
    (search-forward "assign w_sum")
    (backward-char 2)
    (let ((definitions (xref-backend-definitions 'sv-kit "w_sum")))
      (should definitions)
      (should (string-match-p "logic \\[3:0\\] w_sum"
                              (xref-item-summary (car definitions)))))))

(ert-deftest sv-ide-lists-references ()
  (sv-test-with-project
    (let ((references (xref-backend-references 'sv-kit "i_data")))
      (should (> (length references) 1))
      (should (cl-every (lambda (item) (stringp (xref-item-summary item)))
                        references)))))

(ert-deftest sv-ide-describes-the-name-at-point ()
  (sv-test-with-source
      "module probe;\n  logic [7:0] w_data;\n  sub_block u_sub (.i_data (w_data));\nendmodule\n"
    (search-forward "w_data")
    (backward-char 2)
    (should (string-match-p "logic \\[7:0\\] w_data"
                            (sv-ide-documentation-at-point)))
    (goto-char (point-min))
    (search-forward ".i_data")
    (backward-char 2)
    (should (equal (sv-ide-documentation-at-point)
                   "sub_block.i_data: input logic [W-1:0]"))))

(defconst sv-test-struct-source
  "module probe;
     typedef struct packed { logic valid; logic [7:0] payload; } inner_t;
     typedef struct packed { inner_t head; logic [3:0] id; } outer_t;
     outer_t w_pkt;
     outer_t w_arr [4];
   endmodule
"
  "A module with nested packed structs, for the field-aware features.")

(defun sv-test-insert-in-module (text)
  "Insert TEXT just above the `endmodule' of this buffer, and stay after it."
  (goto-char (point-min))
  (search-forward "endmodule")
  (beginning-of-line)
  (insert text))

(ert-deftest sv-ide-completes-the-fields-of-a-struct ()
  (sv-test-with-source sv-test-struct-source
    (sv-test-insert-in-module "  assign x = w_pkt.")
    (should (equal (nth 2 (sv-ide-completion-at-point)) '("head" "id")))))

(ert-deftest sv-ide-completes-through-a-nested-struct-and-an-array ()
  (sv-test-with-source sv-test-struct-source
    (sv-test-insert-in-module "  assign x = w_pkt.head.")
    (should (equal (nth 2 (sv-ide-completion-at-point)) '("valid" "payload")))
    (insert "valid;\n  assign y = w_arr[2].")
    (should (equal (nth 2 (sv-ide-completion-at-point)) '("head" "id")))))

(ert-deftest sv-ide-describes-a-struct-field ()
  (sv-test-with-source sv-test-struct-source
    (sv-test-insert-in-module "  assign x = w_pkt.head;\n")
    (goto-char (point-min))
    (search-forward "w_pkt.head")
    (backward-char 2)
    (should (equal (sv-ide-documentation-at-point) "outer_t.head: inner_t head"))))

(ert-deftest sv-ide-resolves-a-dotted-prefix ()
  (sv-test-with-source sv-test-struct-source
    (sv-test-insert-in-module "  assign x = w_arr[1].head.")
    (should (equal (sv-ide-dotted-prefix (1- (point))) '("w_arr" "head")))))

(defmacro sv-test-with-hierarchy (module &rest body)
  "Open the tree of MODULE over the fixture project and run BODY in it."
  (declare (indent 1) (debug t))
  `(sv-test-with-project
     (unwind-protect
         (progn (sv-hierarchy ,module)
                (with-current-buffer "*sv-hierarchy*"
                  (goto-char (point-min))
                  ,@body))
       (when (get-buffer "*sv-hierarchy*") (kill-buffer "*sv-hierarchy*")))))

(defun sv-test-tree-text ()
  "Return the text of the tree buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest sv-hierarchy-finds-the-top-of-a-design ()
  (sv-test-with-project
    ;; sub_block is instantiated, so only top_block is a top.
    (should (equal (sv-hierarchy-tops) '("top_block")))))

(ert-deftest sv-hierarchy-opens-closed-and-expands-on-demand ()
  (sv-test-with-hierarchy nil
    ;; One line per top, marked as having something under it.
    (should (equal (string-trim-right (sv-test-tree-text))
                   (car (split-string (sv-test-tree-text) "\n"))))
    (should (string-match-p "\\`\\+ top_block" (sv-test-tree-text)))
    (sv-hierarchy-toggle)
    (let ((text (sv-test-tree-text)))
      (should (string-match-p "^- top_block" text))
      (should (string-match-p "u_first : sub_block" text))
      (should (string-match-p "u_second : sub_block" text)))
    ;; And closing it takes them away again.
    (goto-char (point-min))
    (sv-hierarchy-toggle)
    (should (string-match-p "\\`\\+ top_block" (sv-test-tree-text)))
    (should-not (string-match-p "u_first" (sv-test-tree-text)))))

(ert-deftest sv-hierarchy-remembers-where-everything-is ()
  (sv-test-with-hierarchy "top_block"
    (sv-hierarchy-toggle)
    (forward-line 1)
    (let ((definition (get-text-property (line-beginning-position)
                                         'sv-hierarchy-definition))
          (instantiation (get-text-property (line-beginning-position)
                                            'sv-hierarchy-instantiation)))
      (should (equal (file-name-nondirectory (car definition)) "sub_block.sv"))
      (should (= (cdr definition) 1))
      (should (equal (file-name-nondirectory (car instantiation))
                     "top_block.sv"))
      (should (= (cdr instantiation) 13)))))

(ert-deftest sv-hierarchy-marks-a-leaf-as-having-nothing-under-it ()
  (sv-test-with-hierarchy "sub_block"
    (should (string-match-p "\\`  sub_block" (sv-test-tree-text)))
    (should-not (get-text-property (point-min) 'sv-hierarchy-expandable))))

(ert-deftest sv-hierarchy-expands-a-whole-subtree ()
  (sv-test-with-hierarchy "top_block"
    (sv-hierarchy-expand-all)
    (should (= (length (split-string (string-trim-right (sv-test-tree-text))
                                     "\n"))
               3))))

(ert-deftest sv-hierarchy-reports-what-instantiates-a-module ()
  (sv-test-with-project
    (let ((sites (sv-hierarchy-callers "sub_block")))
      (should (= (length sites) 2))
      (should (cl-every (lambda (site)
                          (equal (plist-get site :parent) "top_block"))
                        sites))
      (should (equal (sort (mapcar (lambda (site) (plist-get site :instance))
                                   sites)
                           #'string<)
                     '("u_first" "u_second"))))))

(ert-deftest sv-mode-moves-over-a-block ()
  (with-temp-buffer
    (insert "module m;\n  always_comb begin\n    if (a) begin\n      x = 1;\n    end\n  end\nendmodule\n")
    (sv-mode)
    (goto-char (point-min))
    (search-forward "always_comb ")
    (sv-mode-forward-block)
    (should (= (line-number-at-pos) 6))))

(ert-deftest sv-kit-adds-the-ports-an-instance-leaves-out ()
  (sv-test-with-source
      "module probe;\n  logic clk;\n  sub_block u_sub (.i_clk (clk));\nendmodule\n"
    (search-forward "u_sub")
    (sv-kit-update-instance)
    (should (string-match-p "\\.i_data +(i_data)" (buffer-string)))
    (should (string-match-p "\\.o_data +(o_data)" (buffer-string)))
    ;; Running it again changes nothing.
    (let ((before (buffer-string)))
      (goto-char (point-min))
      (search-forward "u_sub")
      (sv-kit-update-instance)
      (should (equal before (buffer-string))))))

(ert-deftest sv-kit-declares-the-signals-a-module-assigns ()
  (sv-test-with-source
      "module probe;\n  logic [7:0] w_data;\n  always_comb w_sum = w_data;\n  assign w_flag = 1'b0;\nendmodule\n"
    (search-forward "always_comb")
    (sv-kit-declare-missing-signals)
    (should (string-match-p "logic +w_flag;" (buffer-string)))
    (should (string-match-p "logic +w_sum;" (buffer-string)))
    ;; The new declarations sit with the others, above the logic.
    (should (< (string-match "w_sum;" (buffer-string))
               (string-match "always_comb" (buffer-string))))))


;;;; Rules added on top of the first set

(ert-deftest sv-lint-finds-a-signal-with-several-drivers ()
  (let ((rules (sv-test-rules "module test (input logic i_a, output logic o_q);
                                 logic w_x;
                                 assign w_x = i_a;
                                 always_comb w_x = ~i_a;
                                 assign o_q = w_x;
                               endmodule")))
    (should (memq 'multiple-drivers rules))))

(ert-deftest sv-lint-accepts-a-signal-driven-in-slices ()
  (should-not
   (memq 'multiple-drivers
         (sv-test-rules "module test (input logic i_a, output logic [3:0] o_q);
                           assign o_q[1:0] = {2{i_a}};
                           assign o_q[3:2] = 2'b00;
                         endmodule"))))

(ert-deftest sv-lint-flags-an-assignment-to-an-input ()
  (should (memq 'assignment-to-input
                (sv-test-rules "module test (input logic i_a, output logic o_q);
                                  always_comb i_a = 1'b0;
                                  assign o_q = i_a;
                                endmodule"))))

(ert-deftest sv-lint-finds-a-repeated-case-label ()
  (should (memq 'duplicate-case-label
                (sv-test-rules "module test (input logic [1:0] i_s, output logic o_q);
                                  always_comb begin
                                    case (i_s)
                                      2'b00 : o_q = 1'b0;
                                      2'b00 : o_q = 1'b1;
                                      default : o_q = 1'b0;
                                    endcase
                                  end
                                endmodule"))))

(ert-deftest sv-lint-flags-a-block-that-mixes-assignment-styles ()
  (should (memq 'mixed-assignment-style
                (sv-test-rules "module test (input logic i_clk, output logic o_q);
                                  logic w_t;
                                  always @(posedge i_clk) begin
                                    w_t = 1'b1;
                                    o_q <= w_t;
                                  end
                                endmodule"))))

(ert-deftest sv-mode-outlines-design-units ()
  (with-temp-buffer
    (insert "module m;\n  always_comb x = 1;\nendmodule\n")
    (sv-mode)
    (goto-char (point-min))
    (should (looking-at-p outline-regexp))
    (forward-line 1)
    (should (looking-at-p outline-regexp))
    (should (= (funcall outline-level) 2))))


;;;; Renaming

(defmacro sv-test-with-temp-project (&rest body)
  "Run BODY with a writable copy of the fixture project as the project.
`sv-test-project-directory\=' holds its path, and every buffer it opened is
discarded afterwards."
  (declare (indent 0) (debug t))
  `(let* ((sv-test-project-directory
           (file-name-as-directory (make-temp-file "sv-kit-test" t)))
          (sv-index-root-markers '("filelist.f"))
          (default-directory sv-test-project-directory)
          (sv-refactor-save-after-rename t))
     (dolist (name '("sub_block.sv" "top_block.sv"))
       (copy-file (expand-file-name name sv-test-fixtures)
                  (expand-file-name name sv-test-project-directory)))
     (write-region "" nil (expand-file-name "filelist.f"
                                            sv-test-project-directory))
     (sv-index-invalidate)
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (let ((file (buffer-file-name buffer)))
           (when (and file (string-prefix-p sv-test-project-directory file))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer))))
       (delete-directory sv-test-project-directory t)
       (sv-index-invalidate))))

(defun sv-test-file-contents (path)
  "Return the contents of PATH."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defmacro sv-test-answering-yes (&rest body)
  "Run BODY with every yes-or-no prompt answered yes."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
     ,@body))

(ert-deftest sv-refactor-renames-a-declaration-and-its-uses ()
  (sv-test-with-source
      "module probe;
         typedef struct packed { logic count; } rec_t;
         logic [3:0] count;
         rec_t       w_rec;
         // count in a comment
         assign x = count + 1;
         assign y = w_rec.count;
         assign z = \"count\";
         sub u_sub (.count (count));
       endmodule
"
    (search-forward "logic [3:0] count")
    (backward-char 2)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (sv-kit-rename "w_total"))
    (let ((text (buffer-string)))
      ;; The declaration and the plain use moved.
      (should (string-match-p "logic \\[3:0\\] w_total;" text))
      (should (string-match-p "assign x = w_total \\+ 1;" text))
      ;; A comment, a string, a struct member and a port name did not.
      (should (string-match-p "// count in a comment" text))
      (should (string-match-p "\"count\"" text))
      (should (string-match-p "logic count; } rec_t" text))
      (should (string-match-p "w_rec\\.count" text))
      ;; In `.count (count)' only the right-hand side is this file's signal.
      (should (string-match-p "\\.count (w_total)" text)))))

(ert-deftest sv-refactor-refuses-an-ambiguous-name ()
  (sv-test-with-source
      "module probe;
         if (X) begin : a
           localparam P = 1;
         end else begin : b
           localparam P = 2;
         end
       endmodule
"
    (search-forward "localparam P")
    (backward-char 1)
    (let ((error-text (should-error (sv-kit-rename "Q") :type 'user-error)))
      (should (string-match-p "separate scopes" (format "%s" error-text))))))

(ert-deftest sv-refactor-refuses-a-name-already-taken ()
  (sv-test-with-source
      "module probe;\n  logic a;\n  logic b;\n  assign a = b;\nendmodule\n"
    (search-forward "logic a")
    (backward-char 1)
    (should-error (sv-kit-rename "b") :type 'user-error)))

(ert-deftest sv-refactor-refuses-an-unusable-name ()
  (sv-test-with-source
      "module probe;\n  logic a;\n  assign x = a;\nendmodule\n"
    (search-forward "logic a")
    (backward-char 1)
    (should-error (sv-kit-rename "module") :type 'user-error)
    (should-error (sv-kit-rename "2bad") :type 'user-error)))

(ert-deftest sv-refactor-renames-a-port-and-its-connections ()
  (sv-test-with-temp-project
    (let ((sub (expand-file-name "sub_block.sv" sv-test-project-directory))
          (top (expand-file-name "top_block.sv" sv-test-project-directory)))
      (with-current-buffer (find-file-noselect sub)
        (goto-char (point-min))
        (search-forward "i_data")
        (backward-char 2)
        (sv-test-answering-yes (sv-kit-rename "i_payload"))
        (save-buffer))
      (should (string-match-p "i_payload" (sv-test-file-contents sub)))
      (let ((text (sv-test-file-contents top)))
        ;; The connection followed the port.
        (should (string-match-p "\\.i_payload +(i_data)" text))
        (should (string-match-p "\\.i_payload +(w_stage)" text))
        ;; top_block's own port of that name did not move.
        (should (string-match-p "input  logic \\[W-1:0\\] i_data," text))))))

(ert-deftest sv-refactor-renames-a-design-unit-across-the-project ()
  (sv-test-with-temp-project
    (let ((sub (expand-file-name "sub_block.sv" sv-test-project-directory))
          (top (expand-file-name "top_block.sv" sv-test-project-directory)))
      (with-current-buffer (find-file-noselect sub)
        (goto-char (point-min))
        (search-forward "module sub_block")
        (backward-char 2)
        (sv-test-answering-yes (sv-kit-rename "leaf_block"))
        (save-buffer))
      (should (string-match-p "module leaf_block" (sv-test-file-contents sub)))
      (let ((text (sv-test-file-contents top)))
        (should (string-match-p "leaf_block #(\\.W (W)) u_first" text))
        (should (string-match-p "leaf_block #(\\.W (W)) u_second" text))
        (should-not (string-match-p "sub_block" text))))))

(ert-deftest sv-parser-survives-a-truncated-connection-list ()
  "A buffer cut off inside an instance must not signal: it is being typed."
  (dolist (text '("module m;\n  sub u_sub (\n    .c"
                  "module m;\n  sub u_sub (.a (b), .c"
                  "module m;\n  sub u_sub (\n    ."
                  "module m;\n  sub u_sub (\n    .c ("
                  "module m;\n  sub u_sub ("))
    (should (sv-parse-string text))))

(ert-deftest sv-parser-keeps-the-last-name-of-a-truncated-group ()
  "An unterminated group has no closing bracket to drop, so nothing is lost."
  (let* ((unit (sv-test-unit "module m;\n  sub u_sub (\n    .c"))
         (inst (car (plist-get unit :items)))
         (conns (plist-get inst :connections)))
    (should (equal (plist-get inst :module) "sub"))
    (should (equal (mapcar (lambda (c) (plist-get c :name)) conns) '("c")))))

(ert-deftest sv-parser-ignores-a-connection-that-is-only-a-dot ()
  "A bare `.' is a connection being typed, not a positional one."
  (let* ((unit (sv-test-unit "module m;\n  sub u_sub (.a (b), ."))
         (inst (car (plist-get unit :items)))
         (conns (plist-get inst :connections)))
    (should (equal (mapcar (lambda (c) (plist-get c :name)) conns) '("a")))
    (should-not (cl-some (lambda (c) (plist-get c :positional)) conns))))

(ert-deftest sv-parse-unwrap-drops-only-a-bracket-that-is-there ()
  (should (equal (mapcar #'sv-token-text
                         (sv-parse-unwrap (append (sv-lex-significant (sv-lex-string "(a + b)")) nil)))
                 '("a" "+" "b")))
  (should (equal (mapcar #'sv-token-text
                         (sv-parse-unwrap (append (sv-lex-significant (sv-lex-string "(a + b")) nil)))
                 '("a" "+" "b"))))

(provide 'sv-kit-test)

;;; sv-kit-test.el ends here
