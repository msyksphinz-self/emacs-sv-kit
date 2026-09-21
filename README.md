# sv-kit — Emacs 向け SystemVerilog パーサ / リンタ / フォーマッタ

SystemVerilog を「正規表現で頑張る」のではなく、**字句解析 → 構文解析 → 構文木**
を経由して扱う Emacs Lisp パッケージです。同じ構文木の上に、リンタ・フォーマッタ・
imenu・インスタンス雛形生成を載せています。外部ツール（Verilator, Verible など）の
インストールは不要で、Emacs 単体で完結します。

| ファイル | 役割 |
| --- | --- |
| `lisp/sv-lexer.el` | トークナイザ。コメント・空白も保持するので原文復元が可能 |
| `lisp/sv-parser.el` | 再帰下降パーサ。design unit / ポート / 宣言 / 手続き文 / インスタンスを構文木に |
| `lisp/sv-lint.el` | 構文木に対する静的チェック（23 ルール）とプラグマによる抑制 |
| `lisp/sv-format.el` | インデント再計算・空白正規化・桁揃え |
| `lisp/sv-kit.el` | Emacs 統合（Flymake / imenu / キーバインド）と CLI |
| `bin/sv-kit` | コマンドライン版（CI 用） |

## インストール

```elisp
(add-to-list 'load-path "/path/to/scariv/tools/emacs-sv-kit/lisp")
(require 'sv-kit)
(add-hook 'verilog-mode-hook #'sv-kit-mode)   ; verilog-ts-mode でも可
(add-hook 'verilog-mode-hook #'flymake-mode)  ; 保存せずに指摘を表示したい場合
```

`sv-kit-mode` はマイナーモードなので、既存の `verilog-mode` / `verilog-ts-mode` の
フォントロックや設定はそのまま使えます。

## キーバインド

`sv-kit-mode` 有効時:

| キー | コマンド | 内容 |
| --- | --- | --- |
| `C-c C-f` | `sv-format-buffer` | バッファ全体を整形 |
| `C-c C-r` | `sv-format-region` | リージョンだけ整形 |
| `C-c C-l` | `sv-kit-lint` | 指摘を `compilation-mode` バッファに一覧（`M-g n` でジャンプ） |
| `C-c C-i` | `sv-kit-insert-instance` | プロジェクト内のモジュールからインスタンス雛形を挿入 |
| `C-c C-u` | `sv-kit-goto-unit` | ファイル内の design unit へジャンプ |

`TAB`（`sv-format-indent-line`）もパーサ由来のインデントになります。無効化するには
`(setq sv-kit-use-indent-function nil)`。保存時に自動整形したい場合は
`M-x sv-kit-format-on-save-mode`、あるいは `(setq sv-kit-format-on-save t)`。

## リンタ

`always_ff` 内のブロッキング代入、`always_comb` から生まれるラッチ、default の無い
case、未宣言 / 未使用の信号、駆動されない出力、位置指定のポート接続などを検出します。

```console
$ bin/sv-kit lint common/rtl/bit/bit_cnt.sv
common/rtl/bit/bit_cnt.sv:10:1: info: Generate block has no label; name it `begin : name'. [unlabeled-generate-block]
common/rtl/bit/bit_cnt.sv:14:1: warning: `lo' is declared but never used. [unused-declaration]
```

ルール一覧は `bin/sv-kit lint --list-rules`、既定の severity は
`error` / `warning` / `info` の 3 段です。主なもの:

- `blocking-in-always-ff` — `always_ff` 内の `=`（ブロック内の自動変数は除外）
- `nonblocking-in-always-comb` — 組合せ回路の `<=`
- `implicit-latch` — 全経路で代入されない信号（`if` に `else` が無い等）
- `case-without-default` — `unique` / `priority` が付いていない default 無し case
- `incomplete-sensitivity` — `always @(a)` の感度リスト漏れ
- `undeclared-identifier` / `unused-declaration` / `unused-parameter`
- `undriven-output` — 一度も駆動されない output ポート
- `instance-unknown-port` / `instance-missing-port` — 同一プロジェクト内の
  モジュール定義と突き合わせたポート名の検査
- `positional-port-connection` / `unconnected-port`
- `module-filename-mismatch`, `unlabeled-generate-block`, `duplicate-declaration`
- `line-too-long`, `trailing-whitespace`, `tab-indentation`

### 指摘の抑制

```systemverilog
logic unused;                                  // sv-lint: disable=unused-declaration
// sv-lint: disable-next-line=case-without-default
case (sel) ...
// sv-lint: disable-file=line-too-long
/* verilator lint_off CASEINCOMPLETE */        // Verilator のプラグマも尊重する
```

既存コードの `verilator lint_off` を再利用できるよう、Verilator の警告コードを
対応ルールに読み替えます（`sv-lint-verilator-alias` で変更可）。

設定例:

```elisp
(setq sv-lint-max-line-length 120)
(setq sv-lint-disabled-rules '(line-too-long))
;; ポート名の命名規則を強制する（既定では無効）
(setq sv-lint-disabled-rules (delq 'port-naming sv-lint-disabled-rules))
(setq sv-lint-port-prefixes '((input . "\\`i_") (output . "\\`o_")))
```

## フォーマッタ

**行の分割・結合は一切しません。** 行の折り返し方は書いた人の意図として残し、

1. トークン列から各行のインデントを計算し直す
2. 連続する空白を 1 個に詰め、行末の空白とタブを除去する
3. 桁を揃える（ポートの型・宣言名・case のコロン・`=`・`.port (` ・行末コメント）

だけを行います。結果として **冪等**（2 回かけても変わらない）で、かつ
**トークン列が変化しない**（= 意味が変わらない）ことをテストで保証しています。

```systemverilog
// before
always_comb begin
case (r_a)
0 : q = 1;
default : q = 0;
endcase
end
sub u_sub (
.a (clk),
.bbb (q));

// after
always_comb begin
  case (r_a)
    0       : q = 1;
    default : q = 0;
  endcase
end
sub u_sub (
  .a   (clk),
  .bbb (q));
```

主な設定:

```elisp
(setq sv-format-indent-offset 2)        ; 1 段のインデント幅
(setq sv-format-continuation-offset 4)  ; 継続行の追加インデント
(setq sv-format-indent-unit-body nil)   ; module 直下を字下げしない（本リポジトリの既存スタイル）
(setq sv-format-align '(decl-name assign-op))  ; 揃える対象を絞る。nil で桁揃えなし
(setq sv-format-directive-column 'code) ; `ifdef を 0 桁固定にせずコードとして扱う
```

## コマンドライン / CI

```console
$ bin/sv-kit lint  --strict  $(find rtl -name '*.sv')   # 指摘があれば exit 1
$ bin/sv-kit format --check  rtl/foo.sv                 # 未整形なら exit 1
$ bin/sv-kit format --write  rtl/foo.sv                 # その場で整形
$ bin/sv-kit format --diff   rtl/foo.sv                 # 差分を表示
$ bin/sv-kit parse           rtl/foo.sv                 # ポート一覧を表示
```

`make lint` / `make format-check` / `make test` も用意しています
（`make help` で一覧）。

## 開発

```console
$ make check   # byte-compile（警告はエラー扱い）+ ERT 59 テスト
```

パーサは例外を投げません。解釈できない構文は次の `;` や `end` まで読み飛ばして
局所的に劣化するだけなので、マクロ多用のコードや書きかけのファイルでも
リンタ・フォーマッタは動き続けます。
