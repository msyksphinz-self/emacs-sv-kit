# sv-kit — Emacs 向け SystemVerilog パーサ / リンタ / フォーマッタ

SystemVerilog を「正規表現で頑張る」のではなく、**字句解析 → 構文解析 → 構文木**
を経由して扱う Emacs Lisp パッケージです。同じ構文木の上に、ハイライト・リンタ・
フォーマッタ・インデント・補完・定義ジャンプ・ElDoc・imenu を載せています。外部ツール
（Verilator, Verible など）も他の Verilog パッケージも不要で、Emacs 単体で完結します。

| ファイル | 役割 |
| --- | --- |
| `lisp/sv-lexer.el` | トークナイザ。コメント・空白も保持するので原文復元が可能 |
| `lisp/sv-parser.el` | 再帰下降パーサ。design unit / ポート / 宣言 / 手続き文 / インスタンスを構文木に |
| `lisp/sv-lint.el` | 構文木に対する静的チェック（30 ルール）とプラグマによる抑制 |
| `lisp/sv-format.el` | インデント再計算・空白正規化・桁揃え |
| `lisp/sv-width.el` | 定数式の畳み込みとビット幅推論 |
| `lisp/sv-index.el` | バッファ／プロジェクトのシンボル索引（ファイル単位でキャッシュ） |
| `lisp/sv-ide.el` | 補完・定義ジャンプ（xref）・ElDoc |
| `lisp/sv-refactor.el` | リネーム（構文木ベース、プロジェクト横断） |
| `lisp/sv-kit.el` | Emacs 統合（Flymake / imenu / キーバインド）と CLI |
| `lisp/sv-mode.el` | メジャーモード。シンタックステーブル・ハイライト・インデント・移動 |
| `bin/sv-kit` | コマンドライン版（CI 用） |

## インストール

```elisp
(add-to-list 'load-path "/path/to/scariv/tools/emacs-sv-kit/lisp")
(require 'sv-mode)
```

これだけで `.sv` / `.svh` / `.v` / `.vh` が `sv-mode` で開き、**ハイライト・
インデント・lint（Flymake）** が有効になります。追加設定は要りません。

既存の `verilog-mode` / `verilog-ts-mode` を使い続けたい場合は、マイナーモードの
`sv-kit-mode` だけを重ねられます（ハイライトとインデントは元のモードのものが使われ、
lint・整形・imenu・インスタンス挿入だけが追加されます）。

```elisp
(require 'sv-kit)
(add-hook 'verilog-mode-hook #'sv-kit-mode)
(add-hook 'verilog-mode-hook #'flymake-mode)
```

## ハイライト

font-lock はパーサではなく正規表現で行うので入力中でも軽量ですが（実測: 24 ファイル
27 KB を 0.13 秒で fontify）、**その場で宣言された型名だけはパーサから取得**します。
つまり自分で書いた `typedef` も組み込み型と同じように色が付きます。

色分けの対象:

- キーワード / データ型（`logic`, `wire`, `parameter`, `typedef` …）
- 数値リテラル（`8'hff`, `'0`, `1.5e-3`, `10ns`）と文字列
- コンパイラ指令とマクロ（`` `ifdef ``、`` `else `` はキーワード `else` と誤認しない）
- `module` / `interface` / `package` / `class` / `function` / `task` の名前
- 宣言された信号名・パラメータ名・ループ変数（`for (int i = 0; ...)` の `i` も）
- インスタンス（モジュール名と `u_foo` のインスタンス名を区別）
- `.i_clk (clk)` のポート名、`begin : name` / `endmodule : name` のラベル
- ファイル内で `typedef` された型名と enum リテラル

顔（face）は `sv-mode-port-face` / `sv-mode-label-face` / `sv-mode-directive-face` /
`sv-mode-instance-face` としてカスタマイズできます。色数を減らしたいときは
`font-lock-maximum-decoration` を 1 か 2 にしてください。`typedef` を書き足した直後に
色を反映させたい場合は `C-c C-t`（`sv-mode-update-user-types`、保存時には自動実行）。

## インデント

インデントはパーサと同じトークン列から計算するので、正規表現では難しいケース
（`begin` の無い `if` / `else` の連鎖、`case` の項目、複数行のポートリスト、
`generate` の中）も崩れません。`end` は必ず対応する `begin` の**行頭桁**に戻ります。

- `TAB` … その行をインデント（`sv-format-indent-line`）
- `C-M-\` … リージョンをインデント（`sv-format-indent-region`、バッファを 1 回だけ字句解析）
- `end` や `else` を打った瞬間に行が再インデントされます
  （不要なら `(setq sv-mode-electric-keywords nil)`）

インデントだけでなく空白の正規化や桁揃えまで行いたい場合は後述のフォーマッタ
（`C-c C-f`）を使ってください。

## キーバインド

`sv-mode`（および `sv-kit-mode`）有効時:

| キー | コマンド | 内容 |
| --- | --- | --- |
| `C-c C-f` | `sv-format-buffer` | バッファ全体を整形 |
| `C-c C-r` | `sv-format-region` | リージョンだけ整形 |
| `C-c C-l` | `sv-kit-lint` | 指摘を `compilation-mode` バッファに一覧（`M-g n` でジャンプ） |
| `C-c C-i` | `sv-kit-insert-instance` | プロジェクト内のモジュールからインスタンス雛形を挿入 |
| `C-c C-p` | `sv-kit-update-instance` | カーソル位置のインスタンスに未接続ポートを追加 |
| `C-c C-d` | `sv-kit-declare-missing-signals` | 代入されているのに未宣言の信号を `logic` で宣言 |
| `M-.` / `M-?` | `xref-find-definitions` / `-references` | 定義へジャンプ／参照一覧 |
| `C-M-i` | `completion-at-point` | 文脈を見た補完 |
| `C-c C-u` | `sv-kit-goto-unit` | ファイル内の design unit へジャンプ |
| `C-c C-h` | `sv-kit-hierarchy` | インスタンス階層をツリー表示 |
| `C-c C-n` | `sv-kit-rename` | カーソル位置の名前をリネーム |
| `C-c C-t` | `sv-mode-update-user-types` | `typedef` を読み直してハイライトを更新 |
| `C-M-a` / `C-M-e` | `beginning-of-defun` / `end-of-defun` | module / function 単位で移動 |

保存時に自動整形したい場合は `M-x sv-kit-format-on-save-mode`、あるいは
`(setq sv-kit-format-on-save t)`。

## 補完・ジャンプ・ElDoc

いずれもプロジェクト全体の索引（`sv-index`）に基づきます。索引はファイル単位で
更新時刻を見てキャッシュするので、2 回目以降はほぼ無コストです
（実測: 24 ファイルの初回 0.08 秒、以降 0.4 ミリ秒）。

**補完** (`C-c C-i` ではなく `C-M-i` / company / corfu から)

カーソル位置の文脈を見て候補を変えます。

- インスタンスの `.` の直後 → **そのモジュールが実際に持つポート**のうち、まだ
  接続していないものだけ。注釈に方向と型が出ます
- 信号の `.` の直後 → **その構造体のメンバ**。入れ子（`pkt.head.` → 内側の
  メンバ）と配列要素（`arr[2].`）も辿ります。型は同じファイル内の `typedef`
  でもプロジェクト内の他ファイルのものでも構いません
- `` ` `` の直後 → プロジェクト内の `` `define `` マクロ
- `$` の直後 → システムタスク
- それ以外 → 同じ module 内の信号・パラメータ・型を先頭に、続いてプロジェクトの
  モジュール名、最後にキーワード

**定義ジャンプ** (`M-.`)

信号・パラメータ・型・enum リテラル・関数・モジュール名のいずれでも、宣言箇所へ
飛びます。別ファイルのモジュールや `typedef` も対象です。`M-?` で参照一覧、
`C-M-.` で名前の絞り込み検索ができます。

**ElDoc**

カーソル下の名前の宣言を 1 行で表示します。インスタンスのポート名
（`.i_data` など）の上では**接続先モジュール側の宣言**を、構造体のメンバの上では
**その型のメンバ宣言**を表示します。

```
sub_block.i_data: input logic [W-1:0]
outer_t.head: inner_t head
logic [7:0] w_data  [var in probe]
```

## 便利なコマンド

- `C-c C-p` (`sv-kit-update-instance`) … カーソル位置のインスタンスに、モジュール
  定義にあって未接続のポートを `.port (port)` の形で追加します。定義に無いポートを
  繋いでいる場合はその名前を知らせます。何度実行しても結果は変わりません
- `C-c C-d` (`sv-kit-declare-missing-signals`) … 代入されているのに宣言されていない
  信号を探し、既存の宣言の直後に `logic` で宣言します。ざっとロジックを書いてから
  まとめて宣言する、という書き方ができます
- `C-c C-i` (`sv-kit-insert-instance`) … モジュール名を選ぶとインスタンス雛形を挿入
- `C-c C-n` (`sv-kit-rename`) … カーソル位置の名前をリネームします。**検索置換では
  なく構文木に基づく**ので、コメント・文字列・同名の構造体メンバは巻き込みません。
  対象によって範囲が変わります。

  | 対象 | 範囲 |
  | --- | --- |
  | 信号・パラメータ・型・インスタンス名 | その design unit 内の宣言と全参照 |
  | ポート | 上記に加えて、プロジェクト内の全インスタンス記述の `.port` 名（確認あり） |
  | module / interface / package | プロジェクト内の定義箇所と全インスタンス記述（確認あり） |

  特に効くのは `.sig (sig)` の扱いです。左側は**接続先モジュール**の名前、右側は
  **このファイル**の信号なので、ローカル信号 `sig` のリネームでは右側だけが変わります。

  ```systemverilog
  // logic count; を w_total にリネームした結果
  logic [3:0] w_total;                       // 宣言
  assign x = w_total + 1;                    // 参照
  assign y = w_rec.count;                    // 構造体メンバは別物なので不変
  assign z = "count";                        // 文字列は不変
  sub u_sub (.count (w_total));              // ポート名は不変、式だけ変わる
  ```

  同じ名前が 1 つの unit 内の**複数スコープ**で宣言されている場合（別の generate 枝の
  `localparam` など）は、名前だけではどちらか決められないので中止して行番号を示します。
  新しい名前が既に使われている場合も中止します。ポート名を変えた後は、崩れた
  `.port (...)` の桁揃えを自動で直します（`sv-refactor-realign-after-rename`）。
  他ファイルの変更は既定では保存せず modified のまま残すので、差分を確認してから
  保存できます（`sv-refactor-save-after-rename` で変更可）。

- `C-c C-h` (`sv-kit-hierarchy`) … モジュールのインスタンス階層をツリー表示します。
  モジュール名はボタンになっていて、押すと定義箇所へ飛べます。再帰インスタンスや
  プロジェクト外のモジュールも明示されます

```
axi_cdc
  i_axi_cdc_src : axi_cdc_src
    i_cdc_fifo_gray_src_aw : cdc_fifo_gray_src
      i_sync : sync
      i_wptr_b2g : binary_to_gray
```

`M-x hs-minor-mode` で `begin`/`end`、`case`/`endcase`、`module`/`endmodule` の
折りたたみもできます（`hideshow` 用の移動関数を登録済みです）。

## リンタ

`sv-mode` では Flymake が既定で有効なので、編集しながら指摘が表示されます
（切るには `(setq sv-mode-enable-flymake nil)`）。`C-c C-l` で一覧表示、
`M-x sv-kit-lint-project` でプロジェクト全体を検査できます。

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
- `width-truncation` — 代入で上位ビットが落ちる（幅推論に基づく）
- `constant-overflow` — 定数が代入先の幅に収まらない
- `port-width-mismatch` — インスタンス接続の幅がポート幅と違う（既定では無効）
- `multiple-drivers` — 同じ信号を複数の always / assign が駆動（部分選択は除外）
- `assignment-to-input` — 入力ポートへの代入
- `duplicate-case-label` — 同じ case ラベルが 2 回現れる（後者は到達不能）
- `mixed-assignment-style` — 1 つの `always` 内で `=` と `<=` が混在
- `module-filename-mismatch`, `unlabeled-generate-block`, `duplicate-declaration`
- `line-too-long`, `trailing-whitespace`, `tab-indentation`

### ビット幅の検査

`sv-width.el` が**定数式を畳み込み**、そこから**式のビット幅を推論**します。
パラメータは相互参照も解決し（`localparam PTR = $clog2(D)` のような連鎖も可）、
`$clog2` / `$bits` / キャスト / packed struct / 多次元も扱います。

```systemverilog
logic [7:0] i_a;
logic [3:0] o_q;
assign o_q = i_a;        // width-truncation: 4 ビットに 8 ビットを代入
assign o_q = 5'd20;      // constant-overflow: 20 は 4 ビットに入らない
```

誤検知を避けるため、**分からないものは黙る**方針で作ってあります。

- 幅が確定しない式（型パラメータ、未解決の型、評価できないパラメータ）は
  無検査。「幅指定はあるが評価できない」場合に 1 ビットと決めつけません
- サイズ無し即値（`42`, `'0`）は文脈依存なので幅比較の対象外。ただし
  「定数の値が入らない」場合だけは指摘します
- **`generate` の条件を評価**し、実際に展開される枝だけを検査します
  （`if (Width == 1)` の枝を Width=8 の前提で見る、といった誤りをしません）
- 配列要素 `x[i]` を 1 ビットと決めつけません（構造体配列なら要素幅）

`port-width-mismatch` はモジュールを跨ぐ検査で、パラメータ上書きを評価した上で
比較しますが、型パラメータを多用する設計では精度が落ちるため既定では無効です。
有効にするには `(setq sv-lint-disabled-rules (delq 'port-width-mismatch sv-lint-disabled-rules))`。

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
$ make check   # byte-compile（警告はエラー扱い）+ ERT 119 テスト
```

パーサは例外を投げません。解釈できない構文は次の `;` や `end` まで読み飛ばして
局所的に劣化するだけなので、マクロ多用のコードや書きかけのファイルでも
リンタ・フォーマッタは動き続けます。

## 検証

実世界の SystemVerilog で継続的に検証しています。PULP の
[axi](https://github.com/pulp-platform/axi) と
[common_cells](https://github.com/pulp-platform/common_cells)
の **225 ファイル・314 design unit**（interface / package / 構造体 / アサーション /
`` `ifdef `` / マクロを多用するコード）に対して:

- パースエラー **0 件**
- フォーマッタはトークン列を 1 つも変えず（意味不変）、冪等性も **全ファイルで成立**
- lint の指摘は 1939 → 979 件まで精査。`duplicate-declaration` の誤検知は
  スコープ／`` `ifdef `` 分岐を考慮して **0 件** になりました
- 幅検査は 225 ファイルで 9 件（いずれも実在の切り詰め）。SCARIV の RTL では 0 件

この検証で見つかったパーサの不具合（`` `endif `` 直後の `endmodule` の取りこぼし、
代入パターン `'{...}` の括弧不整合、マクロ文が次の文を飲み込む問題など）は
すべて修正し、回帰テストを追加してあります。

```console
$ git submodule update --init --depth 1   # 検証用コーパスを取得
$ ./bin/sv-kit lint $(find vendor -name '*.sv')
```
