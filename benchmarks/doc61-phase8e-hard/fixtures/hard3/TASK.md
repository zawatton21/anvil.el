# タスク: tt テーブル描画に alignment と CJK 幅対応を追加する

`tt-*.el` はプレーンテキスト表を描画する小さなライブラリです。現状は
左寄せ固定で、幅計算も単純な文字数ベースです。以下を実装してください。

1. 列 spec の `:align` を解釈し、`left` / `right` / `center` をサポートすること。
2. 日本語など全角文字を含むセルでも列幅が崩れないよう、repo の幅関数の仕様に
   従って表示幅を計算すること。
3. header と body の両方で同じ padding/alignment ルールを使うこと。

注意:

- 仕様は `README.org`、docstring、`tests/visible-test.el` コメントに分散して
  います。
- 幅計算を直すと、今まで見えていなかった罫線長のズレが表面化します。
  `tt-draw.el` も確認してください。
- その場しのぎで header/body 別々に寄せ処理を書くより、小さな helper に寄せた
  方が安全です。

スコープ外:

- 複数行セル
- ANSI color
- Unicode box drawing characters への変更

完了条件: `make check` が全てパスすること。
