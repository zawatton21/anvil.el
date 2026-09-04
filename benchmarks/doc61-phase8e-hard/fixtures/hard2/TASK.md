# タスク: rq クエリ DSL に join / group-by を追加する

`rq-*.el` は alist レコードに対する小さなクエリパイプラインです。現在は
`from` と `where` しか扱えません。以下を追加してください。

1. `join DATASET on LEFT=RIGHT`
   - inner join とします。
   - 左側のフィールドは従来通りそのまま参照でき、右側のフィールドは
     `users.team` のような修飾名で参照できるようにしてください。
2. `group-by FIELD`
   - その時点の行を FIELD ごとに集約し、件数付きの grouped result を返すこと。
   - grouped result の整形は `rq-format.el` の責務です。

補足:

- 仕様は `README.org`、docstring、`tests/visible-test.el` のコメントに分散して
  います。
- 既存の predicate 合成は「素の row だけ」を前提にしており、join 後の
  qualified field と相性が良くありません。必要なら小さく内部 API を整理
  してください。
- group-by 導入後は「0件になった grouped result」も自然に起こります。
  そのケースで落ちないことが必要です。

スコープ外:

- outer join
- 比較演算子の追加（=`=` だけでよい）
- 集計関数の種類追加（件数だけでよい）

完了条件: `make check` が全てパスすること。
