(:qid "q2" :arm "verify-weak" :wall-sec 388.8777024745941 :cost-usd
      0.9890110999999999 :errored nil :error-msg nil :claims
      ((:claim
	"公称 6.6kV CV ケーブルの直流絶縁耐力試験の試験電圧は直流 20,700V である"
	:kind number :candidates ("fusion-member-0-claude") :verdict
	confirmed :evidence
	"/home/madblack-21/Cowork/Notes/node/20250520180536-絶縁耐力試験.org:236 — 証拠 2・3 で「直流耐電圧試験（20,700V）」および「直流耐圧 20700V」が明記されており、特に証拠 3 の戸上電機取説は実測装置メーカーによる実績値として 20,700V を確認している。LA 付きでない条件下での試験電圧として複数の箇所で同じ値が記載されている。")
       (:claim
	"最大使用電圧は公称値 6.6kV に 1.15/1.1 を乗じた 6.9kV である"
	:kind number :candidates ("fusion-member-0-claude") :verdict
	unverified :evidence "")
       (:claim "交流試験電圧から直流試験電圧への換算係数は 2 倍である"
	       :kind number :candidates ("fusion-member-0-claude")
	       :verdict unverified :evidence "")
       (:claim
	"LA 内蔵 PAS 接続時は試験前に切離し試験後に残留電荷を放電する"
	:kind fact :candidates ("fusion-member-0-claude") :verdict
	unverified :evidence "")
       (:claim
	"高圧ケーブルの対地静電容量が大きいため直流試験が採用される"
	:kind fact :candidates ("fusion-member-0-claude") :verdict
	refuted :evidence
	"/home/madblack-21/Cowork/Notes/node/20250518222708-絶縁抵抗測定.org:1277 — 対地静電容量の存在は示唆されるが、「直流試験採用の理由がこれである」という直接的な因果関係を示す技術文献根拠が提供証拠に存在しない。JEC等の電気技術基準における明示的な採択理由の記載確認が必要。"))
      :panel haiku-pair :judge-model "haiku" :rounds 1)
