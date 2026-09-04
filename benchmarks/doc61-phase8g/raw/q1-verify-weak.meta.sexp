(:qid "q1" :arm "verify-weak" :wall-sec 375.66206765174866 :cost-usd
      0.9584184 :errored nil :error-msg nil :claims
      ((:claim "遮断時間1.2秒に対応する許容接触電圧は300Vである" :kind
	       fact :candidates ("fusion-member-0-claude") :verdict
	       unverified :evidence "")
       (:claim "遮断時間1.2秒に対応する許容接触電圧は100Vである" :kind
	       fact :candidates ("fusion-member-1-claude") :verdict
	       unverified :evidence "")
       (:claim "B種接地抵抗の最大値は50Ωである" :kind number
	       :candidates ("fusion-member-0-claude") :verdict
	       unverified :evidence "")
       (:claim "B種接地抵抗の最大値は16.7Ωである" :kind number
	       :candidates ("fusion-member-1-claude") :verdict
	       unverified :evidence ""))
      :panel haiku-pair :judge-model "haiku" :rounds 1)
