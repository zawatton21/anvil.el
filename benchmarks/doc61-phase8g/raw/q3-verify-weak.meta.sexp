(:qid "q3" :arm "verify-weak" :wall-sec 318.90421628952026 :cost-usd
      1.1734454 :errored nil :error-msg nil :claims
      ((:claim "低圧電路の絶縁抵抗基準値は1MΩ以上である" :kind number
	       :candidates ("fusion-member-0-claude") :verdict
	       unverified :evidence "")
       (:claim "低圧電路の絶縁抵抗基準値は0.1MΩ以上である" :kind
	       number :candidates ("fusion-member-1-claude") :verdict
	       unverified :evidence "")
       (:claim "低圧絶縁抵抗の根拠条文は電気設備技術基準第56条である"
	       :kind citation :candidates ("fusion-member-0-claude")
	       :verdict unverified :evidence "")
       (:claim "低圧絶縁抵抗の根拠条文は電気設備技術基準第50条である"
	       :kind citation :candidates ("fusion-member-1-claude")
	       :verdict unverified :evidence "")
       (:claim "低圧絶縁抵抗の測定電圧は直流750Vである" :kind number
	       :candidates ("fusion-member-0-claude") :verdict refuted
	       :evidence
	       "/home/madblack-21/Cowork/Notes/node/20260614220903_catchall_box.org:2349 — JIS C 1302（絶縁抵抗計の規格）に記載される標準定格測定電圧は 25V, 50V, 100V, 125V, 250V, 500V, 1000V であり、直流 750V は含まれていない。また、ローカル KB（法令 orgDB）でも低圧絶縁抵抗測定の標準電圧として 750V の記載がない。"))
      :panel haiku-pair :judge-model "haiku" :rounds 1)
