(:id "q1"
 :required ((:key "B種接地" :desc "B種接地と明示" :points 1)
            (:key "第1?7条" :desc "第17条を明示" :points 1)
            (:key "300\\s-*[／/]\\s-*6\\|6\\s-*A.*300" :desc "1秒超2秒以内なので分子300を使う" :points 1)
            (:key "50\\s-*Ω" :desc "最大接地抵抗50Ω" :points 1)
            (:key "接地抵抗値.*第.?17条.*値以下" :desc "年次点検側の確認根拠に触れる" :points 1))
 :traps ((:key "15\\s-*Ω" :desc "3秒/10A例の丸写し" :penalty 1)
         (:key "25\\s-*Ω" :desc "分子150の誤用" :penalty 1))
 :max-score 5)
