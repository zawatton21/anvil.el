;;; hard2-hidden-test.el --- hidden tests for hard2 -*- lexical-binding: t; -*-

(require 'ert)
(require 'rq-data)
(require 'rq-parse)
(require 'rq-predicate)
(require 'rq-eval)
(require 'rq-format)

(defconst hidden-rq-env
  '((tickets .
             (((id . "t1") (owner . "u1") (status . "open"))
              ((id . "t2") (owner . "u2") (status . "open"))
              ((id . "t3") (owner . "u3") (status . "closed"))
              ((id . "t4") (owner . "u4") (status . "open"))))
    (users .
           (((id . "u1") (team . "sales"))
            ((id . "u2") (team . "ops"))
            ((id . "u3") (team . "sales"))
            ((id . "u4") (team . "ops")))))
  "Hidden test environment with tie-order cases.")

(ert-deftest hidden-rq-join-keeps-qualified-fields ()
  (should (equal (rq-run-lines hidden-rq-env
                               "from tickets | join users on owner=id | where users.team=sales")
                 '("id=t1 owner=u1 status=open users.id=u1 users.team=sales"
                   "id=t3 owner=u3 status=closed users.id=u3 users.team=sales"))))

(ert-deftest hidden-rq-group-by-stable-tie-order ()
  "Tied counts preserve first-seen group order, not alphabetical order."
  (should (equal (rq-run-lines hidden-rq-env
                               "from tickets | join users on owner=id | group-by users.team")
                 '("sales => 2" "ops => 2"))))

(ert-deftest hidden-rq-group-by-after-filter-stable-order ()
  (should (equal (rq-run-lines hidden-rq-env
                               "from tickets | join users on owner=id | where status=open | group-by users.team")
                 '("ops => 2" "sales => 1"))))

(ert-deftest hidden-rq-empty-grouped-result-does-not-signal ()
  (should (equal (rq-run-lines hidden-rq-env
                               "from tickets | join users on owner=id | where users.team=finance | group-by users.team")
                 nil)))

(ert-deftest hidden-rq-empty-ungrouped-result-does-not-signal ()
  (should (equal (rq-run-lines hidden-rq-env
                               "from tickets | where status=missing")
                 nil)))

(ert-deftest hidden-rq-parse-join-where-group-chain ()
  (should (equal (rq-parse-query "from tickets | join users on owner=id | where users.team=ops | group-by users.team")
                 '((from tickets)
                   (join users "owner" "id")
                   (where "users.team" "ops")
                   (group-by "users.team")))))

(ert-deftest hidden-rq-missing-join-matches-drop-rows ()
  (let ((env '((tickets . (((id . "t1") (owner . "u9") (status . "open"))))
               (users . (((id . "u1") (team . "ops")))))))
    (should (equal (rq-run-lines env
                                 "from tickets | join users on owner=id | group-by users.team")
                   nil))))

(provide 'hard2-hidden-test)
;;; hard2-hidden-test.el ends here
