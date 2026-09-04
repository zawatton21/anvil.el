;;; visible-test.el --- visible tests for hard2 -*- lexical-binding: t; -*-

(require 'ert)
(require 'rq-data)
(require 'rq-parse)
(require 'rq-predicate)
(require 'rq-eval)
(require 'rq-format)

(defconst visible-rq-env
  '((tickets .
             (((id . "t1") (owner . "u1") (status . "open"))
              ((id . "t2") (owner . "u2") (status . "open"))
              ((id . "t3") (owner . "u3") (status . "closed"))))
    (users .
           (((id . "u1") (team . "ops"))
            ((id . "u2") (team . "ops"))
            ((id . "u3") (team . "sales")))))
  "Small visible test environment.")

(ert-deftest visible-rq-where-basic ()
  (should (= (length (rq-run visible-rq-env "from tickets | where status=open")) 2)))

(ert-deftest visible-rq-parse-join-and-group-by ()
  (should (equal (rq-parse-query "from tickets | join users on owner=id | group-by users.team")
                 '((from tickets)
                   (join users "owner" "id")
                   (group-by "users.team")))))

(ert-deftest visible-rq-join-where-group-happy-path ()
  "The visible suite checks a non-empty grouped path with distinct counts."
  (should (equal (rq-run-lines visible-rq-env
                               "from tickets | join users on owner=id | where status=open | group-by users.team")
                 '("ops => 2"))))

(ert-deftest visible-rq-qualified-field-after-join ()
  (should (equal (rq-run-lines visible-rq-env
                               "from tickets | join users on owner=id | where users.team=sales")
                 '("id=t3 owner=u3 status=closed users.id=u3 users.team=sales"))))

(provide 'visible-test)
;;; visible-test.el ends here
