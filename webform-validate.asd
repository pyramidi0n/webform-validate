(defsystem "webform-validate"
  :version "1.0.0"
  :license "BSD-2"
  :description "A validation library for web form input."
  :author "Stephen Youts"
  :depends-on ("split-sequence"
               "trivial-us-ascii"
               "abnf-match"
               "email-parse"
               "uri-parse")
  :components
  ((:static-file "LICENSE")
   (:static-file "README.md")
   (:module "src"
    :components ((:file "webform-validate"))))
  :in-order-to ((test-op (test-op "webform-validate/tests"))))

(defsystem "webform-validate/tests"
  :author "Stephen Youts"
  :license "BSD-2"
  :description "Tests for webform-validate."
  :depends-on ("webform-validate")
  :components ((:module "tests"
                :components
                ((:file "webform-validate-tests"))))
  :perform (test-op (op c) (symbol-call :webform-validate.tests :test)))
