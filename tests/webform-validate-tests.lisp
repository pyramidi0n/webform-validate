(in-package :cl-user)
(defpackage webform-validate.tests
  (:use
   :cl
   :webform-validate)
  (:export :test))
(in-package :webform-validate.tests)

;; ------------------------------------------------------------------------------

(defun html-checkbox-test ()
  (assert (string= (html-checkbox "") ""))
  (assert (string= (html-checkbox "on") "on"))
  (assert (string= (html-checkbox "foobar") "foobar"))

  (assert (eq (html-checkbox "" :required t) nil))
  (assert (string= (html-checkbox "on" :required t) "on"))
  (assert (string= (html-checkbox "foobar" :required t) "foobar"))
  t)

(defun html-date-test ()
  (macrolet ((html-date= (form str yyyy* mm dd)
               `(multiple-value-bind (f-str f-yyyy* f-mm f-dd)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,yyyy* f-yyyy*)
                       (string= ,mm f-mm)
                       (string= ,dd f-dd)))))
    (assert (string= (html-date "") ""))
    (assert (eq (html-date "foobar") nil))

    (assert (eq (html-date "yyyy-mm-dd") nil))
    (assert (eq (html-date "yyyy-mm") nil))
    (assert (eq (html-date "yyyy") nil))

    (assert (eq (html-date "0000-00-00") nil))
    (assert (eq (html-date "9999-00-00") nil))
    (assert (eq (html-date "9999-01-00") nil))
    (assert (eq (html-date "9999-00-01") nil))

    (assert (eq (html-date "9999-01-32") nil))
    (assert (eq (html-date "9999-02-29") nil))
    (assert (eq (html-date "9999-03-32") nil))
    (assert (eq (html-date "9999-04-31") nil))
    (assert (eq (html-date "9999-05-32") nil))
    (assert (eq (html-date "9999-06-31") nil))
    (assert (eq (html-date "9999-07-32") nil))
    (assert (eq (html-date "9999-08-32") nil))
    (assert (eq (html-date "9999-09-31") nil))
    (assert (eq (html-date "9999-10-32") nil))
    (assert (eq (html-date "9999-11-31") nil))
    (assert (eq (html-date "9999-12-32") nil))

    (assert (html-date= (html-date "9999-01-31") "9999-01-31" "9999" "01" "31"))
    (assert (html-date= (html-date "9999-02-28") "9999-02-28" "9999" "02" "28"))
    (assert (html-date= (html-date "9999-03-31") "9999-03-31" "9999" "03" "31"))
    (assert (html-date= (html-date "9999-04-30") "9999-04-30" "9999" "04" "30"))
    (assert (html-date= (html-date "9999-05-31") "9999-05-31" "9999" "05" "31"))
    (assert (html-date= (html-date "9999-06-30") "9999-06-30" "9999" "06" "30"))
    (assert (html-date= (html-date "9999-07-31") "9999-07-31" "9999" "07" "31"))
    (assert (html-date= (html-date "9999-08-31") "9999-08-31" "9999" "08" "31"))
    (assert (html-date= (html-date "9999-09-30") "9999-09-30" "9999" "09" "30"))
    (assert (html-date= (html-date "9999-10-31") "9999-10-31" "9999" "10" "31"))
    (assert (html-date= (html-date "9999-11-30") "9999-11-30" "9999" "11" "30"))
    (assert (html-date= (html-date "9999-12-31") "9999-12-31" "9999" "12" "31"))

    ;; leap year
    (assert (eq (html-date "2024-01-32") nil))
    (assert (eq (html-date "2024-02-30") nil))
    (assert (eq (html-date "2024-03-32") nil))
    (assert (eq (html-date "2024-04-31") nil))
    (assert (eq (html-date "2024-05-32") nil))
    (assert (eq (html-date "2024-06-31") nil))
    (assert (eq (html-date "2024-07-32") nil))
    (assert (eq (html-date "2024-08-32") nil))
    (assert (eq (html-date "2024-09-31") nil))
    (assert (eq (html-date "2024-10-32") nil))
    (assert (eq (html-date "2024-11-31") nil))
    (assert (eq (html-date "2024-12-32") nil))

    (assert (html-date= (html-date "2024-01-31") "2024-01-31" "2024" "01" "31"))
    (assert (html-date= (html-date "2024-02-29") "2024-02-29" "2024" "02" "29"))
    (assert (html-date= (html-date "2024-03-31") "2024-03-31" "2024" "03" "31"))
    (assert (html-date= (html-date "2024-04-30") "2024-04-30" "2024" "04" "30"))
    (assert (html-date= (html-date "2024-05-31") "2024-05-31" "2024" "05" "31"))
    (assert (html-date= (html-date "2024-06-30") "2024-06-30" "2024" "06" "30"))
    (assert (html-date= (html-date "2024-07-31") "2024-07-31" "2024" "07" "31"))
    (assert (html-date= (html-date "2024-08-31") "2024-08-31" "2024" "08" "31"))
    (assert (html-date= (html-date "2024-09-30") "2024-09-30" "2024" "09" "30"))
    (assert (html-date= (html-date "2024-10-31") "2024-10-31" "2024" "10" "31"))
    (assert (html-date= (html-date "2024-11-30") "2024-11-30" "2024" "11" "30"))
    (assert (html-date= (html-date "2024-12-31") "2024-12-31" "2024" "12" "31"))

    (assert (eq (html-date "-2024-01-31") nil))
    (assert (eq (html-date "2024-01-31-") nil))
    (assert (eq (html-date "2024--01-31") nil))
    (assert (eq (html-date "2024-01--31") nil))
    (assert (eq (html-date "-2024--01-31") nil))
    (assert (eq (html-date "-2024-01--31") nil))
    (assert (eq (html-date "-2024-01-31-") nil))
    (assert (eq (html-date "-2024--01-31-") nil))
    (assert (eq (html-date "-2024-01--31-") nil))
    (assert (eq (html-date "-2024--01--31-") nil))

    (assert (html-date= (html-date "2024-01-31" :min "2023-01-31") "2024-01-31" "2024" "01" "31"))
    (assert (html-date= (html-date "2024-01-31" :min "2024-01-31") "2024-01-31" "2024" "01" "31"))
    (assert (html-date= (html-date "2024-01-31" :min "2024-01-30") "2024-01-31" "2024" "01" "31"))
    (assert (eq (html-date "2024-01-30" :min "2025-01-30") nil))
    (assert (eq (html-date "2024-01-30" :min "2024-02-29") nil))
    (assert (eq (html-date "2024-01-30" :min "2024-01-31") nil))

    (assert (eq (html-date "" :required t) nil))
    (assert (eq (html-date "foobar" :required t) nil))
    (assert (html-date= (html-date "9999-01-31" :required t) "9999-01-31" "9999" "01" "31"))
    t))

(defun html-file-test ()
  (assert (string= (html-file "") ""))
  (assert (string= (html-file "foobar") "foobar"))
  (assert (string= (html-file "foobar.jpg") "foobar.jpg"))

  (assert (eq (html-file "" :required t) nil))
  (assert (string= (html-file "foobar" :required t) "foobar"))
  (assert (string= (html-file "foobar.jpg" :required t) "foobar.jpg"))
  t)

(defun html-hidden-test ()
  (assert (string= (html-hidden "") ""))
  (assert (string= (html-hidden "foobar") "foobar"))
  t)

(defun html-password-test ()
  (flet ((alpha-only (password)
           (every #'alpha-char-p password)))
    (assert (string= (html-password "") ""))
    (assert (string= (html-password "foobar") "foobar"))

    (assert (eq (html-password "" :required t) nil))
    (assert (string= (html-password "foobar" :required t) "foobar"))

    (assert (string= (html-password "" :pattern-matcher #'alpha-only) ""))
    (assert (eq (html-password "12323" :pattern-matcher #'alpha-only) nil))
    (assert (string= (html-password "foobar" :pattern-matcher #'alpha-only) "foobar"))

    (assert (eq (html-password "a" :min-length 2) nil))
    (assert (eq (html-password "foobar" :max-length 1) nil))
    (assert (string= (html-password "foobar" :min-length 1 :max-length 10) "foobar"))

    (assert (eq (html-password "" :required t :min-length 1) nil))
    (assert (eq (html-password "" :required t :pattern-matcher #'alpha-only) nil))
    t))

(defun html-radio-test ()
  (assert (string= (html-radio "") ""))
  (assert (string= (html-radio "foobar") "foobar"))

  (assert (eq (html-radio "" :required t) nil))
  (assert (string= (html-radio "foobar" :required t) "foobar"))
  t)

(defun html-text-test ()
  (flet ((alpha-only (text)
           (every #'alpha-char-p text)))
    (assert (string= (html-text "") ""))
    (assert (string= (html-text "foobar") "foobar"))

    (assert (eq (html-text "" :required t) nil))
    (assert (string= (html-text "foobar" :required t) "foobar"))

    (assert (string= (html-text "" :pattern-matcher #'alpha-only) ""))
    (assert (eq (html-text "12323" :pattern-matcher #'alpha-only) nil))
    (assert (string= (html-text "foobar" :pattern-matcher #'alpha-only) "foobar"))

    (assert (eq (html-text "a" :min-length 2) nil))
    (assert (eq (html-text "foobar" :max-length 1) nil))
    (assert (string= (html-text "foobar" :min-length 1 :max-length 10) "foobar"))

    (assert (eq (html-text "" :required t :min-length 1) nil))
    (assert (eq (html-text "" :required t :pattern-matcher #'alpha-only) nil))
    t))

;; ------------------------------------------------------------------------------

(defun html5-email-test ()
  (macrolet ((html5-email= (form str local-part domain)
               `(multiple-value-bind (f-str f-local-part f-domain)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,local-part f-local-part)
                       (string= ,domain f-domain)))))
    (flet ((exact-match (email)
             (string= email "simple@example.com")))
      (assert (string= (html5-email "") ""))
      (assert (html5-email= (html5-email "simple@example.com")
                            "simple@example.com"
                            "simple"
                            "example.com"))

      (assert (eq (html5-email "" :required t) nil))
      (assert (html5-email= (html5-email "simple@example.com" :required t)
                            "simple@example.com"
                            "simple"
                            "example.com"))

      (assert (string= (html5-email "" :min-length 1) ""))
      (assert (eq (html5-email "a" :min-length 2) nil))
      (assert (html5-email= (html5-email "simple@example.com" :min-length 1)
                            "simple@example.com"
                            "simple"
                            "example.com"))

      (assert (string= (html5-email "" :max-length 1) ""))
      (assert (eq (html5-email "simple@example.com" :max-length 1) nil))

      (assert (eq (html5-email "simp@example.com" :pattern-matcher #'exact-match) nil))
      (assert (html5-email= (html5-email "simple@example.com" :pattern-matcher #'exact-match)
                            "simple@example.com"
                            "simple"
                            "example.com"))

      (assert (eq (html5-email "simple@example.com"
                               :min-length 20
                               :pattern-matcher #'exact-match)
                  nil))
      (assert (eq (html5-email "simple@example.com"
                               :max-length 3
                               :pattern-matcher #'exact-match)
                  nil))
      (assert (eq (html5-email ""
                               :required t
                               :max-length 3
                               :pattern-matcher #'exact-match)
                  nil))

      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com" :multiple t)
                      '(("simple@example.com" "simple" "example.com")
                        ("foo@bar.com" "foo" "bar.com"))))
      (assert (equalp (webform-validate:html5-email "  simple@example.com, foo@bar.com" :multiple t)
                      '(("simple@example.com" "simple" "example.com")
                        ("foo@bar.com" "foo" "bar.com"))))
      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com  " :multiple t)
                      '(("simple@example.com" "simple" "example.com")
                        ("foo@bar.com" "foo" "bar.com"))))
      (assert (equalp (webform-validate:html5-email " simple@example.com, foo@bar.com " :multiple t)
                      '(("simple@example.com" "simple" "example.com")
                        ("foo@bar.com" "foo" "bar.com"))))
      (assert (equalp (webform-validate:html5-email "simple@example.com,foo@bar.com" :multiple t)
                      '(("simple@example.com" "simple" "example.com")
                        ("foo@bar.com" "foo" "bar.com"))))
      (assert (equalp (webform-validate:html5-email "simple@example.com,foo@bar.com, woo@baz.org" :multiple t)
                      '(("simple@example.com" "simple" "example.com")
                        ("foo@bar.com" "foo" "bar.com")
                        ("woo@baz.org" "woo" "baz.org"))))

      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com"
                                                    :multiple t
                                                    :min-length (length "simple@example.com"))
                      '(("simple@example.com" "simple" "example.com")
                        nil)))
      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com"
                                                    :multiple t
                                                    :max-length (length "foo@bar.com"))
                      '(nil
                        ("foo@bar.com" "foo" "bar.com"))))

      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com"
                                                    :multiple t
                                                    :pattern-matcher #'exact-match)
                      '(("simple@example.com" "simple" "example.com")
                        nil)))
      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com"
                                                    :multiple t
                                                    :min-length (length "simple@example.com")
                                                    :max-length (length "simple@example.com")
                                                    :pattern-matcher #'exact-match)
                      '(("simple@example.com" "simple" "example.com")
                        nil)))
      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com"
                                                    :multiple t
                                                    :max-length (length "foo@bar.com")
                                                    :pattern-matcher #'exact-match)
                      '(nil nil)))
      (assert (equalp (webform-validate:html5-email "simple@example.com, foo@bar.com"
                                                    :multiple t
                                                    :min-length (1+ (length "simple@example.com"))
                                                    :pattern-matcher #'exact-match)
                      '(nil nil)))
      t)))

(defun html5-search-test ()
  (flet ((alpha-only (text)
           (every #'alpha-char-p text)))
    (assert (string= (html5-search "") ""))
    (assert (string= (html5-search "foobar") "foobar"))

    (assert (eq (html5-search "" :required t) nil))
    (assert (string= (html5-search "foobar" :required t) "foobar"))

    (assert (string= (html5-search "" :pattern-matcher #'alpha-only) ""))
    (assert (eq (html5-search "12323" :pattern-matcher #'alpha-only) nil))
    (assert (string= (html5-search "foobar" :pattern-matcher #'alpha-only) "foobar"))

    (assert (string= (html5-search "" :min-length 1) ""))
    (assert (eq (html5-search "a" :min-length 2) nil))
    (assert (eq (html5-search "foobar" :max-length 1) nil))
    (assert (string= (html5-search "foobar" :min-length 1 :max-length 10) "foobar"))

    (assert (eq (html5-search "" :required t :pattern-matcher #'alpha-only) nil))
    t))

(defun html5-tel-test ()
  (flet ((exact-match (tel)
           (string= tel "000-000-0000")))
    (assert (string= (html5-tel "") ""))
    (assert (string= (html5-tel "000-000-0000") "000-000-0000"))

    (assert (eq (html5-tel "" :required t) nil))
    (assert (string= (html5-tel "000-000-0000" :required t) "000-000-0000"))

    (assert (eq  (html5-tel "" :pattern-matcher #'exact-match) ""))
    (assert (eq (html5-tel "foobar" :pattern-matcher #'exact-match) nil))
    (assert (string= (html5-tel "000-000-0000" :pattern-matcher #'exact-match) "000-000-0000"))

    (assert (string= (html5-tel "" :min-length 1) ""))
    (assert (eq (html5-tel "a" :min-length 2) nil))
    (assert (eq (html5-tel "000-000-0000" :max-length 1) nil))
    (assert (string= (html5-tel "000-000-0000" :min-length 1 :max-length 20) "000-000-0000"))

    (assert (eq (html5-tel "" :required t :pattern-matcher #'exact-match) nil))
    t))

(defun html5-url-test ()
  (macrolet ((url= (form &key url scheme userinfo host port path query fragment)
               `(multiple-value-bind (f-url f-scheme f-userinfo f-host f-port f-path f-query f-fragment)
                    ,form
                  (and (string= ,url f-url)
                       (string= ,scheme f-scheme)
                       (string= ,userinfo f-userinfo)
                       (string= ,host f-host)
                       (string= ,port f-port)
                       (string= ,path f-path)
                       (string= ,query f-query)
                       (string= ,fragment f-fragment)))))
    (flet ((exact-match (url)
             (string= url "https://developer.mozilla.org/en-US/docs/Learn")))
      (assert (url= (html5-url "https://developer.mozilla.org/en-US/docs/Learn")
                    :url "https://developer.mozilla.org/en-US/docs/Learn"
                    :scheme "https"
                    :host "developer.mozilla.org"
                    :path "/en-US/docs/Learn"))
      (assert (url= (html5-url "//developer.mozilla.org/en-US/docs/Learn")))
      (assert (url= (html5-url "/en-US/docs/Learn")))
      (assert (url= (html5-url "Skills/Infrastructure/Understanding_URLs")))
      (assert (url= (html5-url "../CSS/display")))

      (assert (url= (html5-url "https://developer.mozilla.org/en-US/docs/Learn"
                               :required t)
                    :url "https://developer.mozilla.org/en-US/docs/Learn"
                    :scheme "https"
                    :host "developer.mozilla.org"
                    :path "/en-US/docs/Learn"))

      (assert (url= (html5-url "https://developer.mozilla.org/en-US/docs/Learn"
                               :pattern-matcher #'exact-match)
                    :url "https://developer.mozilla.org/en-US/docs/Learn"
                    :scheme "https"
                    :host "developer.mozilla.org"
                    :path "/en-US/docs/Learn"))

      (assert (url= (html5-url "https://developer.mozilla.org/en-US/docs/Learn"
                               :max-length 10)))
      (assert (url= (html5-url "https://developer.mozilla.org/en-US/docs/Learn"
                               :min-length 60)))
      t)))

(defun html5-number-test ()
  (macrolet ((number= (form str num-value)
               `(multiple-value-bind (f-str f-num-value)
                    ,form
                  (and (string= ,str f-str)
                       (if ,num-value (= ,num-value f-num-value) t)))))
    (assert (number= (html5-number "") "" nil))
    (assert (number= (html5-number "" :min 0) "" nil))
    (assert (number= (html5-number "" :max 0) "" nil))
    (assert (number= (html5-number "" :min 0 :max 0) "" nil))

    (assert (number= (html5-number "" :required t) nil nil))
    (assert (number= (html5-number "" :required t :min 0) nil nil))
    (assert (number= (html5-number "" :required t :max 0) nil nil))
    (assert (number= (html5-number "" :required t :min 0 :max 0) nil nil))

    (assert (number= (html5-number "50") "50" 50))
    (assert (number= (html5-number "50" :min 0) "50" 50))
    (assert (number= (html5-number "50" :max 100) "50" 50))
    (assert (number= (html5-number "50" :min 0 :max 100) "50" 50))
    (assert (number= (html5-number "50" :max 0) nil nil))
    (assert (number= (html5-number "50" :min 0 :max 0) nil nil))

    (assert (number= (html5-number "-50") "-50" -50))
    (assert (number= (html5-number "-50" :min -100) "-50" -50))
    (assert (number= (html5-number "-50" :max 0) "-50" -50))
    (assert (number= (html5-number "-50" :min -100 :max 0) "-50" -50))
    (assert (number= (html5-number "-50" :max -100) nil nil))
    (assert (number= (html5-number "-50" :min -100 :max -100) nil nil))

    (assert (number= (html5-number "50.55") "50.55" 50.55))
    (assert (number= (html5-number "50.55" :min 0) "50.55" 50.55))
    (assert (number= (html5-number "50.55" :max 100) "50.55" 50.55))
    (assert (number= (html5-number "50.55" :min 0 :max 100) "50.55" 50.55))
    (assert (number= (html5-number "50.55" :max 0) nil nil))
    (assert (number= (html5-number "50.55" :min 0 :max 0) nil nil))

    (assert (number= (html5-number "-50.55") "-50.55" -50.55))
    (assert (number= (html5-number "-50.55" :min -100) "-50.55" -50.55))
    (assert (number= (html5-number "-50.55" :max 0) "-50.55" -50.55))
    (assert (number= (html5-number "-50.55" :min -100 :max 0) "-50.55" -50.55))
    (assert (number= (html5-number "-50.55" :max -100) nil nil))
    (assert (number= (html5-number "-50.55" :min -100 :max -100) nil nil))
    t))

(defun html5-range-test ()
  (macrolet ((range= (form str num-value)
               `(multiple-value-bind (f-str f-num-value)
                    ,form
                  (and (string= ,str f-str)
                       (if ,num-value (= ,num-value f-num-value) t)))))
    (assert (range= (html5-range "") nil nil))
    (assert (range= (html5-range "" :min 0) nil nil))
    (assert (range= (html5-range "" :max 0) nil nil))
    (assert (range= (html5-range "" :min 0 :max 0) nil nil))

    (assert (range= (html5-range "50") "50" 50))
    (assert (range= (html5-range "50" :min 0) "50" 50))
    (assert (range= (html5-range "50" :max 100) "50" 50))
    (assert (range= (html5-range "50" :min 0 :max 100) "50" 50))
    (assert (range= (html5-range "50" :max 0) nil nil))
    (assert (range= (html5-range "50" :min 0 :max 0) nil nil))

    (assert (range= (html5-range "-50") "-50" -50))
    (assert (range= (html5-range "-50" :min -100) "-50" -50))
    (assert (range= (html5-range "-50" :max 0) "-50" -50))
    (assert (range= (html5-range "-50" :min -100 :max 0) "-50" -50))
    (assert (range= (html5-range "-50" :max -100) nil nil))
    (assert (range= (html5-range "-50" :min -100 :max -100) nil nil))

    (assert (range= (html5-range "50.55") "50.55" 50.55))
    (assert (range= (html5-range "50.55" :min 0) "50.55" 50.55))
    (assert (range= (html5-range "50.55" :max 100) "50.55" 50.55))
    (assert (range= (html5-range "50.55" :min 0 :max 100) "50.55" 50.55))
    (assert (range= (html5-range "50.55" :max 0) nil nil))
    (assert (range= (html5-range "50.55" :min 0 :max 0) nil nil))

    (assert (range= (html5-range "-50.55") "-50.55" -50.55))
    (assert (range= (html5-range "-50.55" :min -100) "-50.55" -50.55))
    (assert (range= (html5-range "-50.55" :max 0) "-50.55" -50.55))
    (assert (range= (html5-range "-50.55" :min -100 :max 0) "-50.55" -50.55))
    (assert (range= (html5-range "-50.55" :max -100) nil nil))
    (assert (range= (html5-range "-50.55" :min -100 :max -100) nil nil))
    t))

(defun html5-datetime-local-test ()
  (macrolet ((datetime-local= (form str yyyy* mmonth dd hh mmin ss)
               `(multiple-value-bind (f-str f-yyyy* f-mmonth f-dd f-hh f-mmin f-ss)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,yyyy* f-yyyy*)
                       (string= ,mmonth f-mmonth)
                       (string= ,dd f-dd)
                       (string= ,hh f-hh)
                       (string= ,mmin f-mmin)
                       (string= ,ss f-ss)))))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :required t) "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :required t) "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :required t) "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))

    (assert (datetime-local= (html5-datetime-local "") "" nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "" :required t) nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "-2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "-2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "-2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01-T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01-T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01-T08:30:01.333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T-08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T-08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T-08:30:01.333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30-") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01-") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333-") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.3333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2016-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2016-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2016-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T07:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T07:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T07:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2017-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2017-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2017-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2018-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2018-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2018-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2017-06-01T09:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2017-06-01T09:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2017-06-01T09:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:30" :max "2017-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01" :max "2017-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333" :max "2017-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2016-06-01T08:30" :max "2018-06-01T08:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2016-06-01T08:30:01" :max "2018-06-01T08:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2016-06-01T08:30:01.333" :max "2018-06-01T08:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T07:30" :max "2017-06-01T09:30") "2017-06-01T08:30" "2017" "06" "01" "08" "30" nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T07:30:01" :max "2017-06-01T09:30:01") "2017-06-01T08:30:01" "2017" "06" "01" "08" "30" "01"))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T07:30:01.333" :max "2017-06-01T09:30:01.333") "2017-06-01T08:30:01.333" "2017" "06" "01" "08" "30" "01.333"))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2018-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2018-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2018-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-07-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-07-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-07-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-02T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-02T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-02T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T09:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T09:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T09:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:31") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:31:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:31:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:02") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:02.333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2016-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2016-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2016-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2017-05-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2017-05-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2017-05-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-02T08:30" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-02T08:30:01" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-02T08:30:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2017-06-01T07:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2017-06-01T07:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2017-06-01T07:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :max "2017-06-01T08:29") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2017-06-01T08:29:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2017-06-01T08:29:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :max "2017-06-01T08:30:00") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :max "2017-06-01T08:30:00.333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2018-06-01T08:30" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2018-06-01T08:30:01" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2018-06-01T08:30:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-07-01T08:30" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-07-01T08:30:01" :max  "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-07-01T08:30:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-02T08:30" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-02T08:30:01" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-02T08:30:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T09:30" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T09:30:01" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T09:30:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:31" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:31:01" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:31:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:02" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:02.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))

    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:30" :max "2016-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01" :max "2016-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333" :max "2016-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:30" :max "2017-05-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01" :max "2017-05-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333" :max "2017-05-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-02T08:30" :min "2017-06-02T08:30" :max "2017-06-01T08:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-02T08:30:01" :min "2017-06-02T08:30:01" :max "2017-06-01T08:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-02T08:30:01.333" :min "2017-06-02T08:30:01.333" :max "2017-06-01T08:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:30" :max "2017-06-01T07:30") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01" :max "2017-06-01T07:30:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333" :max "2017-06-01T07:30:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30" :min "2017-06-01T08:30" :max "2017-06-01T08:29") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01" :max "2017-06-01T08:29:01") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333" :max "2017-06-01T08:29:01.333") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01" :min "2017-06-01T08:30:01" :max "2017-06-01T08:30:00") nil nil nil nil nil nil nil))
    (assert (datetime-local= (html5-datetime-local "2017-06-01T08:30:01.333" :min "2017-06-01T08:30:01.333" :max "2017-06-01T08:30:00.333") nil nil nil nil nil nil nil))
    t))

(defun html5-month-test ()
  (macrolet ((html5-month= (form str yyyy* mm)
               `(multiple-value-bind (f-str f-yyyy* f-mm)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,yyyy* f-yyyy*)
                       (string= ,mm f-mm)))))
    (assert (string= (html5-month "") ""))
    (assert (eq (html5-month "foobar") nil))

    (assert (eq (html5-month "yyyy-mm") nil))
    (assert (eq (html5-month "yyyy") nil))
    (assert (eq (html5-month "mm") nil))

    (assert (eq (html5-month "9999-99") nil))
    (assert (eq (html5-month "9999") nil))
    (assert (eq (html5-month "99") nil))

    (assert (eq (html5-month "9999-00") nil))
    (assert (eq (html5-month "9999-13") nil))

    (assert (eq (html5-month "999-01") nil))
    (assert (eq (html5-month "999-12") nil))

    (assert (eq (html5-month "9999-1") nil))
    (assert (eq (html5-month "9999-9") nil))

    (assert (html5-month= (html5-month "9999-01") "9999-01" "9999" "01"))
    (assert (html5-month= (html5-month "9999-02") "9999-02" "9999" "02"))
    (assert (html5-month= (html5-month "9999-03") "9999-03" "9999" "03"))
    (assert (html5-month= (html5-month "9999-04") "9999-04" "9999" "04"))
    (assert (html5-month= (html5-month "9999-05") "9999-05" "9999" "05"))
    (assert (html5-month= (html5-month "9999-06") "9999-06" "9999" "06"))
    (assert (html5-month= (html5-month "9999-07") "9999-07" "9999" "07"))
    (assert (html5-month= (html5-month "9999-08") "9999-08" "9999" "08"))
    (assert (html5-month= (html5-month "9999-09") "9999-09" "9999" "09"))
    (assert (html5-month= (html5-month "9999-10") "9999-10" "9999" "10"))
    (assert (html5-month= (html5-month "9999-11") "9999-11" "9999" "11"))
    (assert (html5-month= (html5-month "9999-12") "9999-12" "9999" "12"))

    (assert (html5-month= (html5-month "0000-01") "0000-01" "0000" "01"))

    (assert (eq (html5-month "-0000-01") nil))
    (assert (eq (html5-month "-9999-01") nil))

    (assert (eq (html5-month "-0000-01-") nil))
    (assert (eq (html5-month "-9999-01-") nil))

    (assert (eq (html5-month "0000-01-") nil))
    (assert (eq (html5-month "9999-01-") nil))

    (assert (html5-month= (html5-month "99999-01") "99999-01" "99999" "01"))
    (assert (html5-month= (html5-month "99999-02") "99999-02" "99999" "02"))
    (assert (html5-month= (html5-month "99999-03") "99999-03" "99999" "03"))
    (assert (html5-month= (html5-month "99999-04") "99999-04" "99999" "04"))
    (assert (html5-month= (html5-month "99999-05") "99999-05" "99999" "05"))
    (assert (html5-month= (html5-month "99999-06") "99999-06" "99999" "06"))
    (assert (html5-month= (html5-month "99999-07") "99999-07" "99999" "07"))
    (assert (html5-month= (html5-month "99999-08") "99999-08" "99999" "08"))
    (assert (html5-month= (html5-month "99999-09") "99999-09" "99999" "09"))
    (assert (html5-month= (html5-month "99999-10") "99999-10" "99999" "10"))
    (assert (html5-month= (html5-month "99999-11") "99999-11" "99999" "11"))
    (assert (html5-month= (html5-month "99999-12") "99999-12" "99999" "12"))

    (assert (eq (html5-month "" :required t) nil))
    (assert (eq (html5-month "foobar" :required t) nil))
    (assert (html5-month= (html5-month "9999-01" :required t) "9999-01" "9999" "01"))
    (assert (html5-month= (html5-month "99999-12" :required t) "99999-12" "99999" "12"))

    (assert (html5-month= (html5-month "9999-02" :min "9999-01") "9999-02" "9999" "02"))
    (assert (html5-month= (html5-month "9999-02" :min "9999-02") "9999-02" "9999" "02"))
    (assert (html5-month= (html5-month "9999-02" :max "9999-02") "9999-02" "9999" "02"))
    (assert (html5-month= (html5-month "9999-02" :max "9999-03") "9999-02" "9999" "02"))
    (assert (html5-month= (html5-month "9999-02" :min "9999-01" :max "9999-03") "9999-02" "9999" "02"))
    (assert (html5-month= (html5-month "9999-02" :min "9999-02" :max "9999-02") "9999-02" "9999" "02"))

    (assert (html5-month= (html5-month "9999-02" :min "9999-03") nil nil nil))
    (assert (html5-month= (html5-month "9999-02" :max "9999-01") nil nil nil))
    (assert (html5-month= (html5-month "9999-02" :min "9999-03" :max "9999-03") nil nil nil))
    (assert (html5-month= (html5-month "9999-02" :min "9999-01" :max "9999-01") nil nil nil))
    (assert (html5-month= (html5-month "9999-02" :min "9999-03" :max "9999-01") nil nil nil))
    t))

(defun html5-week-test ()
  (macrolet ((html5-week= (form str yyyy* ww)
               `(multiple-value-bind (f-str f-yyyy* f-ww)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,yyyy* f-yyyy*)
                       (string= ,ww f-ww)))))
    (assert (string= (html5-week "") ""))
    (assert (eq (html5-week "foobar") nil))

    (assert (eq (html5-week "yyyy-Www") nil))
    (assert (eq (html5-week "yyyy") nil))
    (assert (eq (html5-week "ww") nil))

    (assert (eq (html5-week "9999-W99") nil))
    (assert (eq (html5-week "9999") nil))
    (assert (eq (html5-week "99") nil))

    (assert (eq (html5-week "9999-W00") nil))
    (assert (eq (html5-week "9999-W54") nil))

    (assert (eq (html5-week "999-W01") nil))
    (assert (eq (html5-week "999-W53") nil))

    (assert (eq (html5-week "9999-W1") nil))
    (assert (eq (html5-week "9999-W5") nil))

    (loop for i from 1 to 53
          do (if (< i 10)
                 (assert (html5-week= (html5-week (concatenate 'string "9999-W0" (write-to-string i)))
                                      (concatenate 'string "9999-W0" (write-to-string i))
                                      "9999"
                                      (concatenate 'string "0" (write-to-string i))))
                 (assert (html5-week= (html5-week (concatenate 'string "9999-W" (write-to-string i)))
                                      (concatenate 'string "9999-W" (write-to-string i))
                                      "9999"
                                      (write-to-string i)))))

    (assert (html5-week= (html5-week "0000-W01") "0000-W01" "0000" "01"))

    (assert (eq (html5-week "-0000-W01") nil))
    (assert (eq (html5-week "-9999-W01") nil))

    (assert (eq (html5-week "-0000-W01-") nil))
    (assert (eq (html5-week "-9999-W01-") nil))

    (assert (eq (html5-week "0000-W01-") nil))
    (assert (eq (html5-week "9999-W01-") nil))

    (loop for i from 1 to 53
          do (if (< i 10)
                 (assert (html5-week= (html5-week (concatenate 'string "99999-W0" (write-to-string i)))
                                      (concatenate 'string "99999-W0" (write-to-string i))
                                      "99999"
                                      (concatenate 'string "0" (write-to-string i))))
                 (assert (html5-week= (html5-week (concatenate 'string "99999-W" (write-to-string i)))
                                      (concatenate 'string "99999-W" (write-to-string i))
                                      "99999"
                                      (write-to-string i)))))

    (assert (eq (html5-week "" :required t) nil))
    (assert (eq (html5-week "foobar" :required t) nil))
    (assert (html5-week= (html5-week "9999-W01" :required t) "9999-W01" "9999" "01"))
    (assert (html5-week= (html5-week "99999-W53" :required t) "99999-W53" "99999" "53"))

    (assert (html5-week= (html5-week "9999-W02" :min "9999-W01") "9999-W02" "9999" "02"))
    (assert (html5-week= (html5-week "9999-W02" :min "9999-W02") "9999-W02" "9999" "02"))
    (assert (html5-week= (html5-week "9999-W02" :max "9999-W02") "9999-W02" "9999" "02"))
    (assert (html5-week= (html5-week "9999-W02" :max "9999-W03") "9999-W02" "9999" "02"))
    (assert (html5-week= (html5-week "9999-W02" :min "9999-W01" :max "9999-W03") "9999-W02" "9999" "02"))
    (assert (html5-week= (html5-week "9999-W02" :min "9999-W02" :max "9999-W02") "9999-W02" "9999" "02"))

    (assert (html5-week= (html5-week "9999-W02" :min "9999-W03") nil nil nil))
    (assert (html5-week= (html5-week "9999-W02" :max "9999-W01") nil nil nil))
    (assert (html5-week= (html5-week "9999-W02" :min "9999-W03" :max "9999-W03") nil nil nil))
    (assert (html5-week= (html5-week "9999-W02" :min "9999-W01" :max "9999-W01") nil nil nil))
    (assert (html5-week= (html5-week "9999-W02" :min "9999-W03" :max "9999-W01") nil nil nil))
    t))

(defun html5-time-test ()
  (macrolet ((html5-time= (form str hh mm ss)
               `(multiple-value-bind (f-str f-hh f-mm f-ss)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,hh f-hh)
                       (string= ,mm f-mm)
                       (string= ,ss f-ss)))))
    (assert (html5-time= (html5-time "00:00") "00:00" "00" "00" nil))
    (assert (html5-time= (html5-time "12:00") "12:00" "12" "00" nil))
    (assert (html5-time= (html5-time "12:30") "12:30" "12" "30" nil))
    (assert (html5-time= (html5-time "00:30") "00:30" "00" "30" nil))

    (assert (html5-time= (html5-time "24:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:60") nil nil nil nil))
    (assert (html5-time= (html5-time "00:-30") nil nil nil nil))
    (assert (html5-time= (html5-time "-00:00") nil nil nil nil))
    (assert (html5-time= (html5-time ":00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00::00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00:00") "00:00:00" "00" "00" "00"))
    (assert (html5-time= (html5-time "12:00:00") "12:00:00" "12" "00" "00"))
    (assert (html5-time= (html5-time "12:30:00") "12:30:00" "12" "30" "00"))
    (assert (html5-time= (html5-time "12:00:30") "12:00:30" "12" "00" "30"))
    (assert (html5-time= (html5-time "00:30:00") "00:30:00" "00" "30" "00"))
    (assert (html5-time= (html5-time "00:00:30") "00:00:30" "00" "00" "30"))

    (assert (html5-time= (html5-time "24:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:60:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:60") nil nil nil nil))
    (assert (html5-time= (html5-time "00:-30:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30:-30") nil nil nil nil))
    (assert (html5-time= (html5-time "-00:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time ":00:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00::00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00::00") nil nil nil nil))
    (assert (html5-time= (html5-time "00::00::00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:00:") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00" :min "00:00") "00:00" "00" "00" nil))
    (assert (html5-time= (html5-time "12:00" :min "00:00") "12:00" "12" "00" nil))
    (assert (html5-time= (html5-time "12:30" :min "00:00") "12:30" "12" "30" nil))
    (assert (html5-time= (html5-time "00:30" :min "00:00") "00:30" "00" "30" nil))

    (assert (html5-time= (html5-time "00:00" :max "23:00") "00:00" "00" "00" nil))
    (assert (html5-time= (html5-time "12:00" :max "23:00") "12:00" "12" "00" nil))
    (assert (html5-time= (html5-time "12:30" :max "23:00") "12:30" "12" "30" nil))
    (assert (html5-time= (html5-time "00:30" :max "23:00") "00:30" "00" "30" nil))

    (assert (html5-time= (html5-time "00:00" :min "00:00" :max "23:00") "00:00" "00" "00" nil))
    (assert (html5-time= (html5-time "12:00" :min "00:00" :max "23:00") "12:00" "12" "00" nil))
    (assert (html5-time= (html5-time "12:30" :min "00:00" :max "23:00") "12:30" "12" "30" nil))
    (assert (html5-time= (html5-time "00:30" :min "00:00" :max "23:00") "00:30" "00" "30" nil))

    (assert (html5-time= (html5-time "00:00:00" :min "00:00:00") "00:00:00" "00" "00" "00"))
    (assert (html5-time= (html5-time "12:00:00" :min "00:00:00") "12:00:00" "12" "00" "00"))
    (assert (html5-time= (html5-time "12:30:00" :min "00:00:00") "12:30:00" "12" "30" "00"))
    (assert (html5-time= (html5-time "12:00:30" :min "00:00:00") "12:00:30" "12" "00" "30"))
    (assert (html5-time= (html5-time "00:30:00" :min "00:00:00") "00:30:00" "00" "30" "00"))
    (assert (html5-time= (html5-time "00:00:30" :min "00:00:00") "00:00:30" "00" "00" "30"))

    (assert (html5-time= (html5-time "00:00:00" :max "23:00:00") "00:00:00" "00" "00" "00"))
    (assert (html5-time= (html5-time "12:00:00" :max "23:00:00") "12:00:00" "12" "00" "00"))
    (assert (html5-time= (html5-time "12:30:00" :max "23:00:00") "12:30:00" "12" "30" "00"))
    (assert (html5-time= (html5-time "12:00:30" :max "23:00:00") "12:00:30" "12" "00" "30"))
    (assert (html5-time= (html5-time "00:30:00" :max "23:00:00") "00:30:00" "00" "30" "00"))
    (assert (html5-time= (html5-time "00:00:30" :max "23:00:00") "00:00:30" "00" "00" "30"))

    (assert (html5-time= (html5-time "00:00:00" :min "00:00:00" :max "23:00:00") "00:00:00" "00" "00" "00"))
    (assert (html5-time= (html5-time "12:00:00" :min "00:00:00" :max "23:00:00") "12:00:00" "12" "00" "00"))
    (assert (html5-time= (html5-time "12:30:00" :min "00:00:00" :max "23:00:00") "12:30:00" "12" "30" "00"))
    (assert (html5-time= (html5-time "12:00:30" :min "00:00:00" :max "23:00:00") "12:00:30" "12" "00" "30"))
    (assert (html5-time= (html5-time "00:30:00" :min "00:00:00" :max "23:00:00") "00:30:00" "00" "30" "00"))
    (assert (html5-time= (html5-time "00:00:30" :min "00:00:00" :max "23:00:00") "00:00:30" "00" "00" "30"))

    (assert (html5-time= (html5-time "00:00" :min "00:01") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00" :min "13:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:30" :min "12:31") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30" :min "01:00") nil nil nil nil))

    (assert (html5-time= (html5-time "00:01" :max "00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00" :max "11:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:30" :max "11:29") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30" :max "00:29") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00:00" :min "00:00:01") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:00" :min "13:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:30:00" :min "13:31:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:30" :min "13:00:31") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30:00" :min "00:31:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:30" :min "00:00:31") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00:01" :max "00:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:00" :max "11:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:30:00" :max "11:29:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:30" :max "11:00:29") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30:00" :max "00:29:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:30" :max "00:00:29") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00:00.000") "00:00:00.000" "00" "00" "00.000"))
    (assert (html5-time= (html5-time "12:00:00.012") "12:00:00.012" "12" "00" "00.012"))
    (assert (html5-time= (html5-time "12:30:00.123") "12:30:00.123" "12" "30" "00.123"))
    (assert (html5-time= (html5-time "12:00:30.123") "12:00:30.123" "12" "00" "30.123"))
    (assert (html5-time= (html5-time "00:30:00.03") "00:30:00.03" "00" "30" "00.03"))
    (assert (html5-time= (html5-time "00:00:30.1") "00:00:30.1" "00" "00" "30.1"))

    (assert (html5-time= (html5-time "24:00:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:60:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:60.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:-30:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30:-30.000") nil nil nil nil))
    (assert (html5-time= (html5-time "-00:00:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time ":00:00:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00::00:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00::00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00::00::00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:00.000:") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00:00.000" :min "00:00:00.000") "00:00:00.000" "00" "00" "00.000"))
    (assert (html5-time= (html5-time "12:00:00.000" :min "00:00:00.000") "12:00:00.000" "12" "00" "00.000"))
    (assert (html5-time= (html5-time "12:30:00.000" :min "00:00:00.000") "12:30:00.000" "12" "30" "00.000"))
    (assert (html5-time= (html5-time "12:00:30.000" :min "00:00:00") "12:00:30.000" "12" "00" "30.000"))
    (assert (html5-time= (html5-time "00:30:00.000" :min "00:00:00") "00:30:00.000" "00" "30" "00.000"))
    (assert (html5-time= (html5-time "00:00:30.000" :min "00:00:00") "00:00:30.000" "00" "00" "30.000"))

    (assert (html5-time= (html5-time "00:00:00" :max "23:00:00.000") "00:00:00" "00" "00" "00"))
    (assert (html5-time= (html5-time "12:00:00" :max "23:00:00.000") "12:00:00" "12" "00" "00"))
    (assert (html5-time= (html5-time "12:30:00" :max "23:00:00.000") "12:30:00" "12" "30" "00"))
    (assert (html5-time= (html5-time "12:00:30.000" :max "23:00:00") "12:00:30.000" "12" "00" "30.000"))
    (assert (html5-time= (html5-time "00:30:00.000" :max "23:00:00") "00:30:00.000" "00" "30" "00.000"))
    (assert (html5-time= (html5-time "00:00:30.000" :max "23:00:00") "00:00:30.000" "00" "00" "30.000"))

    (assert (html5-time= (html5-time "00:00:00" :min "00:00:00.000" :max "23:00:00") "00:00:00" "00" "00" "00"))
    (assert (html5-time= (html5-time "12:00:00" :min "00:00:00.000" :max "23:00:00") "12:00:00" "12" "00" "00"))
    (assert (html5-time= (html5-time "12:30:00" :min "00:00:00" :max "23:00:00.000") "12:30:00" "12" "30" "00"))
    (assert (html5-time= (html5-time "12:00:30.000" :min "00:00:00.000" :max "23:00:00") "12:00:30.000" "12" "00" "30.000"))
    (assert (html5-time= (html5-time "00:30:00.000" :min "00:00:00.000" :max "23:00:00") "00:30:00.000" "00" "30" "00.000"))
    (assert (html5-time= (html5-time "00:00:30.000" :min "00:00:00" :max "23:00:00.000") "00:00:30.000" "00" "00" "30.000"))

    (assert (html5-time= (html5-time "00:00:00.000" :min "00:00:01") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:00" :min "13:00:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "12:30:00.000" :min "13:31:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:30" :min "13:00:31.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30:00.000" :min "00:31:00") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:30" :min "00:00:31.000") nil nil nil nil))

    (assert (html5-time= (html5-time "00:00:01" :max "00:00:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:00.000" :max "11:00:00") nil nil nil nil))
    (assert (html5-time= (html5-time "12:30:00" :max "11:29:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "12:00:30.000" :max "11:00:29") nil nil nil nil))
    (assert (html5-time= (html5-time "00:30:00" :max "00:29:00.000") nil nil nil nil))
    (assert (html5-time= (html5-time "00:00:30.000" :max "00:00:29") nil nil nil nil))
    t))

(defun html5-color-test ()
  (macrolet ((color= (form str rrggbb rr gg bb)
               `(multiple-value-bind (f-str f-rrggbb f-rr f-gg f-bb)
                    ,form
                  (and (string= ,str f-str)
                       (string= ,rrggbb f-rrggbb)
                       (string= ,rr f-rr)
                       (string= ,gg f-gg)
                       (string= ,bb f-bb)))))
    (assert (color= (html5-color "#000000") "#000000" "000000" "00" "00" "00"))
    (assert (color= (html5-color "#FFFFFF") "#FFFFFF" "FFFFFF" "FF" "FF" "FF"))
    (assert (color= (html5-color "#ffffff") "#ffffff" "ffffff" "ff" "ff" "ff"))

    (assert (color= (html5-color "#zzzzzz") nil nil nil nil nil))
    (assert (color= (html5-color "000000") nil nil nil nil nil))
    (assert (color= (html5-color "#000000#") nil nil nil nil nil))
    (assert (color= (html5-color "#######") nil nil nil nil nil))
    t))

(defun test ()
  (assert (every #'identity
                 (list (html-checkbox-test)
                       (html-date-test)
                       (html-file-test)
                       (html-hidden-test)
                       (html-password-test)
                       (html-radio-test)
                       (html-text-test)

                       (html5-email-test)
                       (html5-search-test)
                       (html5-tel-test)
                       (html5-url-test)
                       (html5-number-test)
                       (html5-range-test)
                       (html5-datetime-local-test)
                       (html5-month-test)
                       (html5-week-test)
                       (html5-time-test)
                       (html5-color-test))))
  t)
