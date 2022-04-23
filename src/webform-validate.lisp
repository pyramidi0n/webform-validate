(in-package :cl-user)
(defpackage webform-validate
  (:use
   :cl
   :split-sequence)
  (:export
   :html-checkbox
   :html-date
   :html-file
   :html-hidden
   :html-password
   :html-radio
   :html-text

   :html5-email
   :html5-search
   :html5-tel
   :html5-url
   :html5-number
   :html5-range
   :html5-datetime-local
   :html5-month
   :html5-week
   :html5-time
   :html5-color))
(in-package :webform-validate)

;; ------------------------------------------------------------------------------

(declaim (ftype (function (string)
                          string)
                trim-whitespace))

(declaim (ftype (function (function sequence)
                          boolean)
                one))

(declaim (ftype (function (string)
                          boolean)
                uint-string-p))

(declaim (ftype (function (string)
                          boolean)
                int-string-p))

(declaim (ftype (function (string)
                          boolean)
                ufloat-string-p))

(declaim (ftype (function (string)
                          boolean)
                float-string-p))

(declaim (ftype (function (string
                           &optional
                           (or integer float null)
                           (or integer float null))
                          (or integer float null))
                numeric-value))

(defun trim-whitespace (str)
  "Strips leading and trailing whitespace from STR."
  (string-trim '(#\Space
                 #\Newline
                 #\Backspace
                 #\Tab
                 #\Linefeed
                 #\Page
                 #\Return
                 #\Rubout)
               str))

(defun one (predicate seq)
  "True when exactly one element that satisfies the PREDICATE exists in SEQ."
  (= 1 (length (remove-if-not predicate seq))))

(defun uint-string-p (str)
  "True when STR would be parsed as a valid positive integer by the lisp reader."
  (and (> (length str) 0)
       (every #'digit-char-p str)))

(defun int-string-p (str)
  "True when STR would be parsed as a valid integer, either negative or positive,
by the lisp reader."
  (when (> (length str) 0)
    (if (eq #\- (elt str 0))
        (uint-string-p (subseq str 1))
        (uint-string-p str))))

(defun ufloat-string-p (str)
  "True when STR would be parsed as a valid positive float by the lisp reader."
  (flet ((decimal-char-p (char)
           (eq #\. char)))
    (and (> (length str) 1)
         (one #'decimal-char-p str)
         (every #'digit-char-p (remove-if #'decimal-char-p str)))))

(defun float-string-p (str)
  "True when STR would be parsed as a valid float, either positive or negative,
by the lisp reader."
  (when (> (length str) 0)
    (if (eq #\- (elt str 0))
        (ufloat-string-p (subseq str 1))
        (ufloat-string-p str))))

(defun numeric-value (str &optional min max)
  "Returns the number represented by STR, as either an integer or a float,
provided that this number is within MIN and MAX."
  (when (or (int-string-p str)
            (float-string-p str))
    (let ((num (read-from-string str)))
      (and (if min (>= num min) t)
           (if max (<= num max) t)
           num))))

;; ------------------------------------------------------------------------------

;; HTML Date Helpers

(declaim (ftype (function (string)
                          boolean)
                leap-year-p))

(declaim (ftype (function (string string string)
                          (or string null))
                valid-day-of-month))

(declaim (ftype (function (string)
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                parse-date))

(declaim (ftype (function (string string)
                          boolean)
                date=))

(declaim (ftype (function (string string)
                          boolean)
                date>))

(declaim (ftype (function (string string)
                          boolean)
                date<))

(declaim (ftype (function (string string)
                          boolean)
                date>=))

(declaim (ftype (function (string string)
                          boolean)
                date<=))

(defun leap-year-p (yyyy*)
  "True when YYYY* represents a valid leap year."
  (let ((yyyy*-i (parse-integer yyyy*)))
    (or (= (mod yyyy*-i 400) 0)
        (and (not (= (mod yyyy*-i 100) 0))
             (= (mod yyyy*-i 4) 0)))))

(defun valid-day-of-month (yyyy* mm day-string)
  "Returns DAY-STRING when DAY-STRING is a valid day within month MM of
year YYYY*, otherwise returns nil."
  (let* ((mm-i (parse-integer mm))
         (day-upper-bound (case mm-i
                            ((1 3 5 7 8 10 12) 31)
                            (2 (if (leap-year-p yyyy*) 29 28))
                            ((4 6 9 11) 30)))
         (dd-i (parse-integer day-string)))
    (when (and (>= dd-i 1)
               (<= dd-i day-upper-bound))
      day-string)))

(defun parse-date (str)
  "Parses a date string STR and returns four values: STR, the year string,
the month string, and the day string. When the date string is invalid, each
of these return values is nil."
  (when (>= (length str) 10)
    (let ((month-string (subseq str 0 (- (length str) 3)))
          (day-string (subseq str (- (length str) 2))))
      (when (and (eq #\- (char str (- (length str) 3)))
                 (every #'digit-char-p day-string))
        (multiple-value-bind (validated-month-string yyyy* mm)
            (html5-month month-string)
          (when validated-month-string
            (multiple-value-bind (dd)
                (valid-day-of-month yyyy* mm day-string)
              (when dd
                (return-from parse-date (values str yyyy* mm dd)))))))))
  (values nil nil nil nil))

(defun date= (date1 date2)
  "Parses two date strings, DATE1 and DATE2, and returns t when both are valid
and their constituent years, months, and days are equal. Otherwise, returns nil."
  (multiple-value-bind (date1-valid date1-yyyy* date1-mm date1-dd)
      (parse-date date1)
    (multiple-value-bind (date2-valid date2-yyyy* date2-mm date2-dd)
        (parse-date date2)
      (when (and date1-valid date2-valid)
        (and (= (parse-integer date1-yyyy*) (parse-integer date2-yyyy*))
             (= (parse-integer date1-mm) (parse-integer date2-mm))
             (= (parse-integer date1-dd) (parse-integer date2-dd)))))))

(defun date> (date1 date2)
  "Parses two date strings, DATE1 and DATE2, and returns t when both are valid
and DATE1 is chronologically more recent than DATE2. Otherwise, returns nil."
  (multiple-value-bind (date1-valid date1-yyyy* date1-mm date1-dd)
      (parse-date date1)
    (multiple-value-bind (date2-valid date2-yyyy* date2-mm date2-dd)
        (parse-date date2)
      (when (and date1-valid date2-valid)
        (let ((d1-yyyy*-i (parse-integer date1-yyyy*))
              (d1-mm-i (parse-integer date1-mm))
              (d1-dd-i (parse-integer date1-dd))
              (d2-yyyy*-i (parse-integer date2-yyyy*))
              (d2-mm-i (parse-integer date2-mm))
              (d2-dd-i (parse-integer date2-dd)))
          (or (> d1-yyyy*-i d2-yyyy*-i)
              (and (= d1-yyyy*-i d2-yyyy*-i)
                   (> d1-mm-i d2-mm-i))
              (and (= d1-yyyy*-i d2-yyyy*-i)
                   (= d1-mm-i d2-mm-i)
                   (> d1-dd-i d2-dd-i))))))))

(defun date< (date1 date2)
  "Parses two date strings, DATE1 and DATE2, and returns t when both are valid
and DATE2 is chronologically more recent than DATE1. Otherwise, returns nil."
  (date> date2 date1))

(defun date>= (date1 date2)
  "Parses two date strings, DATE1 and DATE2, and returns t when both are valid
and DATE1 is chronologically more recent than DATE2 or DATE1 and DATE2 are
equal. Otherwise, returns nil."
  (or (date= date1 date2)
      (date> date1 date2)))

(defun date<= (date1 date2)
  "Parses two date strings, DATE1 and DATE2, and returns t when both are valid
and DATE2 is chronologically more recent than DATE1 or DATE1 and DATE2 are
equal. Otherwise, returns nil."
  (date>= date2 date1))

;; HTML5 Time Helpers

(declaim (ftype (function (string)
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                validate-hhmm))

(declaim (ftype (function (string)
                          (or string null))
                validate-ss))

(declaim (ftype (function (string)
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                validate-time))

(declaim (ftype (function (string string)
                          boolean)
                time>=))

(declaim (ftype (function (string string)
                          boolean)
                time<=))

(defun validate-hhmm (str)
  "Parses a time string STR containing hours and minutes. When the hhmm segment
of the time string is valid, returns STR, hours, and minutes. Otherwise returns
nil, nil, nil."
  (let ((hh (subseq str 0 2))
        (mm (subseq str 3 5)))
    (and (eq #\: (char str 2))
         (every #'digit-char-p hh)
         (every #'digit-char-p mm)
         (let ((hh-i (parse-integer hh))
               (mm-i (parse-integer mm)))
           (if (and (>= hh-i 0) (<= hh-i 23)
                    (>= mm-i 0) (<= mm-i 59))
               (return-from validate-hhmm (values hh mm nil)))))
    (values nil nil nil)))

(defun validate-ss (str)
  "Parses a time string STR containing seconds. When the ss segment of the time
string is valid, returns the seconds. Otherwise returns nil."
  (let ((ss (subseq str 6)))
    (and (eq #\: (char str 5))
         (let ((ss-num (numeric-value ss 0 59.999)))
           (when ss-num
             (if (integerp ss-num)
                 (when (= (length ss) 2)
                   ss)
                 (when (and (>= (length ss) 4)
                            (<= (length ss) 6))
                   ss)))))))

(defun validate-time (str)
  "Parses a time string STR. When STR is valid, returns STR, hours, minutes,
and seconds when present (otherwise nil). Otherwise, returns nil, nil, nil, nil."
  (let ((hhmmss (and (>= (length str) 8)
                     (<= (length str) 12)))
        (hhmm (= (length str) 5)))
    (if hhmmss
        (multiple-value-bind (hh mm)
            (validate-hhmm str)
          (when (and hh mm)
            (multiple-value-bind (ss)
                (validate-ss str)
              (when ss
                (return-from validate-time (values str hh mm ss))))))
        (when hhmm
          (multiple-value-bind (hh mm)
              (validate-hhmm str)
            (when hh
              (return-from validate-time (values str hh mm nil)))))))
  (values nil nil nil nil))

(defun time>= (t1 t2)
  "Compares two time strings T1 and T2. Returns t when T1 is chronologically
more recent than T2. Otherwise returns nil."
  (multiple-value-bind (t1-valid t1-hh t1-mm t1-ss)
      (validate-time t1)
    (multiple-value-bind (t2-valid t2-hh t2-mm t2-ss)
        (validate-time t2)
      (when (and t1-valid t2-valid)
        (let ((t1-hh-num (parse-integer t1-hh))
              (t1-mm-num (parse-integer t1-mm))
              (t1-ss-num (when t1-ss (read-from-string t1-ss)))
              (t2-hh-num (parse-integer t2-hh))
              (t2-mm-num (parse-integer t2-mm))
              (t2-ss-num (when t2-ss (read-from-string t2-ss))))
          (or (> t1-hh-num t2-hh-num)
              (and (= t1-hh-num t2-hh-num)
                   (or (> t1-mm-num t2-mm-num)
                       (and (= t1-mm-num t2-mm-num)
                            (cond ((and t1-ss-num t2-ss-num) (>= t1-ss-num t2-ss-num))
                                  (t1-ss-num t)
                                  (t2-ss-num (= 0 t2-ss-num))
                                  (t t)))))))))))

(defun time<= (t1 t2)
  "Compares two time strings T1 and T2. Returns t when T2 is chronologically
more recent than T1. Otherwise returns nil."
  (time>= t2 t1))

;; ------------------------------------------------------------------------------

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:expected-value string))
                          (or string null))
                html-checkbox))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:min string)
                                  (:max string))
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html-date))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:accept string))
                          (or string null))
                html-file))

(declaim (ftype (function (string)
                          string)
                html-hidden))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:pattern-matcher function)
                                  (:min-length integer)
                                  (:max-length integer))
                          (or string null))
                html-password))

(declaim (ftype (function (string &key
                                  (:required boolean))
                          (or string null))
                html-radio))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:pattern-matcher function)
                                  (:min-length integer)
                                  (:max-length integer))
                          (or string null))
                html-text))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:pattern-matcher function)
                                  (:min-length integer)
                                  (:max-length integer)
                                  (:multiple boolean))
                          (values (or list string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-email))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:pattern-matcher function)
                                  (:min-length integer)
                                  (:max-length integer))
                          (or string null))
                html5-search))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:pattern-matcher function)
                                  (:min-length integer)
                                  (:max-length integer))
                          (or string null))
                html5-tel))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:pattern-matcher function)
                                  (:min-length integer)
                                  (:max-length integer))
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-url))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:min (or integer float))
                                  (:max (or integer float)))
                          (values (or string null)
                                  (or integer float null)
                                  &optional))
                html5-number))

(declaim (ftype (function (string &key
                                  (:min (or integer float))
                                  (:max (or integer float)))
                          (values (or string null)
                                  (or integer float null)
                                  &optional))
                html5-range))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:min string)
                                  (:max string))
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-datetime-local))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:min string)
                                  (:max string))
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-month))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:min string)
                                  (:max string))
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-week))

(declaim (ftype (function (string &key
                                  (:required boolean)
                                  (:min string)
                                  (:max string))
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-time))

(declaim (ftype (function (string)
                          (values (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  (or string null)
                                  &optional))
                html5-color))

(defun html-checkbox (str &key required)
  "Function html-checkbox

Syntax:

html-checkbox str &key required => result

Arguments and Values:

str---a string designator

required---a boolean

result---str or nil

Description:

Validates an HTML checkbox input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/checkbox#value
  (if required
      (when (not (string= str "")) str)
      str))

(defun html-date (str &key required min max)
  "Function html-date

Syntax:

html-date str &key required min max => result

Arguments and Values:

str---a string designator

required---a boolean

min---a string designator

max---a string designator

result---str year month day or nil nil nil nil

Description:

Validates an HTML date input value STR. When STR is a valid date, returns
STR and the components of the date: year, month, day. Otherwise returns nil,
nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any date STR that, chronologically,
is not at least MIN or at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/date#value
  (flet ((valid-date ()
           (if (and (if min (date>= str min) t)
                    (if max (date<= str max) t))
               (parse-date str)
               (values nil nil nil nil))))
    (if required
        (if (string= str "")
            (values nil nil nil nil)
            (valid-date))
        (if (string= str "")
            (values "" nil nil nil)
            (valid-date)))))

(defun html-file (str &key required accept multiple)
  "Function html-file

Syntax:

html-file str &key required accept multiple => result

Arguments and Values:

str---a string designator

required---a boolean

accept---a string designator

multiple---a boolean

result---str or nil

Description:

Validates an HTML file input value STR. Returns either STR or nil depending
 on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

For now, ignores the ACCEPT and MULTIPLE parameters."
  (declare (ignore accept multiple))
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/file#value
  ;;
  ;; We don't do much of anything here, since the validation of actual files
  ;; is going to depend a lot on parsing the request body and so forth.
  (if required
      (when (not (string= str ""))
        str)
      str))

(defun html-hidden (str)
  "Function html-hidden

Syntax:

html-hidden str => result

Arguments and Values:

str---a string designator

result---str or nil

Description:

Validates an HTML hidden input value STR. Returns either STR or nil depending
on whether STR is valid."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/hidden#value
  ;;
  ;; We do nothing whatsoever here, since hidden input can be arbitrary and does
  ;; not accept a required attribute.
  str)

(defun html-password (str &key required pattern-matcher min-length max-length)
  "Function html-password

Syntax:

html-password str &key required pattern-matcher min-length max-length => result

Arguments and Values:

str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil

Description:

Validates an HTML password input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/password#value
  ;;
  ;; Password input behaves identically to text input for the purpose of
  ;; validation. See html-text.
  (html-text str
             :required required
             :pattern-matcher pattern-matcher
             :min-length min-length
             :max-length max-length))

(defun html-radio (str &key required)
  "Function html-radio

Syntax:

html-radio str &key required => result

Arguments and Values:

str---a string designator

required---a boolean

result---str or nil

Description:

Validates an HTML ratio input value STR. Returns either STR or nil depending
on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/radio#value
  ;;
  ;; Not much here either, since the value is either empty or an arbitrary string.
  (if required
      (when (not (string= str ""))
        str)
      str))

(defun html-text (str &key required pattern-matcher min-length max-length)
  "Function html-text

Syntax:

html-text str &key required pattern-matcher min-length max-length => result

Arguments and Values:

str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil

Description:

Validates an HTML text input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/text#value
  (flet ((valid-text ()
           (when (and (if min-length (>= (length str) min-length) t)
                      (if max-length (<= (length str) max-length) t)
                      (if pattern-matcher (funcall pattern-matcher str) t))
             str)))
    (if required
        (when (not (string= str ""))
          (valid-text))
        (valid-text))))

;; ------------------------------------------------------------------------------

(defun html5-email (str &key
                          required
                          pattern-matcher
                          min-length
                          max-length
                          multiple)
  "Function html5-email

Syntax:

html5-email str &key required pattern-matcher min-length max-length multiple => result

Arguments and Values:

str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

multiple---a boolean

result---str local-part domain or (str local-part domain)* or nil

Description:

Validates an HTML5 email input value STR. When STR is a valid email address
will return STR as well as the local-part and domain. Otherwise returns nil,
nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid.

When MULTIPLE is t, expects STR to be a comma-separated list of email addresses.
Will validate each, sequentially, and return a list of lists. Each list within
the list is either nil when an email address fails validation, or contains
the email address, its local part, and its domain. Validation of each email
address is subject to the other specified parameters. When MULTIPLE is either
nil or not specified, any STR containing multiple email addresses will not
pass validation."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/email#value
  (labels ((valid-email ()
             (when (and (if min-length (>= (length str) min-length) t)
                        (if max-length (<= (length str) max-length) t)
                        (if pattern-matcher (funcall pattern-matcher str) t))
               (if (string= str "")
                   (values "" nil nil)
                   (multiple-value-bind (local-part domain)
                       (email-parse:parse str)
                     (when local-part
                       (values str local-part domain))))))
           (valid-emails ()
             (mapcar (lambda (email)
                       (when (and (if min-length (>= (length email) min-length) t)
                                  (if max-length (<= (length email) max-length) t)
                                  (if pattern-matcher (funcall pattern-matcher email) t))
                         (multiple-value-bind (local-part domain)
                             (email-parse:parse email)
                           (when local-part
                             (list email local-part domain)))))
                     (mapcar #'trim-whitespace
                             (split-sequence #\, str)))))
    (if required
        (if (string= str "")
            (values nil nil nil)
            (if multiple
                (valid-emails)
                (valid-email)))
        (if multiple
            (valid-emails)
            (valid-email)))))

(defun html5-search (str &key required pattern-matcher min-length max-length)
  "Function html5-search

Syntax:

html5-search str &key required pattern-matcher min-length max-length => result

Arguments and Values:

str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil

Description:

Validates an HTML5 search input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/search#value
  ;;
  ;; Search input behaves identically to text input for the purpose of
  ;; validation. See html-text.
  (html-text str
             :required required
             :pattern-matcher pattern-matcher
             :min-length min-length
             :max-length max-length))

(defun html5-tel (str &key required pattern-matcher min-length max-length)
  "Function html5-tel

Syntax:

html5-tel str &key required pattern-matcher min-length max-length => result

Arguments and Values:

str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil

Description:

Validates an HTML5 tel input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/tel#value
  ;;
  ;; Tel input behaves identically to text input for the purpose of
  ;; validation. See html-text.
  (html-text str
             :required required
             :pattern-matcher pattern-matcher
             :min-length min-length
             :max-length max-length))

(defun html5-url (str &key
                        required
                        pattern-matcher
                        min-length
                        max-length)
  "Function html5-url

Syntax:

html5-url str &key required pattern-matcher min-length max-length => result

Arguments and Values:

str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str scheme userinfo host port path query fragment or nil nil nil nil nil nil nil nil

Description:

Validates an HTML5 url input value STR. When STR is a valid url, returns
STR, as well as its scheme, userinfo, host, port, path, query, and fragment.
Otherwise returns nil, nil, nil, nil, nil, nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/url#value
  (labels ((valid-url ()
             (when (and (if min-length (>= (length str) min-length) t)
                        (if max-length (<= (length str) max-length) t)
                        (if pattern-matcher (funcall pattern-matcher str) t))
               (if (string= str "")
                   (values "" nil nil nil nil nil nil nil)
                   (multiple-value-bind (scheme
                                         userinfo
                                         host
                                         port
                                         path
                                         query
                                         fragment)
                       (uri-parse:parse str)
                     ;; MDN doesn't appear to agree with RFC 3986 as to what
                     ;; constitutes an absolute URL/URI. Nevertheless, this
                     ;; should confirm those of the format urlscheme://restofurl.
                     (when scheme
                       (values str
                               scheme
                               userinfo
                               host
                               port
                               path
                               query
                               fragment)))))))
    (if required
        (if (string= str "")
            (values nil nil nil nil nil nil nil nil)
            (valid-url))
        (valid-url))))

(defun html5-number (str &key required min max)
  "Function html5-number

Syntax:

html5-number str &key required min max => result

Arguments and Values:

str---a string designator

required---a boolean

min---an integer or a float

max---an integer or a float

result---str str-numerical-value or nil nil

Description:

Validates an HTML5 number input value STR. Returns either STR and its numerical
value or nil nil depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a numerical value that
is not at least MIN and at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/number#value
  (flet ((valid-numeric ()
           (let ((numeric-value (numeric-value str min max)))
             (if numeric-value
                 (values str numeric-value)
                 (values nil nil)))))
    (if required
        (if (string= str "")
            (values nil nil)
            (valid-numeric))
        (if (string= str "")
            (values "" nil)
            (valid-numeric)))))

(defun html5-range (str &key min max)
  "Function html5-range

Syntax:

html5-range str &key min max => result

Arguments and Values:

str---a string designator

min---an integer or a float

max---an integer or a float

result---str str-numerical-value or nil nil

Description:

Validates an HTML5 range input value STR. Returns either STR and its numerical
value or nil nil depending on whether STR is valid.

When MIN or MAX are specified, will reject any STR with a numerical value that
is not at least MIN and at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/range#value
  (flet ((valid-range ()
           (let ((numeric-value (numeric-value str min max)))
             (if numeric-value
                 (values str numeric-value)
                 (values nil nil)))))
    (if (string= str "")
        (values nil nil)
        (valid-range))))

(defun html5-datetime-local (str &key required min max)
  "Function html5-datetime-local

Syntax:

html5-datetime-local str &key required min max => result

Arguments and Values:

str---a string designator

required---a boolean

min---a string

max---a string

result---str year month day hour minute seconds or nil nil nil nil nil nil nil

Description:

Validates an HTML5 datetime-local input value STR. Returns either STR, year,
month, day, hour, minute, (optionally) seconds or nil, nil, nil, nil, nil,
nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a datetime-local value
that, chronologically, is not at least MIN and at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/datetime-local#value
  (labels ((parse-date (str)
             (let ((delimiter (position #\T str)))
               (when delimiter
                 (subseq str 0 delimiter))))
           (parse-time (str)
             (let ((delimiter (position #\T str)))
               (when delimiter
                 (subseq str (1+ delimiter)))))
           (parse-datetime-local (str)
             (let ((date (parse-date str))
                   (time (parse-time str)))
               (when (and date time)
                 (multiple-value-bind (valid-date yyyy* mmonth dd)
                     (html-date date)
                   (multiple-value-bind (valid-time hh mmin ss)
                       (html5-time time)
                     (when (and valid-date valid-time)
                       (if ss
                           (return-from parse-datetime-local (values str yyyy* mmonth dd hh mmin ss))
                           (return-from parse-datetime-local (values str yyyy* mmonth dd hh mmin nil))))))))
             (values nil nil nil nil nil nil nil))
           (datetime-local<= (dt1 dt2)
             (let ((dt1-valid (parse-datetime-local dt1))
                   (dt2-valid (parse-datetime-local dt2)))
               (when (and dt1-valid dt2-valid)
                 (let ((dt1-date (parse-date dt1))
                       (dt1-time (parse-time dt1))
                       (dt2-date (parse-date dt2))
                       (dt2-time (parse-time dt2)))
                   (or (date< dt1-date dt2-date)
                       (and (date= dt1-date dt2-date)
                            (time<= dt1-time dt2-time)))))))
           (datetime-local>= (dt1 dt2)
             (datetime-local<= dt2 dt1))
           (valid-datetime-local ()
             (cond ((and min max) (when (and (datetime-local>= str min)
                                             (datetime-local<= str max))
                                    (return-from valid-datetime-local
                                      (parse-datetime-local str))))
                   (min (when (datetime-local>= str min)
                          (return-from valid-datetime-local
                            (parse-datetime-local str))))
                   (max (when (datetime-local<= str max)
                          (return-from valid-datetime-local
                            (parse-datetime-local str))))
                   (t (return-from valid-datetime-local
                        (parse-datetime-local str))))
             (values nil nil nil nil nil nil nil)))
    (if required
        (if (string= str "")
            (values nil nil nil nil nil nil nil)
            (valid-datetime-local))
        (if (string= str "")
            (values "" nil nil nil nil nil nil)
            (valid-datetime-local)))))

(defun html5-month (str &key required min max)
  "Function html5-month

Syntax:

html5-month str &key required min max => result

Arguments and Values:

str---a string designator

required---a boolean

min---a string

max---a string

result---str year month or nil nil nil

Description:

Validates an HTML5 month input value STR. Returns either STR, year, month or
nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a month value
that, chronologically, is not at least MIN and at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/month#value
  (labels ((parse-month (str)
             (and (>= (length str) 7)
                  (let ((hyphen-pos (position #\- str)))
                    (and hyphen-pos
                         (= (- (length str) hyphen-pos) 3)
                         (let ((yyyy* (subseq str 0 hyphen-pos))
                               (mm (subseq str (+ hyphen-pos 1))))
                           (and (every #'digit-char-p yyyy*)
                                (every #'digit-char-p mm)
                                (let ((mm-i (parse-integer mm)))
                                  (when (and (>= mm-i 1) (<= mm-i 12))
                                    (return-from parse-month (values str yyyy* mm)))))))))
             (values nil nil nil))
           (month<= (month1 month2)
             (multiple-value-bind (valid-m1 yyyy*-m1 mm-m1)
                 (parse-month month1)
               (multiple-value-bind (valid-m2 yyyy*-m2 mm-m2)
                   (parse-month month2)
                 (when (and valid-m1 valid-m2)
                   (let ((yyyy*-m1-i (parse-integer yyyy*-m1))
                         (yyyy*-m2-i (parse-integer yyyy*-m2))
                         (mm-m1-i (parse-integer mm-m1))
                         (mm-m2-i (parse-integer mm-m2)))
                     (or (< yyyy*-m1-i yyyy*-m2-i)
                         (and (= yyyy*-m1-i yyyy*-m2-i)
                              (<= mm-m1-i mm-m2-i))))))))
           (month>= (month1 month2)
             (month<= month2 month1))
           (valid-month ()
             (cond ((and min max) (when (and (month>= str min)
                                             (month<= str max))
                                    (return-from valid-month
                                      (parse-month str))))
                   (min (when (month>= str min)
                          (return-from valid-month
                            (parse-month str))))
                   (max (when (month<= str max)
                          (return-from valid-month
                            (parse-month str))))
                   (t (return-from valid-month
                        (parse-month str))))
             (values nil nil nil)))
    (if required
        (if (string= str "")
            (values nil nil nil)
            (valid-month))
        (if (string= str "")
            (values "" nil nil)
            (valid-month)))))

(defun html5-week (str &key required min max)
  "Function html5-week

Syntax:

html5-week str &key required min max => result

Arguments and Values:

str---a string designator

required---a boolean

min---a string

max---a string

result---str year week or nil nil nil

Description:

Validates an HTML5 week input value STR. Returns either STR, year, week or
nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a week value
that, chronologically, is not at least MIN and at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/week#value
  (labels ((parse-week (str)
             (and (>= (length str) 8)
                  (let ((hyphen-pos (position #\- str)))
                    (and hyphen-pos
                         (= (- (length str) hyphen-pos) 4)
                         (eq #\W (char str (1+ hyphen-pos)))
                         (let ((yyyy* (subseq str 0 hyphen-pos))
                               (ww (subseq str (+ hyphen-pos 2))))
                           (and (every #'digit-char-p yyyy*)
                                (every #'digit-char-p ww)
                                (let ((ww-i (parse-integer ww)))
                                  (when (and (>= ww-i 1) (<= ww-i 53))
                                    (return-from parse-week (values str yyyy* ww)))))))))
             (values nil nil nil))
           (week<= (week1 week2)
             (multiple-value-bind (valid-w1 yyyy*-w1 ww-w1)
                 (parse-week week1)
               (multiple-value-bind (valid-w2 yyyy*-w2 ww-w2)
                   (parse-week week2)
                 (when (and valid-w1 valid-w2)
                   (let ((yyyy*-w1-i (parse-integer yyyy*-w1))
                         (yyyy*-w2-i (parse-integer yyyy*-w2))
                         (ww-w1-i (parse-integer ww-w1))
                         (ww-w2-i (parse-integer ww-w2)))
                     (or (< yyyy*-w1-i yyyy*-w2-i)
                         (and (= yyyy*-w1-i yyyy*-w2-i)
                              (<= ww-w1-i ww-w2-i))))))))
           (week>= (week1 week2)
             (week<= week2 week1))
           (valid-week ()
             (cond ((and min max) (when (and (week>= str min)
                                             (week<= str max))
                                    (return-from valid-week
                                      (parse-week str))))
                   (min (when (week>= str min)
                          (return-from valid-week
                            (parse-week str))))
                   (max (when (week<= str max)
                          (return-from valid-week
                            (parse-week str))))
                   (t (return-from valid-week
                        (parse-week str))))
             (values nil nil nil)))
    (if required
        (if (string= str "")
            (values nil nil nil)
            (valid-week))
        (if (string= str "")
            (values "" nil nil)
            (valid-week)))))

(defun html5-time (str &key required min max)
  "Function html5-time

Syntax:

html5-time str &key required min max => result

Arguments and Values:

str---a string designator

required---a boolean

min---a string

max---a string

result---str hour minute (optional) seconds or nil nil nil nil

Description:

Validates an HTML5 time input value STR. Returns either STR, hours, minutes,
(optional) seconds or nil, nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a time value
that, chronologically, is not at least MIN and at most MAX."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/time#time_value_format
  (cond ((and min max) (when (and (time>= str min)
                                  (time<= str max))
                         (return-from html5-time (validate-time str))))
        (min (when (time>= str min)
               (return-from html5-time (validate-time str))))
        (max (when (time<= str max)
               (return-from html5-time (validate-time str))))
        (t (return-from html5-time (validate-time str))))
  (values nil nil nil nil))

(defun html5-color (str)
  "Function html5-color

Syntax:

html5-color str => result

Arguments and Values:

str---a string designator

result---str hex red green blue or nil nil nil nil nil

Description:

Validates an HTML5 color input value STR. Returns either STR, STR's hexadecimal
value without the leading '#', red value, green value, blue value or nil, nil,
nil, nil, nil."
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/color#value
  (if (and (= (length str) 7)
           (eq #\# (char str 0))
           (every (lambda (ch)
                    (digit-char-p ch 16))
                  (subseq str 1)))
      (values str
              (subseq str 1)
              (subseq str 1 3)
              (subseq str 3 5)
              (subseq str 5 7))
      (values nil nil nil nil nil)))
