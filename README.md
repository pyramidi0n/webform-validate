# Webform Validate

A validation library for web form input.

## Table of Contents

1. [Overview](#overview)
2. [Installation](#installation)
3. [Usage](#usage)
4. [Links](#links)
5. [Patches](#patches)
6. [License](#license)

## Overview

This library attempts to comply with the standards and MDN documentation for
HTML and HTML5 web form input. It leverages other standards-compliant libraries
to perform email and url validation.

The intended use case is for server-side validation of untrusted input strings.
Right now, the validation is merely of syntax - the library does not provide
validation for things like email deliverability, and so forth. Eventually, this
sort of thing may be added.

## Installation

Webform Validate is available on [Ultralisp](https://ultralisp.org/) and is easy
to install using [Quicklisp](https://www.quicklisp.org/beta/).

Add the Ultralisp repository:

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/")
```

Install Email Parse:

```lisp
CL-USER> (ql:quickload :webform-validate)
```

## Usage

The library provides a suite of functions, each of which corresponds to a type
of HTML or HTML5 form input:
[html-checkbox](#html-checkbox)

[html-date](#html-date)

[html-file](#html-file)

[html-hidden](#html-hidden)

[html-password](#html-password)

[html-radio](#html-radio)

[html-text](#html-text)

[html5-email](#html5-email)
[html5-search](#html5-search)
[html5-tel](#html5-tel)
[html5-url](#html5-url)
[html5-number](#html5-number)
[html5-range](#html5-range)
[html5-datetime-local](#html5-datetime-local)
[html5-month](#html5-month)
[html5-week](#html5-week)
[html5-time](#html5-time)
[html5-color](#html5-color)

These functions always accept at least one string parameter, and may include
keyword parameters relevant to the type of input.

### html-checkbox

*Function* **HTML-CHECKBOX**

**Syntax:**

```
html-checkbox str &key required => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

result---str or nil
```

**Description:**

Validates an HTML checkbox input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

### html-date

*Function* **HTML-DATE**

**Syntax:**

```
html-date str &key required min max => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

min---a string designator

max---a string designator

result---str year month day or nil nil nil nil
```

**Description:**

Validates an HTML date input value STR. When STR is a valid date, returns
STR and the components of the date: year, month, day. Otherwise returns nil,
nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any date STR that, chronologically,
is not at least MIN or at most MAX.

### html-file

*Function* **HTML-FILE**

**Syntax:**

```
html-file str &key required accept multiple => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

accept---a string designator

multiple---a boolean

result---str or nil
```

**Description:**

Validates an HTML file input value STR. Returns either STR or nil depending
 on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

For now, ignores the ACCEPT and MULTIPLE parameters.

### html-hidden

*Function* **HTML-HIDDEN**

**Syntax:**

```
html-hidden str => result
```

**Arguments and Values:**

```
str---a string designator

result---str or nil
```

**Description:**

Validates an HTML hidden input value STR. Returns either STR or nil depending
on whether STR is valid.

### html-password

*Function* **HTML-PASSWORD**

**Syntax:**

```
html-password str &key required pattern-matcher min-length max-length => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil
```

**Description:**

Validates an HTML password input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned.

### html-radio

*Function* **HTML-RADIO**

**Syntax:**

```
html-radio str &key required => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

result---str or nil
```

**Description:**

Validates an HTML ratio input value STR. Returns either STR or nil depending
on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

### html-text

*Function* **HTML-TEXT**

**Syntax:**

```
html-text str &key required pattern-matcher min-length max-length => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil
```

**Description:**

Validates an HTML text input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned.

### html5-email

*Function* **HTML5-EMAIL**

**Syntax:**

```
html5-email str &key required pattern-matcher min-length max-length multiple => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

multiple---a boolean

result---str local-part domain or (str local-part domain)* or nil
```

**Description:**

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
pass validation.

### html5-search

*Function* **HTML5-SEARCH**

**Syntax:**

```
html5-search str &key required pattern-matcher min-length max-length => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil
```

**Description:**

Validates an HTML5 search input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned.

### html5-tel

*Function* **HTML5-TEL**

**Syntax:**

```
html5-tel str &key required pattern-matcher min-length max-length => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str or nil
```

**Description:**

Validates an HTML5 tel input value STR. Returns either STR or nil
depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid and return nil.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid and nil is returned.

### html5-url

*Function* **HTML5-URL**

**Syntax:**

```
html5-url str &key required pattern-matcher min-length max-length => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

pattern-matcher---a predicate of one string argument

min-length---an integer

max-length---an integer

result---str scheme userinfo host port path query fragment or nil nil nil nil nil nil nil nil
```

**Description:**

Validates an HTML5 url input value STR. When STR is a valid url, returns
STR, as well as its scheme, userinfo, host, port, path, query, and fragment.
Otherwise returns nil, nil, nil, nil, nil, nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN-LENGTH or MAX-LENGTH are specified, will reject any STR with a length
that is not at least MIN-LENGTH and at most MAX-LENGTH.

A PATTERN-MATCHER is a predicate of one string argument that will be matched
against STR during validation; when its result is nil, STR is rejected as
invalid.

### html5-number

*Function* **HTML5-NUMBER**

**Syntax:**

```
html5-number str &key required min max => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

min---an integer or a float

max---an integer or a float

result---str str-numerical-value or nil nil
```

**Description:**

Validates an HTML5 number input value STR. Returns either STR and its numerical
value or nil nil depending on whether STR is valid.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a numerical value that
is not at least MIN and at most MAX.

### html5-range

*Function* **HTML5-RANGE**

**Syntax:**

```
html5-range str &key min max => result
```

**Arguments and Values:**

```
str---a string designator

min---an integer or a float

max---an integer or a float

result---str str-numerical-value or nil nil
```

**Description:**

Validates an HTML5 range input value STR. Returns either STR and its numerical
value or nil nil depending on whether STR is valid.

When MIN or MAX are specified, will reject any STR with a numerical value that
is not at least MIN and at most MAX.

### html5-datetime-local

*Function* **HTML5-DATETIME-LOCAL**

**Syntax:**

```
html5-datetime-local str &key required min max => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

min---a string

max---a string

result---str year month day hour minute seconds or nil nil nil nil nil nil nil
```

**Description:**

Validates an HTML5 datetime-local input value STR. Returns either STR, year,
month, day, hour, minute, (optionally) seconds or nil, nil, nil, nil, nil,
nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a datetime-local value
that, chronologically, is not at least MIN and at most MAX.

### html5-month

*Function* **HTML5-MONTH**

**Syntax:**

```
html5-month str &key required min max => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

min---a string

max---a string

result---str year month or nil nil nil
```

**Description:**

Validates an HTML5 month input value STR. Returns either STR, year, month or
nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a month value
that, chronologically, is not at least MIN and at most MAX.

### html5-week

*Function* **HTML5-WEEK**

**Syntax:**

```
html5-week str &key required min max => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

min---a string

max---a string

result---str year week or nil nil nil
```

**Description:**

Validates an HTML5 week input value STR. Returns either STR, year, week or
nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a week value
that, chronologically, is not at least MIN and at most MAX.

### html5-time

*Function* **HTML5-TIME**

**Syntax:**

```
html5-time str &key required min max => result
```

**Arguments and Values:**

```
str---a string designator

required---a boolean

min---a string

max---a string

result---str hour minute (optional) seconds or nil nil nil nil
```

**Description:**

Validates an HTML5 time input value STR. Returns either STR, hours, minutes,
(optional) seconds or nil, nil, nil, nil.

When REQUIRED is specified, will reject an empty STR as invalid.

When MIN or MAX are specified, will reject any STR with a time value
that, chronologically, is not at least MIN and at most MAX.

### html5-color

*Function* **HTML5-COLOR**

**Syntax:**

```
html5-color str => result
```

**Arguments and Values:**

```
str---a string designator

result---str hex red green blue or nil nil nil nil nil
```

**Description:**

Validates an HTML5 color input value STR. Returns either STR, STR's hexadecimal
value without the leading '#', red value, green value, blue value or nil, nil,
nil, nil, nil.

## Links

* [Repository](https://sr.ht/~pyramidion/webform-validate/)

## Patches

Patches are welcome.

## License

Webform Validate is licensed under the two-clause BSD license.

See LICENSE.
