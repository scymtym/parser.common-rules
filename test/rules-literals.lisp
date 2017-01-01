;;;; rules-literals.lisp --- Tests for literal rules.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.common-rules.test)

(def-suite :parser.common-rules.rules.literals
  :in :parser.common-rules)
(in-suite :parser.common-rules.rules.literals)

;;; Boolean literals

(define-rule-test boolean-literal/lower-case
  ;; Some matching inputs.
  ("true"  t)
  ("false" nil nil t)

  ;; Some non-matching inputs.
  ("other" nil))

(define-rule-test boolean-literal/capital-case
  ;; Some matching inputs.
  ("True"  t)
  ("False" nil nil t)

  ;; Some non-matching inputs.
  ("Other" nil))

(define-rule-test boolean-literal/extended
  ;; Some matching inputs.
  ("true"  t)
  ("t"     t)
  ("1"     t)
  ("false" nil nil t)
  ("f"     nil nil t)
  ("0"     nil nil t)

  ;; Some non-matching inputs.
  ("other" nil))

;;; Number literals

(define-rule-test integer-literal/octal/no-sign
  ;; Some matching inputs.
  ("0"   0)
  ("00"  0)
  ("1"   1)
  ("7"   7)
  ("10"  8)

  ;; Some non-matching inputs.
  ("8"   nil)
  ("-1"  nil))

(define-rule-test integer-literal/octal
  ;; Some matching inputs.
  ("0"   0)   ("-0"   0)
  ("00"  0)   ("-00"  0)
  ("1"   1)   ("-1"   -1)
  ("7"   7)   ("-7"   -7)
  ("10"  8)   ("-10"  -8)

  ;; Some non-matching inputs.
  ("8"   nil) ("-8"   nil)
  ("--1" nil))

(defvar *scientific*
  '((nil  .    1)
    ("e0" .    1)  ("e+0" .    1) ("e-0" .  1)
    ("e0" .    1)  ("e+0" .    1) ("e-0" .  1)
    ("e1" .   10)  ("e+1" .   10) ("e-1" . 1/10)
    ("e3" . 1000)  ("e+3" . 1000) ("e-3" . 1/1000)))

(test rule.float-literal
  "Smoke test for the `float-literal' rule."

  (mapc
   (lambda+ ((input expected))
     (let+ (((&flet do-it (&optional (input input))
               (esrap:parse 'float-literal/rational input))))
       (case expected
         (error
          (signals esrap:esrap-parse-error (do-it)))
         (t
          (loop :for (scientific . factor) :in *scientific* :do
             (let ((input    (if scientific
                                 (concatenate 'string input scientific)
                                 input))
                   (expected (* factor expected)))
               (is (equal expected (do-it input)))))))))

   `(("-1."    -1)
     ("+1."    1)
     ("1."     1)
     ("+.5"    1/2)
     ("-.5"    -1/2)
     (".5"     1/2)
     ("+0.5"   1/2)
     ("-0.5"   -1/2)
     ("0.5"    1/2)
     ("+0.001" 1/1000)
     ("-0.001" -1/1000)
     ("0.001"  1/1000))))

(define-rule-test number-literal
  ;; Some matching inputs.
  ("0"      0)
  ("1"      1)
  ("01"     1)
  ("9"      9)
  ("+9"     9)     ("+ 9"    9)
  ("-9"    -9)     ("- 9"   -9)

  ("0x0"    0)
  ("0x1"    1)
  ("0x01"   1)
  ("0xf"    15)
  ("+0xf"   15)    ("+ 0xf"  15)
  ("-0xf"  -15)    ("- 0xf" -15)

  ("0o0"    0)
  ("0o1"    1)
  ("0o01"   1)
  ("0o7"    7)
  ("+0o7"   7)     ("+ 0o7"  7)
  ("-0o7"  -7)     ("- 0o7" -7)

  (".1"      .1f0)
  ("+.1"     .1f0) ("+ .1"    .1f0)
  ("-.1"   -.1f0)  ("- .1"  -.1f0)
  ("1.0"    1.0f0)
  ("+1.0"   1.0f0) ("+ 1.0"  1.0f0)
  ("-1.0"  -1.0f0) ("- 1.0" -1.0f0)

  ;; Some non-matching inputs.
  ("++1"   nil)
  ("--1"   nil)
  ("+-1"   nil)
  ("1.-1"  1.0f0 2)
  ("1.0x1" 1.0f0 3)
  ("0x1.1" 1     3))

(test rule.number-literal.random
  "Random testing of the `number-literal' rule."

  ;; Integers in different bases.
  (for-all ((n         (gen-integer))
            (space     (gen-one-element nil " "))
            (plus-sign (gen-one-element nil #\+))
            (base      (gen-one-element 8 10 16)))
    (let ((input (format nil "~:[~*~;~:*~A~@[~A~]~]~@[~A~]~VR"
                         (case (signum n)
                           (-1 "-")
                           (1  plus-sign))
                         space
                         (case base
                           (8  "0o")
                           (16 "0x"))
                         base (abs n))))
      (is (eql n (esrap:parse 'number-literal input)))))

  ;; Floats.
  (for-all ((n (gen-float :type 'single-float)))
    (let ((input (nsubstitute #\e #\f (with-standard-io-syntax
                                        (prin1-to-string n)))))
      (is (eql n (esrap:parse 'number-literal input))))
    (let ((input (with-standard-io-syntax (format nil "~F" n))))
      (is (eql n (esrap:parse 'number-literal input))))))

;;; String literals

(defun make-expected-escape-sequence-string ()
  (format nil "~{~C~}" '(#\Bel #\Backspace #\Page #\Newline #\Return #\Tab
                         #\Vt)))

(define-rule-test string-literal/single-quotes
  ("'bla\\020 \\' \" \\\\ '" (format nil "bla~C ' \" \\ " #\Dle))
  ("'\\x20'"                 " ")
  ("'\\a\\b\\f\\n\\r\\t\\v'" (make-expected-escape-sequence-string)))

(define-rule-test string-literal/double-quotes
  ("\"bla\\020 \\\" ' \\\\ \"" (format nil "bla~C \" ' \\ " #\Dle))
  ("\"\\x20\""                 " ")
  ("\"\\a\\b\\f\\n\\r\\t\\v\"" (make-expected-escape-sequence-string)))

(define-rule-test string-literal/triple-quotes
  ("'''bla\\020 \\' \" \\\\ '''" "bla\\020 \\' \" \\\\ ")
  ("'''\\x20'''"                 "\\x20")
  ("'''\\a\\b\\f\\n\\r\\t\\v'''" "\\a\\b\\f\\n\\r\\t\\v"))

(define-rule-test string-literal/sextuple-quotes
  ("\"\"\"bla\\020 \\\" ' \\\\ \"\"\"" "bla\\020 \\\" ' \\\\ ")
  ("\"\"\"\\x20\"\"\""                 "\\x20")
  ("\"\"\"\\a\\b\\f\\n\\r\\t\\v\"\"\"" "\\a\\b\\f\\n\\r\\t\\v"))
