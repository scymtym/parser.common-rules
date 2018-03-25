;;;; macros-tokenization.lisp --- Tests for tokenization macros.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.common-rules.test)

(def-suite* :parser.common-rules.macros.tokenization
  :in :parser.common-rules)

(test macro.defrule/s.smoke
  ;; Mock "skippable" support rules.
  (esrap:defrule defrule/s.test.skippable?
      (* #\Space)
    (:text t))
  (esrap:defrule defrule/s.test.skippable
      (and #\Space defrule/s.test.skippable?)
    (:text t))

  ;; Main rules.
  (defrule/s (defrule/s.test
              :skippable-expression  defrule/s.test.skippable
              :skippable?-expression defrule/s.test.skippable?)
      "foo")

  ;; Test variants.
  (parses-are (defrule/s.test)
    ("foo"  "foo" nil)
    ("foo " "foo" 3))

  (parses-are (defrule/s.test/s)
    ("foo"  nil)
    ("foo " "foo" nil))

  (parses-are (defrule/s.test/?s)
    ("foo"  "foo" nil)
    ("foo " "foo" nil)))
