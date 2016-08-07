;;;; package.lisp --- Package definition for parser.common-rules system.
;;;;
;;;; Copyright (C) 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:parser.common-rules
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:esrap)

  ;; Anchor rules
  (:export
   #:<beginning-of-input> #:<end-of-input>
   #:<beginning-of-line>  #:<end-of-line>

   #:<same-line>)

  ;; Whitespace rules
  (:export
   #:whitespace/not-newline #:whitespace/not-newline?
   #:whitespace             #:whitespace?             #:whitespace+ #:whitespace*)

  ;; Comment rules
  (:export
   #:c-style-comment/rest-of-line #:c-style-comment/rest-of-line/trimmed
   #:c-style-comment/delimited    #:c-style-comment/delimited/trimmed

   #:shell-style-comment          #:shell-style-comment/trimmed

   #:lisp-style-comment           #:lisp-style-comment/trimmed)

  #+sbcl (:lock t)

  (:documentation
   "This package contains parsing rules and macros for common parsing
    tasks that are hopefully useful in many grammars."))
