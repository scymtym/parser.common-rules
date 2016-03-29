;;;; macros-operators.lisp --- Tests for the operator grammar macros.
;;;;
;;;; Copyright (C) 2013, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.common-rules.operators.test)

(def-suite :parser.common-rules.operators.macros
    :in :parser.common-rules.operators)
(in-suite :parser.common-rules.operators.macros)

(test define-unary-operator.prefix
  "Test the `define-unary-operator-rule' macro for prefix direction."

  (define-unary-operator-rule minus
      #\- (digit-char-p character)
      :fixity                :prefix
      :skippable?-expression (* #\Space))

  (architecture.builder-protocol:with-builder ('list)
    (parses-are (minus)
      ;; Some matching inputs.
      ("1"   #\1
             nil)
      ("-1"  '(:unary-operator
               (:operand ((#\1)))
               :operator "-" :bounds (0 . 2))
             nil)
      ("- 1" '(:unary-operator
               (:operand ((#\1)))
               :operator "-" :bounds (0 . 3))
             nil)

      ;; Some non-matching inputs.
      ("1-"  #\1 1)
      ("1 -" #\1 1))))

(test define-unary-operator.postfix
  "Test the `define-unary-operator-rule' macro for postfix direction."

  (define-unary-operator-rule plusplus
      "++" (digit-char-p character)
      :fixity                :postfix
      :skippable?-expression (* #\Space))

  (architecture.builder-protocol:with-builder ('list)
    (parses-are (plusplus)
      ;; Some matching inputs.
      ("1"    #\1
              nil)
      ("1++"  '(:unary-operator
                (:operand ((#\1)))
                :operator "++" :bounds (0 . 3))
              nil)
      ("1 ++" '(:unary-operator
                (:operand ((#\1)))
                :operator "++" :bounds (0 . 4))
              nil)

      ;; Some non-matching inputs.
      ("++1"  nil)
      ("++ 1" nil))))

(test define-binary-operator.smoke
  "Smoke test for the `define-binary-operator-rule' macro."

  (define-binary-operator-rule plus
      #\+ (digit-char-p character)
      :skippable?-expression (* #\Space))

  (architecture.builder-protocol:with-builder ('list)
    (parses-are (plus)
      ;; Some matching inputs.
      ("1"     #\1)
      ("1+2"   '(:binary-operator
                 (:operand ((#\1) (#\2)))
                 :operator "+" :bounds (0 . 3)))
      ("1+2+3" '(:binary-operator
                 (:operand (((:binary-operator
                              (:operand ((#\1) (#\2)))
                              :operator "+" :bounds (0 . 3)))
                            (#\3)))
                 :operator "+" :bounds (0 . 5)))

      ;; Some non-matching inputs.
      ("1+"    #\1 1 t)
      ("+1"    nil)
      ("1++2"  #\1 1 t))))

(test define-operators.syntax-errors
  "Test errors signaled for syntactically invalid
   `define-operator-rules' forms."

  (flet ((test-case (form)
           (signals error (eval form))))
    ;; Missing operators and leaf expression.
    (test-case '(define-operator-rules (:skippable?-expression #\Space)))
    ;; Missing operators.
    (test-case '(define-operator-rules (:skippable?-expression #\Space)
                  leaf))

    ;; Invalid arity.
    (test-case '(define-operator-rules (:skippable?-expression #\Space)
                  (0 foo "+")
                  leaf))
    ;; Invalid associativity.
    (test-case '(define-operator-rules (:skippable?-expression #\Space)
                  (2 foo "+" :associativity :foo)
                  leaf))))

(test define-operators.smoke
  "Smoke test for the `define-operators' macro."

  (define-operator-rules (:skippable?-expression (* #\Space))
    (2 assign ":=" :associativity :none)
    (2 term   "+")
    (2 factor "*")
    (2 expon  "^" :associativity :right)
    (1 neg    "-")
    (1 inc    "++" :fixity :postfix)
    (digit-char-p character))

  (architecture.builder-protocol:with-builder ('list)
    (parses-are (assign)
      ;; Some matching inputs.
      ("1"          #\1)
      ("1:=2"       '(:binary-operator
                      (:operand ((#\1) (#\2)))
                      :operator ":=" :bounds (0 . 4)))
      ("1:=2+3^4^5" '(:binary-operator
                      (:operand ((#\1)
                                 ((:binary-operator
                                   (:operand ((#\2)
                                              ((:binary-operator
                                                (:operand ((#\3)
                                                           ((:binary-operator
                                                             (:operand ((#\4) (#\5)))
                                                             :operator "^" :bounds (7 . 10)))))
                                                :operator "^" :bounds (5 . 10)))))
                                   :operator "+" :bounds (3 . 10)))))
                      :operator ":=" :bounds (0 . 10)))
      ("2+5*1++"   '(:binary-operator
                     (:operand ((#\2)
                                ((:binary-operator
                                  (:operand ((#\5)
                                             ((:unary-operator
                                               (:operand ((#\1)))
                                               :operator "++" :bounds (4 . 7)))))
                                  :operator "*" :bounds (2 . 7)))))
                     :operator "+" :bounds (0 . 7)))

      ;; Some non-matching inputs.
      ("1:=2:=3"   '(:binary-operator
                     (:operand ((#\1) (#\2)))
                     :operator ":=" :bounds (0 . 4))
                   4 t))))

(test define-operators.parentheses
  "Smoke test added support for parenthesis to the output of the
   `define-operators' macro."

  (define-operator-rules (:skippable?-expression (* #\Space))
    (2 assign ":=" :associativity :none)
    (2 term   "+")
    (2 factor "*")
    (2 expon  "^" :associativity :right)
    (1 neg    "-")
    (1 inc    "++" :fixity :postfix)
    (or parentheses (digit-char-p character)))

  (esrap:defrule parentheses
      (and #\( assign #\))
    (:function second))

  (architecture.builder-protocol:with-builder ('list)
    (parses-are (assign)
      ;; Some matching inputs.
      ("1"           #\1)
      ("(1)"         #\1)
      ("((1))"       #\1)
      ("(1:=2)*3"    '(:binary-operator
                       (:operand (((:binary-operator
                                    (:operand ((#\1) (#\2)))
                                    :operator ":=" :bounds (1 . 5)))
                                  (#\3)))
                       :operator "*" :bounds (0 . 8)))
      ("1*(2+5)"     '(:binary-operator
                       (:operand ((#\1)
                                  ((:binary-operator
                                    (:operand ((#\2) (#\5)))
                                    :operator "+" :bounds (3 . 6)))))
                       :operator "*" :bounds (0 . 7)))
      ("((2+5)^1)^2" '(:binary-operator
                       (:operand (((:binary-operator
                                    (:operand (((:binary-operator
                                                 (:operand ((#\2) (#\5)))
                                                 :operator "+" :bounds (2 . 5)))
                                               (#\1)))
                                    :operator "^" :bounds (1 . 8)))
                                  (#\2)))
                       :operator "^" :bounds (0 . 11)))
      ("(1)++++"      '(:unary-operator
                        (:operand (((:unary-operator
                                     (:operand ((#\1)))
                                     :operator "++" :bounds (0 . 5)))))
                        :operator "++" :bounds (0 . 7)))

      ;; Some non-matching inputs.
      ("(1"          nil)
      ("1)"          #\1 1 t)
      ("1(+2)"       #\1 1 t))))
