;;;; macros.lisp --- Macros for operators.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:parser.common-rules.operators)

;;; Runtime support
;;;
;;; These function are called by the generated operator rules to
;;; compute the production of the respective rule.

(macrolet
    ((define-unary-operator-production->node (name structure)
       `(defun ,name (production start end)
          (destructuring-bind ,structure production
            (declare (ignore s))
            (if operator
                (architecture.builder-protocol:node*
                    (:unary-operator :operator operator
                                     :bounds   (cons start end))
                  (* :operand (list operand)))
                operand)))))
  (define-unary-operator-production->node
      %unary-operator-production->node/prefix ((operator s) operand))
  (define-unary-operator-production->node
      %unary-operator-production->node/postfix (operand (s operator))))

(defun %binary-operator-production->node (production start end)
  (destructuring-bind (left (s1 operator s2) right)  production
    (declare (ignore s1 s2))
    (if operator
        (architecture.builder-protocol:node*
            (:binary-operator :operator operator
                              :bounds   (cons start end))
          (* :operand (list left right)))
        right)))

;;; Unary and binary operators

(defun make-unary-operator-expression (operator-expression fixity
                                       name next skippable?-expression)
  (let+ (((&flet operator-expression ()
            (ecase fixity
              (:prefix  `(and (and ,operator-expression ,skippable?-expression)
                              ,name))
              (:postfix `(and ,name
                              (and ,skippable?-expression ,operator-expression))))))
         ((&flet fallthrough-expression ()
            (ecase fixity
              (:prefix  `(and (and (and) (and)) ,next))
              (:postfix `(and ,next (and (and) (and))))))))
    `(or ,(operator-expression) ,(fallthrough-expression))))

(defmacro define-unary-operator-rule
    (name operator-expression next
     &key
     (fixity                :prefix)
     (skippable?-expression (skippable-rule-for-name 'skippable? name))
     (definer               'defrule))
  "Define a rule NAME for parsing an unary operator expressions with
   operator OPERATOR-EXPRESSION and operand NEXT.

   FIXITY has to be one of

   :prefix

     Generate a prefix operator, i.e.

       (and OPERATOR-EXPRESSION SKIPPABLE?-EXPRESSION NEXT)

   :postfix

     Generate a postfix operator, i.e.

       (and NEXT SKIPPABLE?-EXPRESSION OPERATOR-EXPRESSION)

   If supplied, SKIPPABLE?-EXPRESSION is the expression to be used for
   parsing skippable input (usually whitespace) between
   OPERATOR-EXPRESSION and NEXT. If SKIPPABLE?-EXPRESSION is not
   supplied, a rule whose name is

     (find-symbol (string '#:skippable?) (symbol-package OPERATOR-NAME))

   is used.

   If supplied, DEFINER names the macro that should be used to define
   the rule. Otherwise `esrap:defrule' is used."
  `(,definer ,name
       ,(make-unary-operator-expression
         operator-expression fixity name next skippable?-expression)
     (:lambda (production &bounds start end)
       (,(ecase fixity
           (:prefix  '%unary-operator-production->node/prefix)
           (:postfix '%unary-operator-production->node/postfix))
        production start end))))

(defun make-binary-operator-expression (operator-expression associativity
                                        name next skippable?-expression)
  (let+ (((&flet operator-expression ()
            (let ((operator-expression
                   `(and ,skippable?-expression
                         ,operator-expression
                         ,skippable?-expression)))
              (ecase associativity
                (:none        `(and ,next ,operator-expression ,next))
                (:left        `(and ,name ,operator-expression ,next))
                (:right       `(and ,next ,operator-expression ,name))
                (:associative `(and ,name ,operator-expression ,next))))))
         ((&flet fallthrough-expression ()
            `(and (and) (and (and) (and) (and)) ,next))))
    `(or ,(operator-expression)
         ,(fallthrough-expression))))

(defmacro define-binary-operator-rule
    (name operator-expression next
     &key
     (associativity         :left)
     (skippable?-expression (skippable-rule-for-name 'skippable? name))
     (definer               'defrule))
  "Define a rule NAME for parsing a binary operator expressions with
   operator OPERATOR-EXPRESSION and operands NEXT.

   ASSOCIATIVITY has to be one of

   :none

     The defined binary operator will be non-associative, i.e. for an
     OPERATOR-EXPRESSION \":=\", the expressions x:=y:=z will not be
     syntatically legal.

   :left

     The defined binary operator will associate to the left, i.e.
     x+y+z will be parsed as (x+y)+z.

   :right

     The defined binary operator will associate to the right, i.e.
     x^y^z will be parsed as x^(y^z).

   :associative

     The defined binary operator will associate to the left (but this
     should not be relied upon).

   If supplied, SKIPPABLE?-EXPRESSION is the expression to be used for
   parsing skippable input (usually whitespace) between
   OPERATOR-EXPRESSION and NEXT. If SKIPPABLE?-EXPRESSION is not
   supplied, a rule whose name is

     (find-symbol (string '#:skippable?) (symbol-package OPERATOR-NAME))

   is used.

   If supplied, DEFINER names the macro that should be used to define
   the rule. Otherwise `esrap:defrule' is used."
  `(,definer ,name
       ,(make-binary-operator-expression
         operator-expression associativity name next skippable?-expression)
     (:lambda (production &bounds start end)
       (%binary-operator-production->node production start end))))

;;; Operator precedence

(defmacro define-operator-rules ((&key skippable?-expression) &body clauses)
  "Define rules for parsing infix operators according to CLAUSES.

   The order of clauses in CLAUSES determines the precedence of
   operators:

     (define-operator-rules ()
       OPERATOR-WITH-LOWEST-BINDING-POWER
       ⋮
       OPERATOR-WITH-HIGHEST-BINDING-POWER
       LEAF-EXPRESSION)

   All but the final clause in CLAUSES are of the form

     (ARITY RULE-NAME OPERATOR-EXPRESSION &rest ARGS &key)

   where

   * ARITY is the number of operands accepted by the operator
     being defined. The ARITY must be either 1 or 2.

   * RULE-NAME is the name of the rule generated for the operator.

   * OPERATOR-EXPRESSION is an expression for parsing the operator
     token, e.g. #\\* for multiplication.

   * ARGS can be any of the keyword arguments accepted by
     `define-unary-operator-rule' or `define-binary-operator-rule'
     depending on ARITY, i.e.

     * :fixity (:prefix | :postfix)

       Only for unary operators. Fixity of the operator being defined.

     * :associativity (:none | :left | :right | :associative)

       Only for binary operators. Associativity of the operator being
       defined.

     * :skippable?-expression EXPRESSION

       See below.

     * :definer RULE-NAME

       The macro used to define the operator rule. Defaults to
       `esrap:defrule'.

   The final LEAF-EXPRESSION clause is just a rule expression,
   describing the \"leafs\" (i.e. not operator expressions) of the
   operator grammar.

   Whitespace handling can be controlled by specifying rules for
   \"skippable\" input using the :skippable?-expression keyword
   argument in ARGS. If supplied, SKIPPABLE?-EXPRESSION is applied to
   all defined operators. If SKIPPABLE?-EXPRESSION is not supplied, a
   rule whose name is

     (find-symbol (string '#:skippable?) (symbol-package OPERATOR-NAME))

   is used.

   Example

     (define-operator-rules (:skippable?-expression (* #\\Space))
       (2 term   \"+\") ; lowest binding power
       (2 factor \"*\")
       (1 neg    \"-\") ; highest binding power
       #\\x)           ; leaf expression

     (architecture.builder-protocol:with-builder ('list)
       (esrap:parse 'term \"x + x * -x\"))
     =>
     (:BINARY-OPERATOR
      (:OPERAND ((\"x\")
                 ((:BINARY-OPERATOR
                   (:OPERAND ((\"x\")
                              ((:UNARY-OPERATOR
                                (:OPERAND ((\"x\")))
                                :OPERATOR \"-\" :BOUNDS (4 . 6)))))
                   :OPERATOR \"*\" :BOUNDS (2 . 6)))))
      :OPERATOR \"+\" :BOUNDS (0 . 6))

   Note that this macro is not concerned with forcing operator
   bindings via parentheses. See the documentation for recommendations
   on that."
  (let ((length (length clauses)))
   (unless (>= length 2)
     (error "~@<~S needs at least two clauses, ~:S has ~R.~@:>"
            'define-operators clauses length)))

  (let* ((leaf-clause      (list (lastcar clauses)))
         (operator-clauses (butlast clauses)))
    `(progn
       ,@(mapcar
          (lambda+ ((arity                name        operator &rest args)
                    (next-first &optional next-second &rest &ign))
            (let+ ((next-name (or next-second next-first))
                   ((&flet make-rule (definer)
                      `(,definer ,name
                         ,operator ,next-name
                         ,@(when skippable?-expression
                             `(:skippable?-expression ,skippable?-expression))
                         ,@args))))
              (ecase arity
                (1 (make-rule 'define-unary-operator-rule))
                (2 (make-rule 'define-binary-operator-rule)))))
          operator-clauses
          (append (rest operator-clauses) (list leaf-clause)))
       nil)))
