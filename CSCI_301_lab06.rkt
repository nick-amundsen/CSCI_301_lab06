#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab #5
;;
;; Nick Amundsen
;; W01323151
;;
;; The purpose of this program is to
;; evaluate expressions recursively
;; using a set environment, adding the lambda functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup)
(provide evaluate)

;Environment that was given to us
(define env (list
(list 'x 5)
(list '+ +)
(list '* *)
(list '= equal?)
(list 'else #t)))

;Test environment
(define e1  (map list
                 '(     x  y  z + - * cons car cdr nil list = else )
                 (list 10 20 30 + - * cons car cdr '() list = #t   )))


;Let function
(define eval-let
  (lambda (ls env)
    (if (null? (cdr ls)) (cons (cons (car(car ls)) (list (evaluate (car(cdr(car ls))) env))) '())
        (cons (cons (car(car ls)) (list (evaluate (car(cdr(car ls))) env))) (eval-let (cdr ls) env)))
    ))



;Cond function
(define eval-cond
  (lambda (list env)
    (if (eqv? (evaluate (car(car list)) env) #t) (eval-list (car(cdr(car list))) env) (eval-cond (cdr list) env))
    ))


;Function to check if a list is a special form
(define special-form?
  (lambda (list)
    (cond ((not (list? list)) #f)
          ((eqv? (car list) 'if) #t)
          ((eqv? (car list) 'cond) #t)
          ((eqv? (car list) 'let) #t)
          ((eqv? (car list) 'lambda) #t)
          (else #f))))


;Function to evaluate special form expressions
(define evaluate-special-form
  (lambda (list env)
    (cond ((and (eqv? (car list) 'if) (eqv? (evaluate (car(cdr list)) env) #t)) (car(cdr(cdr list))))
          ((and (eqv? (car list) 'if) (eqv? (evaluate (car(cdr list)) env) #f)) (car(cdr(cdr(cdr list)))))
          ((eqv? (car list) 'cond) (eval-cond (cdr list) env))
          ((eqv? (car list) 'let) (evaluate (car(cdr(cdr list))) (append (eval-let (car(cdr list)) env) env)))
          ((eqv? (car list) 'lambda) (eval-lambda list env))
          (else (error "cannot evaluate special-form"))
          )))


;Function to evaluate lambda special form
(define eval-lambda
  (lambda (list env)
    (cons 'closure (cdr list))))


;Function to look up symbol inside an environment
(define lookup
  (lambda (s env)
    (cond ((empty? env) (error (string-append (~a s) ": not in environment")))
          ((not (symbol? s)) (error (string-append (~a s) ": not a symbol")))
          ((eqv? s 'cond) s)
          ((eqv? s 'if) s)
          ((eqv? (car (car env)) s) (car (cdr (car env))))
          (else (lookup s (cdr env)))         
          )))


;Function to evaluate each expression inside a list
(define eval-list
  (lambda (exp env)
    (cond ((empty? exp) '())
          ((and (not (list? exp)) (number? exp)) exp)
          ((and (not (list? exp)) (symbol? exp)) (lookup exp env))
          ((number? (car exp)) (cons (car exp) (eval-list (cdr exp) env)))
          ((and (symbol? (car exp)) (not(special-form? exp))) (cons (lookup (car exp) env) (eval-list (cdr exp) env)))
          ((special-form? exp) (evaluate-special-form exp env))
          ((list? (car exp)) (cons (evaluate (car exp) env) (eval-list (cdr exp) env)))
          )))

;Function to spply a procedure  
(define evaluate
  (lambda (exp env)
    (let ((exp-eval (eval-list exp env)))
      (cond ((not (list? exp-eval)) exp-eval)
            ((not (procedure? (car exp-eval))) (error "not a procedure"))
            (else (apply (car exp-eval) (cdr exp-eval)))
          ))))

(define apply-function
  (lambda ()))


;;;;;; Tests.
;(evaluate-special-form '(if (= 1 1) 3 4) env)
;(evaluate '(if (= 1 2) 3 4) env)
;(evaluate '(+ 3 x (+ 2 2) (* 2 (+ 4 1))) env)
;(lookup 'x env)
;(evaluate '10 env)
;(evaluate '(cond ((= 1 2) (+ 1 1))
;                        ((= 2 3) (+ 2 2))
 ;                       ((= 3 3) (+ 3 3))
  ;                      (else (+ 4 4))) env)
;(evaluate '(if (= 1 2) 2 3) env)
;(eval-let
;'((x (+ 2 2))
;(y x)
;(z (* 3 3))) env)
;(cons 3 (list 3))
;(evaluate '(let ((a 1) (b 2)) (+ a b)) e1)
(evaluate '(lambda (x) (+ x y)) e1)
