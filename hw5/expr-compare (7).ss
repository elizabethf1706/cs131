#lang racket

(provide expr-compare)


;; Check whether a symbol represents a lambda.
(define (is-lambda-symbol? sym)
  (or (eq? sym 'lambda) (eq? sym 'λ)))

;; Compare two lambda expressions by comparing their argument lists and bodies.
(define (compare-lambda-structure expr1 expr2 args1 args2 body1 body2)
  (let* ([lambda-tag (if (and (equal? (car expr1) 'lambda)
                              (equal? (car expr2) 'lambda))
                         'lambda
                         'λ)]
         [args-comp (expr-compare args1 args2)]
         [body-comp (expr-compare body1 body2)])
    (list lambda-tag args-comp body-comp)))

;; Recursively translate a list of variables using a mapping dictionary.
(define (translate-variables var-list mapping)
  (if (null? var-list)
      var-list
      (cons (hash-ref mapping (car var-list) "checker")
            (translate-variables (cdr var-list) mapping))))


(define (translate-expression expr mapping inner-scope?)
  (cond
    [(null? expr) expr]
    [(not (list? expr))
     (if (equal? (hash-ref mapping expr "checker") "checker")
         expr
         (hash-ref mapping expr "checker"))]
    [(equal? (car expr) 'quote)
     (list 'quote (cadr expr)
           (translate-expression (cddr expr) mapping inner-scope?))]
    [(and (list? (car expr)) (not inner-scope?))
     (append (list (translate-expression (car expr) mapping inner-scope?))
             (translate-expression (cdr expr) mapping inner-scope?))]
    [else
     (let ([transformed (hash-ref mapping (car expr) "checker")])
       (cond
         [(equal? transformed "checker")
          (if (is-lambda-symbol? (car expr))
              (cons (car expr) (translate-expression (cdr expr) mapping #t))
              (cons (car expr) (translate-expression (cdr expr) mapping inner-scope?)))]
         [(is-lambda-symbol? transformed)
          (cons transformed (translate-expression (cdr expr) mapping #t))]
         [else
          (cons transformed (translate-expression (cdr expr) mapping inner-scope?))]))]))
 
(define (build-dict args new-values)
  (if (null? args)
      (hash)
      (hash-set (build-dict (cdr args) (cdr new-values))
                (car args)
                (car new-values))))

(define (merge-arguments args1 args2)
  (cond
    [(null? args1) args1]
    [(equal? (car args1) (car args2))
     (cons (car args1) (merge-arguments (cdr args1) (cdr args2)))]
    [else
     (cons (string->symbol
            (string-append (symbol->string (car args1))
                           "!"
                           (symbol->string (car args2))))
           (merge-arguments (cdr args1) (cdr args2)))]))


(define (compare-lambda-head expr1 expr2)
  (let* ([vars1 (cadr expr1)]
         [vars2 (cadr expr2)])
    (if (not (equal? (length vars1) (length vars2)))
        (list 'if '% expr1 expr2)
        (let* ([merged-args (merge-arguments vars1 vars2)]
               [dict1 (build-dict vars1 merged-args)]
               [dict2 (build-dict vars2 merged-args)])
          (compare-lambda-structure expr1 expr2
                                    (translate-variables vars1 dict1)
                                    (translate-variables vars2 dict2)
                                    (translate-expression (caddr expr1) dict1 #f)
                                    (translate-expression (caddr expr2) dict2 #f))))))


(define (compare-head expr1 expr2)
  (let ([head1 (car expr1)]
        [head2 (car expr2)])
    (cond
      [(or (is-lambda-symbol? head1) (is-lambda-symbol? head2))
       (if (not (and (is-lambda-symbol? head1) (is-lambda-symbol? head2)))
           (list 'if '% expr1 expr2)
           (compare-lambda-head expr1 expr2))]
      [(or (equal? head1 'quote) (equal? head2 'quote))
       (list 'if '% expr1 expr2)]
      [(equal? head1 head2)
       (compare-body expr1 expr2)]
      [(or (equal? head1 'if) (equal? head2 'if))
       (list 'if '% expr1 expr2)]
      [(and (list? head1) (list? head2))
       (cons (expr-compare head1 head2)
             (expr-compare (cdr expr1) (cdr expr2)))]
      [else
       (compare-body expr1 expr2)])))


(define (compare-body expr1 expr2)
  (if (null? expr1)
      expr1
      (let ([elem1 (car expr1)]
            [elem2 (car expr2)])
        (cond
          [(equal? elem1 elem2)
           (cons elem1 (compare-body (cdr expr1) (cdr expr2)))]
          [(and (boolean? elem1) (boolean? elem2))
           (cons (if elem1 '% '(not %))
                 (compare-body (cdr expr1) (cdr expr2)))]
          [(and (list? elem1) (list? elem2))
           (cons (expr-compare elem1 elem2)
                 (compare-body (cdr expr1) (cdr expr2)))]
          [else
           (cons (list 'if '% elem1 elem2)
                 (compare-body (cdr expr1) (cdr expr2)))]))))

;; Main functiom
(define (expr-compare expr1 expr2)
  (cond
    [(equal? expr1 expr2) expr1]
    [(and (boolean? expr1) (boolean? expr2))
     (if expr1 '% '(not %))]
    [(or (not (list? expr1)) (not (list? expr2)))
     (list 'if '% expr1 expr2)]
    [(not (equal? (length expr1) (length expr2)))
     (list 'if '% expr1 expr2)]
    [else
     (compare-head expr1 expr2)]))
 
(define (test-expr-compare a b)
  (let ((cmp (expr-compare a b)))
    (and (equal? (eval a) (eval `(let ((% #t)) ,cmp)))
         (equal? (eval b) (eval `(let ((% #f)) ,cmp))))))

;; Sample expressions for testing.
(define test-expr-x '(apply * 22 ((lambda (x y) (list x y)) 5 3)))

(define test-expr-y '(apply * 5  ((lambda (x z) (list x z)) 2 3)))