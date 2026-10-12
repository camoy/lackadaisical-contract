#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require data/order
         racket/bool
         racket/contract
         racket/math
         lackadaisical-contract)

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

(define (order-<=? ord)
  (define =? (order-=? ord))
  (define <? (order-<? ord))
  (λ (x y) (or (=? x y) (<? x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; monotonicity

(define (monotone/c ord)
  (define d (order-domain-contract ord))
  (define <=? (order-<=? ord))
  (self/c f ([A d])
    (-> A d)
    (∀ ([x A] [x′ A])
       (implies (<=? x x′)
                (<=? (f x) (f x′))))))

(module+ test
  (let ()
    (define/contract (f x)
      (monotone/c real-order)
      (sqr x))
    (f -3)
    (check-exn
     #rx"where: x is -8, x′ is -3"
     (λ () (f -8)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; injectivity

(define injective/c
  (self/c f ([A any/c])
    (-> A any/c)
    (∀ ([x A] [x′ A])
      (implies (equal? (f x) (f x′))
               (equal? x x′)))))

(module+ test
  (let ()
    (define/contract (f x)
      injective/c
      (floor x))
    (f 3.5)
    (f 4.5)
    (check-exn
     #rx"where: x is 4.5, x′ is 4.85"
     (λ () (f 4.85)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transitivity

(define (transitive/c dom)
  (self/c ⊑ ([A dom])
    (-> A A boolean?)
    (∀ ([x A] [y A] [z A])
       (implies (and (⊑ x y) (⊑ y z)) (⊑ x z)))))

(module+ test
  (let ()
    (define/contract (leq? x y)
      (transitive/c symbol?)
      (and (member (cons x y) '([a . b] [b . c] [a . c] [c . d])) #t))
    (leq? 'a 'b)
    (leq? 'b 'c)
    (leq? 'a 'c)
    (leq? 'c 'd)
    (check-exn
     #rx"where: x is 'a, y is 'c, z is 'd"
     (λ () (leq? 'a 'd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `->l`

(module+ test
  (let ()
    (define/contract (f g)
      (->l ([g (-> integer? integer?)])
           #:pre (g) (zero? (g 1))
           [result integer?])
      (g 1)
      0)

    (f sub1)
    (check-exn
     #rx"failed clause"
     (λ () (f values)))))

(module+ test
  (let ()
    (define/contract (f g)
      (->l ([g (-> integer? integer?)])
           [result (-> integer? integer?)]
           #:post (result) (zero? (result 1)))
      g)

    (define g1 (f sub1))
    (g1 1)
    (define g2 (f values))
    (check-exn
     #rx"failed clause"
     (λ () (g2 1)))))
