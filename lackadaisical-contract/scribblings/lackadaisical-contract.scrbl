#lang scribble/manual

@require[@for-label[lackadaisical-contract
                    racket/base
                    racket/contract]
         scribble/example]

@title{Lackadaisical Contracts}
@author{Cameron Moy}

@defmodule[lackadaisical-contract]

@(define lackadaisical-eval
   (make-base-eval '(require racket/contract
                             racket/bool
			     lackadaisical-contract)))

This library provides contract forms
that implement the lackadaisical checking strategy.
In lackadaisical,
interactions with higher-order values at the contract level
are deferred until the program itself exhibits those same interactions.

@defform[(->l ([dom-id dom-expr] ...) pre [cod-id cod-expr] post)
         #:grammar
         [(pre (code:line)
               (code:line #:pre (id ...) expr pre))
          (post (code:line)
                (code:line #:post (id ...) expr post))]]{
  This contract form is similar to @racket[->*]
  except it implements lackadaisical checking for
  preconditions and postconditions
  that depend on higher-order values.
  @examples[#:eval lackadaisical-eval #:label #f
    (define/contract (bad g)
      (->l ([g (-> integer? integer?)])
           #:pre (g) (zero? (g 1))
           [result integer?])
      (g 1))
    (eval:error (bad values))

    (define/contract (ok g)
      (->l ([g (-> integer? integer?)])
           #:pre (g) (zero? (g 1))
           [result integer?])
      (g 17))
    (ok values)]
  Notice that the precondition does not fail for @racket[ok]
  because its body does not actually apply @racket[g] to @racket[1].
  While the precondition fails for @racket[bad]
  because it can observe the result of @racket[(g 1)]
  and witness that it is not actually @racket[0].
}

@defform[(self/c id ([set-id set-ctc] ...) body clause ...)
         #:grammar
         [(clause (∀ ([id dom] ...) prop-expr ...+))]]{
  The @racket[self/c] contract expresses declarative
  properties of a function via universal quantification.
  This kind of specification is helpful for stating
  mathematical properties in their most natural form.
  Here is an example of injectivity as a contract:
  @examples[#:eval lackadaisical-eval #:label #f
    (define injective/c
      (self/c f ([A any/c])
        (-> A any/c)
        (∀ ([x A] [y A])
          (implies (equal? (f x) (f y))
                   (equal? x y)))))

    (define/contract (floor* x)
      injective/c
      (floor x))
    (floor* 3.5)
    (floor* 4.5)
    (eval:error (floor* 4.85))]
  This contract uses lackadaisical checking,
  in the sense that interactions with @racket[f]
  in the body of universally-quantified expressions
  are not evaluated immediately.
}
