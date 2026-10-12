#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide ->l
         self/c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/contract
         racket/format
         racket/list
         racket/string
         racket/set
         racket/match
         racket/unsafe/ops
         transparent-procedure/synchronize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ->l

(struct arrow-clause (name body))
(struct cond-clause (name deps body))

(define-syntax ->l
  (syntax-parser
    [(_ ([dom:id dom-ctc:expr] ...)
        (~seq #:pre (pre:id ...) pre-ctc:expr) ...
        [cod:id cod-ctc:expr]
        (~seq #:post (post:id ...) post-ctc:expr) ...)
     #`(->l/c-fn
        '#,this-syntax
        (list (arrow-clause 'dom dom-ctc) ...)
        (list (cond-clause '[#:pre (pre ...) pre-ctc] '(pre ...) (λ (pre ...) pre-ctc)) ...)
        (arrow-clause 'cod cod-ctc)
        (list (cond-clause '[#:post (post ...) post-ctc] '(post ...) (λ (post ...) post-ctc)) ...))]))

(define (->l/c-fn name doms pres cod posts)
  (define arg-names (map arrow-clause-name doms))
  (define arg-lnps (map (compose get/build-late-neg-projection coerce-contract/f arrow-clause-body) doms))
  (define cod-name (arrow-clause-name cod))
  (define cod-lnp (get/build-late-neg-projection (coerce-contract/f (arrow-clause-body cod))))
  (make-chaperone-contract
   #:name name
   #:first-order procedure?
   #:late-neg-projection
   (λ (blm)
     (define arg-projs
       (for/list ([lnp (in-list arg-lnps)])
         (lnp (blame-swap blm))))
     (define cod-proj (cod-lnp blm))
     (λ (proc neg)
       (unless (procedure? proc)
         (raise-blame-error blm #:missing-party neg proc '(expected "a procedure" given: "~e") proc))
       (chaperone-procedure
        proc
        (λ args
          (define args*
            (for/list ([arg (in-list args)]
                       [proj (in-list arg-projs)])
              (do-projection proj arg neg)))
          (define (check-result res)
            (define res* (do-projection cod-proj res neg))
            (check-conds! posts (cons cod-name arg-names) (cons res* args*) blm neg)
            (proxy-value res*))
          (check-conds! pres arg-names args* (blame-swap blm) neg)
          (apply values check-result (map proxy-value args*))))))))

(define (proxy-value val)
  (match val
    [(list _run proxy _handle) proxy]
    [(list val) val]))

(define (run-vals rs thk)
  (let go ([rs rs])
    (match rs
      [(list) (thk)]
      [(cons run rst)
       (run (λ () (go rst)))])))

(define (do-projection proj val neg)
  (define val* (proj val neg))
  (cond
    [(procedure? val*)
     (define-values (run proxy handle) (synchronize val*))
     (list run (unsafe-chaperone-procedure val* proxy) handle)]
    [else (list val*)]))

(define (check-conds! conds names vals blm neg)
  (for ([c (in-list conds)])
    (check-cond! c names vals blm neg)))

(define (check-cond! c names vals blm neg)
  (match-define (cond-clause name deps body) c)
  (define runs null)
  (define dep-vals
    (for/list ([dep (in-list deps)])
      (match (list-ref vals (index-of names dep))
        [(list val) val]
        [(list run _proxy handle) (set! runs (cons run runs)) handle])))
  (run-vals
   (reverse runs)
   (λ ()
     (unless (apply body dep-vals)
       (raise-blame-error blm #:missing-party neg #t "failed clause ~a" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; self/c

(struct property-clause (name var-names doms body))

(begin-for-syntax
  (define-syntax-class binding-pair
    #:attributes (name ctc)
    (pattern [name:id ctc:expr]))

  (define-syntax-class clause
    #:datum-literals (∀)
    #:attributes ([var 1] [dom 1] [prop 1])
    (pattern (∀ ([var:id dom:id] ...) prop:expr ...+))))

(define-syntax self/c
  (syntax-parser
    #:datum-literals (∀)
    [(_ self:id (bp:binding-pair ...) body c:clause ...)
     #:declare body (expr/c #'base->?)
     #`(self/c-fn
        '#,this-syntax
        (λ (bp.name ...) body.c)
        `([bp.name . ,bp.ctc] ...)
        (list
         (property-clause
          '(c.prop ...)
          '(c.var ...)
          (λ (bp.name ...) (list c.dom ...))
          (λ (self c.var ...) c.prop ...)) ...))]))

(define (self/c-fn ctc-name make-body ctc-alist props)
  (make-chaperone-contract
   #:name ctc-name
   #:first-order procedure?
   #:late-neg-projection
   (λ (blm)
     (λ (val neg)
       (define prop-error! (make-prop-error! blm neg))
       (check-procedure! blm ctc-name val neg)
       (check-nonkeyword! blm val neg)

       (define-values (run proxy handle) (synchronize val))
       (define dom-ht
         (for/hash ([name+ctc (in-list ctc-alist)])
           (match-define (cons name _ctc) name+ctc)
           (values name (weak-set))))
       (define ctcs
         (for/list ([name+ctc (in-list ctc-alist)])
           (match-define (cons name pre-ctc) name+ctc)
           (make-collector pre-ctc run name dom-ht handle ctc-alist props prop-error!)))
       (define ctc (coerce-contract/f (apply make-body ctcs)))
       (define lnp (get/build-late-neg-projection ctc))
       ((lnp blm) (unsafe-chaperone-procedure val proxy) neg)))))

(define (make-collector pre-ctc run name dom-ht handle ctc-alist props prop-error!)
  (define ctc (coerce-contract/f pre-ctc))
  (define lnp (get/build-late-neg-projection ctc))
  (make-chaperone-contract
   #:name (contract-name ctc)
   #:late-neg-projection
   (λ (blm)
     (define lnp+blm (lnp blm))
     (λ (val neg)
       (for ([prop (in-list props)])
         (match-define (property-clause prop-name var-names make-vars make-prop) prop)
         (define set-names-for-prop (apply make-vars (map car ctc-alist)))
         (define indices (indexes-of set-names-for-prop name))
         (define arg-sets-for-prop
           (for/list ([index-set (in-combinations indices)]
                      #:unless (empty? index-set))
             (apply
              cartesian-product
              (for/list ([set-name (in-list set-names-for-prop)]
                         [k (in-naturals)])
                (if (member k index-set)
                    (list val)
                    (set->list (hash-ref dom-ht set-name)))))))
         (define args-for-prop (apply append arg-sets-for-prop))
         (set-add! (hash-ref dom-ht name) val)

         (for ([args (in-list args-for-prop)])
           (run (λ ()
                  (unless (apply make-prop handle args)
                    (prop-error! prop-name var-names val args))))))
       (lnp+blm val neg)))))

(define ((make-prop-error! blm neg) prop-name var-names val args)
  (define prop-str
    (match prop-name
      [(list x) (~a x)]
      [_ (~a `(let () ,prop-name))]))
  (raise-blame-error
   blm
   val
   (list 'expected: prop-str
         "\n  where:"
         (string-join (map (λ (x y) (format "~a is ~e" x y)) var-names args) ", "))
   #:missing-party neg))

(define (check-procedure! blm name val neg)
  (unless (procedure? val)
    (raise-blame-error
     blm val
     '(expected: "~a" given: "~e")
     name val
     #:missing-party neg)))

(define (check-nonkeyword! blm val neg)
  (define-values (_ kws)
    (procedure-keywords val))
  (unless (empty? kws)
    (raise-blame-error
     blm val
     '(expected: "procedure without keywords" given: "~e")
     val
     #:missing-party neg)))
