#lang info

;; general

(define name "lackadaisical-contract")
(define collection "lackadaisical-contract")
(define pkg-desc "Full implementaion of lackadaisical contract checking.")
(define version "0.0")
(define scribblings '(("scribblings/lackadaisical-contract.scrbl" ())))
(define pkg-authors '(camoy))
(define license 'Apache-2.0)

;; dependencies
;;
(define deps
  '("data-lib"
    "lackadaisical-contract-lib"
    "base"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))
