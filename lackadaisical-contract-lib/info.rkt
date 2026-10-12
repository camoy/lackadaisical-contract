#lang info

;; general

(define name "lackadaisical-contract")
(define collection "lackadaisical-contract")
(define pkg-desc "Core implementation of lackadaisical contract checking (no tests or documentation).")
(define version "0.0")
(define pkg-authors '(camoy))
(define license 'Apache-2.0)

;; dependencies
;;
(define deps
  '("data-lib"
    "transparent-procedure-lib"
    "base"))

(define build-deps '())
