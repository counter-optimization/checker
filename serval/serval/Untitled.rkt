#lang rosette

(define (spec impl x y)
  (define result (impl x y))
  (assert (! (&& (bveq result x)
                 (bveq result y)))))