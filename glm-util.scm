

(define (print-mat4 mat #!optional
                    (preline (lambda () (void)))
                    (postline print)
                    (precell (lambda () (void)))
                    (postcell (lambda () (display '#\space))))
  (do ((i 0 (add1 i)))
      ((>= i 4))
    (preline)
    (do ((j 0 (add1 j)))
        ((>= j 4))
      (precell)
      (display (f32vector-ref mat (+ (* i 4) j)))
      (postcell))
    (postline)))

