

(define (print-mat mat #!optional
                   (port (current-output-port))
                   (preline (lambda _ (void)))
                   (postline print)
                   (precell (lambda _ (void)))
                   (postcell (lambda _ (display '#\space port))))
  (do ((i 0 (add1 i)))
      ((>= i (mat-rows mat)))
    (preline i)
    (do ((j 0 (add1 j)))
        ((>= j (mat-cols mat)))
      (precell i j)
      (display (f32vector-ref (mat-data mat) (+ (* j (mat-cols mat)) i)) port) ;; column-major
      (postcell i j))
    (postline i)))


