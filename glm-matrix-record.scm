

(define (print-mat mat #!optional
                   (port (current-output-port))
                   (preline (lambda _ (void)))
                   (postline (lambda _ (fprintf port "\n")))
                   (precell (lambda _ (void)))
                   (postcell (lambda _ (display '#\space port))))
  (do ((i 0 (add1 i)))
      ((>= i (mat-rows mat)))
    (preline i)
    (do ((j 0 (add1 j)))
        ((>= j (mat-cols mat)))
      (precell i j)
      (display (f32vector-ref (mat-data mat) (+ (* j (mat-rows mat)) i)) port) ;; column-major
      (postcell i j))
    (postline i)))


(define-record mat cols data)

(define (mat-rows x) (/ (f32vector-length (mat-data x)) (mat-cols x)))


(define-record-printer (mat x port)
  (fprintf port "#<mat ~Ax~A:\n" (mat-cols x) (mat-rows x))
  (print-mat x port (lambda _ (void)) (lambda (n) (if (< n (sub1 (mat-rows x))) (fprintf port "\n"))))
  (fprintf port ">\n"))

(define-foreign-type mat f32vector mat-data)




