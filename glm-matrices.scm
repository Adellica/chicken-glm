
(define-record mat cols data)

(define (mat-rows x) (/ (f32vector-length (mat-data x)) (mat-cols x)))


(define-record-printer (mat x port)
  (fprintf port "#<mat ~Ax~A:\n" (mat-cols x) (mat-rows x))
  (print-mat x port (lambda _ (void)) (lambda (n) (if (< n (sub1 (mat-rows x))) (fprintf port "\n"))))
  (fprintf port ">\n"))

(define-foreign-type mat f32vector mat-data)

;; matrix constructors

(template
 `((I 2 3 4))
 
 (template
  `((J 2 3 4))
  
  (define (make-matIxJ fill)    
    (make-mat I (make-f32vector (* I J) fill)))

  (define (matIxJ? mat)
    (and (mat? mat)
         (= I (mat-cols mat))
         (= (f32vector-length (mat-data mat)) (* I J))))

  (define matIxJ! (glm void matIxJ "=" "glm::matIxJ(" float ")"))
  (define (matIxJ diagonal)
    (with-destination (make-matIxJ #f) matIxJ! diagonal))))

(define make-mat2 make-mat2x2)
(define make-mat3 make-mat3x3)
(define make-mat4 make-mat4x4)

(define mat2? mat2x2?)
(define mat3? mat3x3?)
(define mat4? mat4x4?)

(define mat2 mat2x2)
(define mat3 mat3x3)
(define mat4 mat4x4)

;; matrix by matrix multiplication
;; matrix-sizes can be IxJ * KxI
(template
 `((I 2 3 4))

 ;; our I give us all possible types
 (template
  `((J 2 3 4))

  ;; JxK give the three legal multiplication sizes
  (template
   `((K 2 3 4))
  
   (define */matIxJ/matKxI! (glm void matKxJ "=" matIxJ "*" matKxI))
   (define (*/matIxJ/matKxI mat1 mat2)
     (with-destination (make-matKxJ #f) */matIxJ/matKxI! mat1 mat2)))))


(template
 `((T mat3 mat4))
 (define transpose/T! (glm void T "=" "glm::transpose(" T ")"))
 (define (transpose/T mat)  (with-destination (make-T #f) transpose/T! mat)))

(define (transpose/delegate mat)
  (cond ((mat4? mat) transpose/mat4)
        ((mat3? mat) transpose/mat3)))

(define (transpose mat)
  ((transpose/delegate mat) mat))



(template
 `((T mat3 mat4))
 (define determinant/T (glm float "return(" "glm::determinant(" T ")" ")")))



