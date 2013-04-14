
(include "glm-matrix-record.scm")


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

;; element-wise matrix operators
(template
 `((I 2 3 4))
 (template
  `((J 2 3 4))
  (template
   `((<OP> + -))
   (define <OP>/matIxJ! (glm void matIxJ "=" matIxJ "<OP>" matIxJ))
   (define (<OP>/matIxJ mat1 mat2)
     (with-destination (make-matIxJ #f) <OP>/matIxJ! mat1 mat2)))))

(template
 `((T mat3 mat4))
 (define transpose/T! (glm void T "=" "glm::transpose(" T ")"))
 (define (transpose/T mat)  (with-destination (make-T #f) transpose/T! mat)))

(define (transpose/delegate mat)
  (cond ((mat4? mat) transpose/mat4)
        ((mat3? mat) transpose/mat3)))

(define (transpose mat)
  ((transpose/delegate mat) mat))

(define (mat*/delegate mat1 mat2)
  (if (= (mat-cols mat1) (mat-rows mat2))
      (case (mat-cols mat1)
        ((2) (case (mat-rows mat1)
               ((2) (case (mat-cols mat2)
                      ((2) */mat2x2/mat2x2)
                      ((3) */mat2x2/mat3x2)
                      ((4) */mat2x2/mat4x2)) )
               ((3) (case (mat-cols mat2)
                      ((2) */mat2x3/mat2x2)
                      ((3) */mat2x3/mat3x2)
                      ((4) */mat2x3/mat4x2)) )
               ((4) (case (mat-cols mat2)
                      ((2) */mat2x4/mat2x2)
                      ((3) */mat2x4/mat3x2)
                      ((4) */mat2x4/mat4x2)) )))
        ((3) (case (mat-rows mat1)
               ((2) (case (mat-cols mat2)
                      ((2) */mat3x2/mat2x3)
                      ((3) */mat3x2/mat3x3)
                      ((4) */mat3x2/mat4x3)) )
               ((3) (case (mat-cols mat2)
                      ((2) */mat3x3/mat2x3)
                      ((3) */mat3x3/mat3x3)
                      ((4) */mat3x3/mat4x3)) )
               ((4) (case (mat-cols mat2)
                      ((2) */mat3x4/mat2x3)
                      ((3) */mat3x4/mat3x3)
                      ((4) */mat3x4/mat4x3)) )))
        ((4) (case (mat-rows mat1)
               ((2) (case (mat-cols mat2)
                      ((2) */mat4x2/mat2x4)
                      ((3) */mat4x2/mat3x4)
                      ((4) */mat4x2/mat4x4)) )
               ((3) (case (mat-cols mat2)
                      ((2) */mat4x3/mat2x4)
                      ((3) */mat4x3/mat3x4)
                      ((4) */mat4x3/mat4x4)) )
               ((4) (case (mat-cols mat2)
                      ((2) */mat4x4/mat2x4)
                      ((3) */mat4x4/mat3x4)
                      ((4) */mat4x4/mat4x4)) ))))
      (error "cannot multiply matrix sizes" mat1 mat2)))

(define (mat* mat1 mat2)
  ((mat*/delegate mat1 mat2) mat1 mat2))

(template
 `((<OP> + -))
 (define (m<OP>/delegate mat1 mat2)
   (if (and (= (mat-cols mat1) (mat-cols mat2))
            (= (mat-rows mat1) (mat-rows mat2)))
       (case (mat-cols mat1)
         ((2) (case (mat-rows mat1)
                ((2) <OP>/mat2x2) ((3) <OP>/mat2x3) ((4) <OP>/mat2x4)))
         ((3) (case (mat-rows mat1)
                ((2) <OP>/mat3x2) ((3) <OP>/mat3x3) ((4) <OP>/mat3x4)))
         ((4) (case (mat-rows mat1)
                ((2) <OP>/mat4x2) ((3) <OP>/mat4x3) ((4) <OP>/mat4x4))))      
       (error "matrix size must be equal" mat1 mat2)))
 
 (define (m<OP> mat1 mat2)
   ((m<OP>/delegate mat1 mat2) mat1 mat2)))



(template
 `((T mat3 mat4))
 (define determinant/T (glm float "return(" "glm::determinant(" T ")" ")")))



