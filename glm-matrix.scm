
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

(template
 `((I 2 3 4))

 
 ;; matrix-vector operators
 ;; TODO: add mat4x3 * vec4 => vec3
 (template
  `((<OP> *))
   
  (define <OP>/matIxI/vecI! (glm void vecI "=" matIxI "<OP>" vecI))
  (define (<OP>/matIxI/vecI mat vec)
    (with-destination (make-vecI #f) <OP>/matIxI/vecI! mat vec)))

 ;; element-wise matrix operators
 (template
  `((J 2 3 4))

  (template
   `((<OP> + -))
   (define <OP>/matIxJ! (glm void matIxJ "=" matIxJ "<OP>" matIxJ))
   (define (<OP>/matIxJ mat1 mat2)
     (with-destination (make-matIxJ #f) <OP>/matIxJ! mat1 mat2)))))


(define (m*/delegate mat1 mat2)
  (if (mat? mat1)
      (if (mat? mat2)
          ;; matrix-matrix multiplication
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
              (error "cannot multiply matrix sizes" mat1 mat2))
          ;; matrix-vector multiplication
          (if (f32vector? mat2)
              (case (f32vector-length mat2)
                ((2) */mat2x2/vec2)
                ((3) */mat3x3/vec3)
                ((4) */mat4x4/vec4))
              (error "invalid dimensions" mat1 mat2)))
      (error "must be matrix" mat1)))


(define (m* m m/v)
  ((m*/delegate m m/v) m m/v))


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


;; TODO: add all matrix sizes
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
 `((T mat2x2 mat3x3 mat4x4))
 (define determinant/T (glm float "return(" "glm::determinant(" T ")" ")"))
 
 (define inverse/T!    (glm void T "=" "glm::inverse("     T ")"))
 (define (inverse/T mat)
   (with-destination (make-T #f) inverse/T! mat)))


(template
 `((<OP> inverse determinant))
 (define (<OP>/delegate sqmat)
   (if (and (mat? sqmat) (= (mat-cols sqmat) (mat-rows sqmat)))
       (case (mat-cols sqmat)
         ((2) <OP>/mat2x2)
         ((3) <OP>/mat3x3)
         ((4) <OP>/mat4x4))))

 (define (<OP> sqmat)
   ((determinant/delegate sqmat) sqmat)))
