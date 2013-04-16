
;; *** vector constructors

(template
 `((P vec dvec   ivec uvec   bvec)
   (R ,glmtype->schemetype)) ;; f32vector s32vector etc
 
 (define (P2 x y)     (R x y))
 (define (P3 x y z)   (R x y z))
 (define (P4 x y z w) (R x y z w)))


(template
 `((D 2 3 4))
 (define (make-vecD  fill) (make-f32vector D fill))
 (define (make-dvecD fill) (make-f64vector D fill))
 (define (make-ivecD fill) (make-s32vector D fill))
 (define (make-uvecD fill) (make-u32vector D fill))
 (define (make-bvecD fill) (make-u8vector D fill)))




;; OBS: we won't be able to distinguish between vec4 and mat2 for example
(template
 `((D 2 3 4))

 (define ( vecD? vec) (and (f32vector? vec) (= (f32vector-length vec) D)))
 (define (dvecD? vec) (and (f64vector? vec) (= (f64vector-length vec) D)))

 (define (ivecD? vec) (and (s32vector? vec) (= (s32vector-length vec) D)))
 (define (uvecD? vec) (and (u32vector? vec) (= (u32vector-length vec) D)))

 (define (bvecD? vec) (and (u8vector? vec) (= (u8vector-length vec) D))))


;;; vector operations
(template
 `((T  vec2  vec3  vec4
       dvec2 dvec3 dvec4
       uvec2 uvec3 uvec4
       ivec2 ivec3 ivec4
       bvec2 bvec3 bvec4
       )
   (R ,value-type))

 ;; unary operators
 (define length/T (glm R "return(" "glm::length(" T "));"))
   
 ;; infix operators
 (template `((OP + - * /) )
           
           (define OP/T/T! (glm void T "=" T "OP" T))
           (define (OP/T/T operand1 operand2)
             (with-destination (make-T #f) OP/T/T! operand1 operand2)))

 ;; vector unary operators
 (template `((OP
              abs      ceil ;;  floor       fract round roundEven  sign
              sin           ;;  cos  tan  sinh  cosh  tanh
              ;;   asin acos atan asinh acosh atanh
              degrees  radians
              ;;   exp      exp2     inversesqrt log   log2  sqrt
              ;;   normalize
              ))
           (define OP/T! (glm void T "=" "glm::OP(" T ")"))
           (define (OP/T vec) (with-destination (make-T #f) OP/T! vec)))
 

 ;; prefix binary operators, primitive return type
 (template `((OP "dot" "distance"))
           (define OP/T (glm R "return(" "glm::" "OP" "(" T "," T "));"))))

;; vector-scalar infix operators (excludes bvec types)
(template
 `((T  vec2  vec3  vec4
       dvec2 dvec3 dvec4
       uvec2 uvec3 uvec4
       ivec2 ivec3 ivec4
       )
   (R ,value-type))
 
 ;; infix operators
 (template `((OP + - * /) )
      
           (define OP/T/scalar! (glm void T "=" T "OP" R))
           (define (OP/T/scalar vec scalar)
             (with-destination (make-T #f) OP/T/scalar! vec scalar))))

;; cross is only defined for vec3
(template
 `((T vec3 dvec3 ivec3 uvec3 bvec3))
 (define cross/T! (glm void T "=" "glm::cross(" T "," T ")"))
 (define (cross/T veca vecb) (with-destination (make-T #f) cross/T! veca vecb)))


(template
 `((<OP> + -))

 (define (<OP>/vec/scalar/delegate vec scalar)
   (if (f32vector? vec)
       (case (f32vector-length vec)
         ((2) <OP>/vec2/scalar)
         ((3) <OP>/vec3/scalar)
         ((4) <OP>/vec4/scalar))
       (if (f64vector? vec)
           (case (f64vector-length vec)
             ((2) <OP>/dvec2/scalar)
             ((3) <OP>/dvec3/scalar)
             ((4) <OP>/dvec4/scalar))
           (if (s32vector? vec)
               (case (s32vector-length vec)
                 ((2) <OP>/ivec2/scalar)
                 ((3) <OP>/ivec3/scalar)
                 ((4) <OP>/ivec4/scalar))
               (if (u32vector? vec)
                   (case (u32vector-length vec)
                     ((2) <OP>/uvec2/scalar)
                     ((3) <OP>/uvec3/scalar)
                     ((4) <OP>/uvec4/scalar)))))))

 (define (v<OP>/delegate v1 v2)
   (if (number? v2)
       (<OP>/vec/scalar/delegate v1 v2)
       (if (f32vector? v1)      
           (if (f32vector? v2)
               (if (= (f32vector-length v1) (f32vector-length v2))
                   (case (f32vector-length v1)
                     ((2) <OP>/vec2/vec2)
                     ((3) <OP>/vec3/vec3)
                     ((4) <OP>/vec4/vec4))
                   (error "vector dimension mismatch" v1 v2))
               (error "must be vector" v2))
           (if (f64vector? v1)
               (if (f64vector? v2)
                   (if (= (f64vector-length v1) (f64vector-length v2))
                       (case (f64vector-length v1)
                         ((2) <OP>/dvec2/dvec2)
                         ((3) <OP>/dvec3/dvec3)
                         ((4) <OP>/dvec4/dvec4))
                       (error "vector dimension mismatch" v1 v2))
                   (error "must be vector" v2))
               (if (s32vector? v1)
                   (if (s32vector? v2)
                       (if (= (s32vector-length v1) (s32vector-length v2))
                           (case (s32vector-length v1)
                             ((2) <OP>/ivec2/ivec2)
                             ((3) <OP>/ivec3/ivec3)
                             ((4) <OP>/ivec4/ivec4))
                           (error "vector dimension mismatch" v1 v2))
                       (error "must be vector" v2))
                   (if (u32vector? v1)
                       (if (u32vector? v2)
                           (if (= (u32vector-length v1) (u32vector-length v2))
                               (case (u32vector-length v1)
                                 ((2) <OP>/uvec2/uvec2)
                                 ((3) <OP>/uvec3/uvec3)
                                 ((4) <OP>/uvec4/uvec4))
                               (error "vector dimension mismatch" v1 v2))
                           (error "must be vector" v2))
                       (if (u8vector? v1)
                           (if (u8vector? v2)
                               (if (= (u8vector-length v1) (u8vector-length v2))
                                   (case (u8vector-length v1)
                                     ((2) <OP>/bvec2/bvec2)
                                     ((3) <OP>/bvec3/bvec3)
                                     ((4) <OP>/bvec4/bvec4))
                                   (error "vector dimension mismatch" v1 v2))
                               (error "must be vector" v2))
                           (error "unknown vector type" v1))))))))

 (define (v<OP> v1 v2)
   ((v<OP>/delegate v1 v2) v1 v2)))
