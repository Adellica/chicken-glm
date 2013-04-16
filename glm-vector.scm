
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


;; (pp (expand '(vector-length-dispatch variable f32vector +/type/for/variable)))
(define-syntax vector-length-dispatch
  (er-macro-transformer
   (lambda (x r t)     
     (let* ((variable (cadr x))
            (vtype (caddr x))
            (form (cadddr x))
            (precision-prefix (case vtype
                                ((f32vector) "")
                                ((f64vector) 'd)
                                ((s32vector) 'i)
                                ((u32vector) 'u)
                                ((u8vector)  'b)))
            (vector-type? (string->symbol (conc vtype "?")))
            (vector-length (string->symbol (conc vtype "-length"))))
       `(begin (,(r 'case) (,vector-length ,variable)
          ((2) ,(rewrite form variable (conc precision-prefix "vec2")))
          ((3) ,(rewrite form variable (conc precision-prefix "vec3")))
          ((4) ,(rewrite form variable (conc precision-prefix "vec4")))))))))




(template
 `((<OP> + -))

 (define (<OP>/vec/scalar/delegate vec scalar)
   (cond
    ((f32vector? vec) (vector-length-dispatch vec f32vector <OP>/vec/scalar))
    ((f64vector? vec) (vector-length-dispatch vec f64vector <OP>/vec/scalar))
    ((s32vector? vec) (vector-length-dispatch vec s32vector <OP>/vec/scalar))
    ((u32vector? vec) (vector-length-dispatch vec u32vector <OP>/vec/scalar))))

 ;; TODO: make this not terrible
 (define (v<OP>/delegate v1 v2)
   (cond
    ((number? v2) (<OP>/vec/scalar/delegate v1 v2))
    ((f32vector? v1) (if (f32vector? v2)
                         (if (= (f32vector-length v1) (f32vector-length v2))
                             (vector-length-dispatch v1 f32vector <OP>/v1/v1)
                             (error "vector dimension mismatch" v1 v2))
                         (error "must be vector" v2)))
    ((f64vector? v1) (if (f64vector? v2)
                         (if (= (f64vector-length v1) (f64vector-length v2))
                             (vector-length-dispatch v1 f64vector <OP>/v1/v1)
                             (error "vector dimension mismatch" v1 v2))
                         (error "must be vector" v2)))
    ((s32vector? v1) (if (s32vector? v2)
                         (if (= (s32vector-length v1) (s32vector-length v2))
                             (vector-length-dispatch v1 s32vector <OP>/v1/v1)
                             (error "vector dimension mismatch" v1 v2))
                         (error "must be vector" v2)))
    ((u32vector? v1) (if (u32vector? v2)
                         (if (= (u32vector-length v1) (u32vector-length v2))
                             (vector-length-dispatch v1 u32vector <OP>/v1/v1)
                             (error "vector dimension mismatch" v1 v2))
                         (error "must be vector" v2)))
    ((u8vector? v1) (if (u8vector? v2)
                        (if (= (u8vector-length v1) (u8vector-length v2))
                            (vector-length-dispatch v1 u8vector <OP>/v1/v1) 
                            (error "vector dimension mismatch" v1 v2))
                        (error "must be vector" v2)))
    (else (error "unknown vector type" v1))))

 (define (v<OP> v1 v2)
   ((v<OP>/delegate v1 v2) v1 v2)))

;; this should do what we want,
;; but let's keep an eye on this guy:
(define v= equal?)
