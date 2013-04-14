(module glm *
(import chicken scheme foreign)

(use srfi-4)

;; glm matrix layout in memory is column-major 
#>
#include <glm/glm.hpp>
<#



(define (srfi4-vector-ref vec idx)
  (define (check! p) (if p (error "vector of invalid length")))
  ((cond [(f32vector? vec) (check! (<= (f32vector-length vec) 4)) f32vector-ref]
         [(s32vector? vec) (check! (<= (s32vector-length vec) 4)) s32vector-ref]) vec idx))

(define (.x vec) (srfi4-vector-ref vec 0))
(define (.y vec) (srfi4-vector-ref vec 1))
(define (.z vec) (srfi4-vector-ref vec 2))
(define (.w vec) (srfi4-vector-ref vec 3))

(begin-for-syntax
 (define (value-type glmtype)
   (case glmtype
     ((vec vec2 vec3 vec4) 'float)
     ((dvec dvec2 dvec3 dvec4) 'float)
     ((ivec ivec2 ivec3 ivec4) 'int)
     ((uvec uvec2 uvec3 uvec4) 'unsigned-int)
     ((bvec bvec2 bvec3 bvec4) 'unsigned-char)
     ((mat3 mat4) 'float)
     ((dmat3 dmat4) 'double)
     (else (error "no value-type for" glmtype))))

 ;; we apparently need this in order for value-type to be visible by
 ;; compile-time 'eval'
 (define glm#value-type value-type))

(begin-for-syntax
  (define (glmtype->schemetype type)
    (case type
      ((vec vec2  vec3  vec4 mat4 mat3) 'f32vector)
      ((dvec dvec2 dvec3 dvec4) 'f64vector)
      ((ivec ivec2 ivec3 ivec4) 's32vector)
      ((uvec uvec2 uvec3 uvec4) 'u32vector)
      ((bvec bvec2 bvec3 bvec4) 'u8vector)
      ((float double int) type)
      (else (error "cannot convert to scheme-type" type))))

  (define glm#glmtype->schemetype glmtype->schemetype))


;; bug!? without this, we get "warning: reference to unbound variable
;; value-type" event though it's just used in 'eval' in one of the macros.
(define value-type #f)
(define glmtype->schemetype #f)


(define-for-syntax (cast glmtype var)
  (case glmtype
    ;; vects and mats need to cast from f32vector/s32vectors to glm types 
    ((
      vec2 vec3 vec4
      ivec2 ivec3 ivec4 
      uvec2 uvec3 uvec4 
      mat3 mat4)
     (conc "(" "*(glm::" glmtype "*)" var ")"))
    ((float int double) var) ;; primitives don't need cast
    (else (error "cannot cast type" glmtype))))


(define-for-syntax (stringify ops)
  (apply conc
         (let loop ((ops ops)
                    (arg-idx 0)
                    (result '()))
           (if (pair? ops)
               (let ((x (car ops)))
                 (if (symbol? x)
                     (loop (cdr ops)
                           (add1 arg-idx)
                           (cons (cast x (conc "a" arg-idx)) result))
                     (loop (cdr ops)
                           arg-idx
                           (cons  (conc " " x " ") result))))
               (reverse result)))))

;; helper for writing foreign-lambdas to glm
;; arguments are: (glm <foreign return-type> args..)
;; foreign return-type is return-type of the foreign-lambda
;; args are a sequence of strings and symbols, where the symbols
;; represent glm types (eg vec3, mat4) and are added as
;; input-arguments. strings are forwarded to C
;; directly and don't affect argument list.
;; 
;; eg. (glm float "return(" "glm::length(" vec3 ")" ")")
;; will make a foreign-lambda that does `glm::length(some_vec3). types are
;; cast directly from pointers (srfi-4 vectors) so care must be taken.
(define-syntax glm
  (er-macro-transformer
   (lambda (x r t)
     (let* ((return-type (cadr x))
            (rest (cddr x))
            (arguments (filter symbol? rest)))
       `(,(r 'foreign-lambda*) ,return-type
         ( ,@(map (lambda (i a)
                    `(,(glmtype->schemetype a) ,(string->symbol (conc "a" i))))
                  (iota (length arguments))
                  arguments))
         ,(conc (stringify rest) ";"))))))

;; (with-destination <constuctor> <proc> <args> ...)
;; call proc with (cons destination args) and return destination,
;; where destination is allocated from constructor. used for returning
;; making side-effect-free versions of procedures (removing !).
(define-syntax with-destination
  (er-macro-transformer
   (lambda (x r t)
     (let ((constructor (cadr x))
           (proc (cddr x))
           (%dest (gensym 'dest)))
       (let ((head (car proc))
             (rest (cdr proc)))
         `(let ((,%dest ,constructor))
            (,head ,%dest ,@rest) ,%dest))))))



(begin-for-syntax
(define (rewrite x search replace)
  (if (list? x)
      (map (lambda (body) (rewrite body search replace)) x)
      (if (eq? x search)
          (if (procedure? replace) (replace x) replace)
          ;; convert strings back to symbols if neccessary
          ((if (symbol? x) string->symbol values)
           (if (or (symbol? x) (string? x))
               (irregex-replace/all (conc search) (conc x)
                                    ((cond ((symbol? replace) symbol->string)
                                           ((number? replace) number->string)
                                           (else values)) replace))
               x))))))


;; (rewrite `(define length/T (glm void "return(" T "," T ");")) "T" "vec3")

(define-syntax template
  (er-macro-transformer
   (lambda (x r t)
     (let* ((spec (eval (cadr x)))
            (tspec (car spec))   ;; (<template> <element> ...)
            (rspec (cdr spec))   ;; secondary rewriters
            (search (car tspec)) ;; <template>
            (lst (cdr tspec))    ;; (<elements> ...)
            (body (cddr x)))
       
       (cons (r 'begin)
             (apply append
                    (map (lambda (replace)
                           (fold
                            ;; second rewrite (optional lambdas)
                            (lambda (s/r body)
                              (let ((s (car s/r))
                                    (proc (cadr s/r)))
                                (rewrite body s (lambda (match) (proc replace)))))
                            ;; first rewrite to replace tspec
                            (rewrite body search replace)
                            rspec))
                         lst)))))))

;; *** vector constructors
;; TODO: add vec4

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


;; TODO: add mat3x4, mat4x3 etc
(define (make-mat3 fill)  (make-f32vector  9 fill))
(define (make-mat4 fill)  (make-f32vector 16 fill))

;; OBS: we won't be able to distinguish between vec4 and mat2 for example
(template
 `((D 2 3 4))

 (define ( vecD? vec) (and (f32vector? vec) (= (f32vector-length vec) D)))
 (define (dvecD? vec) (and (f64vector? vec) (= (f64vector-length vec) D)))

 (define (ivecD? vec) (and (s32vector? vec) (= (s32vector-length vec) D)))
 (define (uvecD? vec) (and (u32vector? vec) (= (u32vector-length vec) D)))

 (define (bvecD? vec) (and (u8vector? vec) (= (u8vector-length vec) D))))

(define (mat3? mat) (and (f32vector? mat) (= (f32vector-length mat)  9)))
(define (mat4? mat) (and (f32vector? mat) (= (f32vector-length mat) 16)))

;;; matrix constructors
(template
 `((T mat3 mat4))
 (define T! (glm void T "=" "glm::T(" float ")"))
 (define (T diagonal) (with-destination (make-T #f) T! diagonal)))

;;; vector operations
(template
 `((T  vec2  vec3  vec4
       uvec2 uvec3 uvec4
       ivec2 ivec3 ivec4)
   (R ,value-type))

 ;; unary operators
 (define length/T (glm R "return(" "glm::length(" T "));"))
 

 ;; prefix binary operators, primitive return type
 (template `((OP "dot" "distance"))
           (define OP/T (glm R "return(" "glm::" "OP" "(" T "," T "));"))))

;; cross is only defined for vec3
(template
 `((T vec3 ivec3 uvec3))
 (define cross/T! (glm void T "=" "glm::cross(" T "," T ")"))
 (define (cross/T veca vecb) (with-destination (make-T #f) cross/T! veca vecb)))


;; infix operators
(template
 `((T mat3 mat4 vec2 vec3 vec4))
 (template `((OP + - * /) )         
           (define OP/T/T! (glm void T "=" T "OP" T))
           (define (OP/T/T mat1 mat2) (with-destination (make-T #f) OP/T/T! mat1 mat2))))

 ;; vector unary-operators
(template
 `((T vec2  vec3  vec4
      ivec2 ivec3 ivec4
      uvec2 uvec3 uvec4))

 (template `((OP
              abs      ceil ;;  floor       fract round roundEven  sign
              sin           ;;  cos  tan  sinh  cosh  tanh
              ;;   asin acos atan asinh acosh atanh
              ;;   degrees  radians
              ;;   exp      exp2     inversesqrt log   log2  sqrt
              ;;   normalize
              ))
           (define OP/T! (glm void T "=" "glm::OP(" T ")"))
           (define (OP/T vec) (with-destination (make-T #f) OP/T! vec))))

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


(include "glm-util.scm")
(include "gtc-matrix-transform.scm")
;; lookat 
;; degrees radians
;; vec * scalar
;; vec * vec
;; mat * vec 
;; mat * scalar
;; mat * mat
)
