
(module glm *
(import chicken scheme foreign)

(use srfi-4)

#>
#include <glm/glm.hpp>
<#

(define-external (vec2 (float x) (float y))           scheme-object  (f32vector x y))
(define-external (vec3 (float x) (float y) (float z)) scheme-object  (f32vector x y z))

(define-external (ivec2 (int x) (int y))              scheme-object  (s32vector x y))
(define-external (ivec3 (int x) (int y) (int z))      scheme-object  (s32vector x y z))



(define (make-mat3 fill)  (make-f32vector  9 fill))
(define (make-mat4 fill)  (make-f32vector 16 fill))

(define (ivec2.x vec) (s32vector-ref vec 0))
(define (ivec2.y vec) (s32vector-ref vec 1))

(define (srfi4-vector-ref vec idx)
  (define (check! p) (if p (error "vector of invalid length")))
  ((cond [(f32vector? vec) (check! (<= (f32vector-length vec) 4)) f32vector-ref]
         [(s32vector? vec) (check! (<= (s32vector-length vec) 4)) s32vector-ref]) vec idx))

(define (.x vec) (srfi4-vector-ref vec 0))
(define (.y vec) (srfi4-vector-ref vec 1))
(define (.z vec) (srfi4-vector-ref vec 2))
(define (.w vec) (srfi4-vector-ref vec 3))

(define-for-syntax (glmtype->schemetype type)
  (case type
    ((vec2  vec3  vec4 mat4 mat3) 'f32vector)
    ((ivec2 ivec3 ivec4) 's32vector)
    ((float double int) type)
    (else (error "cannot convert to scheme-type" type))))

(define-for-syntax (cast glmtype var)
  (case glmtype
    ;; vects and mats need to cast from f32vector/s32vectors to glm types 
    ((
      vec2 vec3 vec4
      ivec2 ivec2 ivec3 ivec4
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

(define-syntax make-glm-operation
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

;; call proc
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

(define mat3! (make-glm-operation void mat3 "=" "glm::mat3(" float ")"))
(define mat4! (make-glm-operation void mat4 "=" "glm::mat4(" float ")"))
;; (pp (expand '(make-glm-operation void mat4 "=" "glm::mat4(" float ")")))

(define (mat3 diagonal) (with-destination (make-mat3 #f) mat3! diagonal))
(define (mat4 diagonal) (with-destination (make-mat4 #f) mat4! diagonal))

;;(pp (expand '(make-glm-operation void mat4 "=" "glm::ceil" mat4)))
;; (stringify `(mat4 "*" vec3))
;; (stringify `("glm::ceil" mat4))

;; (pp (expand '(make-glm-operation float "return(" "glm::length" vec3 ");")))
;; (pp (expand '(make-glm-operation void mat4 "=" "glm::mat4(" float ")")))
(define length/vect (make-glm-operation float "return(" "glm::length" vec3 ");"))

(define foo (make-glm-operation void mat4 "=" mat4 "*" mat4))
(define bar (make-glm-operation void mat3 "=" mat3 "+" mat3))

(define abs/vec3 (make-glm-operation void vec3 "=" "glm::abs(" vec3 ")"))


(print "beuty;" (let ((m (make-mat4 0)))
         (mat4! m 1) m))

(let ((dest (make-mat4 8))
      (dest2 (make-mat3 6))
      (abs-mat (make-mat4 -1))
      (a (make-mat4 0))
      (b (make-mat4 0)))
  
  (f32vector-set! a 0 2) (f32vector-set! a 1 -3)
  (f32vector-set! b 0 6) (f32vector-set! b 1 3)

  (foo dest a b)
  (bar dest2 a b)
  (abs/vec3 abs-mat a)
  
  (print dest)
  (print dest2)
  (print "abs: " abs-mat)
  (print "length: " (length/vect (vec3 10 10 -10))))

(define-syntax define-unary
  (er-macro-transformer
   (lambda (x r t)
     (let ((operator (cadr x))
           (type (caddr x)))
       `(define ,(string->symbol (conc operator "/" type))
          (,(r 'foreign-safe-lambda*) scheme-object ((,(glmtype->schemetype type) vin))
           ,(conc "glm::" type " res = glm::" operator "(*(glm::" type " *)vin);"
                  "return(" type "(res.x, res.y));")))))))


(define-syntax define/all-types
  (lambda (x r t)
    (let ((operation (cadr x))
          (operators (cddr x)))
      `(begin
         ,@(map (lambda (type)
                  `(begin
                     ,@(map (lambda (op)
                             (list operation op type))
                           operators)))
                '(vec2 ivec2))))))

;; (pp (expand '(define/all-types define-unary abs ceil inversesqrt)))

(define/all-types define-unary
   abs      ceil   ;;  floor       fract round roundEven  sign
;;   degrees  radians
  
   sin ;;  cos  tan  sinh  cosh  tanh
;;   asin acos atan asinh acosh atanh
  
;;   exp      exp2     inversesqrt log   log2  sqrt

;;   normalize
   )

(define-syntax define-binary/infix
  (er-macro-transformer
   (lambda (x r t)
     (let ((operator (cadr x))
           (type1 (caddr x))
           (type2 (cadddr x)))
       `(define ,(string->symbol (conc operator "/" type1 "/" type2))
          (,(r 'foreign-safe-lambda*) scheme-object ((,(glmtype->schemetype type1) a)
                                                (,(glmtype->schemetype type2) b))
           ,(conc "glm::" type1 " r = "
                  "(*(glm::" type1 "*)a)"
                  " " operator " "
                  "(*(glm::" type2 "*)b)"
                  ";\n"
                  "return(" type1 "(r.x, r.y));")))))))

(define-binary/infix + vec2 vec2)
(define-binary/infix - vec2 vec2)
(define-binary/infix * vec2 vec2)
(define-binary/infix / vec2 vec2)


;; lookat cross translate rotate degrees radians
;; vec * scalar
;; vec * vec
;; mat * vec 
;; mat * scalar
;; mat * mat
)
