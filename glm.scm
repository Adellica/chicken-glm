
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

(begin-for-syntax
 (define (value-type glmtype)
   (case glmtype
     ((vec2 vec3 vec4) 'float)
     ((ivec2 ivec3 ivec4) 'int)
     ((uvec2 uvec3 uvec4) 'unsigned-int)
     ((mat3 mat4) 'float)
     ((dmat3 dmat4) 'double)
     (else (error "no value-type for" glmtype))))
 (define glm#value-type value-type))

;; bug!? without this, we get "warning: reference to unbound variable
;; value-type" event though it's just used in 'eval' in one of the macros.
(define value-type #f)

(define-for-syntax (glmtype->schemetype type)
  (case type
    ((vec2  vec3  vec4 mat4 mat3) 'f32vector)
    ((ivec2 ivec3 ivec4) 's32vector)
    ((uvec2 uvec3 uvec4) 'u32vector)
    ((float double int) type)
    (else (error "cannot convert to scheme-type" type))))

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

(define mat3! (make-glm-operation void mat3 "=" "glm::mat3(" float ")"))
(define mat4! (make-glm-operation void mat4 "=" "glm::mat4(" float ")"))
;; (pp (expand '(make-glm-operation void mat4 "=" "glm::mat4(" float ")")))

(define (mat3 diagonal) (with-destination (make-mat3 #f) mat3! diagonal))
(define (mat4 diagonal) (with-destination (make-mat4 #f) mat4! diagonal))



(define length/ivec2 (make-glm-operation int "return(" "glm::length(" ivec2 "));"))
(define length/ivec3 (make-glm-operation int "return(" "glm::length(" ivec3 "));"))
(define length/ivec4 (make-glm-operation int "return(" "glm::length(" ivec4 "));"))

(define length/vec2 (make-glm-operation float "return(" "glm::length(" vec2 "));"))
(define length/vec3 (make-glm-operation float "return(" "glm::length(" vec3 "));"))
(define length/vec4 (make-glm-operation float "return(" "glm::length(" vec4 "));"))

(begin-for-syntax
(define (rewrite x search replace)
   (if (list? x)
       (map (lambda (body) (rewrite body search replace)) x)
       ((if (symbol? x) string->symbol values)
        (irregex-replace/all (conc search)
                             (if (symbol? x) (symbol->string x) x)
                             replace)))))

;; (rewrite `(define length/T (glm void "return(" T "," T ");")) "T" "vec3")

(define-syntax template
  (er-macro-transformer
   (lambda (x r t)
     (let* ((spec (eval (cadr x)))
            (tspec (car spec))   ;; (<template> <element> ...)
            (rspec (cdr spec))   ;; secondary rewriters
            (search (car tspec)) ;; <template>
            (lst (cdr tspec))    ;; (<elements> ...)
            (body (caddr x)))
       (cons (r 'begin)
             (map (lambda (replace)
                    (fold
                     ;; second rewrite (optional lambdas)
                     (lambda (s/r body)
                       (let ((s (car s/r))
                             (proc (cadr s/r)))
                         (rewrite body s (lambda (match) (conc (proc replace))))))
                     ;; first rewrite to replace tspec
                     (rewrite body search (conc replace))
                     rspec))
                  lst))))))

;; (pp (expand '(template `((T vec2 vec3) (R ,(lambda (t) (conc t "++" (value-type t) "++"))))
;;                        (define T/foo/T something R "return T" (returns T)))))


(template
 `((T  vec2  vec3  vec4
       uvec2 uvec3 uvec4
       ivec2 ivec3 ivec4)
   (R ,value-type))
          
 (template `((OP "dot" "distance"))
           (define OP/T (make-glm-operation R "return(" "glm::" "OP" "(" T "," T "));"))))

(print "dot:" (dot/vec2 (vec2 2 -3) (vec2 10 100)))

(print "** length " (length/ivec3 (ivec3 10 10 -10)))
(print "** length " (length/vec3 (vec3 1 1 1)))

(define */mat4/mat4! (make-glm-operation void mat4 "=" mat4 "*" mat4))
(define +/mat4/mat4! (make-glm-operation void mat4 "=" mat4 "+" mat4))

(define (*/mat4/mat4 a b) (with-destination (make-mat4 #f) */mat4/mat4! a b))
(define (+/mat4/mat4 a b) (with-destination (make-mat4 #f) +/mat4/mat4! a b))

(define abs/vec3 (make-glm-operation void vec3 "=" "glm::abs(" vec3 ")"))



(let ((dest (make-mat4 8))
      (dest2 (make-mat3 6))
      (abs-mat (make-mat4 -1))
      (a (make-mat4 0))
      (b (make-mat4 0)))
  
  (f32vector-set! a 0 2) (f32vector-set! a 1 -3)
  (f32vector-set! b 0 6) (f32vector-set! b 1 3)

  (print "*/mat4/mat4 " (*/mat4/mat4 a b))
  (abs/vec3 abs-mat a)
  
  (print dest)
  (print dest2)
  (print "abs: " abs-mat)
  (print "length: " (length/vec3 (vec3 10 10 -10))))

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
