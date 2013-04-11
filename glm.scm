
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

;;(define-external (mat4 (float x00) (float x01) (float x02) (float x03)))

(define (ivec2.x vec) (s32vector-ref vec 0))
(define (ivec2.y vec) (s32vector-ref vec 1))

(define (.x vec)
  (cond [(f32vector? vec) (f32vector-ref vec 0)]
        [(s32vector? vec) (s32vector-ref vec 0)]))

(define-for-syntax (glmtype->schemetype type)
  (case type
    ((vec2  vec3  vec4) 'f32vector)
    ((ivec2 ivec3 ivec4) 's32vector)
    ((mat4 mat3))))

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
  abs  ceil  floor  fract
  round  roundEven  sign
  degrees  radians
  asin  acos
  sin  cos
  exp  exp2
  inversesqrt)

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
)
