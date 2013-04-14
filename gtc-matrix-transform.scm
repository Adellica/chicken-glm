

#>
#include <glm/gtc/matrix_transform.hpp>
<#



(define frustum! (glm void mat4 "="  "glm::frustum("
                      float "," float "," float "," float "," float "," float ")"))
(define (frustum left right bottom top near far)
  (with-destination (make-mat4 #f) frustum! left right bottom top near far))



(define translate! (glm void mat4 "="  "glm::translate(" mat4 "," vec3 ");"))
(define (translate mat vec) (with-destination (make-mat4 #f) translate! mat vec))

(define scale! (glm void mat4 "="  "glm::scale(" mat4 "," vec3 ");"))
(define (scale mat vec) (with-destination (make-mat4 #f) scale! mat vec))

(define rotate! (glm void mat4 "="  "glm::rotate(" mat4 "," float "," vec3 ");"))
(define (rotate mat angle axis) (with-destination (make-mat4 #f) rotate! mat angle axis))

(define look-at! (glm void mat4 "=" "glm::lookAt(" vec3  "," vec3 "," vec3 ")"))
(define (look-at eye center up) (with-destination (make-mat4 #f) look-at! eye center up))

(define perspective! (glm void mat4 "=" "glm::perspective(" float "," float "," float "," float ")" ))
(define (perspective fovy aspect near far)
  (with-destination (make-mat4 #f) perspective! fovy aspect near far))





(define ortho! (glm void mat4 "=" "glm::ortho("
                    float "," float "," float "," float "," ;; lrbt
                    float "," float ")" ;; znear,zfar
                    ))
(define (ortho left right bottom top znear zfar)
  (with-destination (make-mat4 #f) ortho! left right bottom top znear zfar))


(define project! (glm void vec3 "=" "glm::project(" vec3 "," mat4 "," mat4 "," vec4 ")"))
(define (project obj model proj veiwport)
  (with-destination (make-mat4 #f) project! obj model proj veiwport))

