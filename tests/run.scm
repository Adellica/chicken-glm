(use glm test)


(test-group
 "vec?"

 ;; float
 (test #t (vec2? (make-vec2 0)))
 (test #f (vec2? (make-vec3 0)))
 (test #f (vec2? (make-vec4 0)))

 (test #f (vec3? (make-vec2 0)))
 (test #t (vec3? (make-vec3 0)))
 (test #f (vec3? (make-vec4 0)))

 (test #f (vec4? (make-vec2 0)))
 (test #f (vec4? (make-vec3 0)))
 (test #t (vec4? (make-vec4 0)))

 ;; double
 (test #t (dvec2? (make-dvec2 0)))
 (test #f (dvec2? (make-dvec3 0)))
 (test #f (dvec2? (make-dvec4 0)))

 (test #f (dvec3? (make-dvec2 0)))
 (test #t (dvec3? (make-dvec3 0)))
 (test #f (dvec3? (make-dvec4 0)))

 (test #f (dvec4? (make-dvec2 0)))
 (test #f (dvec4? (make-dvec3 0)))
 (test #t (dvec4? (make-dvec4 0)))

 ;; signed int
 (test #t (ivec2? (make-ivec2 0)))
 (test #f (ivec2? (make-ivec3 0)))
 (test #f (ivec2? (make-ivec4 0)))

 (test #f (ivec3? (make-ivec2 0)))
 (test #t (ivec3? (make-ivec3 0)))
 (test #f (ivec3? (make-ivec4 0)))

 (test #f (ivec4? (make-ivec2 0)))
 (test #f (ivec4? (make-ivec3 0)))
 (test #t (ivec4? (make-ivec4 0)))

 ;; unsigned int
 (test #t (uvec2? (make-uvec2 0)))
 (test #f (uvec2? (make-uvec3 0)))
 (test #f (uvec2? (make-uvec4 0)))

 (test #f (uvec3? (make-uvec2 0)))
 (test #t (uvec3? (make-uvec3 0)))
 (test #f (uvec3? (make-uvec4 0)))

 (test #f (uvec4? (make-uvec2 0)))
 (test #f (uvec4? (make-uvec3 0)))
 (test #t (uvec4? (make-uvec4 0)))


 ;; bool
 (test #t (bvec2? (make-bvec2 0)))
 (test #f (bvec2? (make-bvec3 0)))
 (test #f (bvec2? (make-bvec4 0)))

 (test #f (bvec3? (make-bvec2 0)))
 (test #t (bvec3? (make-bvec3 0)))
 (test #f (bvec3? (make-bvec4 0)))

 (test #f (bvec4? (make-bvec2 0)))
 (test #f (bvec4? (make-bvec3 0)))
 (test #t (bvec4? (make-bvec4 0)))

 )

(test-group
 "vector constructors"
 (test (f32vector 1 2) ( vec2 1 2))
 (test (f64vector 1 2) (dvec2 1 2))
 (test (s32vector 1 2) (ivec2 1 2))
 (test (u32vector 1 2) (uvec2 1 2))
 (test (u8vector  1 2) (bvec2 1 2))
 
 (test (f32vector 1 2 3) ( vec3 1 2 3))
 (test (f64vector 1 2 3) (dvec3 1 2 3))
 (test (s32vector 1 2 3) (ivec3 1 2 3))
 (test (u32vector 1 2 3) (uvec3 1 2 3))
 (test (u8vector  1 2 3) (bvec3 1 2 3))

 (test (f32vector 1 2 3 4) ( vec4 1 2 3 4))
 (test (f64vector 1 2 3 4) (dvec4 1 2 3 4))
 (test (s32vector 1 2 3 4) (ivec4 1 2 3 4))
 (test (u32vector 1 2 3 4) (uvec4 1 2 3 4))
 (test (u8vector  1 2 3 4) (bvec4 1 2 3 4)))

;; note: matrix data (f32vector) is stored column-major
(test-group
 "matrix constructors"

 (test (make-mat 2
                 (f32vector 1 0
                            0 1))
       (mat2x2 1))

 (test (make-mat 2
                 (f32vector 1 0 0
                            0 1 0))
       (mat2x3 1))

 ;; OBS: this fails on glm 0.9.4 (I have submitted a patch which might
 ;; fix it)
 (test (make-mat 2
                 (f32vector 1 0 0 0
                            0 1 0 0))
       (mat2x4 1))



 (test (make-mat 3
                 (f32vector 1 0 
                            0 1
                            0 0))
       (mat3x2 1))
 (test (make-mat 3
                 (f32vector 1 0 0
                            0 1 0
                            0 0 1))
       (mat3x3 1))
 (test (make-mat 3
                 (f32vector 1 0 0 0
                            0 1 0 0
                            0 0 1 0))
       (mat3x4 1))


 
 (test (make-mat 4
                 (f32vector 1 0 
                            0 1
                            0 0
                            0 0))
       (mat4x2 1))
 (test (make-mat 4
                 (f32vector 1 0 0
                            0 1 0
                            0 0 1
                            0 0 0))
       (mat4x3 1))
 (test (make-mat 4
                 (f32vector 1 0 0 0
                            0 1 0 0
                            0 0 1 0
                            0 0 0 1))
       (mat4x4 1)))



(test-group
 "vector binary infix"
 (test (vec2 1.1 2.2) (+/vec2/vec2 (vec2 0.1 0.2) (vec2 1 2)))
 (test (vec2 7 14)    (-/vec2/vec2 (vec2 10 20) (vec2 3 6)))
 (test (vec2 3 8)    (*/vec2/vec2 (vec2 1 2) (vec2 3 4)))
 (test (vec2 5 25)    (//vec2/vec2 (vec2 10 100) (vec2 2 4)))

 (test (vec3 1.1 2.2 3.3) (+/vec3/vec3 (vec3 0.1 0.2 0.3) (vec3 1 2 3)))
 (test (vec3 7 14 21)     (-/vec3/vec3 (vec3 10 20 30) (vec3 3 6 9)))
 (test (vec3 4 10 18)     (*/vec3/vec3 (vec3 1 2 3) (vec3 4 5 6)))
 (test (vec3 5 25 125)    (//vec3/vec3 (vec3 10 100 1000) (vec3 2 4 8)))

 (test (vec4 6 8 10 12) (+/vec4/vec4 (vec4 1 2 3 4) (vec4 5 6 7 8)))
 )

(test (ivec2 64 123) (abs/ivec2 (ivec2 -64 -123)))

(test-group
 "ceil/vec2"
 (test (vec2  65 -123) (ceil/vec2 (vec2 64.4 -123.9)))
 (test (vec2  2.0 2.0) (ceil/vec2 (vec2 1.9   1.1))))


(test (vec2 25 -30) (*/vec2/vec2 (vec2 2.5  3)
                            (vec2 10 -10)))


(test-group
 "vector length"
 (test (sqrt 2) (length/vec2 (vec2 1 1)))
 (test 141      (length/ivec2 (ivec2 -100 100)))
 (test 141      (length/uvec2 (uvec2 100 100))))



(test-group
 "transpose"

 (test
 (f32vector 0 1 5 8
            0 2 6 9
            0 3 7 9
            0 4 7 10)
 (transpose/mat4 (f32vector 0 0 0 0
                            1 2 3 4
                            5 6 7 7
                            8 9 9 10)))
 
 (test transpose/mat4 (transpose/delegate (make-mat4 #f)))
 (test transpose/mat3 (transpose/delegate (make-mat3 #f))))

(test-group
 "cross"
 (test (vec3 -30 60 -30) (cross/vec3 (vec3 10 20 30) (vec3 4 5 6)))
 (test (ivec3 -30 60 -30) (cross/ivec3 (ivec3 10 20 30) (ivec3 4 5 6)))
 (test (uvec3 0 0 0) (cross/uvec3 (uvec3 1 2 3) (uvec3 1 2 3))) ;; TODO: how to test this?
 )

(test-group
 "determinant"
 (test -13.0 (determinant/mat3 (f32vector 1 3 4
                                          4 5 6
                                          4 3 5))))

(test-group
 "gtc marix-transform"

 (test "translate"
       ;; column-major
       (f32vector 1 0 0 0
                  0 1 0 0
                  0 0 1 0
                  4 5 6 1)
       (translate (mat4 1) (vec3 4 5 6)))

 (test "scale"
       (f32vector 4 0 0 0
                  0 5 0 0
                  0 0 6 0
                  0 0 0 1)
       (scale (mat4 1) (vec3 4 5 6)))

 (test
  (f32vector 1 0 0 0
             0 1 0 0
             0 0 1 0
             0 0 0 1)
  (rotate (mat4 1) 0 (vec3 1 0 0)))

 (test
  ;; TODO: allow some uncertainty here so we can round to 1 for convenience
  (f32vector -4.37113882867379e-08 0.0 -1.0 0.0
             0.0 0.999999940395355 0.0 0.0
             1.0 0.0 -4.37113882867379e-08 0.0
             0.0 0.0 0.0 1.0)
  (rotate (mat4 1) 90 (vec3 0 1 0))))

(print-mat4 (look-at (vec3 0 0 0) ;; eye: position of camera
                     (vec3 0 0 1) ;; center: lookint at point
                     (vec3 0 1 0) ;; camera orientation
                     ))

(test-group
 "mat* delegate"
 
 (test */mat2x2/mat2x2 (mat*/delegate (make-mat2x2 #f) (make-mat2x2 #f)))
 (test */mat2x3/mat2x2 (mat*/delegate (make-mat2x3 #f) (make-mat2x2 #f)))
 (test */mat2x4/mat2x2 (mat*/delegate (make-mat2x4 #f) (make-mat2x2 #f)))

 (test */mat2x2/mat3x2 (mat*/delegate (make-mat2x2 #f) (make-mat3x2 #f)))
 (test */mat2x3/mat3x2 (mat*/delegate (make-mat2x3 #f) (make-mat3x2 #f)))
 (test */mat2x4/mat3x2 (mat*/delegate (make-mat2x4 #f) (make-mat3x2 #f)))

 (test */mat2x2/mat4x2 (mat*/delegate (make-mat2x2 #f) (make-mat4x2 #f)))
 (test */mat2x3/mat4x2 (mat*/delegate (make-mat2x3 #f) (make-mat4x2 #f)))
 (test */mat2x4/mat4x2 (mat*/delegate (make-mat2x4 #f) (make-mat4x2 #f)))

 (test */mat3x2/mat2x3 (mat*/delegate (make-mat3x2 #f) (make-mat2x3 #f)))
 (test */mat3x3/mat2x3 (mat*/delegate (make-mat3x3 #f) (make-mat2x3 #f)))
 (test */mat3x4/mat2x3 (mat*/delegate (make-mat3x4 #f) (make-mat2x3 #f)))

 (test */mat3x2/mat3x3 (mat*/delegate (make-mat3x2 #f) (make-mat3x3 #f)))
 (test */mat3x3/mat3x3 (mat*/delegate (make-mat3x3 #f) (make-mat3x3 #f)))
 (test */mat3x4/mat3x3 (mat*/delegate (make-mat3x4 #f) (make-mat3x3 #f)))

 (test */mat3x2/mat4x3 (mat*/delegate (make-mat3x2 #f) (make-mat4x3 #f)))
 (test */mat3x3/mat4x3 (mat*/delegate (make-mat3x3 #f) (make-mat4x3 #f)))
 (test */mat3x4/mat4x3 (mat*/delegate (make-mat3x4 #f) (make-mat4x3 #f)))


 (test */mat4x2/mat2x4 (mat*/delegate (make-mat4x2 #f) (make-mat2x4 #f)))
 (test */mat4x3/mat2x4 (mat*/delegate (make-mat4x3 #f) (make-mat2x4 #f)))
 (test */mat4x4/mat2x4 (mat*/delegate (make-mat4x4 #f) (make-mat2x4 #f)))

 (test */mat4x2/mat3x4 (mat*/delegate (make-mat4x2 #f) (make-mat3x4 #f)))
 (test */mat4x3/mat3x4 (mat*/delegate (make-mat4x3 #f) (make-mat3x4 #f)))
 (test */mat4x4/mat3x4 (mat*/delegate (make-mat4x4 #f) (make-mat3x4 #f)))

 (test */mat4x2/mat4x4 (mat*/delegate (make-mat4x2 #f) (make-mat4x4 #f)))
 (test */mat4x3/mat4x4 (mat*/delegate (make-mat4x3 #f) (make-mat4x4 #f)))
 (test */mat4x4/mat4x4 (mat*/delegate (make-mat4x4 #f) (make-mat4x4 #f)))

 )

(print (sin/vec2 (vec2 3.14 (/ 3.14 2))))
