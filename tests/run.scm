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
 "matrix multiplication"

 (test "* mat4/mat4"
       (make-mat 4 (f32vector 900   1000   1100   1200
                              2020   2280   2540   2800
                              3140   3560   3980   4400
                              4260   4840   5420   6000))
       (*/mat4x4/mat4x4 (make-mat 4 (f32vector 1 2 3 4
                                               5 6 7 8
                                               9 10 11 12
                                               13 14 15 16)) 
                        (make-mat 4 (f32vector 10 20 30 40
                                               50 60 70 80
                                               90 100 110 120
                                               130 140 150 160 ))))

 (test "* mat"
       (make-mat 4 (f32vector 22 28
                              49 64
                              76 100
                              103
                              136))
       (*/mat3x2/mat4x3 (make-mat 3 (f32vector 1 2
                                               3 4
                                               5 6))
                        (make-mat 4 (f32vector 1 2 3
                                               4 5 6
                                               7 8 9
                                               10 11 12))))
 )


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

(test-group
 "vector + delegate"
 (test +/vec2/vec2 (v+/delegate (make-vec2 0) (make-vec2 0)))
 (test +/vec3/vec3 (v+/delegate (make-vec3 0) (make-vec3 0)))
 (test +/vec4/vec4 (v+/delegate (make-vec4 0) (make-vec4 0)))
 ;; it worked for all +, let's just test one - (it's the same code)
 (test -/vec2/vec2 (v-/delegate (make-vec2 0) (make-vec2 0))))

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
  (make-mat 4 (f32vector 0 1 5 8
                         0 2 6 9
                         0 3 7 9
                         0 4 7 10))
  (transpose/mat4 (make-mat 4 (f32vector 0 0 0 0
                                         1 2 3 4
                                         5 6 7 7
                                         8 9 9 10))))
 
 (test transpose/mat4 (transpose/delegate (make-mat4 #f)))
 (test transpose/mat3 (transpose/delegate (make-mat3 #f))))

(test-group
 "cross"
 (test (vec3 -30 60 -30) (cross/vec3 (vec3 10 20 30) (vec3 4 5 6)))
 (test (ivec3 -30 60 -30) (cross/ivec3 (ivec3 10 20 30) (ivec3 4 5 6)))
 (test (uvec3 0 0 0) (cross/uvec3 (uvec3 1 2 3) (uvec3 1 2 3))) ;; TODO: how to test this?
 )

(test-group
 "gtc marix-transform"

 (test "translate"
       (make-mat 4 (f32vector 1 0 0 0
                              0 1 0 0
                              0 0 1 0
                              4 5 6 1))
       (translate (mat4 1) (vec3 4 5 6)))

 (test "scale"
       (make-mat 4 (f32vector 4 0 0 0
                              0 5 0 0
                              0 0 6 0
                              0 0 0 1))
       (scale (mat4 1) (vec3 4 5 6)))

 (test
  (make-mat 4 (f32vector 1 0 0 0
                         0 1 0 0
                         0 0 1 0
                         0 0 0 1))
  (rotate (mat4 1) 0 (vec3 1 0 0)))

 (test
  ;; TODO: allow some uncertainty here so we can round to 1 for convenience
  (make-mat 4 (f32vector -4.37113882867379e-08 0.0 -1.0 0.0
                         0.0 0.999999940395355 0.0 0.0
                         1.0 0.0 -4.37113882867379e-08 0.0
                         0.0 0.0 0.0 1.0))
  (rotate (mat4 1) 90 (vec3 0 1 0))))

(test
 (make-mat 4 (f32vector -1 0 -.0 0
                         0 1 -.0 0
                         0 -.0 -1 0
                         -.0 -.0 0 1))
 (look-at (vec3 0 0 0) ;; eye: position of camera
          (vec3 0 0 1) ;; center: lookint at point
          (vec3 0 1 0) ;; camera orientation
          ))

(test-group
 "mat* delegate"
 
 (test */mat2x2/mat2x2 (m*/delegate (make-mat2x2 #f) (make-mat2x2 #f)))
 (test */mat2x3/mat2x2 (m*/delegate (make-mat2x3 #f) (make-mat2x2 #f)))
 (test */mat2x4/mat2x2 (m*/delegate (make-mat2x4 #f) (make-mat2x2 #f)))

 (test */mat2x2/mat3x2 (m*/delegate (make-mat2x2 #f) (make-mat3x2 #f)))
 (test */mat2x3/mat3x2 (m*/delegate (make-mat2x3 #f) (make-mat3x2 #f)))
 (test */mat2x4/mat3x2 (m*/delegate (make-mat2x4 #f) (make-mat3x2 #f)))

 (test */mat2x2/mat4x2 (m*/delegate (make-mat2x2 #f) (make-mat4x2 #f)))
 (test */mat2x3/mat4x2 (m*/delegate (make-mat2x3 #f) (make-mat4x2 #f)))
 (test */mat2x4/mat4x2 (m*/delegate (make-mat2x4 #f) (make-mat4x2 #f)))

 (test */mat3x2/mat2x3 (m*/delegate (make-mat3x2 #f) (make-mat2x3 #f)))
 (test */mat3x3/mat2x3 (m*/delegate (make-mat3x3 #f) (make-mat2x3 #f)))
 (test */mat3x4/mat2x3 (m*/delegate (make-mat3x4 #f) (make-mat2x3 #f)))

 (test */mat3x2/mat3x3 (m*/delegate (make-mat3x2 #f) (make-mat3x3 #f)))
 (test */mat3x3/mat3x3 (m*/delegate (make-mat3x3 #f) (make-mat3x3 #f)))
 (test */mat3x4/mat3x3 (m*/delegate (make-mat3x4 #f) (make-mat3x3 #f)))

 (test */mat3x2/mat4x3 (m*/delegate (make-mat3x2 #f) (make-mat4x3 #f)))
 (test */mat3x3/mat4x3 (m*/delegate (make-mat3x3 #f) (make-mat4x3 #f)))
 (test */mat3x4/mat4x3 (m*/delegate (make-mat3x4 #f) (make-mat4x3 #f)))


 (test */mat4x2/mat2x4 (m*/delegate (make-mat4x2 #f) (make-mat2x4 #f)))
 (test */mat4x3/mat2x4 (m*/delegate (make-mat4x3 #f) (make-mat2x4 #f)))
 (test */mat4x4/mat2x4 (m*/delegate (make-mat4x4 #f) (make-mat2x4 #f)))

 (test */mat4x2/mat3x4 (m*/delegate (make-mat4x2 #f) (make-mat3x4 #f)))
 (test */mat4x3/mat3x4 (m*/delegate (make-mat4x3 #f) (make-mat3x4 #f)))
 (test */mat4x4/mat3x4 (m*/delegate (make-mat4x4 #f) (make-mat3x4 #f)))

 (test */mat4x2/mat4x4 (m*/delegate (make-mat4x2 #f) (make-mat4x4 #f)))
 (test */mat4x3/mat4x4 (m*/delegate (make-mat4x3 #f) (make-mat4x4 #f)))
 (test */mat4x4/mat4x4 (m*/delegate (make-mat4x4 #f) (make-mat4x4 #f)))

 )

(test-group
 "mat vec multiplication delegate"
 (test */mat2x2/vec2 (m*/delegate (mat2x2 1) (vec2 0 0)))
 (test */mat3x3/vec3 (m*/delegate (mat3x3 1) (vec3 0 0 0)))
 (test */mat4x4/vec4 (m*/delegate (mat4x4 1) (vec4 0 0 0 0))))

(test-group
 "m+ delegate"
 (test +/mat2x2 (m+/delegate (mat2x2 1) (mat2x2 1)))
 (test +/mat2x3 (m+/delegate (mat2x3 1) (mat2x3 1)))
 (test +/mat2x4 (m+/delegate (mat2x4 1) (mat2x4 1)))

 (test +/mat3x2 (m+/delegate (mat3x2 1) (mat3x2 1)))
 (test +/mat3x3 (m+/delegate (mat3x3 1) (mat3x3 1)))
 (test +/mat3x4 (m+/delegate (mat3x4 1) (mat3x4 1)))
 
 (test +/mat4x2 (m+/delegate (mat4x2 1) (mat4x2 1)))
 (test +/mat4x3 (m+/delegate (mat4x3 1) (mat4x3 1)))
 (test +/mat4x4 (m+/delegate (mat4x4 1) (mat4x4 1))))

(test-group
 "determinant"
 (test 4.0 (determinant (mat2 2)))
 (test 8.0 (determinant (mat3 2)))
 (test 16.0 (determinant (mat4 2))))

(test-group
 "inverse"
 ;; TODO: make -0.0 == 0.0 ==> true
 ;; (test (mat2 0.5) (inverse (mat2 2)))
 ;; (test (mat3 0.5) (inverse (mat3 2)))
 ;; (test (mat4 0.5) (inverse (mat4 2)))
 )

(test-group
 "m+ vector-vector"
 (test "m+"
       (make-mat2 5)
       (m+ (make-mat 2 (f32vector 1 2
                                  3 4))
           (make-mat 2 (f32vector 4 3
                                  2 1))))

 (test (vec2 11 22)        (m+ (vec2 1 2)      (vec2 10 20)))
 (test (vec3 11 22 33)     (m+ (vec3 1 2 3)    (vec3 10 20 30)))
 (test (vec4 11 22 33 44)  (m+ (vec4 1 2 3 4)  (vec4 10 20 30 40)))

 (test (dvec2 11 22)       (m+ (dvec2 1 2)     (dvec2 10 20)))
 (test (dvec3 11 22 33)    (m+ (dvec3 1 2 3)   (dvec3 10 20 30)))
 (test (dvec4 11 22 33 44) (m+ (dvec4 1 2 3 4) (dvec4 10 20 30 40)))

 (test (ivec2 11 22)       (m+ (ivec2 1 2)     (ivec2 10 20)))
 (test (ivec3 11 22 33)    (m+ (ivec3 1 2 3)   (ivec3 10 20 30)))
 (test (ivec4 11 22 33 44) (m+ (ivec4 1 2 3 4) (ivec4 10 20 30 40)))

 (test (uvec2 11 22)       (m+ (uvec2 1 2)     (uvec2 10 20)))
 (test (uvec3 11 22 33)    (m+ (uvec3 1 2 3)   (uvec3 10 20 30)))
 (test (uvec4 11 22 33 44) (m+ (uvec4 1 2 3 4) (uvec4 10 20 30 40))))


(test-group
 "m+ vector-scalar"
 
 (test (vec2 11 12)       (m+ (vec2 1 2) 10))
 (test (vec3 11 12 13)    (m+ (vec3 1 2 3) 10))
 (test (vec4 11 12 13 14) (m+ (vec4 1 2 3 4) 10))

 (test (dvec2 11 12)       (m+ (dvec2 1 2) 10))
 (test (dvec3 11 12 13)    (m+ (dvec3 1 2 3) 10))
 (test (dvec4 11 12 13 14) (m+ (dvec4 1 2 3 4) 10))

 (test (ivec2 11 12)       (m+ (ivec2 1 2) 10))
 (test (ivec3 11 12 13)    (m+ (ivec3 1 2 3) 10))
 (test (ivec4 11 12 13 14) (m+ (ivec4 1 2 3 4) 10))

 (test (uvec2 11 12)       (m+ (uvec2 1 2) 10))
 (test (uvec3 11 12 13)    (m+ (uvec3 1 2 3) 10))
 (test (uvec4 11 12 13 14) (m+ (uvec4 1 2 3 4) 10)))

(test-group
 "m* vector-scalar"
 
 (test (vec2 10 20)       (m* (vec2 1 2) 10))
 (test (vec3 10 20 30)    (m* (vec3 1 2 3) 10))
 (test (vec4 10 20 30 40) (m* (vec4 1 2 3 4) 10))

 (test (dvec2 10 20)       (m* (dvec2 1 2) 10))
 (test (dvec3 10 20 30)    (m* (dvec3 1 2 3) 10))
 (test (dvec4 10 20 30 40) (m* (dvec4 1 2 3 4) 10))

 (test (ivec2 10 20)       (m* (ivec2 1 2) 10))
 (test (ivec3 10 20 30)    (m* (ivec3 1 2 3) 10))
 (test (ivec4 10 20 30 40) (m* (ivec4 1 2 3 4) 10))

 (test (uvec2 10 20)       (m* (uvec2 1 2) 10))
 (test (uvec3 10 20 30)    (m* (uvec3 1 2 3) 10))
 (test (uvec4 10 20 30 40) (m* (uvec4 1 2 3 4) 10)))

(test-group
 "v/ vector-scalar"
 
 (test (vec2 10 20)       (v/ (vec2 100 200) 10))
 (test (vec3 10 20 30)    (v/ (vec3 100 200 300) 10))
 (test (vec4 10 20 30 40) (v/ (vec4 100 200 300 400) 10))
 ;; the other types probably work too ... :S
 )

(test-group
 "v/ vector-vector"
 
 (test (vec2 10 10)       (v/ (vec2 100 200) (vec2 10 20)))
 (test (vec3 10 10 10)    (v/ (vec3 100 200 300) (vec3 10 20 30)))
 (test (vec4 10 10 10 10) (v/ (vec4 100 200 300 400) (vec4 10 20 30 40)))
 ;; the other types probably work too ... :S
 )


 


(test-group
 "m* vector-vector"
 
 (test (vec2 10 40)        (m* (vec2 1 2)     (vec2 10 20)))
 (test (vec3 10 40 90)     (m* (vec3 1 2 3)   (vec3 10 20 30)))
 (test (vec4 10 40 90 160) (m* (vec4 1 2 3 4) (vec4 10 20 30 40)))
 
 (test (dvec2 10 40)        (m* (dvec2 1 2)     (dvec2 10 20)))
 (test (dvec3 10 40 90)     (m* (dvec3 1 2 3)   (dvec3 10 20 30)))
 (test (dvec4 10 40 90 160) (m* (dvec4 1 2 3 4) (dvec4 10 20 30 40)))
 
 (test (ivec2 10 40)        (m* (ivec2 1 2)     (ivec2 10 20)))
 (test (ivec3 10 40 90)     (m* (ivec3 1 2 3)   (ivec3 10 20 30)))
 (test (ivec4 10 40 90 160) (m* (ivec4 1 2 3 4) (ivec4 10 20 30 40)))
 
 (test (uvec2 10 40)        (m* (uvec2 1 2)     (uvec2 10 20)))
 (test (uvec3 10 40 90)     (m* (uvec3 1 2 3)   (uvec3 10 20 30)))
 (test (uvec4 10 40 90 160) (m* (uvec4 1 2 3 4) (uvec4 10 20 30 40))))


(test-group
 "length/vec"
 
 (test (sqrt 2) (length/vec (vec2 1 1)))
 (test (sqrt 3) (length/vec (vec3 1 1 1)))
 (test (sqrt 4) (length/vec (vec4 1 1 1 1)))
 
 (test (sqrt 2) (length/vec (dvec2 1 1)))
 (test (sqrt 3) (length/vec (dvec3 1 1 1)))
 (test (sqrt 4) (length/vec (dvec4 1 1 1 1)))
 
 (test 14 (length/vec (ivec2 10 10)))
 (test 17 (length/vec (ivec3 10 10 10)))
 (test 20 (length/vec (ivec4 10 10 10 10)))
 
 (test 14 (length/vec (uvec2 10 10)))
 (test 17 (length/vec (uvec3 10 10 10)))
 (test 20 (length/vec (uvec4 10 10 10 10)))

 (test 0 (length/vec (bvec2 0 0)))
 (test 1 (length/vec (bvec2 0 1)))
 (test 1 (length/vec (bvec2 1 1)))
 (test 1 (length/vec (bvec2 1 0)))
 (test 1 (length/vec (bvec3 1 1 1)))
 (test 1 (length/vec (bvec4 1 1 1 1)))

 )
(test-group
 "dot"
 
 (test  50.0 (dot (vec2 1 2)     (vec2 10 20)))
 (test 140.0 (dot (vec3 1 2 3)   (vec3 10 20 30)))
 (test 300.0 (dot (vec4 1 2 3 4) (vec4 10 20 30 40)))

 (test  50.0 (dot (dvec2 1 2)     (dvec2 10 20)))
 (test 140.0 (dot (dvec3 1 2 3)   (dvec3 10 20 30)))
 (test 300.0 (dot (dvec4 1 2 3 4) (dvec4 10 20 30 40)))
  
 (test  50 (dot (ivec2 1 2)     (ivec2 10 20)))
 (test 140 (dot (ivec3 1 2 3)   (ivec3 10 20 30)))
 (test 300 (dot (ivec4 1 2 3 4) (ivec4 10 20 30 40)))
  
 (test  50 (dot (uvec2 1 2)     (uvec2 10 20)))
 (test 140 (dot (uvec3 1 2 3)   (uvec3 10 20 30)))
 (test 300 (dot (uvec4 1 2 3 4) (uvec4 10 20 30 40)))
  
 (test 0 (dot (bvec2 0 1)     (bvec2 1 0)))
 (test 1 (dot (bvec3 1 2 3)   (bvec3 10 20 30)))
 (test 1 (dot (bvec4 1 2 3 4) (bvec4 10 20 30 40)))

 )


(test-exit)
