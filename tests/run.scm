(use glm test)


(test-group
 "vec?"
 (test #t (vec2? (make-vec2 0)))
 (test #f (vec2? (make-vec3 0)))
 (test #f (vec2? (make-vec4 0)))

 (test #f (vec3? (make-vec2 0)))
 (test #t (vec3? (make-vec3 0)))
 (test #f (vec3? (make-vec4 0)))

 (test #f (vec4? (make-vec2 0)))
 (test #f (vec4? (make-vec3 0)))
 (test #t (vec4? (make-vec4 0))))

(test-group
 "matrix constructors"
 (test '#f32(
             1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0) (mat4 1))

 (test '#f32(
             1.0 0.0 0.0
                 0.0 1.0 0.0
                 0.0 0.0 1.0) (mat3 1)))

(test "* mat4/mat4"
      '#f32(
            900   1000   1100   1200
            2020   2280   2540   2800
            3140   3560   3980   4400
            4260   4840   5420   6000)
      (*/mat4/mat4 '#f32(
                         1 2 3 4
                         5 6 7 8
                         9 10 11 12
                         13 14 15 16) 
                   '#f32(
                         10 20 30 40
                         50 60 70 80
                         90 100 110 120
                         130 140 150 160 )))


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
 (test -13.0 (determinant/mat3(f32vector 1 3 4
                                         4 5 6
                                         4 3 5))))

(print (sin/vec2 (vec2 3.14 (/ 3.14 2))))
