(print "\n\n\nrunning run.scm")
(use glm test)



(test (ivec2 64 123) (abs/ivec2 (ivec2 -64 -123)))

(test-group
 "ceil/vec2"
 (test (vec2  65 -123) (ceil/vec2 (vec2 64.4 -123.9)))
 (test (vec2  2.0 2.0) (ceil/vec2 (vec2 1.9   1.1))))


(test (vec2 25 -30) (*/vec2/vec2 (vec2 2.5  3)
                            (vec2 10 -10)))

(test (vec2 0 0) (+/vec2/vec2 (vec2 -123.5  55.5)
                              (vec2  123.5 -55.5)))

(print (sin/vec2 (vec2 3.14 (/ 3.14 2))))
