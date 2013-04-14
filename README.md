chicken-glm
===========

Chicken Scheme bindings for the GLM.

[GLM](http://glm.g-truc.net/) is a matrix and vector math library inspired by GLSL.

## Quickstart

```scheme
#;1> (use glm)
; loading /usr/local/lib/chicken/6/glm.import.so ...
; loading /usr/local/lib/chicken/6/srfi-4.import.so ...
; loading /usr/local/lib/chicken/6/glm.so ...
#;2> (vec3 1 2 3)
 #f32(1.0 2.0 3.0)

#;3> (vec4 4 3 1 0)
 #f32(4.0 3.0 1.0 0.0)

#;4> (mat4 1)
#<mat 4x4:
1.0 0.0 0.0 0.0 
0.0 1.0 0.0 0.0 
0.0 0.0 1.0 0.0 
0.0 0.0 0.0 1.0 >

#;5> (scale (mat4 1) (vec3 1 2 3))
#<mat 4x4:
1.0 0.0 0.0 0.0 
0.0 2.0 0.0 0.0 
0.0 0.0 3.0 0.0 
0.0 0.0 0.0 1.0 >

#;6> (m* (scale (mat4 1) (vec3 1 2 3)) (vec4 10 20 30 0))
#f32(10.0 40.0 90.0 0.0)
```
