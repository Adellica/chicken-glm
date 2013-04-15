chicken-glm
===========

Chicken Scheme bindings for the GLM.

[GLM](http://glm.g-truc.net/) is a matrix and vector math library inspired by GLSL.

## Quickstart

```scheme
#> (use glm)
; loading /usr/local/lib/chicken/6/glm.import.so ...
; loading /usr/local/lib/chicken/6/srfi-4.import.so ...
; loading /usr/local/lib/chicken/6/glm.so ...
#> (vec3 1 2 3)
 #f32(1.0 2.0 3.0)

#> (vec4 4 3 1 0)
 #f32(4.0 3.0 1.0 0.0)

#> (vec3 1 2 3)
 #f32(1.0 2.0 3.0)

;; v+ for vector-vector addition
#> (v+ (vec3 1 2 3) (vec3 10 20 30))
 #f32(11.0 22.0 33.0)

;; m+ for matrix-matrix, matrix-vector or vector-vector addition
#> (m+ (vec3 1 2 3) (vec3 10 20 30))
 #f32(11.0 22.0 33.0)

#> (m* (mat2 2) (vec2 -1 -2))
 #f32(-2.0 -4.0)

#> (inverse (inverse (mat2 4)))
#<mat 2x2:
4.0 0.0 
0.0 4.0 >

;; creating an identity matrix
#> (mat2 1)
#<mat 2x2:
1.0 0.0 
0.0 1.0 >

;; or a bigger one
#> (mat4 1)
#<mat 4x4:
1.0 0.0 0.0 0.0 
0.0 1.0 0.0 0.0 
0.0 0.0 1.0 0.0 
0.0 0.0 0.0 1.0 >

;; most of functions from
;; gtc_matrix_transform are present
#> (scale (mat4 1) (vec3 1 2 3))
#<mat 4x4:
1.0 0.0 0.0 0.0 
0.0 2.0 0.0 0.0 
0.0 0.0 3.0 0.0 
0.0 0.0 0.0 1.0 >

#> (translate (scale (mat4 1) (vec3 1 2 3)) (vec3 10 20 30))
#<mat 4x4:
1.0 0.0 0.0 10.0 
0.0 2.0 0.0 40.0 
0.0 0.0 3.0 90.0 
0.0 0.0 0.0 1.0 >

#> (m* (scale (mat4 1) (vec3 1 2 3)) (vec4 10 20 30 0))
#f32(10.0 40.0 90.0 0.0)

;; all primitives from glm/glsl are in the works:
;; (don't yet work with v+, but you can do (+/ivec2/ivec2 ... ))

#> (dvec2 1 2)
 #f64(1.0 2.0)
#> (ivec2 1 2)
 #s32(1 2)
#> (bvec2 1 2)
 #u8(1 2)
#> (uvec2 1 2)
 #u32(1 2)
```
