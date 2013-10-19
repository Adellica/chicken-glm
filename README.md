# chicken-glm

  [GLM]:http://glm.g-truc.net/

Chicken Scheme bindings for [GLM]. [GLM] is a matrix and vector math library inspired by GLSL. 
These bindings are not complete, but should cover the basic matrix and vector needs within the graphics-domain.

## Quickstart

Install with:
```bash
$ chicken-install glm
```
Then run `csi` and try this:

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

;; all primitives from glm/glsl are supported:
#> (dvec2 1 2)
 #f64(1.0 2.0)
#> (ivec2 1 2)
 #s32(1 2)
#> (bvec2 1 2)
 #u8(1 2)
#> (uvec2 1 2)
 #u32(1 2)
```

Working with OpenGL, you might do something like shown below. The `mat` record holds a raw column-major 
srfi-vector and a slot with the number of columns. 
Use `mat-data` to get hold of the raw data (which is what OpenGL wants).

```scheme
(glUniformMatrix4fv transl 1 GL_FALSE
                      (mat-data (rotate (mat4 1)
                                        some-angle
                                        (vec3 0 0 1))))
```

# TODO

- Add column-selectors, eg `(mat-col mat3x4 2) => vec4`
- Better way to hard-code matrices, like `(list->mat '((1 0) (0 1))) => mat2`
- Type-conversions, eg `vec4->ivec4`, `dmat->mat`
- Dispatchers (like `v+`) for all operations (eg. we have `length/dvec3` but no generic `length/vec`)
- Hide helper-macros like template
