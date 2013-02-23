## Lisp Tiler

This is an implementation of the Squarified tiling algorithm in lisp.

The actual function is in squarify.lisp

The drawing is very primitive, it will just take the returned rectangles from `squarify` and draws them using cl-opengl. The point of it was more to just demonstrate the tiling.

## Example

The below code gives an example of how to tile a set of areas inside of a 200x200 rectangle.

```lisp
(setf my-areas (list 100 200 300 400))
(setf rect (create-rectangle 0 0 200 200))
(squarify my-areas rect)
```

Here is the calculated rectangles:

```
(400/3, 0, 200/3, 60)
(0, 0, 400/3, 60)
(0, 60, 600/7, 140)
(600/7, 60, 800/7, 140)
```

And here is what it looks like visually:
[alt visual example][https://raw.github.com/lerp/lisp-tiler/master/example.png]
