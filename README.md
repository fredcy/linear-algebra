# linear-algebra
Linear algebra in pure Elm.

This code is based on https://github.com/elm-community/elm-linear-algebra.

It differs in using only Elm code and no native Javascript code.

It's currently a PoC with enough of Math.Matrix4 coded to support the "crate" example in elm-webgl.

However, the crate example builds against this code but then crashes immediately with this error:

    Uncaught TypeError: Failed to execute 'uniformMatrix4fv' on 'WebGLRenderingContext': No function was found that matched the signature provided.
