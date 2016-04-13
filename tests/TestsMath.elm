module TestsMath where

import ElmTest exposing (..)
import Math.Matrix2
import Math.SL2R


tolerance  = 1e-10

all : Test
all =
    suite "Test SL2R Functions"
        [
         test "ExponentiationReal" (assert <| Math.Matrix2.within tolerance
                                      (e, 0, 0, 1/e)
                                      (Math.SL2R.exp (1,0,0,-1))),
         test "ExponentiationComplex" (assert <| Math.Matrix2.within tolerance
                                         (sqrt 0.5, sqrt 0.5, -1 * sqrt 0.5, sqrt 0.5)
                                         (Math.SL2R.exp (0, pi/4, -1 * pi/4, 0))),
         test "ExponentiationDegenerate" (assert <| Math.Matrix2.within tolerance
                                         (1, 1, 0, 1)
                                         (Math.SL2R.exp (0, 1, 0, 0))),
         test "LogarithmReal" (assert <| Math.Matrix2.within tolerance
                                 (logBase e 2, 0, 0, -1 * (logBase e 2))
                                 (Math.SL2R.log (2, 0, 0, 1/2))),
         test "LogarithmComplex" (assert <| Math.Matrix2.within tolerance
                                 (0, -1 * pi/6, pi/6, 0)
                                 (Math.SL2R.log (cos(pi/6), -1 * sin(pi/6), sin(pi/6), cos(pi/6)))),
         test "LogarithmDegenerate" (assert <| Math.Matrix2.within tolerance
                                       (0, 0.8, 0, 0)
                                       (Math.SL2R.log (1, 0.8, 0, 1)))
        ]
