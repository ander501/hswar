module TestsMath where

import ElmTest exposing (..)
import Math.SL2R


all : Test
all =
    suite "Test SL2R Functions"
        [
            test "Exponentiation" (assertEqual (3 + 7) 10),
            test "Logarithms" (assertEqual "a" "b"),
            test "This test should fail" (assert False)
        ]
