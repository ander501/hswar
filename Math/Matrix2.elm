module Math.Matrix2 where

type Matrix2 = Matrix2 Float Float Float Float

m1 -*- m2 = multiply m1 m2
infixl 5 -*-

m1 -+- m2 = add m1 m2
infixl 9 -+-

add : Matrix2 -> Matrix2 -> Matrix2
add (Matrix2 a1 b1 c1 d1) (Matrix2 a2 b2 c2 d2) = Matrix2 (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)

multiply : Matrix2 -> Matrix2 -> Matrix2
multiply (Matrix2 a1 b1 c1 d1) (Matrix2 a2 b2 c2 d2) = Matrix2 ( a1 * a2 + b1 * c2) (a1 * b2 + b1 * d2) (c1 * a2 + d1 * c2) (c1 * b2 + d1 * d2)

det : Matrix2 -> Float
det (Matrix2 a b c d) = a * d - b * c

scale : Float -> Matrix2 -> Matrix2
scale m (Matrix2 a b c d) = Matrix2 (m * a) (m * b) (m * c) (m * d)

norm : Matrix2 -> Float
norm (Matrix2 a b c d) = a * a + b * b + c * c + d * d

id : Matrix2
id = (Matrix2 1 0 0 1)


