module Math.Matrix2 where

type alias Matrix2 = (Float, Float, Float, Float)

(-*-) = multiply
infixl 5 -*-

(-+-) = add
infixl 9 -+-

add : Matrix2 -> Matrix2 -> Matrix2
add (a1, b1, c1, d1) (a2, b2, c2, d2) = (a1 + a2, b1 + b2, c1 + c2, d1 + d2)

multiply : Matrix2 -> Matrix2 -> Matrix2
multiply (a1, b1, c1, d1) (a2, b2, c2, d2) = ( a1 * a2 + b1 * c2, a1 * b2 + b1 * d2, c1 * a2 + d1 * c2, c1 * b2 + d1 * d2)

det : Matrix2 -> Float
det (a, b, c, d) = a * d - b * c

scale : Float -> Matrix2 -> Matrix2
scale m (a, b, c, d) = (m * a, m * b, m * c, m * d)

norm : Matrix2 -> Float
norm (a, b, c, d) = a * a + b * b + c * c + d * d

id : Matrix2
id = (1, 0, 0, 1)


