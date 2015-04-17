module Math.SL2R where

import Math.Matrix2 (..)

-- Represent a point as a 2x2 matrix in SL2R
type alias Point = Matrix2
type alias Tangent = Matrix2

toPoincareHalfplane : Point -> (Float, Float)
toPoincareHalfplane (Matrix2 a b c d) 
    = let denom = ( c * c + d * d )
      in ( (b * d - a * c) / denom, (a * d - b * c) / denom )

toPoincareDisc : Point -> (Float, Float)
toPoincareDisc (Matrix2 a b c d) 
    = let denom = ( a + d ) * ( a + d ) + ( b - c ) * (b - c)
      in ( (a*a + b*b - c*c - d*d )/ denom, -2 * ( a*c + b*d )/ denom )

translate : Point -> Point -> Point
translate = multiply

exp : Float -> Tangent -> Point
exp t a = let dis = det a
          in if | dis > 0  -> expComplex t a
                | dis == 0 -> expDegen t a
                | dis < 0  -> expReal t a

expComplex : Float -> Tangent -> Point
expComplex t a = let lambda = sqrt (det a)
                 in (scale ( cos <| t * lambda) id) 
                    -+- (scale ( sin <| t * lambda) a) 

sinh x = 0.5 * ( e^x - e^(-x) )
cosh x = 0.5 * ( e^x + e^(-x) )

expDegen : Float -> Tangent -> Point
expDegen t a = id -+- (scale t a)

expReal : Float -> Tangent -> Point
expReal t a = let lambda = sqrt (-1 * det a)
              in scale (cosh <| t * lambda) id
                 -+- scale (sinh <| -t * lambda) a

inv : Point -> Point
inv (Matrix2 a b c d) = (Matrix2 d -b -c a)