module Math.SL2R where

import Math.Matrix2 exposing (..)

-- Represent a point as a 2x2 matrix in SL2R
type alias Point = Matrix2
type alias Tangent = Matrix2

toPoincareHalfplane : Point -> (Float, Float)
toPoincareHalfplane (a, b, c, d) 
    = let denom = ( c * c + d * d )
      in ( (b * d + a * c) / denom, (a * d - b * c) / denom )

toPoincareDisc : Point -> (Float, Float)
toPoincareDisc (a, b, c, d) 
    = let denom = ( a + d ) * ( a + d ) + ( b - c ) * (b - c)
      in ( (a*a + b*b - c*c - d*d )/ denom, -2 * ( a*c + b*d )/ denom )

translate : Point -> Point -> Point
translate = multiply

exp : Tangent -> Point
exp a = let dis = det a
        in if | dis > 0  -> expComplex a
              | dis == 0 -> expDegen a
              | dis < 0  -> expReal a

expComplex : Tangent -> Point
expComplex a = let lambda = sqrt (det a)
                 in (scale (cos lambda) id) 
                    -+- (scale (( sin lambda) / lambda) a) 

sinh x = 0.5 * ( e^x - e^(-x) )
cosh x = 0.5 * ( e^x + e^(-x) )

icosh x = logBase e (x + sqrt(x * x - 1))
isinh x = logBase e (x + sqrt(x * x + 1))

expDegen : Tangent -> Point
expDegen a = id -+- a

expReal : Tangent -> Point
expReal a = let lambda = sqrt (-1 * det a)
            in scale (cosh lambda) id
                 -+- scale ((sinh lambda) / lambda) a

log : Point -> Tangent
log p
    = let dis (a, b, c, d) = (a + d) * (a + d) 
      in if | dis p > 4 -> logReal p  
            | dis p == 4 -> logDegen p  {--@TODO do this for values near 4 --}
            | dis p < 4 -> logComplex p

logReal (a, b, c, d)
    = let lambda = ( a + d )/ 2 + sqrt ( ( (a + d) / 2)^2 - 1 )
      in let detS   = ( lambda - d) * ( 1 - lambda * a ) - lambda * b * c 
         in scale (( logBase e lambda) / detS ) 
                (detS, -2 * (lambda - d) * lambda * b, 2 * c * (1 - lambda * a), -detS) 

logComplex (a, b, c, d)
    = let mu = ( acos( ( a + d )/ 2)) / ( sqrt ( 4 - (a + d)^2 ))
      in ( ( a - d ) * mu , 2 * b * mu, 2 * c * mu, ( d - a ) * mu )

logDegen (a, b, c, d)
    = (a - 1.0, b, c, 1.0 - d )
          
inv : Point -> Point
inv (a, b, c, d) = (d, -b, -c, a)

distance : Point -> Point -> Float
distance x y 
    = let maxTrace (a, b, c, d) 
              = sqrt <|  (a + d)  * ( a + d ) + ( b - c ) * (b - c )
          diagonal tr = tr + (sqrt <| tr * tr + 1)          
      in e ^ (diagonal <| 0.5 * (maxTrace <| y -*- (x |> inv)))
