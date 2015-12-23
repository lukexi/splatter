module Splatter where
import Graphics.GL.Pal
import Control.Lens.Extra
import Data.Fixed

-- Primitives
sdPlane :: V3 GLfloat -> GLfloat
sdPlane = view _y

sdSphere :: V3 GLfloat -> GLfloat -> GLfloat
sdSphere p s = norm p - s

-- Operators
opS :: GLfloat -> GLfloat -> GLfloat
opS d1 d2 = max (-d2) d1

opU :: V2 GLfloat -> V2 GLfloat -> V2 GLfloat
opU d1 d2 = if d1^._x < d2^._x then d1 else d2

opRep :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat
opRep p c = (mod' <$> p <*> c) - 0.5 * c



-- Shader lib functions
mix :: Num a => a -> a -> a -> a
mix x y a = x * (1 - a) + y * a

reflect :: (Fractional (f a), Real a, Metric f) => f a -> f a -> f a
reflect i n = i - 2 * (realToFrac (dot n i)) * n

smoothstep :: (Fractional a, Ord a) => a -> a -> a -> a
smoothstep edge0 edge1 x0 = x * x * (3 - 2 * x)
    -- Scale, bias and saturate x to 0..1 range
    where x = clamp ((x0 - edge0) / (edge1 - edge0)) 0 1

clamp :: Ord a => a -> a -> a -> a
clamp n l h = max l (min h n)
