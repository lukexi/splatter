{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Graphics.GL.Pal
import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Halive.Utils
import Data.Fixed

import Control.Lens.Extra
-- import Control.Monad
-- import Control.Monad.Trans
import Data.Foldable



-- Primitives
sdPlane :: V3 GLfloat -> GLfloat
sdPlane = (^. _y)

sdSphere :: V3 GLfloat -> GLfloat -> GLfloat
sdSphere p s = norm p - s

-- Operators
opS :: GLfloat -> GLfloat -> GLfloat
opS d1 d2 = max (-d2) d1

opU :: V2 GLfloat -> V2 GLfloat -> V2 GLfloat
opU d1 d2 = if d1^._x < d2^._x then d1 else d2

opRep :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat
opRep p c = (mod' <$> p <*> c) - 0.5 * c

-- World Map
mapToWorld :: V3 GLfloat -> V2 GLfloat
mapToWorld pos = 
               --   opU (V2 (sdSphere (pos - (V3 0 0.25 0)) 0.25) 3)
               -- . opU (V2 (sdSphere (pos - (V3 0 0.25 0)) 0.5 ) 8)
               -- $ V2 (sdPlane pos) 1
               V2 (sdPlane pos) 1

castRay :: V3 GLfloat -> V3 GLfloat -> V2 GLfloat
castRay ro rd = 
    let tmin   = 1
        tmax   = 20
        precis    = 0.002
        rayStart  = V2 tmin (-1) -- start at tmin with no material
        -- FIXME shouldn't calc res anymore once done is true
        V2 tResult mResult = foldl' (\(V2 t m) _ -> 
            let V2 rX rY = mapToWorld (ro + rd * realToFrac t)
                t'       = t + rX
                m'       = rY
                done     = rX < precis || t > tmax
            in if done then V2 t m else V2 t' m'
            ) rayStart [0..50::Int]
        mResult' = if tResult > tmax then -1 else mResult
    in V2 tResult mResult'

-- Rendering
calcNormal :: V3 GLfloat -> V3 GLfloat
calcNormal pos = 
    let eps = V3 0.001 0 0
        nor = V3 ((mapToWorld (pos + eps^._xyy))^._x - (mapToWorld (pos - eps^._xyy))^._x)
                 ((mapToWorld (pos + eps^._yxy))^._x - (mapToWorld (pos - eps^._yxy))^._x)
                 ((mapToWorld (pos + eps^._yyx))^._x - (mapToWorld (pos - eps^._yyx))^._x)
    in normalize nor

render :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat
render ro rd = 
    let col = V3 0.7 0.9 1.0 + realToFrac (rd^._y * 0.8)
        V2 t m = castRay ro rd
        col' = if m > (-0.5) 
            then
                let pos = ro + realToFrac t * rd
                    nor = calcNormal pos
                    ref = reflect rd nor
                    -- material
                    colOrSky = if m < 1.5 
                        then 
                            let f = realToFrac $ (mod ((floor (5 * pos ^. _z) + floor (5 * pos ^. _x))) 2 :: Int)
                            in 0.4 + 0.1 * f * 1
                        else 0.45 + 0.3 * (sin <$> (V3 0.05 0.08 0.10 * (realToFrac m - 1)))
                    lig = normalize (V3 (-0.6) 0.7 (-0.5))
                    amb = realToFrac $ clamp (0.5 + 0.5 * nor ^._y) 0 1
                    dif = realToFrac $ clamp (dot nor lig) 0 1
                    bac = realToFrac $ clamp 
                            (dot nor 
                                (normalize (V3 ((-lig)^._x) 
                                           0 
                                           ((-lig)^._z))
                                )
                            ) 
                            0 1
                        * clamp (1-pos^._y) 0 1
                    dom = smoothstep (-0.1) 0.1 (realToFrac $ ref^._y)
                    fre = clamp (1 + realToFrac (dot nor rd)) 0 1 ** 2
                    spe = clamp (realToFrac $ dot ref lig)    0 1 ** 16

                    lin = (0 :: V3 GLfloat)
                          + 1.20 * dif * V3 1.00 0.85 0.55 
                          + 1.20 * spe * V3 1.00 0.85 0.55 * dif
                          + 0.20 * amb * V3 0.50 0.70 1.00 
                          + 0.30 * dom * V3 0.50 0.70 1.00 
                          + 0.30 * bac * V3 0.25 0.25 0.25 
                          + 0.40 * fre * V3 1.00 1.00 1.00 
                in colOrSky*(lin :: V3 GLfloat)
            else col
        col'' = mix col' (V3 0.8 0.9 1.0) (1.0 - exp (-0.002 * realToFrac t * realToFrac t))
    in clamp col'' 0 1

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

setCamera :: V3 GLfloat -> V3 GLfloat -> GLfloat -> M33 GLfloat
setCamera ro ta cr = let
    cw = normalize (ta - ro)
    cp = V3 (sin cr) (cos cr) 0
    cu = normalize (cross cw cp)
    cv = normalize (cross cu cw)
    in V3 cu cv cw

iResolution :: V2 GLfloat
iResolution = V2 640 480
iMouse :: V2 GLfloat
iMouse = V2 320 240
iGlobalTime :: GLfloat
iGlobalTime = 0

mainImage :: V2 GLfloat -> V4 GLfloat
mainImage fragCoord = let
    -- q = fragCoord^._xy / iResolution^._xy
    q          = fragCoord
    V2 pX pY   = q & _x *~ (iResolution^._x / iResolution^._y)
    V2 moX moY = iMouse / iResolution
    time       = 15 + iGlobalTime
    ro = V3
        ((-0.5) + 3.5 * cos (0.1 * time + 6 * moX))
        (1 + 2 * moY)
        (0.5 + 3.5 * sin (0.1 * time + 6 * moX))
    ta = V3 (-0.5) (-0.4) 0.5

    ca = setCamera ro ta 0

    rd = ca !* normalize (V3 pX pY 2)
    col = render ro rd
    V3 colR colG colB = col ** 0.4545
    fragColor = V4 colR colG colB 1
    in fragColor

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) 
  } deriving Data

main :: IO ()
main = do
    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Geometry Test" [UseOpenVR]

    shader        <- createShaderProgram "shaders/geo.vert" "shaders/geo.frag"
    Uniforms{..}  <- acquireUniforms shader

    -- cubeGeo       <- cubeGeometry 1 5
    -- cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))

    (pointsVAO, colorBuffer, pointsVertCount) <- makeLine shader
    
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
    whileVR vrPal $ \headM44 _hands -> do
    
        processEvents gpEvents $ closeOnEscape gpWindow

        -- t <- getNow
        let player = newPose & posPosition .~ V3 0 0 2

        let newColors = map (mainImage . view _xy) pixelList
        bufferSubData colorBuffer (concatMap toList newColors)

        let clearFrame = 
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)


        renderWith vrPal player headM44 clearFrame $ \projM44 eyeViewM44 -> do
            useProgram shader
            let model = identity
            uniformM44 uMVP (projM44 !*! eyeViewM44 !*! model)
            withVAO pointsVAO $ 
              glDrawArrays GL_POINTS 0 pointsVertCount

            -- withShape cubeShape $ do
            --     let model = mkTransformation (axisAngle (V3 1 1 0) 0) 0
            --     uniformM44 uMVP (projM44 !*! eyeViewM44 !*! model)
            --     drawShape

resX :: GLfloat
resX = 640
resY :: GLfloat
resY = 480

pixelList :: [V3 GLfloat]
pixelList = [V3 (x/resX * 2 - 1) (y/resY * 2 - 1) 0 | x <- [0..resX], y <- [0..resY] ]

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer, GLsizei)
makeLine shader = do

  let verts = pixelList
      vertCount = length verts
      colors  = replicate vertCount (V4 0 1 1 1)
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts   :: [GLfloat])
  colorsBuffer    <- bufferData GL_DYNAMIC_DRAW (concatMap toList colors  :: [GLfloat])

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT 3
    withArrayBuffer colorsBuffer    $ assignFloatAttribute shader "aColor"    GL_FLOAT 4

  return (vao, colorsBuffer, fromIntegral vertCount)


