{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Graphics.GL.Pal
import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Halive.Utils

import Control.Lens.Extra
import Control.Monad.Trans
import Data.Foldable
import Splatter


setCamera :: V3 GLfloat -> V3 GLfloat -> GLfloat -> M33 GLfloat
setCamera ro ta cr = let
    cw = normalize (ta - ro)
    cp = V3 (sin cr) (cos cr) 0
    cu = normalize (cross cw cp)
    cv = normalize (cross cu cw)
    in V3 cu cv cw

iResolution :: V2 GLfloat
iResolution = V2 resX resY
iMouse :: V2 GLfloat
iMouse = V2 0 0
iGlobalTime :: GLfloat
iGlobalTime = 0

mainImage :: GLfloat -> V2 GLfloat -> (GLfloat, V4 GLfloat, V3 GLfloat)
mainImage time fragCoord = let
    q          = fragCoord
    V2 pX pY   = q & _x *~ (iResolution^._x / iResolution^._y)
    V2 moX moY = iMouse / iResolution
    time'      = 15 + time
    ro = V3
        ((-0.5) + 3.5 * cos (0.1 * time' + 6 * moX))
        (1 + 2 * moY)
        (0.5 + 3.5 * sin (0.1 * time' + 6 * moX))
    ta = V3 (-0.5) (-0.4) 0.5

    ca = setCamera ro ta 0

    rd = ca !* normalize (V3 pX pY 2)
    in castRay ro rd

-- World Map
mapToWorld :: V3 GLfloat -> V2 GLfloat
mapToWorld pos = 
                 opU (V2 (sdSphere (pos - (V3 0 0.25 0)) 0.25) 0.2)
               . opU (V2 (sdSphere (pos - (V3 1 0.5 0)) 0.5 )  0.4)
               $      V2 (sdPlane pos)                         0.6
               

castRay :: V3 GLfloat -> V3 GLfloat -> (GLfloat, V4 GLfloat, V3 GLfloat)
castRay ro rd = 
    let tmin      = 1
        tmax      = 20
        precis    = 0.002
        rayStart  = (tmin, 0.8, 0) -- start at tmin with no material
        -- FIXME shouldn't calc res anymore once done is true
        (tResult, mResult, posResult) = foldl' (\(t, m, pos) _ -> 
            let pos' = ro + rd * realToFrac t
                V2 rT rM = mapToWorld pos'
                t'       = t + rT
                m'       = rM
                done     = rT < precis || t > tmax
            in if done then (t, m, pos) else (t', m', pos')
            ) rayStart [0..50::Int]
        color = if tResult > tmax then hslColor 0 0 0 0 else hslColor mResult 0.8 0.8 1
    in (tResult, color, posResult)

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) 
  } deriving Data

main :: IO ()
main = do
    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Geometry Test" [UseOpenVR]

    shader        <- createShaderProgram "app/Splitter.vert" "App/Splitter.frag"
    Uniforms{..}  <- acquireUniforms shader

    -- cubeGeo       <- cubeGeometry 1 5
    -- cubeShape     <- (makeShape cubeGeo shader :: IO (Shape Uniforms))
    
    (pointsVAO, positionsBuffer, colorsBuffer, pointsVertCount) <- makeLine shader
    
    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
    whileVR vrPal $ \headM44 _hands -> do
        
        processEvents gpEvents $ closeOnEscape gpWindow

        t <- getNow
        let player = newPose & posPosition .~ V3 0 0 2

        let results = map (mainImage t) pixelList
            colors = map (\(_, col, _) -> col) results
            positions = map (\(_, _, pos) -> pos & _z .~ (-20) ) results

        bufferSubData colorsBuffer    (concatMap toList colors)
        bufferSubData positionsBuffer (concatMap toList positions)
        
        let clearFrame = 
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)


        renderWith vrPal player headM44 clearFrame $ \projM44 eyeViewM44 -> do
            useProgram shader
            let model = identity
            uniformM44 uMVP (projM44 !*! eyeViewM44 !*! model)
            withVAO pointsVAO $ 
              glDrawArrays GL_POINTS 0 pointsVertCount


resX :: GLfloat
resX = 160
resY :: GLfloat
resY = 120

pixelList :: [V2 GLfloat]
pixelList = [V2 (x/resX * 2 - 1) (y/resY * 2 - 1) | x <- [0..resX], y <- [0..resY] ]

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer, ArrayBuffer, GLsizei)
makeLine shader = do

  let verts = map (\(V2 x y) -> V3 x y 0) pixelList
      vertCount = length verts
      colors  = replicate vertCount (V4 0 1 1 1)
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts   :: [GLfloat])
  colorsBuffer    <- bufferData GL_DYNAMIC_DRAW (concatMap toList colors  :: [GLfloat])

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT 3
    withArrayBuffer colorsBuffer    $ assignFloatAttribute shader "aColor"    GL_FLOAT 4

  return (vao, positionsBuffer, colorsBuffer, fromIntegral vertCount)


