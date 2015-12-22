{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Graphics.GL.Pal
import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Halive.Utils
import Data.Fixed

import Control.Lens
import Control.Monad
import Data.Foldable

_xyy, _yxy, _yyx :: R3 t => Lens' (t a) (V3 a)
_xyy f = _xyz $ \(V3 a b c) -> f (V3 a b b) <&> \(V3 a' b' _) -> V3 a' b' b'
{-# INLINE _xyy #-}

_yxy f = _xyz $ \(V3 a b c) -> f (V3 b a b) <&> \(V3 a' b' _) -> V3 b' a' b'
{-# INLINE _yxy #-}

_yyx f = _xyz $ \(V3 a b c) -> f (V3 b b a) <&> \(V3 a' b' _) -> V3 b' b' a'
{-# INLINE _yyx #-}

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
mapToWorld pos = opU (V2 (sdSphere (pos - (V3 0 0.25 0)) 0.25) 3)
               . opU (V2 (sdSphere (pos - (V3 0 0.25 0)) 0.5 ) 8)
               $ (V2 (sdPlane pos) 1)

castRay :: V3 GLfloat -> V3 GLfloat -> V2 GLfloat
castRay ro rd = 
    let tmin = 1 :: GLfloat
        tmax = 20 :: GLfloat
        precis = 0.002 :: GLfloat
        t = tmin :: GLfloat
        m = -1 -- Start with no material
        V2 tResult mResult = foldl' (\(V2 t m) _i -> 
            let res = mapToWorld (ro + rd * realToFrac t)
                t' = t + res^._x
                m' = res^._y
                done = res^._x < precis || t > tmax
            -- FIXME shouldn't calc res anymore once done is true
            in if done then V2 t m else V2 t' m'
            ) (V2 tmin (-1.0)) [0..50]
        mResult' = if t > tmax then -1 else m
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
        res = castRay ro rd
        t = res ^. _x
        m = res ^. _y
        col' = if m > (-0.5) 
            then
                let pos = ro + realToFrac t * rd :: V3 GLfloat
                    nor = calcNormal pos         :: V3 GLfloat
                    ref = reflect rd nor         :: V3 GLfloat
                    -- material
                    colOrSky = if m < 1.5 
                        then 
                            let f = realToFrac $ mod ((floor (5 * pos ^. _z) + floor (5 * pos ^. _x))) 2
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
                    dom = smoothstep (-0.1) 0.1 (ref^._y)
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

mix x y a = x * (1 - a) + y * a

reflect i n = i - 2 * (realToFrac (dot n i)) * n

smoothstep edge0 edge1 x0 = x * x * (3 - 2 * x)
    -- Scale, bias and saturate x to 0..1 range
    where x = clamp ((x - edge0) / (edge1 - edge0)) 0 1

clamp :: Ord a => a -> a -> a -> a
clamp n l h = max l (min h n)

{-

vec3 render( in vec3 ro, in vec3 rd )
{
    vec3 col = vec3(0.7, 0.9, 1.0) + rd.y*0.8;
    vec2 res = castRay(ro,rd);
    float t = res.x;
    float m = res.y;
    if( m > -0.5 )
    {
        vec3 pos = ro + t*rd;
        vec3 nor = calcNormal( pos );
        vec3 ref = reflect( rd, nor );
        
        // material
        col = 0.45 + 0.3*sin( vec3(0.05,0.08,0.10)*(m-1.0) );
        
        if( m<1.5 )
        {
            float f = mod( floor(5.0*pos.z) + floor(5.0*pos.x), 2.0);
            col = 0.4 + 0.1*f*vec3(1.0);
        }

        // lighting
        float occ = calcAO( pos, nor );
        vec3  lig = normalize( vec3(-0.6, 0.7, -0.5) );
        float amb = clamp( 0.5+0.5*nor.y, 0.0, 1.0 );
        float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
        float bac = clamp( dot( nor, normalize(vec3(-lig.x,0.0,-lig.z))), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
        float dom = smoothstep( -0.1, 0.1, ref.y );
        float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );
        float spe = pow( clamp( dot( ref, lig ), 0.0, 1.0 ),16.0);

        vec3 lin = vec3(0.0);
        lin += 1.20*dif*vec3(1.00,0.85,0.55);
        lin += 1.20*spe*vec3(1.00,0.85,0.55)*dif;
        lin += 0.20*amb*vec3(0.50,0.70,1.00)*occ;
        lin += 0.30*dom*vec3(0.50,0.70,1.00)*occ;
        lin += 0.30*bac*vec3(0.25,0.25,0.25)*occ;
        lin += 0.40*fre*vec3(1.00,1.00,1.00)*occ;
        col = col*lin;

        col = mix( col, vec3(0.8,0.9,1.0), 1.0-exp( -0.002*t*t ) );

    }

    return vec3( clamp(col,0.0,1.0) );
}

-}

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) 
  } deriving Data

main :: IO ()
main = do
    vrPal@VRPal{..} <- reacquire 0 $ initVRPal "Geometry Test" [UseOpenVR]

    shader        <- createShaderProgram "shaders/geo.vert" "shaders/geo.frag"
    Uniforms{..}  <- acquireUniforms shader

    (lineVAO, lineBuffer, lineVertCount) <- makeLine shader

    glEnable GL_DEPTH_TEST
    glClearColor 0.0 0.0 0.1 1
    whileVR vrPal $ \headM44 hands -> do
    
        processEvents gpEvents $ closeOnEscape gpWindow

        t <- getNow
        let player = newPose

        newVerts <- randomVerts lineVertCount
        bufferSubData lineBuffer (concatMap toList newVerts)

        renderWith vrPal player headM44 (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)) $ \projM44 eyeViewM44 -> do
            let model = identity
            uniformM44 uMVP (projM44 !*! eyeViewM44 !*! model)
            withVAO lineVAO $ 
              glDrawArrays GL_LINE_STRIP 0 lineVertCount

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer, GLsizei)
makeLine shader = do

  let verts = map (\x -> V3 x 0 0) [-1,-0.95..1]
      vertCount = length verts
      normals = replicate vertCount (V3 0 0 1)
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts    :: [GLfloat])
  normalsBuffer   <- bufferData GL_STATIC_DRAW  (concatMap toList normals  :: [GLfloat])

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT 3
    withArrayBuffer normalsBuffer   $ assignFloatAttribute shader "aNormal"   GL_FLOAT 3

  return (vao, positionsBuffer, fromIntegral vertCount)

randomVerts :: (Integral a, Fractional b) 
            => a -> IO [V3 GLfloat]
randomVerts lineVertCount = forM [0..lineVertCount-1] $ \i -> do
  let x = fromIntegral i / fromIntegral lineVertCount
      x' = x * 2 - 1
  return (V3 x' 1 0)

