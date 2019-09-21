module Main where

import Control.Exception
import Control.Monad
import Data.Function
import Data.IORef
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL.Core46
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.GLU.Errors as GLE
import Graphics.UI.GLFW as GLFW
import Text.RawString.QQ

main :: IO ()
main =
    withGLFW $
    withWindow 1280 720 "particles" \win ->
    withDrawer initPos initVel \drawer -> do
        viewFactor <- newIORef (0.01 :: GLfloat)
        viewOffset <- newIORef (Vector2 (0 :: GLfloat) 0)
        disturbPos <- newIORef (Vector2 (0 :: GLfloat) 0)
        disturbEna <- newIORef (0 :: GLint)
        clearColor $= Color4 0 0 0 0
        setMouseButtonCallback win (Just (onMouse viewFactor viewOffset disturbPos disturbEna))
        fix $ \loop -> do
            clear [ColorBuffer]
            vf <- readIORef viewFactor
            vo <- readIORef viewOffset
            dp <- readIORef disturbPos
            de <- readIORef disturbEna
            draw drawer vf vo
            step drawer dp de vf
            swap drawer
            swapBuffers win
            pollEvents
            closep <- windowShouldClose win
            if closep 
            then putStrLn "exiting"
            else loop

onMouse :: IORef GLfloat -> IORef (Vector2 GLfloat) -> IORef (Vector2 GLfloat) -> IORef GLint -> Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
onMouse viewFactor viewOffset disturbPos disturbEna win button state mod = do
    (sx, sy) <- r2f <$> getCursorPos win
    (wid, hei) <- r2f <$> getWindowSize win
    vf <- readIORef viewFactor
    Vector2 ox oy <- readIORef viewOffset
    let dx = (sx / wid * 2 - 1) / vf
        dy = ((hei - sy) / hei * 2 - 1) / vf
    case (button, state) of
        (MouseButton'1, MouseButtonState'Pressed) -> do
            modifyIORef viewFactor (* 2)
            modifyIORef viewOffset (\(Vector2 x y) -> Vector2 (x + dx / 2) (y + dy / 2))
        (MouseButton'2, MouseButtonState'Pressed) -> do
            modifyIORef viewFactor (/ 2)
            modifyIORef viewOffset (\(Vector2 x y) -> Vector2 (x + dx / 2) (y + dy / 2))
        (MouseButton'3, MouseButtonState'Pressed) -> do
            writeIORef disturbPos (Vector2 (dx + ox) (dy + oy))
            writeIORef disturbEna 1
        (MouseButton'3, MouseButtonState'Released) -> do
            writeIORef disturbEna 0
        _ -> return ()
    where r2f (x, y) = (realToFrac x, realToFrac y)

simSize :: Int
simSize = 10000

initPos :: [(GLfloat, GLfloat)]
initPos = concatMap (\x -> map (\y -> (fromIntegral x, fromIntegral y)) [0 .. 99]) [0 .. 99]

initVel :: [(GLfloat, GLfloat)]
initVel = replicate simSize (0, 0)

data RenderException
    = DrawerException String
    | GLException String [GLE.Error]
    | GLFWException String
    deriving Show

instance Exception RenderException

data Drawer
    = Drawer { drawer_vao_front :: VertexArrayObject
             , drawer_vao_back :: VertexArrayObject
             , drawer_vbo_front :: BufferObject
             , drawer_vbo_back :: BufferObject
             , drawer_is_reversed :: IORef Bool
             , drawer_program :: Program
             , drawer_compute :: Program
             }

withGLFW :: IO a -> IO a
withGLFW action = do
    GLFW.setErrorCallback $ Just \err str ->
        throwIO . GLFWException $ show err ++ ": " ++ str
    success <- GLFW.init
    unless success . throwIO . GLFWException $ "Unable to initialize GLFW."
    action `finally` GLFW.terminate

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO a) -> IO a
withWindow width height title action = do
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    win <- GLFW.createWindow width height title Nothing Nothing
    case win of
        Nothing -> throwIO . GLFWException $ "Unable to create windows."
        Just w -> body w `finally` GLFW.destroyWindow w
    where body win = do
              (wid, hei) <- GLFW.getFramebufferSize win
              onResize win wid hei
              GLFW.setFramebufferSizeCallback win (Just onResize)
              action win
          onResize win wid hei = do
              GLFW.makeContextCurrent (Just win)
              viewport $= (Position 0 0, Size (fromIntegral wid) (fromIntegral hei))

withDrawer :: [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)] -> (Drawer -> IO a) -> IO a
withDrawer pos vel action = do
    drawer <- createDrawer pos vel
    action drawer `finally` destroyDrawer drawer

createDrawer :: [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)] -> IO Drawer
createDrawer pos vel
    = withObjectName2 \vao_front vbo_front ->
      withObjectName2 \vao_back vbo_back ->
      loadProgram \prog ->
      loadCompute \comp -> do
          -- config vertex attribs
          let vpos = AttribLocation 1
              vvel = AttribLocation 2
          let stride = fromIntegral $ (2 + 2) * 4
          bindVertexArrayObject $= Just vao_front
          bindBuffer ArrayBuffer $= Just vbo_front
          vertexAttribArray vpos $= Enabled
          vertexAttribPointer vpos $=
              (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset (0 * 4)))
          vertexAttribArray vvel $= Enabled
          vertexAttribPointer vvel $=
              (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset (2 * 4)))
          bindVertexArrayObject $= Just vao_back
          bindBuffer ArrayBuffer $= Just vbo_back
          vertexAttribArray vpos $= Enabled
          vertexAttribPointer vpos $=
              (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset (0 * 4)))
          vertexAttribArray vvel $= Enabled
          vertexAttribPointer vvel $=
              (ToFloat, VertexArrayDescriptor 2 Float stride (glFloatOffset (2 * 4)))
          bindVertexArrayObject $= Nothing
          bindBuffer ArrayBuffer $= Nothing
          checkGLError
          -- feed initial data
          let buffer_front = concatMap (\(p, v) -> t2l p ++ t2l v) (zip pos vel)
              t2l (x, y) = [x, y]
          withArray buffer_front \ptr -> do
              let size = fromIntegral $ length buffer_front * 4
              bindBuffer ArrayBuffer $= Just vbo_front
              bufferData ArrayBuffer $= (size, ptr, DynamicCopy)
              bindBuffer ArrayBuffer $= Nothing
          let buffer_back = replicate (simSize * 4) 0 :: [GLfloat]
          withArray buffer_back \ptr -> do
              let size = fromIntegral $ length buffer_back * 4
              bindBuffer ArrayBuffer $= Just vbo_back
              bufferData ArrayBuffer $= (size, ptr, DynamicCopy)
              bindBuffer ArrayBuffer $= Nothing
          checkGLError
          r <- newIORef False
          return (Drawer vao_front vao_back vbo_front vbo_back r prog comp)

destroyDrawer :: Drawer -> IO ()
destroyDrawer (Drawer vao_front vao_back vbo_front vbo_back r prog comp) = do
    deleteObjectName vao_front
    deleteObjectName vao_back
    deleteObjectName vbo_front
    deleteObjectName vbo_back
    deleteObjectName prog
    deleteObjectName comp

draw :: Drawer -> GLfloat -> Vector2 GLfloat -> IO ()
draw drawer viewFactor viewOffset = do
    currentProgram $= Just (drawer_program drawer)
    r <- readIORef (drawer_is_reversed drawer)
    bindVertexArrayObject $= Just (
        if r then drawer_vao_back drawer
        else drawer_vao_front drawer)
    let vf = UniformLocation 1
        vo = UniformLocation 2
    uniform vf $= viewFactor
    uniform vo $= viewOffset
    drawArrays Points 0 (fromIntegral simSize)
    bindVertexArrayObject $= Nothing
    checkGLError

step :: Drawer -> Vector2 GLfloat -> GLint -> GLfloat -> IO ()
step drawer disturbPos disturbEna viewFactor = do
    currentProgram $= Just (drawer_compute drawer)
    r <- readIORef (drawer_is_reversed drawer)
    bindBufferBase IndexedShaderStorageBuffer 1 $= Just (
        if r then drawer_vbo_back drawer
        else drawer_vbo_front drawer)
    bindBufferBase IndexedShaderStorageBuffer 2 $= Just (
        if r then drawer_vbo_front drawer
        else drawer_vbo_back drawer)
    let dp = UniformLocation 1
        de = UniformLocation 2
        dr = UniformLocation 3
    uniform dp $= disturbPos
    uniform de $= disturbEna
    uniform dr $= 0.1 / viewFactor
    glDispatchCompute (fromIntegral simSize) 1 1
    bindBuffer ShaderStorageBuffer $= Nothing
    checkGLError

swap :: Drawer -> IO ()
swap drawer = do
    r <- readIORef (drawer_is_reversed drawer)
    writeIORef (drawer_is_reversed drawer) (not r)

withShader :: ShaderType -> String -> (Shader -> IO a) -> IO a
withShader ty source action = do
    createShader ty `bracket` deleteObjectName $ \shader -> do
        shaderSourceBS shader $= packUtf8 source
        compileShader shader
        stat <- compileStatus shader
        unless stat $
            shaderInfoLog shader >>= throwIO . DrawerException
        action shader

loadProgram :: (Program -> IO a) -> IO a
loadProgram action = do
    createProgram `bracketOnError` deleteObjectName $ \prog -> do
        withShader VertexShader vertexShaderSource \vs -> do
            attachShader prog vs
            withShader FragmentShader fragmentShaderSource \fs -> do
                attachShader prog fs
                linkProgram prog
                stat <- linkStatus prog
                unless stat $
                    programInfoLog prog >>= throwIO . DrawerException
                action prog

loadCompute :: (Program -> IO a) -> IO a
loadCompute action = do
    createProgram `bracketOnError` deleteObjectName $ \comp -> do
        withShader ComputeShader computeShaderSource \cs -> do
            attachShader comp cs
            linkProgram comp
            stat <- linkStatus comp
            unless stat $
                programInfoLog comp >>= throwIO . DrawerException
            action comp

vertexShaderSource :: String
vertexShaderSource
    = [r| #version 430 core
          layout(location = 1) in vec2 vpos;
          layout(location = 2) in vec2 vvel;
          out vec4 fcolor;
          layout(location = 1) uniform float view_factor;
          layout(location = 2) uniform vec2 view_offset;
          void main() {
              gl_Position = vec4((vpos - view_offset) * view_factor, 0, 1);
              // fcolor = vec4(vvel * view_factor * 10, 0, 1);
              if (vpos.y > 0) fcolor = vec4(0, 1, 0, 1);
              else fcolor = vec4(1, 0, 0, 1);
          }
    |]

fragmentShaderSource :: String
fragmentShaderSource
    = [r| #version 430 core
          in vec4 fcolor;
          out vec4 color;
          void main() {
              color = fcolor;
          }
    |]

computeShaderSource :: String
computeShaderSource
    = [r| #version 430 core
          layout(local_size_x = 1) in;
          struct Particle {
              vec2 pos;
              vec2 vel;
          };
          layout(std430, binding = 1) buffer VertexBufferFront {
              Particle front[];
          };
          layout(std430, binding = 2) buffer VertexBufferBack {
              Particle back[];
          };
          layout(location = 1) uniform vec2 disturb_pos;
          layout(location = 2) uniform bool disturb_ena;
          layout(location = 3) uniform float disturb_rad;
          const float dtime = 0.01;
          void main() {
              int p = int(gl_GlobalInvocationID.x);
              vec2 acc = vec2(0, 0);
              acc += vec2(0, -9.8);
              for (int i = 0; i < front.length(); ++i) {
                  if (i == p) continue;
                  vec2 d = front[i].pos - front[p].pos;
                  float r = length(d);
                  acc += d / r * min(1 / r / r * 10, 10000);
              }
              float y = front[p].pos.y;
              if (y < 0) {
                  float f = -y * 1000;
                  if (front[p].vel.y > 0) f *= 0.5;
                  acc += vec2(0, f);
              }
              if (disturb_ena) {
                  vec2 d = front[p].pos - disturb_pos;
                  float r = length(d);
                  if (r < disturb_rad)
                      acc += d / r * 1000 * r / disturb_rad;
              }
              back[p].pos = front[p].pos + front[p].vel * dtime;
              back[p].vel = front[p].vel + acc * dtime;
          }
      |]

glFloatOffset :: Int -> Ptr GLfloat
glFloatOffset = plusPtr nullPtr

checkGLError :: IO ()
checkGLError = do
    es <- get GLE.errors
    when (not . null $ es) .
        throwIO . GLException "" $ es

withObjectName :: GeneratableObjectName a => (a -> IO b) -> IO b
withObjectName action = do
    obj <- genObjectName
    action obj `onException` deleteObjectName obj

withObjectName2 :: (GeneratableObjectName a, GeneratableObjectName b)
                => (a -> b -> IO c)
                -> IO c
withObjectName2 action
    = withObjectName \a ->
        withObjectName \b ->
            action a b
