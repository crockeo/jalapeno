-- | Loading and storing an OpenGL shader in a @'Shader'@ type.
module FRP.Jalapeno.Assets.Shader ( ShaderProgram (..)
                                  , loadShader
                                  , loadShaderProgram
                                  ) where

-------------
-- Imports --
import Graphics.Rendering.OpenGL.Raw
import Foreign.Marshal.Alloc
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Storable
import System.Directory
import Control.Monad
import Foreign.Ptr
import GHC.Ptr

----------
-- Code --

-- | A wrapper around a @'GLuint'@ to be clear that this is an OpenGL shader.
newtype ShaderProgram = ShaderProgram GLuint
  deriving (Eq, Show, Read)

-- | Reading in the contents of a file and storing them inside the appropriate
--   form for creating a shader.
loadShaderContents :: FilePath -> IO (Ptr CString)
loadShaderContents path =
  fmap (castPtr . castStablePtrToPtr) $ readFile path >>=
                                        newCString    >>=
                                        newStablePtr

-- | Taking the data from a given file (stored in the appropriate manner for
--   loading a shader) and freeing the respective pointers.
freeShaderContents :: Ptr CString -> IO ()
freeShaderContents contentsPtr =
  let contentsStablePtr = castPtrToStablePtr $ castPtr contentsPtr in do
    contents <- deRefStablePtr contentsStablePtr
    free contents
    freeStablePtr contentsStablePtr

-- | Loading a single portion of a shader (vertex, geometry, or fragment).
loadShader :: FilePath -> GLenum -> IO GLuint
loadShader path shaderType = do
  exists <- doesFileExist path
  if not exists
    then return 0
    else do
      contents <- loadShaderContents path
      sid      <- glCreateShader shaderType

      glShaderSource sid 1 contents nullPtr
      glCompileShader sid
      freeShaderContents contents

      -- TODO: Add error logging instead of a binary pass or fail.
      alloca $ \pCompiled -> do
        glGetShaderiv sid gl_COMPILE_STATUS pCompiled
        compiled <- peek pCompiled

        case compiled of
          gl_TRUE -> return sid
          _       -> do
            glDeleteShader sid
            return 0

-- | Performing some piece of @'IO'@ on a list of @'GLuint'@ so long as that
--   value is not 0.
shaderAction :: [GLuint] -> (GLuint -> IO ()) -> IO ()
shaderAction    []       _ =             return ()
shaderAction (0:xs) action =             shaderAction xs action
shaderAction (n:xs) action = action n >> shaderAction xs action

-- | Loading a @'ShaderProgram'@ from a location on disk. For a given path p, it
--   will look for a vertex, geometry, and fragment shader at p.vert, p.geom,
--   and p.frag respectively.
loadShaderProgram :: FilePath -> IO (Either String ShaderProgram)
loadShaderProgram path = do
  vert <- loadShader (path ++ ".vert") gl_VERTEX_SHADER
  geom <- loadShader (path ++ ".geom") gl_GEOMETRY_SHADER
  frag <- loadShader (path ++ ".frag") gl_FRAGMENT_SHADER

  case [vert, geom, frag] of
    [0, 0, 0] -> return $ Left "No shaders were loaded."
    ss        -> do
      spid <- glCreateProgram

      shaderAction ss $ glAttachShader spid

      withCString "out_color" $ \cStr ->
        glBindFragDataLocation spid 0 cStr
      glLinkProgram spid

      shaderAction ss glDeleteShader

      alloca $ \pLinked -> do
        glGetProgramiv spid gl_LINK_STATUS pLinked
        linked <- peek pLinked

        case linked of
          gl_TRUE -> return $ Right $ ShaderProgram spid
          _       -> do
            glDeleteProgram spid
            return $ Left "Failed to link shader program."
