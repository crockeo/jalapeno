-- | Loading and storing an OpenGL texture inside of a @'Texture'@.
module FRP.Jalapeno.Assets.Texture ( Texture (..)
                                   , loadTexture
                                   ) where

-------------
-- Imports --
import Graphics.Rendering.OpenGL.Raw
import Codec.Picture

----------
-- Code --

-- | A wrapper around a @'GLuint'@ for greater type clarity.
newtype Texture = Texture GLuint

-- | Loading a @'Texture'@ from a location on disk.
loadTexture :: FilePath -> IO Texture
loadTexture = undefined
