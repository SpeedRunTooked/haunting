module Data.Ghost where

import GHC.Word

data GhostEntry = GE { buttState  :: Word8
                     , frameCount :: Word8
                     , aStickY    :: Word8
                     , aStickX    :: Word8
                     }
