module IniSettings where

import Evaluator
import Data.IORef

defaultSettings :: IO IniSettings
defaultSettings = mapM makeRef defaults >>= newIORef
    where makeRef (n, v) = newIORef v >>= return . (,) n

defaults = [ ("arg_separator.input", "&")
           , ("display_errors", "1")
           , ("log_errors", "0")
           , ("track_errors", "0")
           ]
