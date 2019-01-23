{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.Regexp where

import           Foreign
import           Foreign.C.Types

#include <pcre.h>
