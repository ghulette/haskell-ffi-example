{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.RegexDefs where

import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
  deriving (Eq,Show)

#{enum PCREOption, PCREOption
, caseless       = PCRE_CASELESS
, dollarEndonly  = PCRE_DOLLAR_ENDONLY
, dotall         = PCRE_DOTALL
}
