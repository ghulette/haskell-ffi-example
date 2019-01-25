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

newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }
  deriving (Eq, Show)

#{enum PCREExecOption, PCREExecOption
, anchored = PCRE_ANCHORED
, notEOL   = PCRE_NOTEOL
}

newtype PCREInfo = PCREInfo { unPCREInfo :: CInt }
  deriving (Eq, Show)

#{enum PCREInfo, PCREInfo
, infoSize         = PCRE_INFO_SIZE
, infoCaptureCount = PCRE_INFO_CAPTURECOUNT
}
