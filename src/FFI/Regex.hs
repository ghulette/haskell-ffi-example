{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.Regex where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           FFI.RegexDefs
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           System.IO.Unsafe      (unsafePerformIO)

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr (.|.) 0 . map unPCREOption

data PCRE

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
                   deriving (Eq, Ord, Show)

foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString
                 -> PCREOption
                 -> Ptr CString
                 -> Ptr CInt
                 -> Ptr CUChar
                 -> IO (Ptr PCRE)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str opts = unsafePerformIO $
  BS.useAsCString str $ \cstr -> do
    alloca $ \errPtr -> do
      alloca $ \errOffset -> do
        let opt = combineOptions opts
        pcre_ptr <- c_pcre_compile cstr opt errPtr errOffset nullPtr
        if pcre_ptr == nullPtr then do
          errCStr <- peek errPtr
          err <- peekCString errCStr
          return $ Left err
        else do
          reg <- newForeignPtr finalizerFree pcre_ptr
          return $ Right $ Regex reg str
