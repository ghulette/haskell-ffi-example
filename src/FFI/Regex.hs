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

combinePCREOptions :: [PCREOption] -> PCREOption
combinePCREOptions = PCREOption . foldr (.|.) 0 . map unPCREOption

combinePCREExecOptions :: [PCREExecOption] -> PCREExecOption
combinePCREExecOptions = PCREExecOption . foldr (.|.) 0 . map unPCREExecOption

data PCRE
data PCREExtra

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
                   deriving (Eq, Ord, Show)

-- pcre *pcre_compile(const char *pattern,
--                    int options,
--                    const char **errptr,
--                    int *erroffset,
--                    const unsigned char *tableptr);
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
      let opt = combinePCREOptions opts
      pcre_ptr <- c_pcre_compile cstr opt errPtr errOffset nullPtr
      if pcre_ptr == nullPtr then do
        errCStr <- peek errPtr
        err <- peekCString errCStr
        return $ Left err
      else do
        reg <- newForeignPtr finalizerFree pcre_ptr
        return $ Right $ Regex reg str

-- int pcre_exec(const pcre *code,
--               const pcre_extra *extra,
--               const char *subject,
--               int length,
--               int startoffset,
--               int options,
--               int *ovector,
--               int ovecsize);
foreign import ccall unsafe "pcre.h pcre_exec"
  c_pcre_exec :: Ptr PCRE
              -> Ptr PCREExtra
              -> CString
              -> CInt
              -> CInt
              -> PCREExecOption
              -> Ptr CInt
              -> CInt
              -> IO CInt

-- int pcre_fullinfo(const pcre *,
--                   const pcre_extra *,
--                   int,
--                   void *);
foreign import ccall unsafe "pcre.h pcre_fullinfo"
  c_pcre_fullinfo :: Ptr PCRE
                  -> Ptr PCREExtra
                  -> PCREInfo
                  -> Ptr a
                  -> IO CInt

capturedCount :: Regex -> Int
capturedCount (Regex pcre _) = unsafePerformIO $
  withForeignPtr pcre $ \pcre_ptr -> do
  alloca $ \nptr -> do
    c_pcre_fullinfo pcre_ptr nullPtr infoCaptureCount nptr
    n <- peek (nptr :: Ptr CInt)
    return $ fromIntegral n
