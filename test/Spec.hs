{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified FFI.Regex             as Regex
import           Test.Tasty
import           Test.Tasty.HUnit

testCompileOk :: ByteString -> TestTree
testCompileOk pat = testCase (BS.unpack pat) $ do
  case Regex.compile pat [] of
    Left msg -> assertFailure msg
    Right _  -> return ()

testCompileFail :: ByteString -> TestTree
testCompileFail pat = testCase (BS.unpack pat) $ do
  case Regex.compile pat [] of
    Left _  -> return ()
    Right _ -> assertFailure $ "Unexpected success"

unitTests = testGroup "Unit tests"
  [ testCompileOk "a.*b"
  , testCompileOk "a.*b[xy]+(foo?)"
  , testCompileFail "*"
  ]

tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests
