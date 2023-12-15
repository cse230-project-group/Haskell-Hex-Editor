import AppData
import Control.Monad (unless)
import Data.Char
import Data.List (foldl')
import Data.Vector (toList)
import GHC.Word (Word8)
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.HUnit
import Lib

runTests :: [TestTree] -> IO ()
runTests groups = defaultMain (localOption (mkTimeout 3000000) (testGroup "Tests" groups))

mkTest :: Eq b => (a -> IO b, a, b, String) -> TestTree
mkTest (f, x, expR, name) =
  testCase name $ do
    actR <- f x
    unless (actR == expR) $ assertFailure "Wrong Result"

mkProp :: QC.Testable prop => (prop, String) -> TestTree
mkProp (prop, name) = mkTest (act, (), True, name)
  where
    act _ = QC.isSuccess <$> QC.labelledExamplesWithResult args prop
    args = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 100 }

main :: IO ()
main = runTests
  [ test_string,
    test_menuList
  ]

test_string:: TestTree
test_string = testGroup "string"
  [ mkProp (prop_trimStr, "prop_trimStr"),
    mkProp (prop_strToByteVec, "prop_strToByteVec")
  ]

genSpaceStr :: QC.Gen String
genSpaceStr = do
  pre <- QC.elements [0..100]
  post <- QC.elements [0..100]
  return $ take pre (repeat ' ') ++ "a b" ++ take post (repeat ' ')

genRandChar :: QC.Gen Char
genRandChar = do
  v <- QC.elements [0..255]
  return $ chr v

genRandStr :: QC.Gen String
genRandStr = QC.listOf genRandChar

prop_trimStr :: QC.Property
prop_trimStr = QC.forAll genSpaceStr $ (== "a b") . trimStr

prop_strToByteVec :: QC.Property
prop_strToByteVec = QC.forAll genRandStr (\str ->
  let 
    vec = stringToWord8Vector str
    isEqual :: String -> [Word8] -> Bool
    isEqual [] [] = True
    isEqual (s:ss) (t:ts) = fromIntegral (ord s) == fromIntegral t && isEqual ss ts
    isEqual _ _ = False
  in
    length str == length vec && isEqual str (toList vec)
  )

test_menuList :: TestTree
test_menuList = mkTest (f, menuList, True, "test_menuList")
  where
    f = return . (f' [] 0)
    f' acc c ml =
      let
        cur = ml !! c
        acc' = c:acc
        helper b (MkMenu _ child) = if b
          then
            case child of
              Nothing -> True
              Just c' -> if elem c' acc'
                then
                  False
                else
                  f' acc' c' ml
          else
            False
      in
        foldl' helper True cur
