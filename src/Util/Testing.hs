{-# LANGUAGE GADTs, BangPatterns #-}
module Util.Testing where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.IO

import Data.Text (Text)
import qualified Data.Text.IO as Text

mapOn :: [a] -> (a -> b) -> [b]
mapOn = flip map

foldMOn :: (Foldable t,Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldMOn i xs f = foldM f i xs

mapMOn :: (Monad m) => [a] -> (a -> m b) -> m [b]
mapMOn = flip mapM

zeroPad :: Int -> String -> String
zeroPad n s = replicate (max 0 $ n - length s) '0' ++ s

abbreviate :: Int -> String -> String
abbreviate n s
  | length s <= n = s
  | otherwise = take (max 0 $ n - 3) s ++ "..."

data Test where
  Test1 :: (Show a,Eq b,Show b) => (String,String,a -> b,[(a,b)]) -> Test
  Test2 :: (Show a,Show b,Eq c,Show c) => (String,String,a -> b -> c,[((a,b),c)]) -> Test
  Test3 :: (Show a,Show b,Show c,Eq d,Show d) => (String,String,a -> b -> c -> d,[((a,b,c),d)]) -> Test
  TestDir :: (Show a,Eq b,Show b) => (String,String,a -> b,String,Text -> IO (a,b)) -> Test

runTests :: [Test] -> IO ()
runTests ts = do
  rs <- forM ts $ \ tst -> do
    y <- case tst of
      Test1 t -> runTests1 t
      Test2 t -> runTests2 t
      Test3 t -> runTests3 t
      TestDir t -> runTestsDir t
    putStrLn ""
    return y
  forM_ (zip [(1::Integer)..] rs) $ \ (m,(n,passed,failed)) -> do
    when (m /= 1) $ putStrLn ""
    putStrLn $ "++ E" ++ zeroPad 2 (show n) ++ " Tests Passed: " ++ show passed
    putStrLn $ "-- E" ++ zeroPad 2 (show n) ++ " Tests Failed: " ++ show failed

showTestResult :: (Eq a,Show a) => String -> Int -> String -> a -> Either String a -> (Int,Int) -> IO (Int,Int)
showTestResult n m fx y y'M (passed,failed) = do
  let eM = case y'M of
        Left e -> Just $ "[ERROR]: " ++ e
        Right y' ->
          if y' == y
          then Nothing
          else Just $ show y'
  case eM of
    Nothing -> do
      putStrLn $ "   " ++ n ++ "." ++ zeroPad 2 (show m) ++ " [TEST PASSED]: " ++ abbreviate 50 fx
      hFlush stdout
      return (passed+1,failed)
    Just s -> do
      putStrLn $ "   " ++ n ++ "." ++ zeroPad 2 (show m) ++ " [TEST FAILED]:"
      putStrLn $ "     -- the input"
      putStrLn $ "     " ++ fx
      putStrLn $ "   =="
      putStrLn $ "     -- the output"
      putStrLn $ "     " ++ s
      putStrLn $ "   /="
      putStrLn $ "     -- the expected result"
      putStrLn $ "     " ++ show y
      hFlush stdout
      return (passed,failed+1)

runTestsN :: (Eq a,Show a) => String -> String -> [(String,() -> a,a)] -> IO (String,Int,Int)
runTestsN n name tests = do
  putStrLn $ ">> " ++ n ++ " Tests: " ++ name
  (passed,failed) <- foldMOn (0,0) (zip [(1::Int)..] tests) $ \ pf (m,(s,fx,y)) -> do
    y'M <- catch (Right <$> evaluate (let y = fx (); !_ = force (show y) in y)) $ \ (SomeException e) -> return $ Left $ chomp $ unwords $ lines $ show e
    showTestResult n m s y y'M pf
  return (n,passed,failed)
  where
    chomp s0 = concat $ mapOn (group s0) $ \ s ->
      if " " `isPrefixOf` s then " " else s

runTests1 :: (Eq b,Show a,Show b) => (String,String,a -> b,[(a,b)]) -> IO (String,Int,Int)
runTests1 (n,name,f,tests) = runTestsN n name $ mapOn tests $ \ (x,y) ->
  (name ++ " " ++ showsPrec 11 x [],\() -> f x,y)

runTests2 :: (Eq c,Show a,Show b,Show c) => (String,String,a -> b -> c,[((a,b),c)]) -> IO (String,Int,Int)
runTests2 (n,name,f,tests) = runTestsN n name $ mapOn tests $ \ ((x,y),z) ->
  (name ++ " " ++ showsPrec 11 x [] ++ " " ++ showsPrec 11 y [],\() -> f x y,z)

runTests3 :: (Eq d,Show a,Show b,Show c,Show d) => (String,String,a -> b -> c -> d,[((a,b,c),d)]) -> IO (String,Int,Int)
runTests3 (n,name,f,tests) = runTestsN n name $ mapOn tests $ \ ((w,x,y),z) ->
  (name ++ " " ++ showsPrec 11 w [] ++ " " ++ showsPrec 11 x [] ++ " " ++ showsPrec 11 y [],\() -> f w x y,z)

runTestsDir :: (Show a,Eq b,Show b) => (String,String,a -> b,String,Text -> IO (a,b)) -> IO (String,Int,Int)
runTestsDir (n,name,f,dir,parse) = do
  fns <- fmap (sort . filter notHidden) $ listDirectory dir
  xys <- mapMOn fns $ \ fn -> do
    s <- Text.readFile $ dir ++ "/" ++ fn
    parse s
  runTests1 (n,name,f,xys)
  where
    notHidden cs = case cs of
      [] -> False
      ('.':_) -> False
      _ -> True
