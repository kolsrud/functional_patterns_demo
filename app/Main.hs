module Main where

import Control.Exception (SomeException, catch)
import System.IO (hFlush, stdout)
import Test.QuickCheck
import AbsSyntax

main :: IO ()
main = runTest 1000000 prop_simplificationReducesSize
     
runTest :: Int -> (Expr -> Bool) -> IO ()
runTest n f = do v <- generate (vectorOf n arbitrary)
                 runTests (zip [1..] v) f

runTests :: [(Int, Expr)] -> (Expr -> Bool) -> IO ()
runTests [] _ = putStrLn "No errors found."
runTests ((n,t):ts) f = do
  -- Demo cheat: Make sure the test case found is sufficiantly large
  --             to make the effect of shrinking more apparent.
  b <- if size t < 25 then return True else checkCase f t 
  if b then
    runTests ts f
   else do
    putStr $ "Found failing test case: " ++ (printExpr t)
    hFlush stdout
    new_t <- performShrink 1 f t
    putStrLn $ "Shrink complete: " ++ printExpr new_t
    putStrLn $ "Abstract syntax: " ++ show new_t

performShrink ::  Int -> (Expr -> Bool) -> Expr -> IO Expr
performShrink n f e = do
  hFlush stdout
  c <- getChar
  case c of
    'q' -> return e
    _ -> do m_failure <- findFailureM f (shrink e)
            case m_failure of
              Nothing -> putStrLn "" >> return e
              Just new_e -> do
                  putStr $ "Shrink succeeded (" ++ show n ++ "): " ++ printExpr new_e
                  performShrink (n+1) f new_e

findFailureM :: (a -> Bool) -> [a] -> IO (Maybe a)
findFailureM _ [] = return Nothing
findFailureM f (e:es) = do
  b <- checkCase f e
  if b then
    findFailureM f es
  else
    return (Just e)

checkCase :: (a -> Bool) -> a -> IO Bool
checkCase f t = catch ( let x = f t
                         -- Force evaluation of 'x' to trigger potential exception.
                         in seq x (return x)
                      )
                      (\e -> const (return False) (e::SomeException))
