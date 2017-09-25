{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Data.Conduit
import Control.Monad.Trans
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CB

source :: Source IO Int
source = CL.sourceList [1..100]

conduit :: Conduit Int IO String
conduit = do
  val <- await
  liftIO $ print val
  case val of
    Nothing -> return ()
    Just n -> do
      if | n `mod` 15 == 0 -> yield "FizzBuzz"
         | n `mod` 5  == 0 -> yield "Fizz"
         | n `mod` 3  == 0 -> yield "Buzz"
         | otherwise       -> return ()
      conduit

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

ex1 :: IO ()
ex1 = do
  putStrLn "List version:"
  mapM_ print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn ""
  putStrLn "Conduit version:"
  runConduit
     $ CB.yieldMany [1..]
    .| CB.take 10
    .| CB.map (* 2)
    .| CB.takeWhile (< 18)
    .| CB.mapM_ print

ex2 :: IO ()
ex2 = do 
  putStrLn $ runConduitPure
    $ CB.yieldMany [1..10 :: Int]
   .| CB.map show
   .| CB.unlines
   .| CB.fold

ex3 :: IO ()
ex3 = do
  print $ runConduitPure $ yield 1 .| await
  print $ runConduitPure $ CB.yieldMany [] .| await
  print $ runConduitPure $ return () .| await
  print $ runConduitPure await
   
main :: IO ()
main = source $$ conduit =$ sink