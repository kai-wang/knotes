{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative

data MyContext = MyContext {
  foo :: String,
  bar :: Int
} deriving (Show)

computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just $ show n)
    else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "haskell" 0

type MyWriter = Writer [Int] String

example :: MyWriter
example = do
  tell [1..3]
  tell [3..5]
  return "foo"

output :: (String, [Int])
output = runWriter example

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

main :: IO()
main = print $ evalState test 0

type Env = [(String, Int)]
type Eval a = ReaderT Env Maybe a

data Expr = Val Int | Add Expr Expr | Var String deriving (Show)

eval :: Expr -> Eval Int
eval ex = case ex of
  Val n -> return n
  Add x y -> do
    a <- eval x
    b <- eval y
    return (a+b)
  Var x -> do
    env <- ask
    val <- lift (lookup x env)
    return val

env :: Env
env = [("x", 55), ("y", 5)]

ex3 :: Eval Int
ex3 = eval(Add (Val 2) (Add (Val 1) (Var "x")))

example1, example2 :: Maybe Int
example1 = runReaderT ex3 env
example2 = runReaderT ex3 []


type Stack = [Int]
type Output = [Int]
type Program = [Instr]

type VM a = ReaderT Program (WriterT Output (State Stack)) a

newtype Comp a = Comp { unComp :: VM a }
  deriving (Monad, MonadReader Program, MonadWriter Output, MonadState Stack)

instance Functor Comp where
  fmap = liftM

instance Applicative Comp where
  pure = return
  (<*>) = ap

data Instr = Push Int | Pop | Puts

evalInstr :: Instr -> Comp ()
evalInstr instr = case instr of 
  Pop -> modify tail
  Push n -> modify (n:)
  Puts -> do
    tos <- gets head
    tell [tos]


eval2 :: Comp ()
eval2 = do
  instr <- ask
  case instr of
    [] -> return ()
    (i:is) -> evalInstr i >> local (const is) eval2

execVM :: Program -> Output
execVM = flip evalState [] . execWriterT . runReaderT (unComp eval2)

program :: Program
program = [Push 42, Push 27, Puts, Pop, Puts, Pop]

run :: IO ()
run = mapM_ print $ execVM program