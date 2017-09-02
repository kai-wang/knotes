import Data.Binary
import Data.Binary.Get

data Exp = IntE Int | OpE  String Exp Exp deriving Show

instance Binary Exp where
	put (IntE i) = do 
		put (0 :: Word8)
		put i
	
	put (OpE s e1 e2) = do 
		put (1 :: Word8)
        put s
        put e1
        put e2

	get = do 
		t <- get :: Get Word8
		case t of
			0 -> do i <- get
					return (IntE i)
            1 -> do s  <- get
                    e1 <- get
                    e2 <- get
					return (OpE s e1 e2)

