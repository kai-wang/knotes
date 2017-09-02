import Crypto.Hash
import Data.ByteString
import qualified Data.ByteString.Char8 as C

sha1 :: ByteString -> Digest SHA1
sha1 = hash

hexSha3_512 :: ByteString -> String
hexSha3_512 bs = show (hash bs :: Digest SHA3_512)

main :: IO ()
main = do
  print $ sha1 $ C.pack "test"
