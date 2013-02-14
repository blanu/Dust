import Dust.Crypto.DustCipher
import Data.ByteString.Char8 (pack)

main = do
  let key = Key (pack "1234567890123456")
  let iv = IV (pack "1234567890123456")

  let plain1 = Plaintext (pack "majestic")
  let cipher = encrypt key iv plain1
  let plain2 = decrypt key iv cipher

  putStrLn (show plain1)
  putStrLn (show cipher)
  putStrLn (show plain2)
