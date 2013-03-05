import Dust.Crypto.DustCipher
import Dust.Network.DustServer

main = dustServer echo

echo :: Plaintext -> IO(Plaintext)
echo plaintext = do
    return plaintext
