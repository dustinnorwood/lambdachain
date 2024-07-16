module Blockchain.Data.Util
  ( byteString2Integer,
    integer2Bytes,
  )
where

import           Data.Bits
import qualified Data.ByteString as B

byteString2Integer :: B.ByteString
                   -> Integer
byteString2Integer = flip go 0
  where
    go bs acc = case B.uncons bs of
      Nothing -> acc
      Just (byte,restofbytes) ->
        let newacc = acc * 256 + fromIntegral byte
          in go restofbytes newacc

integer2Bytes :: Integer
              -> B.ByteString
integer2Bytes = B.pack . go []
  where
    go acc 0 = acc
    go acc x = go (fromInteger (x .&. 255) : acc)
                  (x `shiftR` 8)
