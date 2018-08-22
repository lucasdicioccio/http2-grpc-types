{-# LANGUAGE OverloadedStrings #-}

-- | Module for GRPC binary encoding/decoding from and to binary messages.
--
-- So far, only "pure" compression algorithms are supported as we rely on
-- 'Decoder' (resp. 'Builder') which run with no interleaved side-effects.
module Network.GRPC.HTTP2.Encoding (
  -- * Decoding.
    decoder
  , fromDecoder
  -- * Encoding.
  , encode
  , fromBuilder
  -- * Compression.
  , Compression
  , uncompressed
  , gzip
  ) where

import qualified Codec.Compression.GZip as GZip
import           Data.Binary.Builder (Builder, toLazyByteString, fromByteString, singleton, putWord32be)
import           Data.Binary.Get (getByteString, getInt8, getWord32be, pushChunk, runGet, runGetIncremental, Decoder(..))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.ProtoLens.Encoding (encodeMessage, decodeMessage)
import           Data.ProtoLens.Message (Message)

-- | Decoder for gRPC/HTTP2-encoded Protobuf messages.
decoder :: Message a => Compression -> Decoder (Either String a)
decoder compression = runGetIncremental $ do
    isCompressed <- getInt8      -- 1byte
    let decompress = if isCompressed == 0 then id else (_decompressionFunction compression)
    n <- getWord32be             -- 4bytes
    decodeMessage . decompress <$> getByteString (fromIntegral n)

-- | Tries finalizing a Decoder.
fromDecoder :: Decoder (Either String a) -> Either String a
fromDecoder (Fail _ _ msg) = Left msg
fromDecoder (Partial _)    = Left "got only a subet of the message"
fromDecoder (Done _ _ val) = val

-- | Encodes as binary using gRPC/HTTP2 framing.
encode :: Message m => m -> Compression -> Builder
encode plain compression =
    mconcat [ singleton (if _compressionByteSet compression then 1 else 0)
            , putWord32be (fromIntegral $ ByteString.length bin)
            , fromByteString (_compressionFunction compression $ bin)
            ]
  where
    bin = encodeMessage plain

-- | Finalizes a Builder.
fromBuilder :: Builder -> ByteString
fromBuilder = toStrict . toLazyByteString

-- | Opaque type for handling compression.
--
-- So far, only "pure" compression algorithms are supported.
-- TODO: suport IO-based compression implementations once we move from 'Builder'.
data Compression = Compression {
    _compressionName       :: ByteString
  , _compressionByteSet    :: Bool
  , _compressionFunction   :: (ByteString -> ByteString)
  , _decompressionFunction :: (ByteString -> ByteString)
  }

-- | Do not compress.
uncompressed :: Compression
uncompressed = Compression "identity" False id id

-- | Use gzip as compression.
gzip :: Compression
gzip = Compression "gzip" True (toStrict . GZip.compress . fromStrict) (toStrict . GZip.decompress . fromStrict)
