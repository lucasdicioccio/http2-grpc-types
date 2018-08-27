{-# LANGUAGE OverloadedStrings #-}

-- | Module for GRPC binary encoding/decoding from and to binary messages.
--
-- So far, only "pure" compression algorithms are supported as we rely on
-- 'Decoder' (resp. 'Builder') which run with no interleaved side-effects.
module Network.GRPC.HTTP2.Encoding (
  -- * Decoding.
    decoder
  , fromDecoder
  , decodeInput
  , decodeOutput
  -- * Encoding.
  , encode
  , fromBuilder
  , encodeInput
  , encodeOutput
  -- * Compression.
  , Compression(..)
  , Encoding(..)
  , Decoding(..)
  , grpcCompressionHV
  , uncompressed
  , gzip
  ) where

import qualified Codec.Compression.GZip as GZip
import           Data.Binary.Builder (Builder, toLazyByteString, fromByteString, singleton, putWord32be)
import           Data.Binary.Get (getByteString, getInt8, getWord32be, runGetIncremental, Decoder(..), Get)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.ProtoLens.Encoding (encodeMessage, decodeMessage)
import           Data.ProtoLens.Message (Message)
import           Data.ProtoLens.Service.Types (Service(..), HasMethod, HasMethodImpl(..))

import           Network.GRPC.HTTP2.Types

-- | Decoder for gRPC/HTTP2-encoded Protobuf messages.
decoder :: Message a => Compression -> Decoder (Either String a)
decoder compression = runGetIncremental $ do
    isCompressed <- getInt8      -- 1byte
    let decompress = if isCompressed == 0 then pure else (_decompressionFunction compression)
    n <- getWord32be             -- 4bytes
    decodeMessage <$> (decompress =<< getByteString (fromIntegral n))

-- | Tries finalizing a Decoder.
fromDecoder :: Decoder (Either String a) -> Either String a
fromDecoder (Fail _ _ msg) = Left msg
fromDecoder (Partial _)    = Left "got only a subet of the message"
fromDecoder (Done _ _ val) = val

decodeOutput
  :: (Service s, HasMethod s m)
  => RPC s m
  -> Compression
  -> Decoder (Either String (MethodOutput s m))
decodeOutput _ = decoder

decodeInput
  :: (Service s, HasMethod s m)
  => RPC s m
  -> Compression
  -> Decoder (Either String (MethodInput s m))
decodeInput _ = decoder

-- | Encodes as binary using gRPC/HTTP2 framing.
encode :: Message m => Compression -> m -> Builder
encode compression plain =
    mconcat [ singleton (if _compressionByteSet compression then 1 else 0)
            , putWord32be (fromIntegral $ ByteString.length bin)
            , fromByteString (_compressionFunction compression $ bin)
            ]
  where
    bin = encodeMessage plain

-- | Finalizes a Builder.
fromBuilder :: Builder -> ByteString
fromBuilder = toStrict . toLazyByteString

encodeInput
  :: (Service s, HasMethod s m)
  => RPC s m
  -> Compression
  -> MethodInput s m
  -> Builder
encodeInput _ = encode

encodeOutput
  :: (Service s, HasMethod s m)
  => RPC s m
  -> Compression
  -> MethodOutput s m
  -> Builder
encodeOutput _ = encode

-- | Opaque type for handling compression.
--
-- So far, only "pure" compression algorithms are supported.
-- TODO: suport IO-based compression implementations once we move from 'Builder'.
data Compression = Compression {
    _compressionName       :: ByteString
  , _compressionByteSet    :: Bool
  , _compressionFunction   :: (ByteString -> ByteString)
  , _decompressionFunction :: (ByteString -> Get ByteString)
  }

-- | Compression for Encoding.
newtype Encoding = Encoding { _getEncodingCompression :: Compression }

-- | Compression for Decoding.
newtype Decoding = Decoding { _getDecodingCompression :: Compression }


grpcCompressionHV :: Compression -> HeaderValue
grpcCompressionHV = _compressionName

-- | Do not compress.
uncompressed :: Compression
uncompressed = Compression "identity" False id (\_ -> fail "decoder uninstalled")

-- | Use gzip as compression.
gzip :: Compression
gzip = Compression "gzip" True
     (toStrict . GZip.compress . fromStrict)
     (pure . toStrict . GZip.decompress . fromStrict)
