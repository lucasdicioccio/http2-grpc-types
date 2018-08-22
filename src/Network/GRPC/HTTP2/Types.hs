{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Module for GRPC <> HTTP2 mapping.
module Network.GRPC.HTTP2.Types where

import           Control.Exception (Exception)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

-- | HTTP2 Header Key.
type HeaderKey = ByteString

-- | HTTP2 Header Value.
type HeaderValue = ByteString

grpcTimeoutH :: HeaderKey
grpcTimeoutH = "grpc-timeout"

grpcEncodingH :: HeaderKey
grpcEncodingH = "grpc-encoding"

grpcAcceptEncodingH :: HeaderKey
grpcAcceptEncodingH = "grpc-accept-encoding"

grpcAcceptEncodingHVdefault :: HeaderValue
grpcAcceptEncodingHVdefault = "identity,gzip"

grpcStatusH :: HeaderKey
grpcStatusH = "grpc-status"

grpcMessageH :: HeaderKey
grpcMessageH = "grpc-message"

grpcContentTypeHV :: HeaderValue
grpcContentTypeHV = "application/grpc+proto"

grpcStatusHV :: HeaderValue
grpcStatusHV = "Grpc-Status"

grpcMessageHV :: HeaderValue
grpcMessageHV = "Grpc-Message"

-- https://grpc.io/grpc/core/impl_2codegen_2status_8h.html#a35ab2a68917eb836de84cb23253108eb
data GRPCStatusCode =
    OK
  | CANCELLED
  | UNKNOWN
  | INVALID_ARGUMENT
  | DEADLINE_EXCEEDED
  | NOT_FOUND
  | ALREADY_EXISTS
  | PERMISSION_DENIED
  | UNAUTHENTICATED
  | RESOURCE_EXHAUSTED
  | FAILED_PRECONDITION
  | ABORTED
  | OUT_OF_RANGE
  | UNIMPLEMENTED
  | INTERNAL
  | UNAVAILABLE
  | DATA_LOSS
  deriving (Show, Eq, Ord)

trailerForStatusCode :: GRPCStatusCode -> ByteString
trailerForStatusCode = \case
    OK
      -> "0"
    CANCELLED
      -> "1"
    UNKNOWN
      -> "2"
    INVALID_ARGUMENT
      -> "3"
    DEADLINE_EXCEEDED
      -> "4"
    NOT_FOUND
      -> "5"
    ALREADY_EXISTS
      -> "6"
    PERMISSION_DENIED
      -> "7"
    UNAUTHENTICATED
      -> "16"
    RESOURCE_EXHAUSTED
      -> "8"
    FAILED_PRECONDITION
      -> "9"
    ABORTED
      -> "10"
    OUT_OF_RANGE
      -> "11"
    UNIMPLEMENTED
      -> "12"
    INTERNAL
      -> "13"
    UNAVAILABLE
      -> "14"
    DATA_LOSS
      -> "15"

type GRPCStatusMessage = ByteString

data GRPCStatus = GRPCStatus !GRPCStatusCode !GRPCStatusMessage
  deriving (Show, Eq, Ord)

instance Exception GRPCStatus

trailers :: GRPCStatus -> [(ByteString, ByteString)]
trailers (GRPCStatus s msg) =
    if ByteString.null msg then [status] else [status, message]
  where
    status = ("grpc-status", trailerForStatusCode s)
    message = ("grpc-message", msg)
