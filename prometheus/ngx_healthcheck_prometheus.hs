{-# LANGUAGE TemplateHaskell, TypeApplications, ViewPatterns #-}

module NgxHealthcheckPrometheus where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Prometheus ()
import           NgxExport.Tools.Subrequest ()

import           NgxExport.Healthcheck

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text.Encoding as T
import           Data.Binary
import           Data.Maybe

type MergedStats = MServiceKey AnnotatedFlatPeers
type SharedStats = MServiceKey FlatPeers
type FlatStats = MUpstream Int

toFlatStats :: MServiceKey [a] -> FlatStats
toFlatStats = ML.foldr (flip $ M.foldrWithKey $
                           \k v -> M.alter (setN $ length v) k
                       ) M.empty
    where setN n Nothing = Just n
          setN n (Just m) = Just $ max n m

mergedFlatStats :: ByteString -> L.ByteString
mergedFlatStats =
    encode . toFlatStats . fromJust . readFromByteStringAsJSON @MergedStats

ngxExportYY 'mergedFlatStats

sharedFlatStats :: ByteString -> L.ByteString
sharedFlatStats =
    encode . toFlatStats . fromJust . readFromByteStringAsJSON @SharedStats

ngxExportYY 'sharedFlatStats

nFailedServers :: ByteString -> L.ByteString
nFailedServers v =
    let (T.decodeUtf8 -> u, decode @FlatStats . L.fromStrict . C8.tail -> s) =
            C8.break (== '|') v
    in C8L.pack $ show $ fromMaybe 0 $ M.lookup u s

ngxExportYY 'nFailedServers

