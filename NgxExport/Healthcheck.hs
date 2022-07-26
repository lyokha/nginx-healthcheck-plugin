{-# LANGUAGE CPP, TemplateHaskell, ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings, BangPatterns, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, NumDecimals #-}

module NgxExport.Healthcheck (
    -- basic types are needed in the prometheus example
                              Upstream
                             ,Peer
                             ,ServiceKey
                             ,Peers
                             ) where

import           NgxExport
import           Network.HTTP.Client
import           Network.HTTP.Client.BrReadWithTimeout
import           Network.HTTP.Types.Status
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import           Control.Monad
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           System.IO.Unsafe
import           Data.IORef
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.List
import           Data.Char
import           Data.Ord
import           Data.Function
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Data.Aeson
#if MIN_VERSION_time(1,9,1)
import           Data.Fixed
#endif
import           Data.Int
import           Data.Time.Clock
import           Data.Time.Calendar
import           Safe

#ifdef SNAP_STATS_SERVER
import           Control.Monad.IO.Class
import           Control.Exception.Enclosed (handleAny)
import           Snap.Http.Server
import           Snap.Core
#endif

type Upstream   = Text
type Peer       = Text
type Url        = String
type HttpStatus = Int

data Conf = Conf { upstreams     :: [Upstream]
                 , interval      :: TimeInterval
                 , peerTimeout   :: TimeInterval
                 , endpoint      :: Maybe Endpoint
                 , sendStatsPort :: Maybe Int
                 } deriving Read

data Endpoint = Endpoint { epUrl      :: Url
                         , epPassRule :: PassRule
                         } deriving Read

data PassRule = DefaultPassRule
              | PassRuleByHttpStatus [HttpStatus]
              deriving Read

newtype PassRuleParams = PassRuleParams { responseHttpStatus :: HttpStatus }

defaultPassRuleParams :: PassRuleParams
defaultPassRuleParams = PassRuleParams { responseHttpStatus = 200 }

data TimeInterval = Hr Int
                  | Min Int
                  | Sec Int
                  | HrMin Int Int
                  | MinSec Int Int
                  deriving Read

type ServiceKey = Text

conf :: IORef (Map ServiceKey Conf)
conf = unsafePerformIO $ newIORef M.empty
{-# NOINLINE conf #-}

type Peers = Map Upstream [Peer]

peers :: IORef (Map ServiceKey Peers)
peers = unsafePerformIO $ newIORef M.empty
{-# NOINLINE peers #-}

active :: IORef [ServiceKey]
active = unsafePerformIO $ newIORef []
{-# NOINLINE active #-}

httpManager :: IORef (Map ServiceKey Manager)
httpManager = unsafePerformIO $ newIORef M.empty
{-# NOINLINE httpManager #-}

data StatsServerConf = StatsServerConf { ssPort          :: Int
                                       , ssPurgeInterval :: TimeInterval
                                       } deriving Read

stats :: IORef (UTCTime, Map Int32 (UTCTime, ML.Map ServiceKey Peers))
stats = unsafePerformIO $ newIORef (UTCTime (ModifiedJulianDay 0) 0, M.empty)
{-# NOINLINE stats #-}

both :: Arrow a => a b c -> a (b, b) (c, c)
both = join (***)

#if MIN_VERSION_time(1,9,1)
asIntegerPart :: forall a. HasResolution a => Integer -> Fixed a
asIntegerPart = MkFixed . (resolution (undefined :: Fixed a) *)
{-# SPECIALIZE INLINE asIntegerPart :: Integer -> Pico #-}
#endif

toNominalDiffTime :: TimeInterval -> NominalDiffTime
toNominalDiffTime =
#if MIN_VERSION_time(1,9,1)
    secondsToNominalDiffTime . asIntegerPart
#else
    fromRational . toRational . secondsToDiffTime
#endif
    . fromIntegral . toSec

getUrl :: ServiceKey -> Url -> IO HttpStatus
getUrl skey url = do
    httpManager' <- M.lookup skey <$> readIORef httpManager
    if isJust httpManager'
        then -- using httpNoBody here makes Nginx backends claim about closed
             -- keepalive connections!
             getResponseStatus url $
                 flip httpLbsBrReadWithTimeout $ fromJust httpManager'
        else undefined
    where getResponseStatus u =
            fmap (statusCode . responseStatus) . (parseRequest u >>=)

query :: ServiceKey -> Url -> Peer -> IO (Peer, HttpStatus)
query skey url = runKleisli $ arr id &&& Kleisli (getUrl skey . flip mkAddr url)
    where mkAddr = (("http://" ++) .) . (++) . T.unpack

catchBadResponse :: Peer -> IO (Peer, HttpStatus) -> IO (Peer, HttpStatus)
catchBadResponse p = handle $ \(_ :: SomeException) -> return (p, 0)

threadDelaySec :: Int -> IO ()
threadDelaySec = threadDelay . (* 1e6)

toSec :: TimeInterval -> Int
toSec (Hr h)       = 3600 * h
toSec (Min m)      = 60 * m
toSec (Sec s)      = s
toSec (HrMin h m)  = 3600 * h + 60 * m
toSec (MinSec m s) = 60 * m + s

byPassRule :: PassRule -> PassRuleParams -> Bool
byPassRule DefaultPassRule
    PassRuleParams { responseHttpStatus = st } = st == 200
byPassRule (PassRuleByHttpStatus sts)
    PassRuleParams { responseHttpStatus = st } = st `elem` sts

isActive :: ServiceKey -> IO Bool
isActive skey = (skey `elem`) <$> readIORef active

lookupServiceKey :: ServiceKey -> Map ServiceKey Peers -> Peers
lookupServiceKey = (fromMaybe M.empty .) . M.lookup

throwUserError :: String -> IO a
throwUserError = ioError . userError

throwWhenPeersUninitialized :: ServiceKey -> Peers -> IO ()
throwWhenPeersUninitialized skey ps = when (M.null ps) $ throwUserError $
    "Peers were not initialized for service set " ++ T.unpack skey ++ "!"

reportStats :: Int -> (Int32, ServiceKey, Peers) -> IO ()
reportStats ssp v@(_, skey, _) = do
    httpManager' <- M.lookup skey <$> readIORef httpManager
    if isJust httpManager'
        then handle (\(_ :: SomeException) -> return ()) $ do
            req <- parseRequest "POST http://127.0.0.1"
            let !req' = req { requestBody = RequestBodyLBS $ encode v
                            , port = ssp
                            , Network.HTTP.Client.path = "report"
                            }
            void $ httpNoBody req' $ fromJust httpManager'
        else undefined

checkPeers :: ByteString -> Bool -> IO L.ByteString
checkPeers cf fstRun = do
    let (skey, cf') = C8.break isSpace $ C8.dropWhile isSpace cf
        skey' = T.decodeUtf8 skey
    cf'' <- readIORef conf >>=
        maybe (do
                  let cf'' = readMay $ C8.unpack cf'
                  when (isNothing cf'') $ throwIO $
                      TerminateWorkerProcess "Unreadable peers configuration!"
                  let cf''' = fromJust cf''
                  atomicModifyIORef' conf $ (, ()) . M.insert skey' cf'''
                  return cf'''
              ) return . M.lookup skey'
    let !us  = upstreams cf''
        ep   = endpoint cf''
        int  = toSec $ interval cf''
        pto  = toSec $ peerTimeout cf''
        !ssp = sendStatsPort cf''
    if fstRun
        then do
            peers' <- lookupServiceKey skey' <$> readIORef peers
            let peers'' = foldr (flip (M.insertWith $ const id) []) peers' us
            atomicModifyIORef' peers $ (, ()) . M.insert skey' peers''
            readIORef httpManager >>=
                maybe (atomicModifyIORef' httpManager $
                          (, ()) . M.insert skey'
                                   (unsafePerformIO $
                                       newManager defaultManagerSettings
                                       { managerResponseTimeout =
                                           responseTimeoutMicro $ pto * 1e6
                                       }
                                   )
                      ) (void . return) . M.lookup skey'
            atomicModifyIORef' active $ (, ()) . (skey' :)
        else threadDelaySec int
    peers' <- lookupServiceKey skey' <$> readIORef peers
    throwWhenPeersUninitialized skey' peers'
    when (isJust ssp) $ do
        (fromIntegral -> pid) <- ngxCachedPid
        void $ async $ reportStats (fromJust ssp)
            (pid, skey', M.filter (not . null) peers')
    let concatResult = L.fromStrict . B.concat
    if isJust ep
        then do
            let ep'  = fromJust ep
                url  = epUrl ep'
                rule = epPassRule ep'
            (map (flip B.append "\0\n" . T.encodeUtf8) -> peers'') <-
                forConcurrently us $ \u -> do
                    let !ps = fromJust $ M.lookup u peers'
                    ps' <- forConcurrently ps $ \p ->
                        catchBadResponse p $ query skey' url p
                    let (psGood, psBad) = both (map fst) $
                            partition (byPassRule rule
                                      . (\st -> defaultPassRuleParams
                                            { responseHttpStatus = st }
                                        )
                                      . snd
                                      ) ps'
                        ic = T.intercalate ","
                    return $ T.concat [u, "|", ic psBad, "/", ic psGood]
            return $ concatResult ["1", skey, "\n", B.concat peers'']
        else return $ concatResult $
            "0" : skey : "\n" : map (T.encodeUtf8 . (`T.append` "|\0\n")) us
ngxExportServiceIOYY 'checkPeers

readFlag :: ByteString -> CUIntPtr
readFlag "0" = 0
readFlag "1" = 1
readFlag ""  = error "Unexpectedly empty check peers flag!"
readFlag x   = error $ "Unexpected check peers flag " ++ C8.unpack x ++ "!"

foreign import ccall unsafe "plugin_ngx_http_haskell_healthcheck"
    c_healthcheck :: Ptr () -> Ptr () -> Ptr () -> CUIntPtr -> CUIntPtr ->
                     CString -> Ptr CString -> Ptr CSize -> IO CIntPtr

updatePeers :: ByteString -> IO L.ByteString
updatePeers (C8.lines -> ls)
    | (B.splitAt 1 -> (readFlag -> ck, skey)) : us <- ls = do
        let skey'  = T.decodeUtf8 skey
            skey'' = L.fromStrict skey
        c   <- ngxCyclePtr
        umc <- ngxUpstreamMainConfPtr
        t   <- ngxCachedTimePtr >>= peek
        a   <- isActive skey'
        peers' <- lookupServiceKey skey' <$> readIORef peers
        if a
            then throwWhenPeersUninitialized skey' peers'
            else when (isNothing $ M.lookup skey' peers') $
                atomicModifyIORef' peers $ (, ()) . M.insert skey' M.empty
        usBad <- MV.replicate (length us) Nothing
        forM_ us $ \ps -> do
            let (T.decodeUtf8 . fst -> !u) = C8.break (== '|') ps
            B.unsafeUseAsCString ps $ \ps' ->
                alloca $ \pv ->
                    alloca $ \pl -> do
                        ((0 ==) -> !ok) <-
                            c_healthcheck c umc t ck (fromBool a) ps' pv pl
                        if ok
                            then do
                                v <- peek pv
                                (fromIntegral -> l) <- peek pl
                                (filter (not . T.null) . T.split (== ',')
                                    . T.decodeUtf8 -> ps'') <-
                                    B.unsafePackCStringLen (v, l)
                                let peers'' = fromMaybe [] $ M.lookup u peers'
                                unless (null peers'' && null ps'') $
                                    atomicModifyIORef' peers $
                                        (, ()) . M.update
                                            (Just . M.insert u ps'') skey'
                            else do
                                usBad' <- V.unsafeFreeze usBad
                                let idx = fromJust $
                                        V.findIndex (== Nothing) usBad'
                                usBad'' <- V.unsafeThaw usBad'
                                MV.unsafeWrite usBad'' idx $ Just u
        (V.toList -> usBad') <- V.unsafeFreeze usBad
        let usBad'' = L.fromStrict $ T.encodeUtf8 $ T.intercalate ", " $
                map fromJust $ takeWhile (/= Nothing) usBad'
        return $ if L.null usBad''
                     then ""
                     else L.concat ["Healthcheck: upstreams [", usBad''
                                   ,"] from service set ", skey''
                                   ," have failed to process"
                                   ]
    | otherwise = throwUserError "Parse error when reading saved peers data!"
ngxExportServiceHook 'updatePeers

updateStats :: L.ByteString -> NominalDiffTime -> IO ()
updateStats v int = do
    let s = decode' v
    when (isNothing s) $ throwUserError "Unreadable stats!"
    let (pid, skey, ps) = fromJust s
    !t <- getCurrentTime
    atomicModifyIORef' stats $
        (, ()) . \(t', ps') ->
            let (!tn, f) =
                    if diffUTCTime t t' >= int
                        then (t
                             ,M.filter $
                                 \(t'', _) -> diffUTCTime t t'' < int
                             )
                        else (t', id)
                !psn = f $ M.alter
                           (\old ->
                               let !new' = if isNothing old
                                               then ML.singleton skey ps
                                               else ML.insert skey ps $
                                                   snd $ fromJust old
                               in Just (t, new')
                           ) pid ps'
            in (tn, psn)

receiveStats :: L.ByteString -> ByteString -> IO L.ByteString
receiveStats v sint = do
    let !int = toNominalDiffTime $ readDef (Min 5) $ C8.unpack sint
    updateStats v int
    return "done"
ngxExportAsyncOnReqBody 'receiveStats

sendStats' :: IO (Map Int32 (UTCTime, ML.Map ServiceKey Peers))
sendStats' = snd <$> readIORef stats

sendStats :: ByteString -> IO ContentHandlerResult
sendStats = const $
    (, "text/plain", 200, []) . encode
    . M.map (second $ ML.filter $ not . null)
    <$> sendStats'
ngxExportAsyncHandler 'sendStats

sendMergedStats' :: IO (Map ServiceKey (Map Upstream [(UTCTime, Peer)]))
sendMergedStats' = merge <$> sendStats'
    where merge = M.foldl (ML.unionWith $ M.unionWith pickLatest) ML.empty
                  . M.map (\(t, s) -> ML.map (M.map $ map (t,)) s)
          pickLatest = ((map (maximumBy $ comparing fst)
                        . groupBy ((==) `on` snd)
                        ) .
                       ) . foldr (insertBy (groupEqual `on` snd))
          groupEqual a b | a == b = EQ
                         | otherwise = GT

sendMergedStats :: ByteString -> IO ContentHandlerResult
sendMergedStats = const $
    (, "text/plain", 200, []) . encode
    <$> sendMergedStats'
ngxExportAsyncHandler 'sendMergedStats


#ifdef SNAP_STATS_SERVER

ssConfig :: Int -> Config Snap a
ssConfig p = setPort p
           $ setBind "127.0.0.1"
           $ setAccessLog ConfigNoLog
           $ setErrorLog ConfigNoLog
           $ setVerbose False mempty

ssHandler :: NominalDiffTime -> Snap ()
ssHandler int = route [("report", Snap.Core.method POST $ receiveStatsSnap int)
                      ,("stat", Snap.Core.method GET sendStatsSnap)
                      ,("stat/merge", Snap.Core.method GET sendMergedStatsSnap)
                      ]

receiveStatsSnap :: NominalDiffTime -> Snap ()
receiveStatsSnap int =
    handleStatsExceptions "Exception while receiving stats" $ do
        !s <- readRequestBody 65536
        liftIO $ updateStats s int
        finishWith emptyResponse

sendStatsSnap :: Snap ()
sendStatsSnap =
    handleStatsExceptions "Exception while sending stats" $ do
        s <- liftIO sendStats'
        modifyResponse $ setContentType "application/json"
        writeLBS $ encode $ M.map (second $ ML.filter $ not . null) s

sendMergedStatsSnap :: Snap ()
sendMergedStatsSnap =
    handleStatsExceptions "Exception while sending stats" $ do
        s <- liftIO sendMergedStats'
        modifyResponse $ setContentType "application/json"
        writeLBS $ encode s

handleStatsExceptions :: String -> Snap () -> Snap ()
handleStatsExceptions cmsg = handleAny $ \e ->
    writeErrorResponse 500 $ show (e :: SomeException)
    where writeErrorResponse c msg = do
              modifyResponse $ setResponseStatus c $ T.encodeUtf8 $ T.pack cmsg
              writeBS $ T.encodeUtf8 $ T.pack msg

statsServer :: ByteString -> Bool -> IO L.ByteString
statsServer cf fstRun = do
    if fstRun
        then do
            cf' <- maybe (throwIO $
                             TerminateWorkerProcess
                                 "Unreadable stats server configuration!"
                         ) return $ readMay $ C8.unpack cf
            let !int = toNominalDiffTime $ ssPurgeInterval cf'
            simpleHttpServe (ssConfig $ ssPort cf') $ ssHandler int
        else threadDelaySec 5
    return ""
ngxExportServiceIOYY 'statsServer

#endif


reportPeers :: ByteString -> IO ContentHandlerResult
reportPeers = const $ do
    (M.map $ M.filter $ not . null -> peers') <- readIORef peers
    return (encode peers', "application/json", 200, [])
ngxExportAsyncHandler 'reportPeers

