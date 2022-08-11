module NgxExport.Healthcheck.Types (ServiceKey
                                   ,Upstream
                                   ,Peer
                                   ,Peers
                                   ,AnnotatedPeers
                                   ,MUpstream
                                   ,MServiceKey
                                   ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)

-- | Custom service key.
type ServiceKey = Text
-- | Upstream name.
type Upstream = Text
-- | Peer identifier.
type Peer = Text

-- | List of peers.
type Peers = [Peer]
-- | List of peers annotated by timestamps.
type AnnotatedPeers = [(UTCTime, Peer)]

-- | Map over 'Upstream' keys.
type MUpstream a = Map Upstream a
-- | Map over 'ServiceKey' keys with values of an 'MUpstream' type instance.
type MServiceKey a = Map ServiceKey (MUpstream a)

