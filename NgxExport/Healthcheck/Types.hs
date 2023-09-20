module NgxExport.Healthcheck.Types (ServiceKey
                                   ,Upstream
                                   ,PeerName
                                   ,PeerHostName
                                   ,Peer
                                   ,Peers
                                   ,FlatPeers
                                   ,AnnotatedFlatPeers
                                   ,MUpstream
                                   ,MServiceKey
                                   ) where

import           Data.Text (Text)
import           Data.Map (Map)
import           Data.Time.Clock (UTCTime)

-- | Custom service key.
type ServiceKey = Text
-- | Upstream name.
type Upstream = Text
-- | Peer name (actually, IP address of the peer).
type PeerName = Text
-- | Peer host name (normally, FQDN).
type PeerHostName = Text
-- | Peer identifier.
type Peer = (PeerName, PeerHostName)

-- | List of peers.
type Peers = [Peer]
-- | List of peers without host names.
type FlatPeers = [PeerName]
-- | List of peers without host names annotated by timestamps.
type AnnotatedFlatPeers = [(UTCTime, PeerName)]

-- | Map over 'Upstream' keys.
type MUpstream a = Map Upstream a
-- | Map over 'ServiceKey' keys with values of an 'MUpstream' type instance.
type MServiceKey a = Map ServiceKey (MUpstream a)

