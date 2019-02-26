module HaskPapers.Api.Endpoint
  ( Endpoint(..)
  , endpointCodec
  ) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint = ArchiveEndpoint

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ sum { "ArchiveEndpoint": "papers.json" / noArgs }
