module SnabbdomHelpers where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object, empty)
import Snabbdom (VNodeData, VNodeEventObject, VNodeHookObjectProxy, VNodeProxy(..), patch, toVNode, toVNodeEventObject, toVNodeHookObjectProxy)
import Snabbdom (h) as Snabbdom

data HArgument
  = Selector String
  | Children (Array VNodeProxy)

type HArguments = Array HArgument

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
  }

h :: HArguments -> VNodeProxy
h [Selector s] = Snabbdom.h s emptyVNodeData []
h [Children c] = Snabbdom.h "test" emptyVNodeData c
h _ = Snabbdom.h "test" emptyVNodeData []

