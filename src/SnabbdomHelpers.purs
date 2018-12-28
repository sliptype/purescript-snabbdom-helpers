module SnabbdomHelpers where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object, empty)
import Snabbdom (VNodeData, VNodeProxy(..), toVNodeEventObject, toVNodeHookObjectProxy)
import Snabbdom (h) as Snabbdom

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
  }

h :: VNodeProxy
h = Snabbdom.h "" emptyVNodeData []

sel :: String -> VNodeProxy -> VNodeProxy
sel s (VNodeProxy v) = VNodeProxy $ v { sel = s }

-- TODO: Use a record for props
props :: Object String -> VNodeProxy -> VNodeProxy
props p (VNodeProxy v) = VNodeProxy $ v { data { attrs = p } }

on :: forall a. Object (a -> Effect Unit) -> VNodeProxy -> VNodeProxy
on e (VNodeProxy v) = VNodeProxy $ v { data { on = toVNodeEventObject e } }

-- TODO: Because this isn't using h, the text only children aren't being converted
-- to VNodes
children :: Array VNodeProxy -> VNodeProxy -> VNodeProxy
children c (VNodeProxy v) = VNodeProxy $ v { children = c }

-- h # sel "#div"
--   # props { disabled: "true" }
--   # on { click: doSomething }
--   # children []
