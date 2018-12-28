module SnabbdomHelpers
  ( children
  , h
  , on
  , props
  , sel
  , text
  )
  where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Array (head)
import Foreign.Object (Object, empty)
import Snabbdom (VNodeData, VNodeProxy(..), toVNodeEventObject, toVNodeHookObjectProxy)
import Snabbdom (h, text) as Snabbdom

emptyVNodeData :: VNodeData
emptyVNodeData =
  { attrs : empty
  , on : toVNodeEventObject empty
  , hook : toVNodeHookObjectProxy { insert : Nothing, update : Nothing, destroy : Nothing }
  }

h :: VNodeProxy
h = Snabbdom.h "" emptyVNodeData []

getChildren :: VNodeProxy -> Array VNodeProxy
getChildren (VNodeProxy v) = v.children

getFirstChild :: VNodeProxy -> Maybe VNodeProxy
getFirstChild v = head $ getChildren v

-- Need to run text through h to generate text attribute
-- Might want to change this in purescript-snabbdom
text :: String -> VNodeProxy
text s =
  let
    vNodeFake = Snabbdom.h "div" emptyVNodeData [Snabbdom.text s]
    child = getFirstChild vNodeFake
  in
  case child of
    Nothing -> h
    Just c -> c

-- TODO: Merge with h
sel :: String -> VNodeProxy -> VNodeProxy
sel s (VNodeProxy v) = VNodeProxy $ v { sel = s }

-- TODO: Use a record for props
props :: Object String -> VNodeProxy -> VNodeProxy
props p (VNodeProxy v) = VNodeProxy $ v { data { attrs = p } }

on :: forall a. Object (a -> Effect Unit) -> VNodeProxy -> VNodeProxy
on e (VNodeProxy v) = VNodeProxy $ v { data { on = toVNodeEventObject e } }

children :: Array VNodeProxy -> VNodeProxy -> VNodeProxy
children c (VNodeProxy v) = VNodeProxy $ v { children = c }

-- h # sel "#div"
--   # props { disabled: "true" }
--   # on { click: doSomething }
--   # children []

-- Why not

-- h { sel: "#div"
--   , props: { disabled: "true" }
--   , on { click: doSomthing }
--   , children []
--   }
