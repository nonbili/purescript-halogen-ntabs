module NTabs
  ( Message(..)
  , Query
  , Action
  , HTML
  , setTabProps
  , component
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.UIEvent.MouseEvent as ME

type Props v m =
  { render :: v -> HTML v m
  , initial :: v
  , value :: Maybe v
  }

data Message v = Changed v

type Query = Const Void

data Action v m
  = OnClick v
  | Receive (Props v m)

type HTML v m = H.ComponentHTML (Action v m) () m

type DSL v m = H.HalogenM (State v m) (Action v m) () (Message v) m

type State v m =
  { props :: Props v m
  , value :: v
  }

initialState :: forall v m. Props v m -> State v m
initialState props =
  { props
  , value: props.initial
  }

type TabProps r =
  ( onClick :: ME.MouseEvent
  | r
  )

setTabProps
  :: forall v m r
   . v
  -> Array (HH.IProp (TabProps r) (Action v m))
  -> Array (HH.IProp (TabProps r) (Action v m))
setTabProps v props = props <>
  [ HE.onClick $ Just <<< const (OnClick v)
  ]

render :: forall v m. State v m -> HTML v m
render state =
  HH.div_
  [ state.props.render state.value
  ]

component :: forall v m. H.Component HH.HTML Query (Props v m) (Message v) m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

handleAction :: forall v m. Action v m -> DSL v m Unit
handleAction = case _ of
  OnClick value -> do
    H.modify_ $ _ { value = value }
    H.raise $ Changed value

  Receive props -> do
    H.modify_ \s -> s
      { props = props
      , value = fromMaybe s.value props.value
      }
