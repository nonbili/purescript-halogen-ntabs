module Example.Uncontrolled where

import Example.Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import NTabs as Tabs

type Query = Const Void

type Action = Void

type State =
  { value :: String
  }

type Slot =
  ( tabs :: H.Slot Tabs.Query (Tabs.Message Int) Unit
  )

_tabs = SProxy :: SProxy "tabs"

type HTML = H.ComponentHTML Action Slot Aff

initialState :: State
initialState =
  { value: ""
  }

renderTabs :: Int -> Tabs.HTML Int Aff
renderTabs selected =
  HH.div_
  [ HH.ul
    [ class_ "flex"]
    [ HH.li
      ( Tabs.setTabProps 0
        [ class_ $ getTabCls 0
        ])
      [ HH.text "Tab 0"]
    , HH.li
      ( Tabs.setTabProps 1
        [ class_ $ getTabCls 1
        ])
      [ HH.text "Tab 1"]
    ]
  , HH.div
    [ class_ "mt-1 border p-4"]
    [ case selected of
         0 -> HH.text "tab 0 content"
         1 -> HH.text "tab 1 content"
         _ -> HH.text ""
    ]
  ]
  where
  baseCls = "px-3 cursor-pointer "
  inactiveCls = "text-gray-500"
  activeCls = "text-black border-b border-blue-500"
  getTabCls n = baseCls <> if (n == selected) then activeCls else inactiveCls

render :: State -> HTML
render state =
  HH.div
  [ class_ ""]
  [ HH.slot _tabs unit Tabs.component
    { render: renderTabs
    , initial: 0
    , value: Nothing
    } $ const Nothing
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
  }
