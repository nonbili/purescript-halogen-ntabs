module Example.Controlled where

import Example.Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NTabs as Tabs

type Query = Const Void

data Action
  = HandleTabs (Tabs.Message Tab)
  | OnSelectTab Tab

data Tab
  = TabX
  | TabY

derive instance eqTab :: Eq Tab

type State =
  { tab :: Tab
  }

type Slot =
  ( tabs :: H.Slot Tabs.Query (Tabs.Message Tab) Unit
  )

_tabs = SProxy :: SProxy "tabs"

type HTML = H.ComponentHTML Action Slot Aff

initialState :: State
initialState =
  { tab: TabY
  }

renderTabs :: Tab -> Tabs.HTML Tab Aff
renderTabs selected =
  HH.div_
  [ HH.ul
    [ class_ "flex"]
    [ HH.li
      ( Tabs.setTabProps TabX
        [ class_ $ getTabCls TabX
        ])
      [ HH.text "Tab X"]
    , HH.li
      ( Tabs.setTabProps TabY
        [ class_ $ getTabCls TabY
        ])
      [ HH.text "Tab Y"]
    ]
  , HH.div
    [ class_ "mt-1 border p-4"]
    [ case selected of
         TabX -> HH.text "tab x content"
         TabY -> HH.text "tab y content"
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
    , initial: TabX
    , value: Just state.tab
    } $ Just <<< HandleTabs
  , HH.div
    [ class_ "mt-4"]
    [ HH.div
      [ class_ "mb-1"]
      [ HH.text "Tabs are controlled by the parent state."]
    , HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick $ Just <<< const (OnSelectTab TabX)
      ]
      [ HH.text "select TabX"]
    , HH.button
      [ HP.type_ HP.ButtonButton
      , HE.onClick $ Just <<< const (OnSelectTab TabY)
      ]
      [ HH.text "select TabY"]
    ]
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction }
  }
  where
  handleAction = case _ of
    HandleTabs msg -> case msg of
      Tabs.Changed tab -> H.modify_ $ _ { tab = tab }

    OnSelectTab tab -> H.modify_ $ _ { tab = tab }
