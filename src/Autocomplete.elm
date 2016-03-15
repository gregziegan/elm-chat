module Autocomplete (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import String exposing (..)
import Json.Decode as Json


type alias Item =
  { key : ID
  , text : Text
  , html : Html
  }


type alias ID =
  String


type alias Text =
  String


type alias Model =
  { value : String
  , items : List Item
  , filteredItems : List Item
  }


init : List Item -> Model
init items =
  { value = ""
  , items = items
  , filteredItems = items
  }


initItemCustomHtml : ID -> Text -> Html -> Item
initItemCustomHtml id text html =
  { key = id
  , text = text
  , html = html
  }


initItem : ID -> Text -> Item
initItem id text =
  { key = id
  , text = text
  , html = viewItem text
  }


type Action
  = SetValue String
  | Complete


update : Action -> Model -> Model
update action model =
  case action of
    SetValue value ->
      if value == "" then
        { model
          | value = value
          , filteredItems = model.items
        }
      else
        { model
          | value = value
          , filteredItems = List.filter (\item -> startsWith value item.text) model.items
        }

    Complete ->
      let
        firstItem =
          List.head model.filteredItems
      in
        case firstItem of
          Just item ->
            { model | value = item.text }

          Nothing ->
            model


viewInput : Address Action -> Model -> Attribute -> Html
viewInput address model attribute =
  input
    [ type' "text"
    , on "input" targetValue (Signal.message address << SetValue)
    , onTab address Complete
    , value model.value
    , attribute
    ]
    []


view : Address Action -> Model -> Html
view address model =
  div
    [ id "autocomplete" ]
    [ viewInput address model (style [])
    , viewMenu model
    ]


viewItem : Text -> Html
viewItem text' =
  div [ class "autocomplete-item-default" ] [ text text' ]


viewMenu : Model -> Html
viewMenu model =
  div
    [ class "autocomplete-menu" ]
    (List.map (\item -> item.html) model.filteredItems)


onTab : Signal.Address a -> a -> Attribute
onTab address value =
  onWithOptions
    "keydown"
    { defaultOptions | preventDefault = True }
    (Json.customDecoder keyCode is9)
    (\_ -> Signal.message address value)


is9 : Int -> Result String ()
is9 code =
  if code == 9 then
    Ok ()
  else
    Err "not the right key code"
