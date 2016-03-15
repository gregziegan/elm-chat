module Message (..) where

import User exposing (Model)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (year, hour, minute, second, fromTime)
import String


type Direction
  = Left
  | Right


type alias Model =
  { id : ID
  , content : String
  , sender : Maybe User.Model
  , sentAt : Time
  }


type alias ID =
  Int


init : ID -> User.Model -> Time -> String -> Model
init id sender sentAt content =
  let
    msg =
      initWithoutSender id sentAt content
  in
    { msg | sender = Just sender }


initWithoutSender : ID -> Time -> String -> Model
initWithoutSender id sentAt content =
  { id = id
  , sender = Nothing
  , content = content
  , sentAt = sentAt
  }


type Action
  = Content String


update : Action -> Time -> Model -> Model
update action time model =
  case action of
    Content content ->
      { model | content = content }


view : Signal.Address Action -> Model -> Direction -> Html
view address model direction =
  case model.sender of
    Just sender ->
      viewUserMessage direction sender model

    Nothing ->
      viewAsNotification model


viewUserMessage : Direction -> User.Model -> Model -> Html
viewUserMessage direction sender model =
  case direction of
    Left ->
      viewLeftMessage sender model

    Right ->
      viewRightMessage sender model


viewLeftMessage : User.Model -> Model -> Html
viewLeftMessage sender model =
  div
    [ leftMessageBoxStyle ]
    [ viewAvatar sender Left
    , div [ leftTriangleStyle ] []
    , div
        [ leftMessageStyle ]
        [ p
            [ authorNameStyle ]
            [ span [] [ text sender.name ]
            , text "  "
            , span [ timeStyle ] [ text (currentTime model.sentAt) ]
            ]
        , viewMessageContent model.content
        ]
    ]


viewRightMessage : User.Model -> Model -> Html
viewRightMessage sender model =
  div
    [ rightMessageBoxStyle ]
    [ div
        [ rightMessageStyle ]
        [ p
            [ authorNameStyle ]
            [ span [] [ text sender.name ]
            , text "  "
            , span [ timeStyle ] [ text (currentTime model.sentAt) ]
            ]
        , viewMessageContent model.content
        ]
    , div [ rightTriangleStyle ] []
    , viewAvatar sender Right
    ]


viewAsNotification : Model -> Html
viewAsNotification model =
  div [ notificationStyle ] [ viewMessageContent model.content ]


viewMessageContent : String -> Html
viewMessageContent content =
  p
    [ style [ ( "margin", "4px 4px 4px 4px" ) ] ]
    (String.split "\n" content
      |> List.map (\line' -> p [ style [ ( "margin", "4px 4px 4px 4px" ) ] ] [ text line' ])
    )


viewAvatar : User.Model -> Direction -> Html
viewAvatar sender direction =
  case direction of
    Left ->
      img [ src sender.avatarPath, leftAvatarStyle ] []

    Right ->
      img [ src sender.avatarPath, rightAvatarStyle ] []


messageBoxStyles : List ( String, String )
messageBoxStyles =
  [ ( "background-color", "#F2F5F8" )
  , ( "display", "flex" )
  , ( "color", "white" )
  , ( "line-height", "26px" )
  , ( "font-size", "16px" )
  , ( "margin-bottom", "30px" )
  , ( "width", "100%" )
  ]


notificationStyle : Attribute
notificationStyle =
  style messageBoxStyles


leftMessageBoxStyle : Attribute
leftMessageBoxStyle =
  style
    (List.append
      messageBoxStyles
      [ ( "margin-left", "5px" ) ]
    )


rightMessageBoxStyle : Attribute
rightMessageBoxStyle =
  style
    (List.append
      messageBoxStyles
      [ ( "margin-right", "5px" ) ]
    )


triangleStyles : List ( String, String )
triangleStyles =
  [ ( "width", "0" )
  , ( "height", "0" )
  , ( "align-self", "center" )
  ]


leftTriangleStyle : Attribute
leftTriangleStyle =
  style
    (List.append
      triangleStyles
      [ ( "border-top", "10px solid transparent" )
      , ( "border-bottom", "10px solid transparent" )
      , ( "border-right", "10px solid #EFAD00" )
      ]
    )


rightTriangleStyle : Attribute
rightTriangleStyle =
  style
    (List.append
      triangleStyles
      [ ( "border-top", "10px solid transparent" )
      , ( "border-bottom", "10px solid transparent" )
      , ( "border-left", "10px solid #7ED03A" )
      ]
    )


messageStyles : List ( String, String )
messageStyles =
  [ ( "display", "flex" )
  , ( "flex-direction", "column" )
  , ( "border-radius", "7px" )
  , ( "flex-grow", "2" )
  ]


leftMessageStyle : Attribute
leftMessageStyle =
  style
    (List.append
      messageStyles
      [ ( "padding-left", "5px" )
      , ( "background-color", "#EFAD00" )
      ]
    )


rightMessageStyle : Attribute
rightMessageStyle =
  style
    (List.append
      messageStyles
      [ ( "padding-right", "5px" )
      , ( "background-color", "#7ED03A" )
      ]
    )


authorNameStyle : Attribute
authorNameStyle =
  style
    [ ( "font-size", "15px" )
    , ( "font-weight", "bold" )
    , ( "margin", "4px 4px 4px 4px" )
    ]


timeStyle : Attribute
timeStyle =
  style
    [ ( "font-size", "12px" )
    , ( "font-weight", "normal" )
    , ( "color", "#F2F5F8" )
    ]


avatarStyles : List ( String, String )
avatarStyles =
  [ ( "width", "40px" )
  , ( "height", "40px" )
  , ( "border-radius", "50%" )
  , ( "align-self", "center" )
  ]


leftAvatarStyle : Attribute
leftAvatarStyle =
  style
    (List.append
      avatarStyles
      [ ( "margin-right", "7px" ) ]
    )


rightAvatarStyle : Attribute
rightAvatarStyle =
  style
    (List.append
      avatarStyles
      [ ( "margin-left", "7px" ) ]
    )


currentTime : Time -> String
currentTime t =
  let
    date' =
      fromTime t

    hour' =
      toString (Date.hour date')

    minute' =
      toString (Date.minute date')

    minuteWPadding =
      if String.length minute' == 1 then
        "0" ++ minute'
      else
        minute'

    second' =
      toString (Date.second date')

    secondWPadding =
      if String.length second' == 1 then
        "0" ++ second'
      else
        second'

    year' =
      toString (year date')
  in
    (hour' ++ ":" ++ minuteWPadding ++ ":" ++ secondWPadding)
