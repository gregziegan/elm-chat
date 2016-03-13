module Message where
import User exposing (Model)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (year, hour, minute, second, fromTime)
import String

type alias Model =
  { id : ID
  , content : String
  , sender : Maybe User.Model
  , sentAt : Time
  }

type alias ID = Int

init : ID -> User.Model -> Time -> String -> Model
init id sender sentAt content =
  let msg = initWithoutSender id sentAt content
  in { msg | sender = Just sender }

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

view : Signal.Address Action -> Model -> Html
view address model =
  case model.sender of
    Just sender -> viewSent sender model.sentAt model.content
    Nothing -> viewAsNotification model

viewAsNotification : Model -> Html
viewAsNotification model =
  div [ messageStyle ] [ viewMessageContent model.content ]

viewSent : User.Model -> Time -> String -> Html
viewSent sender sentAt content =
  div [ messageStyle ]
      [ viewAvatar sender
      , div [rightStyle]
            [ p [authorNameStyle]
                [ span [] [ text sender.name ]
                , text "  "
                , span [timeStyle] [ text (currentTime sentAt) ]
                ]
            , viewMessageContent content
            ]
      ]

viewMessageContent : String -> Html
viewMessageContent content =
  p [ style [("margin", "4px 4px 4px 4px")] ]
    (String.split "\n" content
      |> List.map (\line' -> p [ style [("margin", "4px 4px 4px 4px")]] [ text line' ]))


viewAvatar : User.Model -> Html
viewAvatar sender =
  img [src sender.avatarPath, avatarStyle] []

messageStyle : Attribute
messageStyle =
  style
    [ ("background-color", "#F2F2F2")
    , ("display", "flex")
    , ("marginLeft", "5px")
    ]

rightStyle : Attribute
rightStyle =
    style
        [ ("display", "flex")
        , ("flex-direction", "column")
        , ("padding-left", "5px")
        ]

authorNameStyle : Attribute
authorNameStyle =
    style
        [ ("font-size", "15px")
        , ("font-weight", "bold")
        , ("margin", "4px 4px 4px 4px")
        ]

timeStyle : Attribute
timeStyle =
    style
        [ ("font-size", "12px")
        , ("font-weight", "normal")
        , ("color", "grey")
        ]

avatarStyle : Attribute
avatarStyle =
    style
        [ ("width", "40px")
        , ("height", "40px")
        , ("margin", "5px 5px 0 0")
        , ("border-radius", "50%")
        ]

textareaStyle : Attribute
textareaStyle =
    style
        [ ("width", "100%")
        , ("padding", "10px 0")
        , ("font-size", "1em")
        , ("resize", "none")
        , ("overflow", "hidden")
        ]


currentTime : Time -> String
currentTime t =
  let date' = fromTime t
      hour' = toString (Date.hour date')
      minute' = toString (Date.minute date')
      minuteWPadding = if String.length minute' == 1 then "0" ++ minute' else minute'
      second' = toString (Date.second date')
      secondWPadding = if String.length second' == 1 then "0" ++ second' else second'
      year' = toString (year date')
  in
      (hour' ++ ":" ++ minuteWPadding ++ ":" ++ secondWPadding)
