module Participant where
import Html exposing (..)
import Html.Attributes exposing (..)
import User

-- Model

type Status
  = Viewing
  | Idle
  | Typing

statusToString : Status -> String
statusToString status =
  case status of
    Viewing ->
        "Viewing"
    Idle ->
        "Idle"
    Typing ->
        "Typing"

statusFromString : String -> Status
statusFromString string =
  case string of
    "Viewing" ->
      Viewing
    "Idle" ->
      Idle
    "Typing" ->
      Typing
    _ ->
      Idle

defaultStatus : Status
defaultStatus =
  Viewing

type alias Participant =
  { profile : User.Model
  , status : StatusString
  }

type alias StatusString = String

init : User.Model -> Participant
init user =
  { profile = user
  , status = statusToString defaultStatus
  }

initWithStatus : User.Model -> StatusString -> Participant
initWithStatus user status =
  { profile = user
  , status = status
  }

-- View

view : Participant -> Html
view participant =
  let
      avatarStyle =
        case (statusFromString participant.status) of
          Viewing -> avatarViewingStyle
          Idle -> avatarIdleStyle
          Typing -> avatarTypingStyle
  in
      div [ class "participant-avatar" ]
          [ img [ avatarStyle, src participant.profile.avatarPath ] []
          , div [ class "tooltip" ]
                [ p [] [ text participant.profile.name ]
                , p [] [ text participant.status ]
                ]
          ]


baseAvatarStyles : List (String, String)
baseAvatarStyles =
        [ ("border-radius", "50%")
        , ("box-sizing", "border-box")
        , ("-moz-box-sizing", "border-box")
        , ("-webkit-box-sizing", "border-box")
        , ("height", "40px")
        , ("width", "40px")
        , ("margin", "2px")
        ]

avatarViewingStyle : Attribute
avatarViewingStyle =
  style
      baseAvatarStyles

avatarIdleStyle : Attribute
avatarIdleStyle =
  style
      (List.append [ ("opacity", "0.5") ] baseAvatarStyles)

avatarTypingStyle : Attribute
avatarTypingStyle =
  style
      (List.append [ ("border", "2px solid #60B5CC") ] baseAvatarStyles)
