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
      img [ baseAvatarStyle
          , avatarStyle
          , src participant.profile.avatarPath
          ]
          []


baseAvatarStyle : Attribute
baseAvatarStyle =
    style
        [ ("border-radius", "50%")
        , ("height", "40px")
        , ("width", "40px")
        ]

avatarViewingStyle : Attribute
avatarViewingStyle =
  style
      []

avatarIdleStyle : Attribute
avatarIdleStyle =
  style
      [ ("opacity", "0.5") ]

avatarTypingStyle : Attribute
avatarTypingStyle =
  style
      [ ("border", "2px solid #60B5CC") ]
