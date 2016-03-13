import Chat
import Dict exposing (Dict)
import User
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Task
import Effects exposing (Effects, Never)
import Signal exposing (Mailbox)
import Time exposing (Time, second, since)
import TimeApp
import Message
import Draft
import User exposing (anonymous)
import Debug

type alias Model =
    { currentUser : User.Model
    , chat : Chat.Model
    , shift : Bool
    , isTyping : Bool
    , otherTypers : List User.Model
    , users : Dict String User.Model
    }

init : User.Model -> Chat.Model -> Model
init user chat =
    { currentUser = user
    , chat = chat
    , shift = False
    , isTyping = False
    , otherTypers = []
    , users = Dict.fromList [ ("anonymous", User.anonymous) ]
    }

initialModel : Model
initialModel =
  let
      users = (Dict.fromList [ ("anonymous", User.anonymous) ])
      chat = Chat.init "123456" User.anonymous users
      model = init User.anonymous chat
  in
      model

type Action
    = Chat Chat.Action
    | ChangeUser User.Handle
    | SetShift Bool
    | Users (List (User.Handle, User.Model))
    | ListMessages (List Message.Model)
    | IsTyping Bool
    | OtherTypers (List User.Model)

update : Action -> Time -> Model -> (Model, Effects Action)
update action time model =
  case action of
    Chat act ->
      ( { model | chat = Chat.update act time model.chat }
      , Effects.none )
    ChangeUser handle ->
      let
          chat = model.chat
          user = User.getUserOrAnon (Debug.log "handle" handle) model.users
          updatedChat = { chat | viewer = user }
      in
          ( { model
            | currentUser = user
            , chat = Chat.update (Chat.ChangeViewer user) time model.chat
            }
          , Effects.none )
    SetShift bool ->
      ( { model | shift = bool, chat = Chat.update (Chat.SetShift bool) time model.chat }
      , Effects.none )
    Users users ->
      ( { model
        | chat = Chat.update (Chat.UpdateAllUsers (Dict.fromList users)) time model.chat
        , users = Dict.fromList users }
      , Effects.none )
    ListMessages messages ->
      ( { model | chat = Chat.update (Chat.UpdateMessages messages) time model.chat }
      , Effects.none )
    IsTyping bool ->
      ( { model | isTyping = bool }
      , Effects.none )
    OtherTypers typers ->
      ( { model | otherTypers = typers }
      , Effects.none )

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [("width", "400px")]]
      [ viewHeader model
      , Chat.view (Signal.forwardTo address Chat) model.chat
      , viewLogin address model
      , div [] (List.map (\user -> p [] [ text (user.name ++ " is typing") ]) model.otherTypers)
      ]

viewHeader : Model -> Html
viewHeader model =
  div []
      [ h2 [] [ text "Elm Chat"]
      , p [] [ text (if model.currentUser.handle == "anonymous" then "Please login" else ("Welcome, " ++ model.currentUser.name)) ]
      ]

viewLogin : Signal.Address Action -> Model -> Html
viewLogin address model =
  div []
      (p [] [ text "Login as: " ] :: (viewLoginButtons address model))

viewLoginButtons : Signal.Address Action -> Model -> List Html
viewLoginButtons address model =
  Dict.values model.users
    |> List.map (\user -> viewUserButton address user)

viewUserButton : Signal.Address Action -> User.Model -> Html
viewUserButton address user =
  button [ onClick address (ChangeUser user.handle) ] [ text user.handle ]

app : TimeApp.App Model
app =
  TimeApp.start
    { init = (initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = [ Signal.map ListMessages listMessages
               , Signal.map OtherTypers otherTypers
               , Signal.map Users users
               , Signal.map IsTyping (second `since` Keyboard.presses)
               , Signal.map SetShift Keyboard.shift
               ]
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks

port listMessages : Signal (List Message.Model)
port otherTypers : Signal (List User.Model)
port users : Signal (List (User.Handle, User.Model))

port typing : Signal (User.Model, Bool)
port typing =
  Signal.map (\m -> (m.currentUser, m.isTyping)) app.model
    |> Signal.dropRepeats

port drafts : Signal (List Draft.Model)
port drafts =
  Signal.map (\m -> m.chat.outbox) app.model
    |> Signal.dropRepeats
