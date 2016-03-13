module Chat where
import Dict exposing (Dict)
import User
import Message
import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (startsWith)
import Set exposing (Set)
import Autocomplete
import Time exposing (Time)
import Draft

type alias Model =
  { id : ID
  , messages : List Message.Model
  , draft : Draft.Model
  , outbox : (List Draft.Model)
  , viewer : User.Model
  , participants : Set User.Handle
  , team : Set User.Handle
  , allUsers : Dict String User.Model
  , showMentionList : Bool
  , mentions : Set User.Handle
  , shift : Bool
  , mentionComplete : Autocomplete.Model
  , currentMention : Maybe String
  }

type alias ID = String

init : ID -> User.Model -> Dict String User.Model -> Model
init id viewer allUsers =
  let
      team = Dict.keys allUsers
              |> Set.fromList
      participants = Set.fromList [ viewer.handle ]
      candidates = Set.diff team participants |> Set.toList
  in
      { id = id
      , messages = []
      , draft = Draft.init viewer
      , outbox = []
      , viewer = viewer
      , participants = participants
      , team = team
      , allUsers = allUsers
      , showMentionList = False
      , mentions = Set.empty
      , shift = False
      , mentionComplete = initAtMention allUsers candidates
      , currentMention = Nothing
      }

initAtMention : Dict User.Handle User.Model -> List User.Handle -> Autocomplete.Model
initAtMention allUsers candidates =
  Autocomplete.init (List.map
    (\handle -> initCustomAtMentionItem <| User.getUserOrAnon handle allUsers)
    candidates)

initCustomAtMentionItem : User.Model -> Autocomplete.Item
initCustomAtMentionItem user =
  Autocomplete.initItemCustomHtml user.id user.handle (viewInviteRow user)

type Action
  = Deliver
  | UpdateDraft Draft.Action
  | Mention
  | ChangeViewer User.Model
  | UpdateAllUsers (Dict User.Handle User.Model)
  | UpdateMessage Message.ID Message.Action
  | UpdateMessages (List Message.Model)
  | SetShift Bool

update : Action -> Time -> Model -> Model
update action time model =
  case action of
    Deliver ->
      let
          newParticipantsMsgs = Set.diff model.mentions model.participants
            |> Set.toList
            |> List.map (\h -> User.getUserOrAnon h model.allUsers)
            |> List.map (\user -> Message.initWithoutSender -1 time (user.name ++ " has joined!"))
      in
          { model
          | draft = Draft.init model.viewer
          , outbox = model.draft :: model.outbox
          , participants = Set.union model.participants model.mentions
          , mentions = Set.empty
          }
    UpdateDraft act ->
      let
          updatedDraft = Draft.update act time model.draft
          content = updatedDraft.content
          lastMentionIndex = getLastMentionIndex content
          updatedChat = { model | draft = updatedDraft }
      in
          if act == Draft.TabPress then
            update Mention time model
          else if act == Draft.Send then
            update Deliver time model
          else if String.endsWith "@" content then
            { updatedChat | showMentionList = True }
          else
            case lastMentionIndex of
              Just index -> updateMention index updatedChat
              Nothing -> updatedChat
    Mention ->
      let
          updatedMention = Autocomplete.update Autocomplete.Complete model.mentionComplete
          mentionHandle = updatedMention.value
          updatedMentions = Set.insert updatedMention.value model.mentions
          candidates = Set.diff (Set.diff model.team model.participants) updatedMentions
            |> Set.toList
          updatedDraft =
            Draft.update (Draft.Content <| completeMention mentionHandle model.draft.content) time model.draft
      in
          { model
          | mentionComplete = initAtMention model.allUsers candidates
          , mentions = updatedMentions
          , showMentionList = False
          , currentMention = Nothing
          , draft = updatedDraft
          }
    ChangeViewer viewer ->
      { model
      | viewer = viewer
      , draft = Draft.update (Draft.Author viewer) time model.draft
      }
    UpdateAllUsers users ->
      { model | allUsers = users }
    UpdateMessage id act ->
      let
          updateMessage msg =
            if id == msg.id then
              Message.update act time msg
            else
              msg
      in
          { model | messages = List.map updateMessage model.messages }
    UpdateMessages messages ->
      { model | messages = messages }
    SetShift bool ->
      { model | shift = bool, draft = Draft.update (Draft.SetShift bool) time model.draft }


updateMention : Int -> Model -> Model
updateMention lastMentionIndex model =
  let
      mentionUpdate = String.dropLeft (lastMentionIndex + 1) model.draft.content
      updatedModel = { model | currentMention = Just mentionUpdate }
  in
      { updatedModel |
          mentionComplete = (Autocomplete.update (Autocomplete.SetValue mentionUpdate) updatedModel.mentionComplete)
      }

completeMention : User.Handle -> String -> String
completeMention mentionHandle draft =
  let
      lastIndex = getLastMentionIndex draft
  in
      case lastIndex of
        Just index -> String.left (index + 1) draft ++ mentionHandle
        Nothing -> draft

getLastMentionIndex : String -> Maybe Int
getLastMentionIndex message =
    String.indices "@" message
      |> List.reverse
      |> List.head

view : Signal.Address Action -> Model -> Html
view address model =
  let
      messages = List.map (\m -> Message.view (Signal.forwardTo address (UpdateMessage m.id)) m) model.messages
      draft = Draft.view (Signal.forwardTo address UpdateDraft) model.draft
  in
      div []
      [ viewChatHistory model messages
      , draft
      ]

viewChatHistory : Model -> List Html -> Html
viewChatHistory model messages =
  div [ chatHistoryStyle ] messages

viewInviteRow : User.Model -> Html
viewInviteRow user =
  div []
      [ img [ src user.avatarPath] []
      , p [] [ text (user.name ++ "  (" ++ user.handle ++ ")") ]
      ]

viewParticipants : List User.Model -> Html
viewParticipants participants =
  div []
    (List.map viewParticipant participants)


viewParticipant : User.Model -> Html
viewParticipant user =
  img [ src user.avatarPath ] []


chatHistoryStyle : Attribute
chatHistoryStyle =
  style
      [ ("padding", "30px 30px 20px")
      , ("border-bottom", "2px solid white")
      , ("overflow-y", "scroll")
      , ("height", "50vh")
      ]
