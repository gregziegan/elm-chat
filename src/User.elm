module User (..) where

import Dict exposing (Dict)


type alias Model =
  { id : ID
  , handle : Handle
  , name : String
  , avatarPath : String
  }


type alias ID =
  String


type alias Handle =
  String


anonymous : Model
anonymous =
  { id = "0"
  , handle = "anonymous"
  , name = "Anonymous"
  , avatarPath = "http://api.adorable.io/avatar/40/anonymous"
  }


getUserOrAnon : Handle -> Dict Handle Model -> Model
getUserOrAnon handle users =
  Maybe.withDefault anonymous <| Dict.get handle users


isAnonymous : Model -> Bool
isAnonymous user =
  user.handle == anonymous.handle
