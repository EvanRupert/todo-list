port module Todo exposing (..)


import Html exposing (..)
import Html.Events exposing ( onInput, onClick )
import Html.Attributes exposing (..)
import Char exposing (fromCode)


--TODO: figure out how to get the styling correct
--TODO: add 'event' for pressing the enter key to submit item
--FIXME: fix length issue for task strings


type alias Item =
    { content : String
    , id : Int
    , completed : Bool
    }



type alias Model =
    { content : List Item
    , cItem : Item
    , cId : Int
    }


type Msg 
    = Editing String
    | Submit
--  | Remove Int


main =
    beginnerProgram { model = blankModel, view = view, update = update }


blankModel : Model
blankModel = 
    --TODO: return start to empty list after testing
    { content = testList
    , cItem = blankItem
    , cId = 0
    }


testList : List Item  
testList =
    [ { content = "Something", id = 0, completed = False }
    , { content = "Something Else", id = 1, completed = False }
    ]


blankItem : Item
blankItem =
    { content = ""
    , id = 0
    , completed = False
    }


view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ h1 [ id "title" ] [ text "Todo List" ]
        , div [ id "list-div" ] [ol [ id "list" ] <| List.map itemToHtml model.content]
        , input [ placeholder "What to do...?"
                , onInput Editing
                , value model.cItem.content
                ] []
        , button [ onClick Submit ] [ text "Add" ]
        ]


itemToHtml : Item -> Html Msg
itemToHtml item =
    let 
        listify node = 
            if item.completed then
                li [ class "completed" ] [ node ]
            else
                li [] [ node ]
    in
        div [ class "item-div" ]
            [ div [ class "done-button-div" ] [ button [ class "done-button" ] [] ]
            , div [ class "task-content-div" ] [ span [ class "task-content" ] [ text item.content ] ]
            , div [ class "close-button-div" ] [ button [ class "close-button" ] [ text <| toString <| fromCode 10006 ] ]
            ]
        |> listify


update : Msg -> Model -> Model
update msg model =

    let
        current = model.cItem
    in
        case msg of
            Editing s ->
                { model | cItem = { current | content = s } }
            
            Submit ->
                { model 
                | content = model.content ++ [current]
                , cItem = blankItem
                }























