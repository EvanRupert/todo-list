port module Todo exposing (..)


import Html exposing (..)
import Html.Events exposing ( onInput, onClick, on, keyCode )
import Html.Attributes exposing (..)
import Char exposing (fromCode)
import Json.Decode as Json


--FIXME: list items do not update until the filter has been changed
    --IDEA: this could be done by moving the actual filtering to the
    -- view function as a part of the listItems assignment
    -- this would also allow the 'visible' field to be discarded
    -- and the ChangeFilter match to only change the 'filter' field in
    -- the model


type alias Item =
    { content : String
    , id : Int
    , completed : Bool
    , visible : Bool
    }



type alias Model =
    { content : List Item
    , cItem : Item
    , cId : Int
    , filter : Filter
    }


type Filter
    = AllFilter
    | CompletedFilter
    | UnfinishedFilter


type Msg 
    = Editing String
    | Submit
    | Remove Int
    | Completed Int
    | ClearCompleted
    | ChangeFilter Filter


main =
    beginnerProgram { model = blankModel, view = view, update = update }


blankModel : Model
blankModel = 
    { content = []
    , cItem = blankItem
    , cId = 0
    , filter = AllFilter
    }



blankItem : Item
blankItem =
    { content = ""
    , id = 0
    , completed = False
    , visible = True
    }


--VIEW


view : Model -> Html Msg
view model =
    let
        items = model.content

        itemFilter =
            case model.filter of
                AllFilter -> identity
                CompletedFilter -> List.filter .completed
                UnfinishedFilter -> List.filter (not << .completed)

    in
        div [ id "main" ]
            [ h1 [ id "title" ] [ text "Todo List" ]
            , input [ placeholder "What to do...?"
                    , onInput Editing
                    , onEnter Submit
                    , value model.cItem.content
                    , id "input-field"
                    ] []
            , button [ onClick Submit
                    , disableIfEmpty model
                    ] [ text "Submit" ]
            , filtersBar model
            , div [ id "list-div" ] 
                [ ol [ id "list" ] 
                    (items
                    |> itemFilter
                    |> List.map itemToHtml)
                ]
            ]


disableIfEmpty : Model -> Attribute Msg
disableIfEmpty model =
    if String.isEmpty model.cItem.content then
        id "disabled-submit-button"
    else
        id "submit-button"


clearCompletedButton : Model -> Html Msg
clearCompletedButton model =
    let
        visibility =
            if exists .completed model.content then
                style [("visibility", "visible")]
            else
                style [("visibility", "hidden")]
    in
        button [ class "filter-button"
               , visibility
               , onClick ClearCompleted
               ] [ text "Clear Finished Items" ]


filtersBar : Model -> Html Msg
filtersBar model = 
    let
        visibility = 
            if List.isEmpty model.content then
                style [("visibility", "hidden")]
            else
                style [("visibility", "visible")]
    in
        div [ id "filter-bar", visibility ]
            [ button [ class "filter-button"
                    , if model.filter == AllFilter then class "selected-filter-button" else class "nothing"
                    , onClick <| ChangeFilter AllFilter
                    ] [ text "All" ]
            , button [ class "filter-button" 
                     , if model.filter == UnfinishedFilter then class "selected-filter-button" else class "nothing"
                     , onClick <| ChangeFilter UnfinishedFilter
                     ] [ text "Unfinished" ]
            , button [ class "filter-button"
                    , if model.filter == CompletedFilter then class "selected-filter-button" else class "nothing"
                    , onClick <| ChangeFilter CompletedFilter
                    ] [ text "Finished" ]
            , clearCompletedButton model
            ]


itemToHtml : Item -> Html Msg
itemToHtml item =
    let 
        ifCompleted item attrib =
            if item.completed then
                attrib
            else 
                class "nothing"

    in
        div [ class "item-div", ifCompleted item (class "completed") ]
            [ div [ class "done-button-div" ] 
                [ button [ class "done-button"
                         , onClick <| Completed item.id                             
                         ] [] 
                ]
            , div [ class "task-content-div" ] [ span [ class "task-content"
                                                      , ifCompleted item (style [("text-decoration", "line-through")])
                                                      ] [ text item.content ] 
                                               ]
            , div [ class "close-button-div" ] 
                [ button 
                    [ class "close-button"
                    , onClick <|Remove item.id 
                    ] [] 
                ]
            ]
        |> \x -> li [] [x]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let 
        isEnter code =
            if code ==13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


--UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        current = model.cItem
    in
        case msg of
            Editing s ->
                { model | cItem = { current | content = s } }
            
            Submit ->
                if not <| String.isEmpty current.content then
                    { model
                    | content = current :: model.content
                    , cItem = { blankItem | id = model.cId + 1 }
                    , cId = model.cId + 1
                    }
                else
                    model
            
            Remove idx ->
                { model
                | content = List.filter (\i -> i.id /= idx) model.content
                }

            Completed idx ->
                { model
                | content = List.map (\i ->
                    if i.id == idx && i.completed then
                        { i | completed = False }
                    else if i.id == idx then
                        { i | completed = True }
                    else
                        i
                    ) model.content
                }

            ClearCompleted ->
                { model 
                | content = List.filter ( not << .completed ) model.content
                }

            ChangeFilter fil ->
                { model | filter = fil }
                    -- AllFilter -> 
                    --     { model
                    --     | content = List.map (\i ->
                    --         { i | visible = True } ) model.content
                    --     , filter = AllFilter
                    --     }

                    -- CompletedFilter -> 
                    --     { model 
                    --     | content = List.map (\i ->
                    --         if i.completed then
                    --             { i | visible = True }
                    --         else
                    --             { i | visible = False }) model.content
                    --     , filter = CompletedFilter
                    --     }

                    -- UnfinishedFilter ->
                    --     { model
                    --     | content = List.map (\i ->
                    --         if i.completed then 
                    --             { i | visible = False }
                    --         else
                    --             { i | visible = True }) model.content
                    --     , filter = UnfinishedFilter
                    --     }


--UTILITY


exists : (a -> Bool) -> List a -> Bool
exists f =
    List.foldl (\x acc -> if f x then True else acc) False









