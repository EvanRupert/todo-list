port module Todo exposing (..)



--JACOB NOTES
--single file for ports
--Do not write logic in view function


import Html exposing (..)
import Html.Events exposing ( onInput, onClick, on, keyCode )
import Html.Attributes exposing (..)
import Json.Decode as Json


{-| Type used to represent one item in the todo list
    'content' is the main string of the todo item
    'id' the unique id number that helps identify the item
    'completed' a bool that represents if the item has been marked as completed or not
-}
type alias Item =
    { content : String
    , id : Int
    , completed : Bool
    }


{-| Model type used to represent all of the persistant data
    needed in the application
    'content' the list of items that represent the whole todo list
    'cItem' is the current item being updated
    'cId' newest id of the last item created
    'filter' is the current filter in place

-}
type alias Model =
    { content : List Item
    , cItem : Item
    , cId : Int
    , filter : Filter
    }


{-| Type to represent the current filter being used
-}
type Filter
    = AllFilter
    | CompletedFilter
    | UnfinishedFilter


{-| Type to represent the input from the page flowing back into the application
-}
type Msg 
    = Editing String
    | Submit
    | Remove Int
    | Completed Int
    | ClearCompleted
    | ChangeFilter Filter

--main beginnerProgram start location
main : Program Never Model Msg
main =
    beginnerProgram { model = blankModel, view = view, update = update }


{-| Represents the starting state of the application
-}
blankModel : Model
blankModel = 
    { content = []
    , cItem = blankItem
    , cId = 0
    , filter = AllFilter
    }


{-| Empty template for creating new items with default values
-}
blankItem : Item
blankItem =
    { content = ""
    , id = 0
    , completed = False
    }


--VIEW


{-| Function that is given the current state and produces the html to be
    displayed on the page
-}
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


{-| Sets the class of the list div as to not show the list
    if there are currently no elements in it
-}
disableIfEmpty : Model -> Attribute Msg
disableIfEmpty model =
    if String.isEmpty model.cItem.content then
        id "disabled-submit-button"
    else
        id "submit-button"


{-| Function to produce the 'Clear Finished Items' button given the current app state
    Will only show the button if there are some completed items currently in the list
-}
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


{-| Function to produce the filter button bar given the current state of the program
    Will only show the buttons if there is any item in the list
-}
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


{-| Given an item this function will produce the html to represent that item on a webpage
    This includes both the 'completed' and 'close' buttons as well as the class assignments 
    to style these elements
-}
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


{-| Event created to Submit an item to the list whenever the enter key is pressed
-}
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


{-| Main update function that takes input from the app user and the current state of the app
    and updates the app according to what input is given by the user
-}
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



--UTILITY


{-| Helper function to return true if any element in a list returns true
    for the predicate function given.
-}
exists : (a -> Bool) -> List a -> Bool
exists f =
    List.foldl (\x acc -> if f x then True else acc) False









