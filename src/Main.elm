module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Hotkeys exposing (onEnter)
import Html exposing (Html)
import Material.Button as MButton
import Random exposing (..)
import UUID exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { todos : List String
    , todoInput : String
    }


init : Model
init =
    Model [] ""



-- update


type Msg
    = AddTodo
    | DeleteTodo String
    | InputChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            { model | todos = model.todos ++ [ model.todoInput ], todoInput = "" }

        DeleteTodo id ->
            { model | todos = List.filter (\x -> x /= id) model.todos }

        InputChange newContent ->
            { model | todoInput = newContent }


view : Model -> Html Msg



-- view model =
--     div []
--         [ div [] (List.map (\x -> div [] [ Html.text x, button [ onClick (DeleteTodo x) ] [ Html.text "-" ] ]) model.todos)
--         , input [ placeholder "Text to reverse", value model.todoInput, onInput InputChange ] []
--         , button [ onClick AddTodo ] [ Html.text "Add Todo" ]
--         ]


view model =
    elementView model


elementView : Model -> Html Msg
elementView model =
    Element.layout [ padding 20 ] (container model)


container : Model -> Element Msg
container model =
    column [ centerX, centerY, spacing 5 ]
        [ header
        , todoInput model
        , todos model
        ]


header : Element msg
header =
    column [ centerX ]
        [ paragraph []
            [ text "Simple Todo List" ]
        ]


todos : Model -> Element Msg
todos model =
    column [ Border.color color.lightGrey, Border.width 1 ]
        (List.map
            (\x ->
                row []
                    [ paragraph [ padding 10 ] [ text x ]
                    , Input.button buttonAttrs { onPress = Just (DeleteTodo x), label = text " - " }
                    ]
            )
            model.todos
        )


todoInput : Model -> Element Msg
todoInput model =
    row [ padding 25, spacing 5, Border.width 1, Border.rounded 10, Border.color color.lightBlue ]
        [ Input.text [ padding 5, Element.htmlAttribute <| onEnter AddTodo ]
            { onChange = InputChange
            , text = model.todoInput
            , placeholder = Just <| Input.placeholder [] <| text "Task name"
            , label = Input.labelHidden "hidden"
            }
        , MButton.outlined
            (MButton.config |> MButton.setOnClick AddTodo)
            " + "
            |> Element.html
            |> el [ padding 5 ]

        -- , Input.button [ padding 5, Border.width 1, Border.color color.blue, Background.color color.lightBlue ]
        --     { onPress = Just AddTodo
        --     , label = text " + "
        --     }
        ]


buttonAttrs : List (Attribute msg)
buttonAttrs =
    [ padding 5, Border.width 1, Border.color color.blue, Background.color color.lightBlue ]


color : { blue : Color, darkCharcoal : Color, lightBlue : Color, lightGrey : Color, white : Color }
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }


uuidV4 : String
uuidV4 =
    Random.step UUID.generator (Random.initialSeed 12345)
        |> Tuple.first
        |> UUID.toRepresentation Urn
