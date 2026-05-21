module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, h1, h2, img, input, label, p, select, span, strong, text)
import Html.Attributes exposing (class, classList, for, id, placeholder, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput)


-- MODEL

type Color
    = White
    | Black
    | Blue
    | Pink


type Print
    = Logo
    | Smile
    | Abstract
    | Plain


type alias Form =
    { name : String
    , card : String
    , exp : String
    , cvc : String
    }


type alias Model =
    { color : Color
    , print : Print
    , modalOpen : Bool
    , form : Form
    , ordered : Bool
    }


type Msg
    = SelectColor Color
    | SelectPrint Print
    | OpenModal
    | CloseModal
    | UpdateName String
    | UpdateCard String
    | UpdateExp String
    | UpdateCvc String
    | SubmitOrder
    | ResetOrder


init : Model
init =
    { color = White
    , print = Logo
    , modalOpen = False
    , form = { name = "", card = "", exp = "", cvc = "" }
    , ordered = False
    }


-- UPDATE

update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectColor color ->
            { model | color = color, ordered = False }

        SelectPrint print ->
            { model | print = print, ordered = False }

        OpenModal ->
            { model | modalOpen = True }

        CloseModal ->
            { model | modalOpen = False }

        UpdateName name ->
            { model | form = { model.form | name = name } }

        UpdateCard card ->
            { model | form = { model.form | card = card } }

        UpdateExp exp ->
            { model | form = { model.form | exp = exp } }

        UpdateCvc cvc ->
            { model | form = { model.form | cvc = cvc } }

        SubmitOrder ->
            { model
                | modalOpen = False
                , ordered = True
                , form = { name = "", card = "", exp = "", cvc = "" }
            }

        ResetOrder ->
            init


-- VIEW

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ div [ class "shell" ]
            [ div [ class "panel" ]
                [ div [ class "header" ]
                    [ div []
                        [ h1 [] [ text "Закажи чашку на Bank43" ]
                        , p [] [ text "Выбери цвет и принт, затем нажми " , strong [] [ text "Заказать" ] , text ". Это портфолио-демо, платежные данные — фиктивные." ]
                        ]
                    ]
                ]

            , div [ class "options" ]
                [ sectionView "Цвет" (colorOptions model.color)
                , sectionView "Принт" (printOptions model.print)
                ]
            ]

        , div [ class "preview-panel panel" ]
            [ div [ class "cup-preview" ]
                [ div
                    [ class "cup-art"
                    , style "background" (cupBackground model.color)
                    ]
                    [ span [ class "cup-text" ] [ text (printLabel model.print) ]
                    ]
                ]
            , div [ class "details" ]
                [ p [] [ strong [] [ text "Цвет:" ] , text (" " ++ colorLabel model.color) ]
                , p [] [ strong [] [ text "Принт:" ] , text (" " ++ printLabel model.print) ]
                , p [ class "description" ] [ text "Идеально подходящая чашка для кофе, чая и хорошего настроения. В этом макете можно попробовать любой набор данных и увидеть успешный заказ." ]
                ]
            , button [ class "cta-button", onClick OpenModal ] [ text "Заказать чашку" ]
            , if model.ordered then
                div [ class "success-banner" ]
                    [ p [] [ strong [] [ text "Готово!" ] , text " Заказ успешно оформлен. Спасибо за покупку." ] ]
              else
                text ""
            ]
        ]
    , if model.modalOpen then
        modalView model
      else
        text ""
    ]


sectionView : String -> List (Html Msg) -> Html Msg
sectionView title items =
    div [ class "section" ]
        ([ label [] [ text title ]] ++ items)


colorOptions : Color -> List (Html Msg)
colorOptions selectedColor =
    List.map
        (	heme ->
            button
                [ classList
                    [ ( "active", theme == selectedColor ) ]
                , onClick (SelectColor theme)
                ]
                [ span [] [ text (colorLabel theme) ] ]
        )
        [ White, Black, Blue, Pink ]


printOptions : Print -> List (Html Msg)
printOptions selectedPrint =
    List.map
        (	heme ->
            button
                [ classList
                    [ ( "active", theme == selectedPrint ) ]
                , onClick (SelectPrint theme)
                ]
                [ span [] [ text (printLabel theme) ] ]
        )
        [ Logo, Smile, Abstract, Plain ]


modalView : Model -> Html Msg
modalView model =
    div [ class "modal-layer" ]
        [ div [ class "modal-card" ]
            [ h2 [] [ text "Оформление заказа" ]
            , p [] [ text "Введите банковские данные. Любой текст принимает форму, потом появляется сообщение об успешном заказе." ]
            , form []
                [ div [ class "form-group" ]
                    [ label [ for "name" ] [ text "Имя держателя" ]
                    , input [ id "name", type_ "text", placeholder "Иван Иванов", value model.form.name, onInput UpdateName ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "card" ] [ text "Номер карты" ]
                    , input [ id "card", type_ "text", placeholder "1234 5678 9012 3456", value model.form.card, onInput UpdateCard ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "exp" ] [ text "Срок действия" ]
                    , input [ id "exp", type_ "text", placeholder "08/27", value model.form.exp, onInput UpdateExp ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "cvc" ] [ text "CVC/CVV" ]
                    , input [ id "cvc", type_ "text", placeholder "123", value model.form.cvc, onInput UpdateCvc ] []
                    ]
                , div [ class "form-actions" ]
                    [ button [ class "cta-button", onClick SubmitOrder ] [ text "Оплатить и заказать" ]
                    , button [ class "secondary-button", onClick CloseModal ] [ text "Отмена" ]
                    ]
                ]
            ]
        ]


colorLabel : Color -> String
colorLabel color =
    case color of
        White ->
            "Белый"

        Black ->
            "Черный"

        Blue ->
            "Синий"

        Pink ->
            "Розовый"


printLabel : Print -> String
printLabel printType =
    case printType of
        Logo ->
            "Логотип"

        Smile ->
            "Улыбка"

        Abstract ->
            "Абстракция"

        Plain ->
            "Минимал"


cupBackground : Color -> String
cupBackground color =
    case color of
        White ->
            "linear-gradient(180deg, #ffffff 0%, #f3f7ff 100%)"

        Black ->
            "linear-gradient(180deg, #111827 0%, #1f2937 100%)"

        Blue ->
            "linear-gradient(180deg, #dbeafe 0%, #93c5fd 100%)"

        Pink ->
            "linear-gradient(180deg, #fbcfe8 0%, #fda4af 100%)"
