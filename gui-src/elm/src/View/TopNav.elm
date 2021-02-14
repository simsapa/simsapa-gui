module View.TopNav exposing (Model, Msg(..), initialModel, update, view)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements as BE exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout as BL exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography as BT exposing (textColor)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route exposing (Route)


cM =
    columnModifiers


cSM =
    columnsModifiers


view : (Msg m -> m) -> Model -> Maybe (List (Html m)) -> Maybe (List (Html m)) -> Html m
view lift model buttons search =
    let
        buttonsContent =
            case buttons of
                Nothing ->
                    []

                Just x ->
                    x

        searchContent =
            if model.isMenuOpen then
                []

            else
                case search of
                    Nothing ->
                        []

                    Just x ->
                        x
    in
    fixedNavbar Top
        navbarModifiers
        []
        [ navbarBrand []
            (navbarBurger model.isMenuOpen
                [ onClick (lift ToggleMenu)
                , style "margin-left" "0"
                ]
                [ span [] []
                , span [] []
                , span [] []
                ]
            )
            [ navbarItem False
                []
                [ img [ src "/assets/icons/logo-letter-w96.png" ] [] ]
            , navbarItem False
                [ style "margin" "auto" ]
                buttonsContent
            ]
        , navbarMenu model.isMenuOpen
            []
            [ navbarStart []
                [ menuItem "Texts" "mdi-book-open" Route.SearchTexts False lift model
                , menuItem "Dictionary" "mdi-notebook" Route.SearchDictionary False lift model
                ]
            , navbarEnd []
                [ menuItem "Settings" "mdi-settings" Route.Settings model.settingsHaveMessage lift model
                , div [ style "width" "2em" ] []
                ]
            ]
        , navbarItem False
            [ class "navbar-fixed-search" ]
            [ columns cSM
                []
                [ column cM
                    [ class "is-half" ]
                    searchContent
                ]
            ]
        ]


menuItem itemText itemIcon itemRoute haveMessage lift model =
    let
        isActive_ =
            isActive model itemRoute

        itemClass =
            if isActive_ then
                "is-tab is-active"

            else
                "is-tab"

        messageIcon =
            if haveMessage then
                icon Standard [ textColor BT.Info ] [ i [ class ("mdi " ++ "mdi-message-alert") ] [] ]

            else
                span [] []
    in
    navbarItemLink isActive_
        [ onClick (lift (NavigateTo itemRoute))
        , class itemClass
        ]
        [ icon Standard [] [ i [ class ("mdi " ++ itemIcon) ] [] ]
        , messageIcon
        , span [] [ text itemText ]
        ]


isActive : Model -> Route -> Bool
isActive model route =
    case model.activeLink of
        Just r ->
            r == route

        Nothing ->
            False


type alias Model =
    { activeLink : Maybe Route
    , isMenuOpen : Bool
    , settingsHaveMessage : Bool
    }


initialModel =
    { activeLink = Just Route.SearchTexts
    , isMenuOpen = False
    , settingsHaveMessage = False
    }


type Msg m
    = NoOp
    | NavigateTo Route
    | ToggleMenu


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavigateTo route ->
            ( { model | activeLink = Just route, isMenuOpen = False }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )
