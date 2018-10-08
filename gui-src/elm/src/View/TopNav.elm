module View.TopNav exposing (Model, Msg(..), initialModel, update, view)

import Bulma.Components exposing (..)
import Bulma.Elements as BE exposing (..)
import Bulma.Form exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route exposing (Route)

-- NOTE: transparent doesn't seem to work, using custom css instead
-- { navbarModifiers | transparent = True }

view : (Msg m -> m) -> Model -> Maybe (List (Html m)) -> Html m
view lift model buttons =
    let
        buttonsContent =
            case buttons of
                Nothing ->
                    []

                Just x ->
                    x
    in
    navbar navbarModifiers
        [ class "is-fixed-top" ]
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
                [ menuItem "Texts" "mdi-book-open" Route.SearchTexts lift model
                , menuItem "Dictionary" "mdi-notebook" Route.SearchDictionary lift model
                ]
            ]
        ]


menuItem itemText itemIcon itemRoute lift model =
    let
        isActive_ =
            isActive model itemRoute

        itemClass =
            if isActive_ then
                "is-tab is-active"

            else
                "is-tab"
    in
    navbarItemLink isActive_
        [ onClick (lift (NavigateTo itemRoute))
        , class itemClass
        ]
        [ icon Standard [] [ i [ class ("mdi " ++ itemIcon) ] [] ]
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
    }


initialModel =
    { activeLink = Just Route.SearchTexts
    , isMenuOpen = False
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
            ( { model | activeLink = Just route }, Cmd.none )

        ToggleMenu ->
            ( { model | isMenuOpen = not model.isMenuOpen }, Cmd.none )
