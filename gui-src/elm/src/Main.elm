module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Bulma.Columns as Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Url
import View.SearchDictionary
import View.SearchTexts
import View.TopNav


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \x -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { route : Route
    , topNav : View.TopNav.Model
    , searchTexts : View.SearchTexts.Model
    , searchDictionary : View.SearchDictionary.Model
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TopNavMsg (View.TopNav.Msg Msg)
    | SearchTextsMsg (View.SearchTexts.Msg Msg)
    | SearchDictionaryMsg (View.SearchDictionary.Msg Msg)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel, Cmd.none )


initialModel =
    { route = Route.SearchTexts
    , topNav = View.TopNav.initialModel
    , searchTexts = View.SearchTexts.initialModel
    , searchDictionary = View.SearchDictionary.initialModel
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        TopNavMsg msg_ ->
            let
                ( topNav, effects ) =
                    View.TopNav.update TopNavMsg msg_ model.topNav

                m_ =
                    { model | topNav = topNav }

                m__ =
                    case msg_ of
                        View.TopNav.NavigateTo route ->
                            { m_ | route = route }

                        _ ->
                            m_
            in
            ( m__, effects )

        SearchTextsMsg msg_ ->
            let
                ( searchTexts, effects ) =
                    View.SearchTexts.update SearchTextsMsg msg_ model.searchTexts
            in
            ( { model | searchTexts = searchTexts }, effects )

        SearchDictionaryMsg msg_ ->
            let
                ( searchDictionary, effects ) =
                    View.SearchDictionary.update SearchDictionaryMsg msg_ model.searchDictionary
            in
            ( { model | searchDictionary = searchDictionary }, effects )


view : Model -> Browser.Document Msg
view model =
    { title = "Siṃsapā Dhamma Reader"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        topNav =
            View.TopNav.view TopNavMsg model.topNav
    in
    case model.route of
        Route.Home ->
            [ topNav Nothing
            , div [] [ text "hey home" ]
            ]

        Route.ReadText ->
            [ topNav Nothing
            , div [] [ text "hey read texts" ]
            ]

        Route.SearchTexts ->
            View.SearchTexts.view SearchTextsMsg model.searchTexts topNav

        Route.SearchDictionary ->
            View.SearchDictionary.view SearchDictionaryMsg model.searchDictionary topNav
