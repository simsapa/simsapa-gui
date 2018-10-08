module View.SearchDictionary exposing (Model, Msg, initialModel, update, view)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements as BE exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout as BL exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Markdown
import Markdown.Config as MDConf
import RemoteData exposing (WebData)
import Url.Builder as UB exposing (absolute)


cM =
    columnModifiers


cSM =
    columnsModifiers


view : (Msg m -> m) -> Model -> (Maybe (List (Html m)) -> Html m) -> List (Html m)
view lift model topNav =
    let
        bM route =
            if model.subRoute == route then
                { buttonModifiers | color = Primary }

            else
                buttonModifiers

        buttons =
            [ BE.button (bM Searching)
                [ onClick (lift (SetSubRoute Searching)) ]
                [ icon Standard [] [ i [ class "mdi mdi-magnify" ] [] ] ]
            , BE.button (bM Reading)
                [ onClick (lift (SetSubRoute Reading)) ]
                [ icon Standard [] [ i [ class "mdi mdi-book-open-variant" ] [] ] ]
            ]
    in
    case model.subRoute of
        Searching ->
            [ columns myColumnsModifiers
                [ class "page-wrap-with-scroll is-hidden-desktop" ]
                [ column cM
                    [ class "page-content-outer-controls-with-scroll" ]
                    [ div [ class "page-content-inner-controls-with-scroll" ]
                        [ topNav (Just buttons)
                        , searchInput lift model
                        , allPaliLetterButtons lift model
                        , viewLookupResults lift model
                        ]
                    ]
                ]
            , columns myColumnsModifiers
                [ class "page-wrap-with-scroll is-hidden-touch" ]
                [ column cM
                    [ class "page-content-outer-controls-with-scroll" ]
                    [ div [ class "page-content-inner-controls-with-scroll" ]
                        [ topNav (Just buttons)
                        , BL.section Spaced
                            []
                            [ searchInput lift model
                            , allPaliLetterButtons lift model
                            , viewLookupResults lift model
                            ]
                        ]
                    ]
                , column cM
                    [ class "page-content-outer-controls-with-scroll" ]
                    [ div
                        [ class "page-content-inner-controls-with-scroll" ]
                        [ div [ class "dictionary-results" ]
                            (List.map (\x -> viewSelectedResultRow x lift model) model.selectedWordsList)
                        ]
                    ]
                ]
            ]

        Reading ->
            [ columns myColumnsModifiers
                [ class "page-wrap-with-scroll" ]
                [ column cM
                    [ class "page-content-outer-controls-with-scroll" ]
                    [ div [ class "page-content-inner-controls-with-scroll" ]
                        [ topNav (Just buttons)
                        , div [ class "dictionary-results" ]
                            (List.map (\x -> viewSelectedResultRow x lift model) model.selectedWordsList)
                        ]
                    ]
                ]
            ]


searchInput : (Msg m -> m) -> Model -> Html m
searchInput lift model =
    let
        searchIcon =
            ( Medium, [], icon Standard [] [ i [ class "mdi mdi-magnify", style "color" "black" ] [] ] )

        myControlInputModifiers : ControlInputModifiers m
        myControlInputModifiers =
            { controlInputModifiers | size = Medium, iconLeft = Just searchIcon }

        myControlAttrs : List (Attribute m)
        myControlAttrs =
            []

        myInputAttrs : List (Attribute m)
        myInputAttrs =
            [ placeholder "Search in dictionaries, e.g.: nirodha, cessation ..."
            , autofocus True
            , value model.lookupQuery
            , onInput (\x -> lift (SetDictLookupQuery x))
            ]
    in
    field []
        [ controlLabel [] []
        , controlText myControlInputModifiers
            myControlAttrs
            myInputAttrs
            []
        ]


allPaliLetterButtons lift model =
    div []
        (List.map (\x -> paliLetterButton x lift model) (String.words "ā ī ū ṃ ṅ ñ ṭ ḍ ṇ ḷ"))



-- FIXME paliLetterButton : String -> (Msg m -> m) -> Model -> Html m


paliLetterButton letter lift model =
    BE.button { buttonModifiers | rounded = True }
        [ onClick (lift (AddToDictInput letter))
        , style "font-size" "1.2rem"
        , style "text-transform" "lowercase"
        ]
        [ text letter ]


viewLookupResults : (Msg m -> m) -> Model -> Html m
viewLookupResults lift model =
    case model.lookupResults of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ text "loading" ]

        RemoteData.Failure _ ->
            div [] [ text "Error: query failure" ]

        RemoteData.Success res ->
            div [] (List.map (\x -> viewLookupResultRow x lift model) res)



-- TODO: for word matches, show the summary
-- TODO: for fulltext matches, show the definition_plain snippet


viewLookupResultRow : DictWord -> (Msg m -> m) -> Model -> Html m
viewLookupResultRow dictWord lift model =
    let
        s =
            if String.length dictWord.summary > 0 then
                dictWord.summary

            else
                dictWord.definition_plain

        words =
            String.words s

        snippet =
            if List.length words > 30 then
                String.join " " (List.take 30 words) ++ "..."

            else
                String.join " " words

        isSelected =
            let
                wordList =
                    List.filter
                        (\x -> x.word == dictWord.word && x.entry_source == dictWord.entry_source)
                        model.selectedWordsList
            in
            List.length wordList > 0

        pinClass =
            if isSelected then
                "mdi-pin"

            else
                "mdi-pin-outline"
    in
    div
        [ class "hover-gray"
        , style "padding" "0.5em"
        , onClick (lift (AddToSelectedResults dictWord))
        ]
        [ columns { cSM | display = MobileAndBeyond }
            []
            [ column cM
                []
                [ span [ style "font-weight" "bold" ]
                    [ text dictWord.word ]
                , span [ style "padding" "0 1em" ] [ text "∙" ]
                , span []
                    [ text dictWord.entry_source ]
                ]
            , column cM
                [ class "is-one-fifth"
                , style "text-align" "right"
                ]
                [ icon Standard [] [ i [ class ("mdi " ++ pinClass) ] [] ] ]
            ]
        , columns cSM
            []
            [ column cM
                [ class "is-full" ]
                [ div [] <| Markdown.toHtml Nothing snippet ]
            ]
        ]



-- FIXME viewSelectedResultRow : DictWord -> (Msg m -> m) -> Model -> Html m


viewSelectedResultRow dictWord lift model =
    card []
        [ cardHeader []
            [ cardTitle [] [ text dictWord.word ]
            , cardIcon []
                [ icon Standard
                    [ onClick (lift (RemoveFromSelectedResults dictWord)) ]
                    [ i [ class "mdi mdi-close" ] [] ]
                ]
            ]
        , cardContent []
            [ div [] [ text dictWord.grammar ]
            , div [] [ text ("Source: " ++ dictWord.entry_source) ]
            , div [] [ text dictWord.summary ]
            , div [] <|
                Markdown.toHtml mdRawHtml dictWord.definition_html
            ]
        ]


myColumnsModifiers : ColumnsModifiers
myColumnsModifiers =
    { multiline = False
    , gap = Gap1
    , display = MobileAndBeyond
    , centered = True
    }


type alias Model =
    { lookupQuery : String
    , lookupResults : WebData (List DictWord)
    , selectedWordsList : List DictWord
    , subRoute : SubRoute
    }


initialModel =
    { lookupQuery = ""
    , lookupResults = RemoteData.NotAsked
    , selectedWordsList = []
    , subRoute = Searching
    }


type alias DictWord =
    { id : Int
    , word : String
    , definition_plain : String
    , definition_html : String
    , summary : String
    , grammar : String
    , entry_source : String
    , from_lang : String
    , to_lang : String
    }


type SubRoute
    = Searching
    | Reading


type Msg m
    = NoOp
    | SetDictLookupQuery String
    | DictLookupDataReceived (WebData (List DictWord))
    | AddToSelectedResults DictWord
    | RemoveFromSelectedResults DictWord
    | AddToDictInput String
    | SetSubRoute SubRoute


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDictLookupQuery query ->
            let
                lookupCmd =
                    if String.length query > 2 then
                        fetchDictWord lift query

                    else
                        Cmd.none
            in
            ( { model | lookupQuery = query }, lookupCmd )

        DictLookupDataReceived data ->
            ( { model | lookupResults = data }, Cmd.none )

        AddToSelectedResults dictWord ->
            let
                m =
                    let
                        wordList =
                            List.filter (\x -> not (x.word == dictWord.word && x.entry_source == dictWord.entry_source)) model.selectedWordsList
                    in
                    { model | selectedWordsList = dictWord :: wordList }
            in
            ( m, Cmd.none )

        RemoveFromSelectedResults dictWord ->
            let
                m =
                    let
                        wordList =
                            List.filter (\x -> not (x.word == dictWord.word)) model.selectedWordsList
                    in
                    { model | selectedWordsList = wordList }
            in
            ( m, Cmd.none )

        -- focus back on the input field
        -- FIXME Task.attempt (\_ -> NoOp) <| Dom.focus "query-input"

        AddToDictInput letter ->
            let
                q = model.lookupQuery ++ letter
            in
            ( { model | lookupQuery = q }, fetchDictWord lift q )

        SetSubRoute x ->
            ( { model | subRoute = x }, Cmd.none )


dictWordDecoder : Decoder DictWord
dictWordDecoder =
    Decode.succeed DictWord
        |> required "id" int
        |> required "word" string
        |> required "definition_plain" string
        |> required "definition_html" string
        |> required "summary" string
        |> required "grammar" string
        |> required "entry_source" string
        |> required "from_lang" string
        |> required "to_lang" string


fetchDictWord : (Msg m -> m) -> String -> Cmd m
fetchDictWord lift query =
    list dictWordDecoder
        |> Http.get (UB.absolute [ "search", "dict_words" ] [ UB.string "query" query ])
        |> RemoteData.sendRequest
        |> Cmd.map (\x -> lift (DictLookupDataReceived x))


mdRawHtml =
    Just (MDConf.Options False MDConf.ParseUnsafe)
