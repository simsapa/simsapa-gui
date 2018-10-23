module View.SearchDictionary exposing (Model, Msg, initialModel, update, view)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements as BE exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout as BL exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
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


mdRawHtml =
    Just (MDConf.Options False MDConf.ParseUnsafe)


view : (Msg m -> m) -> Model -> (Maybe (List (Html m)) -> Maybe (List (Html m)) -> Html m) -> List (Html m)
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
                [ class "page-wrap with-fixed-search is-hidden-desktop" ]
                [ column cM
                    [ class "page-content-outer-controls" ]
                    [ div [ class "page-content-inner-controls" ]
                        [ topNav (Just buttons) (Just [ searchInput lift model ])
                        , allPaliLetterButtons lift model
                        , viewLookupResults lift model
                        ]
                    ]
                ]
            , columns myColumnsModifiers
                [ class "page-wrap with-fixed-search is-hidden-touch" ]
                [ column cM
                    [ class "page-content-outer-controls" ]
                    [ div [ class "page-content-inner-controls" ]
                        [ topNav (Just buttons) (Just [ searchInput lift model ])
                        , BL.section Spaced
                            []
                            [ allPaliLetterButtons lift model
                            , viewLookupResults lift model
                            ]
                        ]
                    ]
                , column cM
                    [ class "page-content-outer-controls" ]
                    [ div
                        [ class "page-content-inner-controls" ]
                        [ div [ class "dictionary-results" ]
                            (List.map (\x -> viewSelectedResultRow x lift model) model.selectedWordsList)
                        ]
                    ]
                ]
            ]

        Reading ->
            [ columns myColumnsModifiers
                [ class "page-wrap" ]
                [ column cM
                    [ class "page-content-outer-controls" ]
                    [ div [ class "page-content-inner-controls" ]
                        [ topNav (Just buttons) Nothing
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
    div []
        [ field []
              [ controlLabel [] []
              , controlText myControlInputModifiers
                  myControlAttrs
                      myInputAttrs
                      []
              ]
        , searchOptions lift model.queryOptions
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
            viewQueryData lift res model


viewQueryData : (Msg m -> m) -> DictSearches -> Model -> Html m
viewQueryData lift data model =
    let
        hits =
            "Results: " ++ String.fromInt data.total_count

        all_words =
            data.contains_exactly
                |> List.append data.fulltext
                |> List.append data.word_contains
                |> List.append data.word_starts_with

        p =
            model.paginationInfo

        t =
            List.drop ((p.currentPage - 1) * p.pageLength) all_words

        show_words =
            List.take p.pageLength t
    in
    div []
        [ div [ style "padding-bottom" "2em" ] [ text hits ]
        , wordPages lift model.paginationInfo
        , div [] (List.map (\x -> viewLookupResultRow lift x model) show_words)
        ]


wordPages : (Msg m -> m) -> PaginationInfo -> Html m
wordPages lift paginationInfo =
    let
        pInf =
            paginationInfo

        cP =
            pInf.currentPage

        tP =
            pInf.totalPages

        pageList =
            if pInf.totalPages <= 10 then
                List.map
                    (\x ->
                        paginationLink (x == cP)
                            [ onClick (lift (SetPagination x)) ]
                            [ text (String.fromInt x) ]
                    )
                    (List.range 1 tP)

            else if cP <= 3 then
                [ paginationLink (1 == cP) [ onClick (lift (SetPagination 1)) ] [ text "1" ]
                , paginationLink (2 == cP) [ onClick (lift (SetPagination 2)) ] [ text "2" ]
                , paginationLink (3 == cP) [ onClick (lift (SetPagination 3)) ] [ text "3" ]
                , paginationEllipsis [] [ text "..." ]
                , paginationLink False
                    [ onClick (lift (SetPagination tP)) ]
                    [ text (String.fromInt tP) ]
                ]

            else if cP > 3 && cP < tP - 2 then
                [ paginationLink False
                    [ onClick (lift (SetPagination 1)) ]
                    [ text "1" ]
                , paginationEllipsis [] [ text "..." ]
                , paginationLink False
                    [ onClick (lift (SetPagination (cP - 1))) ]
                    [ text (String.fromInt (cP - 1)) ]
                , paginationLink True
                    []
                    [ text (String.fromInt cP) ]
                , paginationLink False
                    [ onClick (lift (SetPagination (cP + 1))) ]
                    [ text (String.fromInt (cP + 1)) ]
                , paginationEllipsis [] [ text "..." ]
                , paginationLink False
                    [ onClick (lift (SetPagination tP)) ]
                    [ text (String.fromInt tP) ]
                ]

            else
                [ paginationLink False
                    [ onClick (lift (SetPagination 1)) ]
                    [ text "1" ]
                , paginationEllipsis [] [ text "..." ]
                , paginationLink (tP - 2 == cP)
                    [ onClick (lift (SetPagination (tP - 2))) ]
                    [ text (String.fromInt (tP - 2)) ]
                , paginationLink (tP - 1 == cP)
                    [ onClick (lift (SetPagination (tP - 1))) ]
                    [ text (String.fromInt (tP - 1)) ]
                , paginationLink (tP == cP)
                    [ onClick (lift (SetPagination tP)) ]
                    [ text (String.fromInt tP) ]
                ]
    in
    pagination Left
        []
        [ paginationPrev [ onClick (lift PaginationPrev) ] [ text "Previous" ]
        , paginationNext [ onClick (lift PaginationNext) ] [ text "Next" ]
        , paginationList [] pageList
        ]



-- TODO: for word matches, show the summary
-- TODO: for fulltext matches, show the definition_plain snippet


viewLookupResultRow : (Msg m -> m) -> DictWord -> Model -> Html m
viewLookupResultRow lift dictWord model =
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

        onClickFn =
            if isSelected then
                lift (RemoveFromSelectedResults dictWord)

            else
                lift (AddToSelectedResults dictWord)
    in
    div
        [ class "hover-gray"
        , style "padding" "0.5em"
        , onClick onClickFn
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


searchOptions : (Msg m -> m) -> QueryOptions -> Html m
searchOptions lift opts =
    fields Left
        []
        [ controlCheckBox False
            []
            []
            [ onCheck (\x -> lift (SetOptWordStartsWith x))
            , checked opts.wordStartsWith
            ]
            [ text "word starts with" ]
        , controlCheckBox False
            []
            []
            [ onCheck (\x -> lift (SetOptWordContains x))
            , checked opts.wordContains
            ]
            [ text "word contains" ]
        , controlRadio []
            [ controlRadioButton False
                opts.fulltext
                "fulltext"
                []
                [ onCheck (\x -> lift (SetOptFulltext x)) ]
                [ text "fulltext matching" ]
            , controlRadioButton False
                opts.containsExactly
                "exactly"
                []
                [ onCheck (\x -> lift (SetOptContainsExactly x)) ]
                [ text "exactly matching" ]
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
    , lookupResults : WebData DictSearches
    , selectedWordsList : List DictWord
    , paginationInfo : PaginationInfo
    , subRoute : SubRoute
    , queryOptions : QueryOptions
    }


initialModel =
    { lookupQuery = ""
    , lookupResults = RemoteData.NotAsked
    , selectedWordsList = []
    , paginationInfo = initialPaginationInfo
    , subRoute = Searching
    , queryOptions = initialQueryOptions
    }


initialPaginationInfo : PaginationInfo
initialPaginationInfo =
    { pageLength = 30
    , currentPage = 1
    , totalPages = 1
    }


initialQueryOptions : QueryOptions
initialQueryOptions =
    { wordStartsWith = True
    , wordContains = True
    , fulltext = True
    , containsExactly = False
    }


type alias QueryOptions =
    { wordStartsWith : Bool
    , wordContains : Bool
    , fulltext : Bool
    , containsExactly : Bool
    }


type alias PaginationInfo =
    { pageLength : Int
    , currentPage : Int
    , totalPages : Int
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


type alias DictSearches =
    { word_starts_with : List DictWord
    , word_contains : List DictWord
    , contains_exactly : List DictWord
    , fulltext : List DictWord
    , total_count : Int
    }


type SubRoute
    = Searching
    | Reading


type Msg m
    = NoOp
    | SetDictLookupQuery String
    | DictLookupDataReceived (WebData DictSearches)
    | AddToSelectedResults DictWord
    | RemoveFromSelectedResults DictWord
    | AddToDictInput String
    | SetSubRoute SubRoute
    | PaginationPrev
    | PaginationNext
    | SetPagination Int
    | SetOptWordStartsWith Bool
    | SetOptWordContains Bool
    | SetOptFulltext Bool
    | SetOptContainsExactly Bool


lookupCmd lift query opts =
    if String.length query > 2 then
        fetchDictWord lift query opts

    else
        Cmd.none


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDictLookupQuery query ->
            ( { model | lookupQuery = query }, lookupCmd lift query model.queryOptions )

        DictLookupDataReceived data ->
            case data of
                RemoteData.NotAsked ->
                    ( { model | lookupResults = data }, Cmd.none )

                RemoteData.Loading ->
                    ( { model | lookupResults = data }, Cmd.none )

                RemoteData.Failure _ ->
                    ( { model | lookupResults = data }, Cmd.none )

                RemoteData.Success res ->
                    let
                        p =
                            model.paginationInfo

                        count =
                            res.total_count

                        t =
                            ceiling (toFloat count / toFloat p.pageLength)

                        curr =
                            if p.currentPage > t then
                                t

                            else
                                p.currentPage

                        p_ =
                            { p | totalPages = t, currentPage = curr }
                    in
                    ( { model | lookupResults = data, paginationInfo = p_ }, Cmd.none )

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
                q =
                    model.lookupQuery ++ letter
            in
            ( { model | lookupQuery = q }, fetchDictWord lift q model.queryOptions )

        SetSubRoute x ->
            ( { model | subRoute = x }, Cmd.none )

        PaginationPrev ->
            let
                p =
                    model.paginationInfo

                curr =
                    if p.currentPage > 1 then
                        p.currentPage - 1

                    else
                        p.currentPage

                p_ =
                    { p | currentPage = curr }
            in
            ( { model | paginationInfo = p_ }, Cmd.none )

        PaginationNext ->
            let
                p =
                    model.paginationInfo

                curr =
                    if p.currentPage < p.totalPages then
                        p.currentPage + 1

                    else
                        p.currentPage

                p_ =
                    { p | currentPage = curr }
            in
            ( { model | paginationInfo = p_ }, Cmd.none )

        SetPagination x ->
            let
                p =
                    model.paginationInfo

                p_ =
                    { p | currentPage = x }
            in
            ( { model | paginationInfo = p_ }, Cmd.none )

        SetOptWordStartsWith x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | wordStartsWith = x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )

        SetOptWordContains x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | wordContains = x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )

        SetOptFulltext x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | fulltext = x, containsExactly = not x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )

        SetOptContainsExactly x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | fulltext = not x, containsExactly = x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )


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


dictSearchesDecoder : Decoder DictSearches
dictSearchesDecoder =
    Decode.succeed DictSearches
        |> required "word_starts_with" (list dictWordDecoder)
        |> required "word_contains" (list dictWordDecoder)
        |> required "contains_exactly" (list dictWordDecoder)
        |> required "fulltext" (list dictWordDecoder)
        |> required "total_count" int


optsToQuery opts =
    let
        f =
            \x ->
                if Tuple.second x then
                    UB.string (Tuple.first x) "true"

                else
                    UB.string (Tuple.first x) "false"
    in
    List.map f
        [ ( "word_starts_with", opts.wordStartsWith )
        , ( "word_contains", opts.wordContains )
        , ( "fulltext", opts.fulltext )
        , ( "contains_exactly", opts.containsExactly )
        ]


fetchDictWord : (Msg m -> m) -> String -> QueryOptions -> Cmd m
fetchDictWord lift query opts =
    dictSearchesDecoder
        |> Http.get
            (UB.absolute
                [ "search", "dict_words" ]
                (List.append
                    [ UB.string "query" query ]
                    (optsToQuery opts)
                )
            )
        |> RemoteData.sendRequest
        |> Cmd.map (\x -> lift (DictLookupDataReceived x))
