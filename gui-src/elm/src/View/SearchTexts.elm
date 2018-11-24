module View.SearchTexts exposing (Model, Msg, initialModel, update, view)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements as BE exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout as BL exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onSubmit)
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
            , BE.button (bM Selecting)
                [ onClick (lift (SetSubRoute Selecting)) ]
                [ icon Standard [] [ i [ class "mdi mdi-pin" ] [] ] ]
            , BE.button (bM Reading)
                [ onClick (lift (SetSubRoute Reading)) ]
                [ icon Standard [] [ i [ class "mdi mdi-book-open-variant" ] [] ] ]
            ]

        selectedTextColumn =
            case model.selectedText of
                Nothing ->
                    column cM [] []

                Just _ ->
                    column cM
                        [ class "page-content-outer-reading" ]
                        [ div [ class "page-content-inner-reading" ]
                            [ readingHero div "split-view" lift model ]
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
                        , viewLookupResults lift model
                        ]
                    ]
                ]
            , columns myColumnsModifiers
                [ class "page-wrap with-fixed-search is-hidden-touch" ]
                [ column cM
                    [ class "page-content-outer-controls is-half" ]
                    [ div [ class "page-content-inner-controls" ]
                        [ topNav (Just buttons) (Just [ searchInput lift model ])
                        , BL.section Spaced
                            []
                            [ viewLookupResults lift model ]
                        ]
                    ]
                , selectedTextColumn
                ]
            ]

        Reading ->
            [ columns myColumnsModifiers
                [ class "page-wrap" ]
                [ column cM
                    [ class "page-content-outer-reading" ]
                    [ div [ class "page-content-inner-reading" ]
                        [ topNav (Just buttons) Nothing
                        , readingHero container "" lift model
                        ]
                    ]
                ]
            ]

        Selecting ->
            [ columns myColumnsModifiers
                [ class "page-wrap" ]
                [ column cM
                    [ class "page-content-outer-controls" ]
                    [ div [ class "page-content-inner-controls" ]
                        [ topNav (Just buttons) Nothing
                        , div [ class "selected-text-list-tabs" ]
                            [ selectedTextListTabs lift model ]
                        ]
                    ]
                ]
            ]


readingHero containerItem extraClasses lift model =
    let
        contentHeading =
            [ viewSelectedTextHeader lift model ]

        contentBody =
            [ viewSelectedTextBody lift model ]
    in
    div []
        [ containerItem
            [ class (String.join " " [ "reading", extraClasses ]) ]
            [ hero { bold = False, size = Medium, color = Default }
                []
                [ heroBody [] contentHeading ]
            , div [ class "content" ] contentBody
            ]
        ]


selectedTextListTabs : (Msg m -> m) -> Model -> Html m
selectedTextListTabs lift model =
    tabs { style = Toggle, alignment = Left, size = Standard }
        []
        []
        (List.map (\x -> textListTab x lift model) model.selectedTextList)


textListTab : SelectedText -> (Msg m -> m) -> Model -> Html m
textListTab t lift model =
    let
        isCurrentReading =
            case model.selectedText of
                Just sel_t ->
                    getUid t == getUid sel_t

                Nothing ->
                    False
    in
    case t of
        SelectedRootText t_ ->
            tab isCurrentReading
                []
                []
                [ span [ onClick (lift (SetSelectedReadText t)) ]
                    [ span [ class "tab-acronym" ] [ text t_.acronym ]
                    , span [ class "tab-title" ] [ text t_.title ]
                    ]
                , icon Standard
                    [ onClick (lift (RemoveFromSelectedTexts (SelectedRootText t_))) ]
                    [ i [ class "mdi mdi-close" ] [] ]
                ]

        SelectedTranslatedText t_ ->
            tab isCurrentReading
                []
                []
                [ span [ onClick (lift (SetSelectedReadText t)) ]
                    [ span [ class "tab-acronym" ] [ text t_.acronym ]
                    , span [ class "tab-title" ] [ text t_.title ]
                    ]
                , icon Standard
                    [ onClick (lift (RemoveFromSelectedTexts (SelectedTranslatedText t_))) ]
                    [ i [ class "mdi mdi-close" ] [] ]
                ]


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


viewQueryData : (Msg m -> m) -> TextQueryData -> Model -> Html m
viewQueryData lift data model =
    let
        hits =
            "Results: "
                ++ String.fromInt data.root_texts.total_count
                ++ " in Pali texts, "
                ++ String.fromInt data.translated_texts.total_count
                ++ " in translated texts"

        all_root_texts =
            data.root_texts.contains_exactly
                |> List.append data.root_texts.fulltext
                |> List.append data.root_texts.title_contains
                |> List.append data.root_texts.acronym_contains
                |> List.map (\x -> ItemRootText x)

        all_translated_texts =
            data.translated_texts.contains_exactly
                |> List.append data.translated_texts.fulltext
                |> List.append data.translated_texts.title_contains
                |> List.append data.translated_texts.acronym_contains
                |> List.map (\x -> ItemTranslatedText x)

        all_texts =
            List.append all_root_texts all_translated_texts

        p =
            model.paginationInfo

        t =
            List.drop ((p.currentPage - 1) * p.pageLength) all_texts

        show_texts =
            List.take p.pageLength t
    in
    div [ class "search-results" ]
        [ div [ style "padding-bottom" "2em" ] [ text hits ]
        , textPages lift model.paginationInfo
        , div [] (List.map (\x -> viewTextRow lift x model) show_texts)
        ]


textPages : (Msg m -> m) -> PaginationInfo -> Html m
textPages lift paginationInfo =
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


viewTextRow : (Msg m -> m) -> TextItem -> Model -> Html m
viewTextRow lift text_item model =
    case text_item of
        ItemRootText t ->
            viewRootTextRow lift t model

        ItemTranslatedText t ->
            viewTranslatedTextRow lift t model


viewRootTextRow : (Msg m -> m) -> RootText -> Model -> Html m
viewRootTextRow lift root_text model =
    let
        words =
            String.words root_text.content_plain

        snippet =
            if List.length words > 30 then
                String.join " " (List.take 30 words) ++ "..."

            else
                String.join " " words

        isSelected =
            let
                t_uid =
                    root_text.uid

                textList =
                    List.map (\x -> getUid x) model.selectedTextList
            in
            List.member t_uid textList

        pinClass =
            if isSelected then
                "mdi-pin"

            else
                "mdi-pin-outline"

        onClickFn =
            if isSelected then
                lift (RemoveFromSelectedTexts (SelectedRootText root_text))

            else
                lift (AddToSelectedTexts (SelectedRootText root_text))
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
                [ div [ style "font-weight" "bold" ]
                    [ text root_text.acronym ]
                , div [ style "font-weight" "bold" ]
                    [ text root_text.title ]
                ]
            , column cM
                [ class "is-one-fifth"
                , style "text-align" "right"
                ]
                [ icon Standard [] [ i [ class ("mdi " ++ pinClass) ] [] ] ]
            ]
        , div [ style "padding-left" "1em" ] <|
            Markdown.toHtml Nothing snippet
        ]


viewTranslatedTextRow : (Msg m -> m) -> TranslatedText -> Model -> Html m
viewTranslatedTextRow lift translated_text model =
    let
        words =
            String.words translated_text.content_plain

        snippet =
            if List.length words > 30 then
                String.join " " (List.take 30 words) ++ "..."

            else
                String.join " " words

        isSelected =
            let
                t_uid =
                    translated_text.uid

                textList =
                    List.map (\x -> getUid x) model.selectedTextList
            in
            List.member t_uid textList

        pinClass =
            if isSelected then
                "mdi-pin"

            else
                "mdi-pin-outline"

        onClickFn =
            if isSelected then
                lift (RemoveFromSelectedTexts (SelectedTranslatedText translated_text))

            else
                lift (AddToSelectedTexts (SelectedTranslatedText translated_text))
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
                [ div [ style "font-weight" "bold" ]
                    [ text translated_text.acronym ]
                , div [ style "font-weight" "bold" ]
                    [ text translated_text.title ]
                , div [ style "font-style" "italic" ]
                    [ text translated_text.author_uid ]
                ]
            , column cM
                [ class "is-one-fifth"
                , style "text-align" "right"
                ]
                [ icon Standard [] [ i [ class ("mdi " ++ pinClass) ] [] ] ]
            ]
        , div [ style "padding-left" "1em" ] <|
            Markdown.toHtml Nothing snippet
        ]


viewSelectedRootTextHeader : RootText -> Html m
viewSelectedRootTextHeader t =
    header
        []
        [ p [ class "suttaref" ] [ text t.acronym ]
        , h1 [] [ text t.title ]
        , h3 [] [ text ("translated by " ++ t.author_uid) ]
        ]


viewSelectedTranslatedTextHeader : TranslatedText -> Html m
viewSelectedTranslatedTextHeader t =
    header
        []
        [ p [ class "suttaref" ] [ text t.acronym ]
        , h1 [] [ text t.root_title ]
        , h2 [] [ text t.title ]
        , h3 [] [ text ("translated by " ++ t.author_uid) ]
        ]


viewSelectedTextHeader : (Msg m -> m) -> Model -> Html m
viewSelectedTextHeader lift model =
    case model.selectedText of
        Nothing ->
            div [] []

        Just t ->
            case t of
                SelectedTranslatedText t_ ->
                    viewSelectedTranslatedTextHeader t_

                SelectedRootText t_ ->
                    viewSelectedRootTextHeader t_


viewSelectedTextBody : (Msg m -> m) -> Model -> Html m
viewSelectedTextBody lift model =
    case model.selectedText of
        Nothing ->
            div [] [ text "No selected text." ]

        Just t ->
            case t of
                SelectedTranslatedText t_ ->
                    div [] <|
                        Markdown.toHtml mdRawHtml t_.content_html

                SelectedRootText t_ ->
                    div [] <|
                        Markdown.toHtml mdRawHtml t_.content_html


onEnter : msg -> Attribute msg
onEnter msg =
    keyCode
        |> Decode.andThen
            (\key ->
                if key == 13 then
                    Decode.succeed msg

                else
                    Decode.fail "Not enter"
            )
        |> on "keyup"


searchInput : (Msg m -> m) -> Model -> Html m
searchInput lift model =
    let
        searchButton =
            let
                bM =
                    { buttonModifiers | size = Medium }
            in
            BE.button bM
                [ onClick (lift SubmitLookupQuery) ]
                [ icon Standard [] [ i [ class "mdi mdi-magnify", style "color" "black" ] [] ] ]

        myControlInputModifiers : ControlInputModifiers m
        myControlInputModifiers =
            { controlInputModifiers | size = Medium }

        myControlAttrs : List (Attribute m)
        myControlAttrs =
            [ style "width" "100%" ]

        myInputAttrs : List (Attribute m)
        myInputAttrs =
            [ placeholder "Search in texts, e.g.: middle way, majjhima patipada, DN 16 ..."
            , autofocus True
            , onInput (\x -> lift (SetTextLookupQuery x))
            , onEnter (lift SubmitLookupQuery)
            ]
    in
    div []
        [ connectedFields Left
            []
            [ controlText
                myControlInputModifiers
                myControlAttrs
                myInputAttrs
                []
            , searchButton
            ]
        , searchOptions lift model.queryOptions
        ]


searchOptions : (Msg m -> m) -> QueryOptions -> Html m
searchOptions lift opts =
    fields Left
        []
        [ controlCheckBox False
            []
            []
            [ onCheck (\x -> lift (SetOptRootTexts x))
            , checked opts.rootTexts
            ]
            [ text "in Pali texts" ]
        , controlCheckBox False
            []
            []
            [ onCheck (\x -> lift (SetOptTranslatedTexts x))
            , checked opts.translatedTexts
            ]
            [ text "in Translated texts" ]
        , controlCheckBox False
            []
            []
            [ onCheck (\x -> lift (SetOptAcronymContains x))
            , checked opts.acronymContains
            ]
            [ text "in acronym" ]
        , controlCheckBox False
            []
            []
            [ onCheck (\x -> lift (SetOptTitleContains x))
            , checked opts.titleContains
            ]
            [ text "in title" ]
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
    , lookupResults : WebData TextQueryData
    , selectedText : Maybe SelectedText
    , selectedTextList : List SelectedText
    , paginationInfo : PaginationInfo
    , subRoute : SubRoute
    , queryOptions : QueryOptions
    }


initialModel =
    { lookupQuery = ""
    , lookupResults = RemoteData.NotAsked
    , selectedText = Nothing
    , selectedTextList = []
    , paginationInfo = initialPaginationInfo
    , subRoute = Searching
    , queryOptions = initialQueryOptions
    }


initialTranslatedText : TranslatedText
initialTranslatedText =
    { id = 999888
    , uid = "sn56.11-initial/thanissaro"
    , author_uid = "thanissaro"
    , acronym = "SN 56.11"
    , volpage = "PTS"
    , title = "Setting the Wheel of Dhamma in Motion"
    , root_title = "Dhammacakkappavattana Sutta"
    , content_language = "en"
    , content_plain = "Lorem ipsum"
    , content_html = "<p>Lorem ipsum</p><p><em>Lorem ispum</em></p>"
    }


initialPaginationInfo : PaginationInfo
initialPaginationInfo =
    { pageLength = 30
    , currentPage = 1
    , totalPages = 1
    }


initialQueryOptions : QueryOptions
initialQueryOptions =
    { rootTexts = True
    , translatedTexts = True
    , acronymContains = True
    , titleContains = True
    , fulltext = True
    , containsExactly = False
    }


type alias QueryOptions =
    { rootTexts : Bool
    , translatedTexts : Bool
    , acronymContains : Bool
    , titleContains : Bool
    , fulltext : Bool
    , containsExactly : Bool
    }


type alias PaginationInfo =
    { pageLength : Int
    , currentPage : Int
    , totalPages : Int
    }


type alias Author =
    { id : Int
    , uid : String
    , blurb : String
    , long_name : String
    , short_name : String
    }


type alias RootText =
    { id : Int
    , uid : String
    , author_uid : String
    , acronym : String
    , volpage : String
    , title : String
    , content_language : String
    , content_plain : String
    , content_html : String
    }


type alias TranslatedText =
    { id : Int
    , uid : String
    , author_uid : String
    , acronym : String
    , volpage : String
    , title : String
    , root_title : String
    , content_language : String
    , content_plain : String
    , content_html : String
    }


type alias RootTextSearches =
    { acronym_contains : List RootText
    , title_contains : List RootText
    , contains_exactly : List RootText
    , fulltext : List RootText
    , total_count : Int
    }


type alias TranslatedTextSearches =
    { acronym_contains : List TranslatedText
    , title_contains : List TranslatedText
    , contains_exactly : List TranslatedText
    , fulltext : List TranslatedText
    , total_count : Int
    }


type alias TextQueryData =
    { root_texts : RootTextSearches
    , translated_texts : TranslatedTextSearches
    }


type SelectedText
    = SelectedRootText RootText
    | SelectedTranslatedText TranslatedText


type TextItem
    = ItemRootText RootText
    | ItemTranslatedText TranslatedText


type SubRoute
    = Searching
    | Selecting
    | Reading


type Msg m
    = NoOp
    | AddToSelectedTexts SelectedText
    | RemoveFromSelectedTexts SelectedText
    | SetTextLookupQuery String
    | SubmitLookupQuery
    | TextQueryDataReceived (WebData TextQueryData)
    | SetSelectedReadText SelectedText
    | SetSubRoute SubRoute
    | PaginationPrev
    | PaginationNext
    | SetPagination Int
    | SetOptRootTexts Bool
    | SetOptTranslatedTexts Bool
    | SetOptAcronymContains Bool
    | SetOptTitleContains Bool
    | SetOptFulltext Bool
    | SetOptContainsExactly Bool


lookupCmd lift query opts =
    if String.length query > 2 then
        fetchTextQuery lift query opts

    else
        Cmd.none


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetTextLookupQuery query ->
            ( { model | lookupQuery = query }, Cmd.none )

        SubmitLookupQuery ->
            ( model, lookupCmd lift model.lookupQuery model.queryOptions )

        TextQueryDataReceived data ->
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
                            res.root_texts.total_count + res.translated_texts.total_count

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

        AddToSelectedTexts selectedText ->
            let
                t_uid =
                    getUid selectedText

                m =
                    { model | selectedText = Just selectedText }

                m_ =
                    let
                        textList =
                            List.filter (\x -> not (getUid x == t_uid)) m.selectedTextList
                    in
                    { m | selectedTextList = selectedText :: textList }
            in
            ( m_, Cmd.none )

        RemoveFromSelectedTexts t ->
            let
                t_uid =
                    getUid t

                m =
                    let
                        textList =
                            List.filter (\x -> not (getUid x == t_uid)) model.selectedTextList
                    in
                    { model | selectedTextList = textList }

                m_ =
                    { m | selectedText = List.head m.selectedTextList }
            in
            ( m_, Cmd.none )

        SetSelectedReadText t ->
            ( { model | selectedText = Just t }, Cmd.none )

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

        SetOptRootTexts x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | rootTexts = x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )

        SetOptTranslatedTexts x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | translatedTexts = x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )

        SetOptAcronymContains x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | acronymContains = x }
            in
            ( { model | queryOptions = o_ }, lookupCmd lift model.lookupQuery o_ )

        SetOptTitleContains x ->
            let
                o =
                    model.queryOptions

                o_ =
                    { o | titleContains = x }
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


getUid t =
    case t of
        SelectedRootText t_ ->
            t_.uid

        SelectedTranslatedText t_ ->
            t_.uid


authorDecoder : Decoder Author
authorDecoder =
    Decode.succeed Author
        |> required "id" int
        |> required "uid" string
        |> required "blurb" string
        |> required "long_name" string
        |> required "short_name" string


rootTextDecoder : Decoder RootText
rootTextDecoder =
    Decode.succeed RootText
        |> required "id" int
        |> required "uid" string
        |> required "author_uid" string
        |> required "acronym" string
        |> required "volpage" string
        |> required "title" string
        |> required "content_language" string
        |> required "content_plain" string
        |> required "content_html" string


translatedTextDecoder : Decoder TranslatedText
translatedTextDecoder =
    Decode.succeed TranslatedText
        |> required "id" int
        |> required "uid" string
        |> required "author_uid" string
        |> required "acronym" string
        |> required "volpage" string
        |> required "title" string
        |> required "root_title" string
        |> required "content_language" string
        |> required "content_plain" string
        |> required "content_html" string


textRootTextSearchesDecoder : Decoder RootTextSearches
textRootTextSearchesDecoder =
    Decode.succeed RootTextSearches
        |> required "acronym_contains" (list rootTextDecoder)
        |> required "title_contains" (list rootTextDecoder)
        |> required "contains_exactly" (list rootTextDecoder)
        |> required "fulltext" (list rootTextDecoder)
        |> required "total_count" int


textTranslatedTextSearchesDecoder : Decoder TranslatedTextSearches
textTranslatedTextSearchesDecoder =
    Decode.succeed TranslatedTextSearches
        |> required "acronym_contains" (list translatedTextDecoder)
        |> required "title_contains" (list translatedTextDecoder)
        |> required "contains_exactly" (list translatedTextDecoder)
        |> required "fulltext" (list translatedTextDecoder)
        |> required "total_count" int


textQueryDataDecoder : Decoder TextQueryData
textQueryDataDecoder =
    Decode.succeed TextQueryData
        |> required "root_texts" textRootTextSearchesDecoder
        |> required "translated_texts" textTranslatedTextSearchesDecoder


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
        [ ( "root_texts", opts.rootTexts )
        , ( "translated_texts", opts.translatedTexts )
        , ( "acronym_contains", opts.acronymContains )
        , ( "title_contains", opts.titleContains )
        , ( "fulltext", opts.fulltext )
        , ( "contains_exactly", opts.containsExactly )
        ]


fetchTextQuery : (Msg m -> m) -> String -> QueryOptions -> Cmd m
fetchTextQuery lift query opts =
    textQueryDataDecoder
        |> Http.get
            (UB.absolute
                [ "search", "texts" ]
                (List.append
                    [ UB.string "query" query ]
                    (optsToQuery opts)
                )
            )
        |> RemoteData.sendRequest
        |> Cmd.map (\x -> lift (TextQueryDataReceived x))
