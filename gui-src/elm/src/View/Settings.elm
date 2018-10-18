module View.Settings exposing (Model, Msg(..), fetchLocalVersion, fetchRemoteVersion, initialModel, update, view)

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
import RemoteData exposing (WebData)
import Round
import Url.Builder as UB exposing (absolute)


cM =
    columnModifiers


cSM =
    columnsModifiers


view : (Msg m -> m) -> Model -> (Maybe (List (Html m)) -> Maybe (List (Html m)) -> Html m) -> List (Html m)
view lift model topNav =
    [ columns myColumnsModifiers
        [ class "page-wrap" ]
        [ column cM
            [ class "page-content-outer-controls" ]
            [ div [ class "page-content-inner-controls" ]
                [ topNav Nothing Nothing
                , BL.section Spaced
                    []
                    [ content Standard
                        []
                        [ h1 [] [ text "Settings" ]
                        , simsapaUpdate lift model
                        , assetUpdate lift model
                        ]
                    ]
                ]
            ]
        ]
    ]


assetUpdate : (Msg m -> m) -> Model -> Html m
assetUpdate lift model =
    let
        newAssets =
            isNewAssetAvailable model

        updIcon =
            ( Standard, [], icon Standard [] [ i [ class "mdi mdi-update" ] [] ] )

        updateButton =
            BE.button { buttonModifiers | color = Primary, iconLeft = Just updIcon }
                [ onClick (lift RestartForceUpdateAssets) ]
                [ text "Restart and update" ]
    in
    case newAssets of
        NewVersion assets ->
            div []
                [ h2 [] [ text "Assets" ]
                , p [] [ text "Updated assets are available for download." ]
                , p [] [ text "What will be downloaded:" ]
                , willDownloadInfo assets
                , p [] [ text "Click the button to restart the application and download the updates." ]
                , updateButton
                ]

        CurrentVersion assets ->
            div []
                [ h2 [] [ text "Assets" ]
                , assetsVersions assets
                ]

        EmptyData ->
            div [] []


assetsVersions : List AssetVersion -> Html m
assetsVersions assets =
    ul []
        (List.map
            (\x -> li [] [ text (x.description ++ ": v" ++ x.version) ])
            assets
        )


simsapaUpdate : (Msg m -> m) -> Model -> Html m
simsapaUpdate lift model =
    let
        newSimsapa =
            isNewSimsapaAvailable model

        updIcon =
            ( Standard, [], icon Standard [] [ i [ class "mdi mdi-open-in-new" ] [] ] )

        updateButton =
            BE.button { buttonModifiers | color = Primary, iconLeft = Just updIcon }
                []
                [ text "Visit releases page" ]
    in
    case newSimsapa of
        NewVersion version ->
            div []
                [ h2 [] [ text "Simsapa" ]
                , p [] [ text "A new version of the Simsapa application is available for download." ]
                , p [] [ text "Visit the releases page to download:" ]
                , p [] [ a [ href version.url ] [ text version.url ] ]
                , updateButton
                ]

        CurrentVersion version ->
            div []
                [ h2 [] [ text "Simsapa" ]
                , ul []
                    [ li [] [ text (version.description ++ ": v" ++ version.version) ]
                    ]
                ]

        EmptyData ->
            div [] []


bytesToMb : Int -> Float
bytesToMb x =
    toFloat x / 1024.0 / 1024.0


willDownloadInfo assets =
    ul []
        (List.map
            (\x -> li [] [ text (x.description ++ ", " ++ Round.round 1 (bytesToMb x.size) ++ " MB") ])
            assets
        )



-- TODO compare versions using semver logic


type VersionType t
    = NewVersion t
    | CurrentVersion t
    | EmptyData


isThereNewMessage model =
    let
        haveNewAsset =
            case isNewAssetAvailable model of
                NewVersion _ ->
                    True

                _ ->
                    False

        haveNewSimsapa =
            case isNewSimsapaAvailable model of
                NewVersion _ ->
                    True

                _ ->
                    False
    in
    haveNewAsset || haveNewSimsapa


isNewSimsapaAvailable model =
    let
        lv =
            case model.localVersion of
                RemoteData.Success x ->
                    x

                _ ->
                    emptyVersionInfo

        rv =
            case model.remoteVersion of
                RemoteData.Success x ->
                    x

                _ ->
                    emptyVersionInfo
    in
    if lv.simsapa_electron.version == "" || rv.simsapa_electron.version == "" then
        EmptyData

    else if lv.simsapa_electron.version /= rv.simsapa_electron.version then
        NewVersion rv.simsapa_electron

    else
        CurrentVersion lv.simsapa_electron


isNewAssetAvailable model =
    let
        lv =
            case model.localVersion of
                RemoteData.Success x ->
                    x

                _ ->
                    emptyVersionInfo

        rv =
            case model.remoteVersion of
                RemoteData.Success x ->
                    x

                _ ->
                    emptyVersionInfo

        lvList =
            [ lv.appdata_tar, lv.assets_tar ]

        rvList =
            [ rv.appdata_tar, rv.assets_tar ]

        combList =
            List.map2 Tuple.pair lvList rvList

        newList =
            List.filterMap
                (\x ->
                    if (Tuple.first x).version /= (Tuple.second x).version then
                        Just (Tuple.first x)

                    else
                        Nothing
                )
                combList
    in
    if lv.appdata_tar.version == "" || rv.appdata_tar.version == "" then
        EmptyData

    else if lv.appdata_tar.version /= rv.appdata_tar.version || lv.assets_tar.version /= rv.assets_tar.version then
        NewVersion newList

    else
        CurrentVersion lvList


myColumnsModifiers : ColumnsModifiers
myColumnsModifiers =
    { multiline = False
    , gap = Gap1
    , display = MobileAndBeyond
    , centered = True
    }


type alias Model =
    { localVersion : WebData VersionInfo
    , remoteVersion : WebData VersionInfo
    , haveMessage : Bool
    }


initialModel =
    { localVersion = RemoteData.NotAsked
    , remoteVersion = RemoteData.NotAsked
    , haveMessage = False
    }


type Msg m
    = NoOp
    | LocalVersionDataReceived (WebData VersionInfo)
    | RemoteVersionDataReceived (WebData VersionInfo)
    | RestartForceUpdateAssets
    | IgnoreResult (Result Http.Error String)


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LocalVersionDataReceived x ->
            let
                m =
                    { model | localVersion = x }

                m_ =
                    { m | haveMessage = isThereNewMessage m }
            in
            ( m_, Cmd.none )

        RemoteVersionDataReceived x ->
            let
                m =
                    { model | remoteVersion = x }

                m_ =
                    { m | haveMessage = isThereNewMessage m }
            in
            ( m_, Cmd.none )

        RestartForceUpdateAssets ->
            ( model, hitRestartForceUpdate lift )

        IgnoreResult (Ok _) ->
            ( model, Cmd.none )

        IgnoreResult (Err _) ->
            ( model, Cmd.none )


type alias SimsapaVersion =
    { description : String
    , version : String
    , url : String
    }


type alias AssetVersion =
    { description : String
    , version : String
    , url : String
    , size : Int
    , md5 : String
    , saveAs : String
    }


type alias VersionInfo =
    { simsapa_electron : SimsapaVersion
    , appdata_tar : AssetVersion
    , appdata_sqlite3 : AssetVersion
    , assets_tar : AssetVersion
    }


emptySimsapaVersion : SimsapaVersion
emptySimsapaVersion =
    { description = ""
    , version = ""
    , url = ""
    }


emptyAssetVersion : AssetVersion
emptyAssetVersion =
    { description = ""
    , version = ""
    , url = ""
    , size = 0
    , md5 = ""
    , saveAs = ""
    }


emptyVersionInfo : VersionInfo
emptyVersionInfo =
    { simsapa_electron = emptySimsapaVersion
    , appdata_tar = emptyAssetVersion
    , appdata_sqlite3 = emptyAssetVersion
    , assets_tar = emptyAssetVersion
    }


simsapaVersionDecoder : Decoder SimsapaVersion
simsapaVersionDecoder =
    Decode.succeed SimsapaVersion
        |> required "description" string
        |> required "version" string
        |> required "url" string


assetVersionDecoder : Decoder AssetVersion
assetVersionDecoder =
    Decode.succeed AssetVersion
        |> required "description" string
        |> required "version" string
        |> required "url" string
        |> required "size" int
        |> required "md5" string
        |> required "saveAs" string


versionInfoDecoder : Decoder VersionInfo
versionInfoDecoder =
    Decode.succeed VersionInfo
        |> required "simsapa_electron" simsapaVersionDecoder
        |> required "appdata_tar" assetVersionDecoder
        |> required "appdata_sqlite3" assetVersionDecoder
        |> required "assets_tar" assetVersionDecoder


fetchLocalVersion : (Msg m -> m) -> Cmd m
fetchLocalVersion lift =
    versionInfoDecoder
        |> Http.get (UB.absolute [ "local-version" ] [])
        |> RemoteData.sendRequest
        |> Cmd.map (\x -> lift (LocalVersionDataReceived x))


fetchRemoteVersion : (Msg m -> m) -> Cmd m
fetchRemoteVersion lift =
    versionInfoDecoder
        |> Http.get (UB.absolute [ "remote-version" ] [])
        |> RemoteData.sendRequest
        |> Cmd.map (\x -> lift (RemoteVersionDataReceived x))


hitRestartForceUpdate : (Msg m -> m) -> Cmd m
hitRestartForceUpdate lift =
    Http.getString (UB.absolute [ "restart-force-update" ] [])
        |> Http.send IgnoreResult
        |> Cmd.map (\x -> lift NoOp)
