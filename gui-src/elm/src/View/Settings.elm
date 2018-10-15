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
                , updateSection lift model
                ]
            ]
        ]
    ]


updateSection : (Msg m -> m) -> Model -> Html m
updateSection lift model =
    let
        updIcon =
            ( Standard, [], icon Standard [] [ i [ class "mdi mdi-update" ] [] ] )

        updateButton =
            BE.button { buttonModifiers | color = Primary, iconLeft = Just updIcon }
                [ onClick (lift StartUpdateAssets) ]
                [ text "Start update" ]
    in
    if isNewVersionAvailable model then
        BL.section Spaced
            []
            [ content Standard
                []
                [ h2 [] [ text "Updates" ]
                , p [] [ text "Updated assets are available for download." ]
                , updateButton
                ]
            ]

    else
        BL.section Spaced
            []
            [ content Standard
                []
                [ h2 [] [ text "Updates" ]
                , p [] [ text "Assets are up to date." ]
                ]
            ]



-- TODO compare versions using semver logic


isNewVersionAvailable model =
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
    if lv.appdata_tar.version == "" then
        False

    else if lv.appdata_tar.version /= rv.appdata_tar.version || lv.assets_tar.version /= rv.assets_tar.version then
        True

    else
        False


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
    }


initialModel =
    { localVersion = RemoteData.NotAsked
    , remoteVersion = RemoteData.NotAsked
    }


type Msg m
    = NoOp
    | LocalVersionDataReceived (WebData VersionInfo)
    | RemoteVersionDataReceived (WebData VersionInfo)
    | StartUpdateAssets


update : (Msg m -> m) -> Msg m -> Model -> ( Model, Cmd m )
update lift msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LocalVersionDataReceived x ->
            ( { model | localVersion = x }, Cmd.none )

        RemoteVersionDataReceived x ->
            ( { model | remoteVersion = x }, Cmd.none )

        StartUpdateAssets ->
            ( model, Cmd.none )


type alias SimsapaVersion =
    { version : String
    , url : String
    }


type alias AssetVersion =
    { version : String
    , url : String
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
    { version = ""
    , url = ""
    }


emptyAssetVersion : AssetVersion
emptyAssetVersion =
    { version = ""
    , url = ""
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
        |> required "version" string
        |> required "url" string


assetVersionDecoder : Decoder AssetVersion
assetVersionDecoder =
    Decode.succeed AssetVersion
        |> required "version" string
        |> required "url" string
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
