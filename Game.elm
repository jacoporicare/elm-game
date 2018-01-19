module Game exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


consts =
    { canvasWidth = 800
    , canvasHeight = 600
    , groundHeight = 50
    }



-- MODEL


type alias Model =
    { vx : Float
    , vy : Float
    , x : Float
    , y : Float
    , moveDir : Float
    , angle : Float
    , playerSize : Float
    , hSpeed : Float
    , vSpeed : Float
    }


model : Model
model =
    { vx = 0
    , vy = 0
    , x = 0
    , y = 0
    , moveDir = 0
    , angle = 0
    , playerSize = 50
    , hSpeed = 0.5
    , vSpeed = 1.0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | ChangeY String
    | ChangePlayerSize String
    | ChangeHSpeed String
    | ChangeVSpeed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( step dt model, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )

        ChangeY value ->
            case String.toFloat value of
                Ok y ->
                    ( { model | y = y }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangePlayerSize value ->
            case String.toFloat value of
                Ok playerSize ->
                    ( { model | playerSize = playerSize }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeHSpeed value ->
            case String.toFloat value of
                Ok hSpeed ->
                    ( { model | hSpeed = hSpeed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeVSpeed value ->
            case String.toFloat value of
                Ok vSpeed ->
                    ( { model | vSpeed = vSpeed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            { model | moveDir = -1 }

        ArrowRight ->
            { model | moveDir = 1 }

        ArrowUp ->
            if model.vy == 0 then
                { model | vy = model.vSpeed }
            else
                model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            { model | moveDir = 0 }

        ArrowRight ->
            { model | moveDir = 0 }

        _ ->
            model


step : Float -> Model -> Model
step dt model =
    model
        |> move dt
        |> physics dt
        |> gravity dt
        |> friction dt


move : Float -> Model -> Model
move dt model =
    if model.moveDir /= 0 then
        { model | vx = clamp -model.hSpeed model.hSpeed (model.vx + dt / 300 * model.moveDir) }
    else
        model


gravity : Float -> Model -> Model
gravity dt model =
    { model
        | vy =
            if model.y > 0 then
                model.vy - dt / 300
            else
                0
    }


friction : Float -> Model -> Model
friction dt model =
    if model.vx /= 0 then
        let
            dir =
                if model.vx > 0 then
                    1
                else
                    -1
        in
            { model | vx = model.vx - (min (abs model.vx) (dt / 2000) * dir) }
    else
        model


physics : Float -> Model -> Model
physics dt model =
    let
        xChange =
            dt * model.vx

        playerCircumference =
            pi * model.playerSize

        edgeX =
            consts.canvasWidth / 2 - model.playerSize / 2
    in
        { model
            | x = clamp -edgeX edgeX (model.x + xChange)
            , y = max 0 (model.y + dt * model.vy)
            , angle = model.angle + turns (xChange / playerCircumference)
        }



-- VIEW


px : Float -> String
px v =
    toString v ++ "px"


translate3d : Float -> Float -> Float -> String
translate3d x y z =
    "translate3d(" ++ toString x ++ "px, " ++ toString y ++ "px, " ++ toString z ++ "px)"


rotate : Float -> String
rotate a =
    "rotate(" ++ toString a ++ "rad)"


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", px consts.canvasWidth )
            , ( "height", px consts.canvasHeight )
            , ( "background-color", "#eee" )
            ]
        ]
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "left", px <| consts.canvasWidth / 2 - model.playerSize / 2 )
                , ( "top", px <| consts.canvasHeight - model.playerSize - consts.groundHeight )
                , ( "background-color", "brown" )
                , ( "width", px model.playerSize )
                , ( "height", px model.playerSize )
                , ( "border-radius", "50%" )
                , ( "transform"
                  , translate3d model.x -model.y 0 ++ " " ++ rotate model.angle
                  )
                , ( "will-change", "transform" )
                ]
            ]
            [ div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", px <| model.playerSize / 3 )
                    , ( "left", px <| model.playerSize / 3 - model.playerSize / 10 / 2 )
                    , ( "width", px <| model.playerSize / 10 )
                    , ( "height", px <| model.playerSize / 7 )
                    , ( "border-radius", "50%" )
                    , ( "background-color", "white" )
                    ]
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", px <| model.playerSize / 3 )
                    , ( "left", px <| model.playerSize / 3 * 2 - model.playerSize / 10 / 2 )
                    , ( "width", px <| model.playerSize / 10 )
                    , ( "height", px <| model.playerSize / 7 )
                    , ( "border-radius", "50%" )
                    , ( "background-color", "white" )
                    ]
                ]
                []
            , div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", px <| model.playerSize / 3 * 2 )
                    , ( "left", px <| model.playerSize / 4 )
                    , ( "width", px <| model.playerSize / 2 )
                    , ( "height", px <| model.playerSize / 10 )
                    , ( "border-radius", "50%" )
                    , ( "background-color", "white" )
                    ]
                ]
                []
            ]
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "left", "0" )
                , ( "top", px <| consts.canvasHeight - consts.groundHeight )
                , ( "height", px consts.groundHeight )
                , ( "width", px consts.canvasWidth )
                , ( "background-color", "green" )
                ]
            ]
            []
        , div
            [ style
                [ ( "position", "absolute" )
                , ( "left", "800px" )
                , ( "top", "0" )
                ]
            ]
            [ div [] [ text ("x: " ++ toString model.x) ]
            , div []
                [ text "y: "
                , input [ type_ "text", value <| toString model.y, onInput ChangeY ] []
                ]
            , div [] [ text ("vx: " ++ toString model.vx) ]
            , div [] [ text ("vy: " ++ toString model.vy) ]
            , div [] [ text ("moveDir: " ++ toString model.moveDir) ]
            , div [] [ text ("angle: " ++ toString model.angle) ]
            , div []
                [ text "playerSize: "
                , input [ type_ "text", value <| toString model.playerSize, onInput ChangePlayerSize ] []
                ]
            , div []
                [ text "hSpeed: "
                , input [ type_ "text", value <| toString model.hSpeed, onInput ChangeHSpeed ] []
                ]
            , div []
                [ text "vSpeed: "
                , input [ type_ "text", value <| toString model.vSpeed, onInput ChangeVSpeed ] []
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
