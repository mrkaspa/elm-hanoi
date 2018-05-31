module Main exposing (main)

import Html exposing (Html, text)
import Process
import Stack exposing (Stack)
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, height, width, x, y)
import Task exposing (Task)
import Time exposing (Time)


num_pegs : Int
num_pegs =
    7


delay_ms : Time
delay_ms =
    100


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type Model
    = Init
    | Started Peg Int Hanoi Direction


type Direction
    = Right
    | Left


type Msg
    = Next


init : ( Model, Cmd Msg )
init =
    let
        hanoi =
            new num_pegs

        dir =
            if num_pegs % 2 == 0 then
                Right
            else
                Left
    in
    pile1 hanoi
        |> List.head
        |> Maybe.map (\peg -> ( Started peg 0 hanoi dir, Task.perform identity <| Task.succeed Next ))
        |> Maybe.withDefault ( Init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            case model of
                Init ->
                    ( Init, Cmd.none )

                Started peg moveNum hanoi dir ->
                    if List.length (pile1 hanoi) == 0 && List.length (pile2 hanoi) == 0 then
                        ( model, Cmd.none )
                    else
                        let
                            nextHanoi =
                                if moveNum % 2 == 1 then
                                    case dir of
                                        Right ->
                                            moveRight peg hanoi

                                        Left ->
                                            moveLeft peg hanoi
                                else
                                    moveOtherPeg peg hanoi
                        in
                        ( Started peg (moveNum + 1) nextHanoi dir, Process.sleep delay_ms |> Task.andThen (\_ -> Task.succeed Next) |> Task.perform identity )


view : Model -> Html Msg
view model =
    case model of
        Init ->
            text ""

        Started _ _ hanoi _ ->
            Svg.svg
                [ width "500"
                , height "500"
                ]
                (List.concat
                    [ pileView 1 <| pile1 hanoi
                    , pileView 2 <| pile2 hanoi
                    , pileView 3 <| pile3 hanoi
                    ]
                )


pileView : Int -> List Peg -> List (Svg Msg)
pileView pileNum pegs =
    pegs
        |> List.reverse
        |> List.indexedMap (pegView pileNum)


pegView : Int -> Int -> Peg -> Svg Msg
pegView pileNum index (Peg num) =
    let
        widthPixels =
            num * 10
    in
    Svg.rect
        [ fill "red"
        , width <| toString widthPixels
        , height "10"
        , x <| toString <| (100 * pileNum) - (widthPixels // 2)
        , y <| toString <| 200 - (15 * index)
        ]
        []



-- Hanoi module


type Hanoi
    = Hanoi Pile Pile Pile


type alias Pile =
    Stack Peg


type Peg
    = Peg Int


empty : Hanoi
empty =
    Hanoi Stack.initialise Stack.initialise Stack.initialise


new : Int -> Hanoi
new numPegs =
    List.range 1 numPegs
        |> List.reverse
        |> List.map Peg
        |> List.foldl (\peg (Hanoi p1 p2 p3) -> Hanoi (Stack.push peg p1) p2 p3) empty


moveLeft : Peg -> Hanoi -> Hanoi
moveLeft peg ((Hanoi pile1 pile2 pile3) as hanoi) =
    if Stack.top pile1 == Just peg then
        Hanoi (Tuple.second <| Stack.pop pile1) pile2 (Stack.push peg pile3)
    else if Stack.top pile2 == Just peg then
        Hanoi (Stack.push peg pile1) (Tuple.second <| Stack.pop pile2) pile3
    else
        Hanoi pile1 (Stack.push peg pile2) (Tuple.second <| Stack.pop pile3)


moveRight : Peg -> Hanoi -> Hanoi
moveRight peg ((Hanoi pile1 pile2 pile3) as hanoi) =
    if Stack.top pile1 == Just peg then
        Hanoi (Tuple.second <| Stack.pop pile1) (Stack.push peg pile2) pile3
    else if Stack.top pile2 == Just peg then
        Hanoi pile1 (Tuple.second <| Stack.pop pile2) (Stack.push peg pile3)
    else
        Hanoi (Stack.push peg pile1) pile2 (Tuple.second <| Stack.pop pile3)


moveOtherPeg : Peg -> Hanoi -> Hanoi
moveOtherPeg peg ((Hanoi pile1 pile2 pile3) as hanoi) =
    let
        ( leftPeg_, rightPeg_ ) =
            if Stack.top pile1 == Just peg then
                ( Tuple.first <| Stack.pop pile2, Tuple.first <| Stack.pop pile3 )
            else if Stack.top pile2 == Just peg then
                ( Tuple.first <| Stack.pop pile3, Tuple.first <| Stack.pop pile1 )
            else
                ( Tuple.first <| Stack.pop pile1, Tuple.first <| Stack.pop pile2 )
    in
    case ( leftPeg_, rightPeg_ ) of
        ( Just leftPeg, Nothing ) ->
            moveRight leftPeg hanoi

        ( Nothing, Just rightPeg ) ->
            moveLeft rightPeg hanoi

        ( Nothing, Nothing ) ->
            hanoi

        ( Just ((Peg leftNum) as leftPeg), Just ((Peg rightNum) as rightPeg) ) ->
            if leftNum < rightNum then
                moveRight leftPeg hanoi
            else
                moveLeft rightPeg hanoi


pile1 : Hanoi -> List Peg
pile1 (Hanoi pile1 _ _) =
    Stack.toList pile1


pile2 : Hanoi -> List Peg
pile2 (Hanoi _ pile2 _) =
    Stack.toList pile2


pile3 : Hanoi -> List Peg
pile3 (Hanoi _ _ pile3) =
    Stack.toList pile3
