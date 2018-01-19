module Key exposing (..)


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        38 ->
            ArrowUp

        40 ->
            ArrowDown

        _ ->
            Unknown
