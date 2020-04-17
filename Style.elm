module Style exposing (stylesheet)

import Html exposing (..)
import Html.Attributes exposing (..)


stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "css.css"
            ]
    in
    node tag attrs []
