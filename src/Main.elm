port module Main exposing (..)

import Browser
import Http
import Url.Builder as B
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Print
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)

port prettify : String -> Cmd msg

-- MAIN

main =
  Browser.element
    { init = init,
      update = update,
      subscriptions = subscriptions,
      view = view
    }

-- MODEL

type alias Model =
  { content : String,
    convertTo : String,
    convertFrom : String,
    conversionResult : String,
    radioState : RadioState,
    inputFieldText : String,
    inputFieldTextValue : String,
    inputFieldUrl : String,
    inputFieldUrlValue : String,
    raw : String,
    data : String
  }

type alias Config =
    { indent : Int
    , columns : Int
    }

config = { indent = 4, columns = 10 }

init : () -> (Model, Cmd Msg)
init _ =
  ( 
    { content = "",
        convertTo = "json",
        convertFrom = "",
        conversionResult = "",
        radioState = URL,
        inputFieldText = "hidden",
        inputFieldTextValue = "",
        inputFieldUrl = "shown",
        inputFieldUrlValue = "",
        raw = "",
        data = ""
    },
    Cmd.none
  )

type RadioState
  = URL
  | TEXT

-- UPDATE

type Msg
  = DisplayConversionResult
  | RadioMsg RadioState
  | UpdateFrom String
  | UpdateTo String
  | GotConversion (Result Http.Error String)
  | UpdateInputFieldTextValue String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    DisplayConversionResult -> 
      ({ model | content = model.conversionResult }, getConversion model)

    UpdateFrom val ->
      ({ model | convertFrom = val }, dlog model)
      
    UpdateTo val ->
      ({ model | convertTo = val }, Cmd.none)

    RadioMsg state ->
      case state of
        URL ->
          ({ model | radioState = state, inputFieldText = "hidden", inputFieldUrl = "shown"}, Cmd.none)
        TEXT ->
          ({ model | radioState = state, inputFieldText = "shown", inputFieldUrl = "hidden"}, Cmd.none)

    GotConversion result ->
      case result of
        Ok url ->
          if model.convertTo == "json" then (model, prettify url)
          else ({model | conversionResult = url}, prettify url)

        Err url ->
          ({model | conversionResult = "FAILURE2"}, Cmd.none)
    
    UpdateInputFieldTextValue val ->
      ({model | inputFieldTextValue = val}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div[class "container"]
  [
    div [class "column"]
    [
      div[id "menu"]
      [
        label[]
        [
          text "Input format - ",
          span[]
          [
            label[]
            [
                text "url",
                input[type_ "radio", name "input-type", onClick (RadioMsg URL)][]
            ],
            label[]
            [ 
                text "text",
                input[type_ "radio", name "input-type", onClick (RadioMsg TEXT)][]
            ]
          ]
        ],
        label[]
        [
          input[class model.inputFieldUrl, placeholder "Input URL"][]
        ],
        label[]
        [
            text "Convert From ",
            select []
            [
              option[onClick (UpdateFrom "guess")][text "Guess"],
              option[onClick (UpdateFrom "json")][text "JSON"],
              option[onClick (UpdateFrom "rdfxml")][text "RDF/XML"],
              option[onClick (UpdateFrom "php")][text "RDF/PHP"],
              option[onClick (UpdateFrom "turtle")][text "Turtle"],
              option[onClick (UpdateFrom "ntriples")][text "N-Triples"],
              option[onClick (UpdateFrom "jsonld")][text "JSON-LD"]
            ]
        ],
        label[]
        [
            text "Convert To ",
            select []
            [
              option[onClick (UpdateTo "json")][text "JSON"],
              option[onClick (UpdateTo "rdfxml")][text "RDF/XML"],
              option[onClick (UpdateTo "php")][text "RDF/PHP"],
              option[onClick (UpdateTo "turtle")][text "Turtle"],
              option[onClick (UpdateTo "ntriples")][text "N-Triples"],
              option[onClick (UpdateTo "jsonld")][text "JSON-LD"]
            ]
        ],
        button [onClick DisplayConversionResult] [text "Convert"]
      ],
      div[class "row content-area"]
      [
        div[id "conversion-input", class model.inputFieldText]
        [
          label[]
          [
              text "Input", 
              textarea [cols 60, rows 15, placeholder "test", value model.inputFieldTextValue, onInput UpdateInputFieldTextValue] []
          ]
        ],
        div[]
        [
          label[]
          [
              text "Result", 
              pre [class "prettyprint"] 
              [
                code[id "code"][text model.conversionResult]
              ]
          ]
        ]
      ]
    ]
  ]
    
  
getConversion : Model -> Cmd Msg
getConversion model =
  Http.request
    { method = "POST"
    , headers = []
    , url = "http://10.31.128.87:8080/tool/RDFConverter/easyrdf/converter.php"
    , body = Http.stringBody "application/x-www-form-urlencoded" (test model.inputFieldTextValue model.raw model.convertFrom model.convertTo)
    , expect = Http.expectString GotConversion
    , timeout = Nothing
    , tracker = Nothing
    }

test : String -> String -> String -> String -> String
test input isRaw formatIn formatOut =
  "data=" ++ input ++ "&raw=" ++ isRaw ++ "&input_format=" ++ formatIn ++ "&output_format=" ++ formatOut

formatJsonResult : String -> String
formatJsonResult input = 
  case Json.Print.prettyString config input of
    Ok val -> val
    Err val -> "FAILURE"

queryBuilder : String -> String
queryBuilder input = 
  B.toQuery
    [B.string "" "" ,B.string "data" input, B.string "raw" "true", B.string "input_format" "rdfxml", B.string "output_format" "json"]
    
dlog : Model -> Cmd Msg
dlog model =
  --(log (test model.inputFieldTextValue))
  Cmd.none

updateConfig : Int -> Config -> Config
updateConfig indent conf =
  { conf | indent = indent }