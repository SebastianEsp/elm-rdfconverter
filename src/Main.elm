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

-- Sends a json encoded string to javascript for formatting
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

    -- Updates the model with result of easyrdf conversion
    DisplayConversionResult -> 
      ({ model | content = model.conversionResult }, getConversion model)

    UpdateFrom val ->
      ({ model | convertFrom = val }, Cmd.none)
      
    UpdateTo val ->
      ({ model | convertTo = val }, Cmd.none)

    -- Handles the state of the radio buttons, and the associated effects
    RadioMsg state ->
      case state of
        URL ->
          ({ model | radioState = state, inputFieldText = "hidden", inputFieldUrl = "shown"}, Cmd.none)
        TEXT ->
          ({ model | radioState = state, inputFieldText = "shown", inputFieldUrl = "hidden"}, Cmd.none)

    -- Fired when a conversion is finnished. If the conversion is successfull update model with result. Else print error message
    GotConversion result ->
      case result of
        Ok url ->
          if model.convertTo == "json" then (model, prettify url)
          else ({model | conversionResult = url}, Cmd.none)

        Err url ->
          ({model | conversionResult = "FAILURE"}, Cmd.none)
    
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
    
-- Http Post call to the easyrdf converter  
getConversion : Model -> Cmd Msg
getConversion model =
  Http.request
    { method = "POST"
    , headers = []
    , url = "http://10.31.128.87:8080/tool/RDFConverter/easyrdf/converter.php"
    , body = Http.stringBody "application/x-www-form-urlencoded" (queryBuilder model.inputFieldTextValue model.raw model.convertFrom model.convertTo)
    , expect = Http.expectString GotConversion
    , timeout = Nothing
    , tracker = Nothing
    }

-- Constructs an http query string
queryBuilder : String -> String -> String -> String -> String
queryBuilder input isRaw formatIn formatOut =
  "data=" ++ input ++ "&raw=" ++ isRaw ++ "&input_format=" ++ formatIn ++ "&output_format=" ++ formatOut

-- Prints a message to the console
dlog : Model -> Cmd Msg
dlog model =
  (log (model.convertTo))
  Cmd.none