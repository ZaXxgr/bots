module Sanderling exposing
    ( EffectOnWindowStructure(..)
    , GetMemoryMeasurementResultStructure(..)
    , Location2d
    , MouseButton(..)
    , RequestToVolatileHost(..)
    , ResponseFromVolatileHost(..)
    , VirtualKeyCode(..)
    , buildScriptToGetResponseFromVolatileHost
    , centerFromRegion
    , deserializeResponseFromVolatileHost
    , effectMouseClickAtLocation
    )

import Json.Decode
import Json.Decode.Extra
import Json.Encode
import SanderlingMemoryMeasurement


type RequestToVolatileHost
    = GetEveOnlineProcessesIds
    | GetMemoryMeasurement GetMemoryMeasurementStructure
    | EffectOnWindow (TaskOnWindowStructure EffectOnWindowStructure)


type ResponseFromVolatileHost
    = EveOnlineProcessesIds (List Int)
    | GetMemoryMeasurementResult GetMemoryMeasurementResultStructure


type alias GetMemoryMeasurementStructure =
    { processId : Int }


type GetMemoryMeasurementResultStructure
    = ProcessNotFound
    | Completed MemoryMeasurementCompleted


type alias MemoryMeasurementCompleted =
    { mainWindowId : WindowId
    , reducedWithNamedNodesJson : Maybe String
    }


type alias TaskOnWindowStructure task =
    { windowId : WindowId
    , bringWindowToForeground : Bool
    , task : task
    }


{-| Using names from Windows API and <https://www.nuget.org/packages/InputSimulator/>
-}
type
    EffectOnWindowStructure
    {-
       = MouseMoveTo MouseMoveToStructure
       | MouseButtonDown MouseButtonChangeStructure
       | MouseButtonUp MouseButtonChangeStructure
       | MouseHorizontalScroll Int
       | MouseVerticalScroll Int
       | KeyboardKeyDown VirtualKeyCode
       | KeyboardKeyUp VirtualKeyCode
       | TextEntry String
    -}
    = SimpleMouseClickAtLocation MouseClickAtLocation
    | KeyDown VirtualKeyCode
    | KeyUp VirtualKeyCode


type alias MouseMoveToStructure =
    { location : LocationRelativeToWindow }


type alias MouseButtonChangeStructure =
    { location : LocationRelativeToWindow
    , button : MouseButton
    }


type VirtualKeyCode
    = VirtualKeyCodeFromInt Int
      -- Names from https://docs.microsoft.com/en-us/windows/desktop/inputdev/virtual-key-codes
    | VK_SHIFT
    | VK_CONTROL
    | VK_MENU
    | VK_ESCAPE


type LocationRelativeToWindow
    = ClientArea Location2d -- https://docs.microsoft.com/en-us/windows/desktop/api/winuser/nf-winuser-clienttoscreen


type alias WindowId =
    String


type alias MouseClickAtLocation =
    { location : Location2d
    , mouseButton : MouseButton
    }


type MouseButton
    = MouseButtonLeft
    | MouseButtonRight


type alias Location2d =
    { x : Int, y : Int }


deserializeResponseFromVolatileHost : String -> Result Json.Decode.Error ResponseFromVolatileHost
deserializeResponseFromVolatileHost =
    Json.Decode.decodeString decodeResponseFromVolatileHost


decodeResponseFromVolatileHost : Json.Decode.Decoder ResponseFromVolatileHost
decodeResponseFromVolatileHost =
    Json.Decode.oneOf
        [ Json.Decode.field "eveOnlineProcessesIds" (Json.Decode.list Json.Decode.int)
            |> Json.Decode.map EveOnlineProcessesIds
        , Json.Decode.field "getMemoryMeasurementResult" decodeGetMemoryMeasurementResult
            |> Json.Decode.map GetMemoryMeasurementResult
        ]


encodeRequestToVolatileHost : RequestToVolatileHost -> Json.Encode.Value
encodeRequestToVolatileHost request =
    case request of
        GetEveOnlineProcessesIds ->
            Json.Encode.object [ ( "getEveOnlineProcessesIds", Json.Encode.object [] ) ]

        GetMemoryMeasurement getMemoryMeasurement ->
            Json.Encode.object [ ( "getMemoryMeasurement", getMemoryMeasurement |> encodeGetMemoryMeasurement ) ]

        EffectOnWindow taskOnWindow ->
            Json.Encode.object [ ( "effectOnWindow", taskOnWindow |> encodeTaskOnWindow encodeEffectOnWindowStructure ) ]


encodeTaskOnWindow : (task -> Json.Encode.Value) -> TaskOnWindowStructure task -> Json.Encode.Value
encodeTaskOnWindow taskEncoder taskOnWindow =
    Json.Encode.object
        [ ( "windowId", taskOnWindow.windowId |> Json.Encode.string )
        , ( "bringWindowToForeground", taskOnWindow.bringWindowToForeground |> Json.Encode.bool )
        , ( "task", taskOnWindow.task |> taskEncoder )
        ]


encodeEffectOnWindowStructure : EffectOnWindowStructure -> Json.Encode.Value
encodeEffectOnWindowStructure effectOnWindow =
    case effectOnWindow of
        SimpleMouseClickAtLocation mouseClickAtLocation ->
            Json.Encode.object
                [ ( "simpleMouseClickAtLocation", mouseClickAtLocation |> encodeMouseClickAtLocation )
                ]

        KeyDown virtualKeyCode ->
            Json.Encode.object
                [ ( "keyDown", virtualKeyCode |> encodeKey )
                ]

        KeyUp virtualKeyCode ->
            Json.Encode.object
                [ ( "keyUp", virtualKeyCode |> encodeKey )
                ]


encodeKey : VirtualKeyCode -> Json.Encode.Value
encodeKey virtualKeyCode =
    Json.Encode.object [ ( "virtualKeyCode", virtualKeyCode |> virtualKeyCodeAsInteger |> Json.Encode.int ) ]


encodeMouseClickAtLocation : MouseClickAtLocation -> Json.Encode.Value
encodeMouseClickAtLocation mouseClickAtLocation_ =
    Json.Encode.object
        [ ( "location", mouseClickAtLocation_.location |> encodeLocation2d )
        , ( "mouseButton", mouseClickAtLocation_.mouseButton |> encodeMouseButton )
        ]


encodeLocation2d : Location2d -> Json.Encode.Value
encodeLocation2d location =
    Json.Encode.object
        [ ( "x", location.x |> Json.Encode.int )
        , ( "y", location.y |> Json.Encode.int )
        ]


encodeMouseButton : MouseButton -> Json.Encode.Value
encodeMouseButton mouseButton =
    (case mouseButton of
        MouseButtonLeft ->
            "left"

        MouseButtonRight ->
            "right"
    )
        |> Json.Encode.string


encodeGetMemoryMeasurement : GetMemoryMeasurementStructure -> Json.Encode.Value
encodeGetMemoryMeasurement getMemoryMeasurement =
    Json.Encode.object [ ( "processId", getMemoryMeasurement.processId |> Json.Encode.int ) ]


decodeGetMemoryMeasurementResult : Json.Decode.Decoder GetMemoryMeasurementResultStructure
decodeGetMemoryMeasurementResult =
    Json.Decode.oneOf
        [ Json.Decode.field "processNotFound" (Json.Decode.succeed ProcessNotFound)
        , Json.Decode.field "completed" decodeMemoryMeasurementCompleted
            |> Json.Decode.map Completed
        ]


decodeMemoryMeasurementCompleted : Json.Decode.Decoder MemoryMeasurementCompleted
decodeMemoryMeasurementCompleted =
    Json.Decode.map2 MemoryMeasurementCompleted
        (Json.Decode.field "mainWindowId" Json.Decode.string)
        (Json.Decode.Extra.optionalField "reducedWithNamedNodesJson" Json.Decode.string)


buildScriptToGetResponseFromVolatileHost : RequestToVolatileHost -> String
buildScriptToGetResponseFromVolatileHost request =
    "serialRequest("
        ++ (request
                |> encodeRequestToVolatileHost
                |> Json.Encode.encode 0
                |> Json.Encode.string
                |> Json.Encode.encode 0
           )
        ++ ")"


centerFromRegion : SanderlingMemoryMeasurement.UIElementRegion -> Location2d
centerFromRegion region =
    { x = (region.left + region.right) // 2, y = (region.top + region.bottom) // 2 }


effectMouseClickAtLocation : Location2d -> MouseButton -> EffectOnWindowStructure
effectMouseClickAtLocation location mouseButton =
    SimpleMouseClickAtLocation
        { location = location, mouseButton = mouseButton }


virtualKeyCodeAsInteger : VirtualKeyCode -> Int
virtualKeyCodeAsInteger keyCode =
    -- Mapping from https://docs.microsoft.com/en-us/windows/desktop/inputdev/virtual-key-codes
    case keyCode of
        VirtualKeyCodeFromInt asInt ->
            asInt

        VK_SHIFT ->
            0x10

        VK_CONTROL ->
            0x11

        VK_MENU ->
            0x12

        VK_ESCAPE ->
            0x1B
