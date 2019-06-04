{- This is an EVE Online High Sec Courier Bot, based on the description from dew1960 at https://forum.botengine.org/t/easier-setup-of-mission-running-bot-for-eve-online/1104/18?u=viir (The decline mission part is waiting for an answer to the question in https://forum.botengine.org/t/easier-setup-of-mission-running-bot-for-eve-online/1104/23?u=viir)

   To use this bot:
   Start in Station with an agent you have available for distribution mission.

   bot-catalog-tags:eve-online,courier,mission
-}


module Main exposing
    ( InterfaceBotState
    , State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import Bot_Interface_To_Host_20190529 as InterfaceToHost
import Sanderling exposing (MouseButton(..), centerFromRegion, effectMouseClickAtLocation)
import SanderlingMemoryMeasurement exposing (InfoPanelRouteRouteElementMarker, MemoryMeasurementShipUi)
import SimpleSanderling exposing (BotEventAtTime, BotRequest(..))


type alias MemoryMeasurement =
    SanderlingMemoryMeasurement.MemoryMeasurementReducedWithNamedNodes


{-| The autopilot bot does not need to remember anything from the past; the information on the game client screen is sufficient to decide what to do next.
Therefore we need no state and use an empty tuple '()' to define the type of the state.
-}
type State
    = Travel
    | Courier CourierState


type CourierState
    = StartConversationWithAgent
    | CheckRoute
    | AcceptMission
    | OpenInventory
    | MoveMissionItemToTransportShipCargoHold
    | SetDestination
    | Undock


type TravelProcessRequest
    = CompleteTravel
    | ContinueTravel ( List BotRequest, String )


initState : State
initState =
    Courier StartConversationWithAgent


processEvent : BotEventAtTime -> State -> { newState : State, requests : List BotRequest, statusMessage : String }
processEvent eventAtTime stateBefore =
    case eventAtTime.event of
        SimpleSanderling.MemoryMeasurementCompleted memoryMeasurement ->
            let
                ( newState, requests, statusMessage ) =
                    case stateBefore of
                        Travel ->
                            let
                                ( newStateFromTravel, travelRequests, travelStatusMessage ) =
                                    case travelBotRequestsFromGameClientState memoryMeasurement of
                                        CompleteTravel ->
                                            ( Courier StartConversationWithAgent
                                            , [ TakeMemoryMeasurementAfterDelayInMilliseconds 2000 ]
                                            , "Complete Travel"
                                            )

                                        ContinueTravel ( continueTravelRequests, continueTravelStatusMessage ) ->
                                            ( Travel, continueTravelRequests, continueTravelStatusMessage )
                            in
                            ( newStateFromTravel, travelRequests, "Travel:\n" ++ travelStatusMessage )

                        Courier courierStateBefore ->
                            let
                                ( newStateFromCourier, courierRequests, courierStatusMessage ) =
                                    courierLessTravelBotRequestsFromState memoryMeasurement courierStateBefore
                            in
                            ( newStateFromCourier
                            , courierRequests ++ [ TakeMemoryMeasurementAfterDelayInMilliseconds 2000 ]
                            , "Courier:\n" ++ courierStatusMessage
                            )
            in
            { newState = newState
            , requests = requests
            , statusMessage = statusMessage
            }

        SimpleSanderling.SetBotConfiguration botConfiguration ->
            { newState = stateBefore
            , requests = []
            , statusMessage =
                if botConfiguration |> String.isEmpty then
                    ""

                else
                    "I have a problem with this configuration: I am not programmed to support configuration at all. Maybe the bot catalog (https://to.botengine.org/bot-catalog) has a bot which better matches your use case?"
            }


travelBotRequestsFromGameClientState : MemoryMeasurement -> TravelProcessRequest
travelBotRequestsFromGameClientState memoryMeasurement =
    case memoryMeasurement |> infoPanelRouteFirstMarkerFromMemoryMeasurement of
        Nothing ->
            CompleteTravel

        Just infoPanelRouteFirstMarker ->
            case memoryMeasurement.shipUi of
                Nothing ->
                    ContinueTravel
                        ( [ TakeMemoryMeasurementAfterDelayInMilliseconds 4000 ]
                        , "I cannot see if the ship is warping or jumping. I wait for the ship UI to appear on the screen."
                        )

                Just shipUi ->
                    if shipUi |> isShipWarpingOrJumping then
                        ContinueTravel
                            ( [ TakeMemoryMeasurementAfterDelayInMilliseconds 4000 ]
                            , "I see the ship is warping or jumping. I wait until that maneuver ends."
                            )

                    else
                        let
                            ( requests, statusMessage ) =
                                botRequestsWhenNotWaitingForShipManeuver
                                    memoryMeasurement
                                    infoPanelRouteFirstMarker
                        in
                        ContinueTravel
                            ( requests ++ [ TakeMemoryMeasurementAfterDelayInMilliseconds 2000 ], statusMessage )


courierLessTravelBotRequestsFromState : MemoryMeasurement -> CourierState -> ( State, List BotRequest, String )
courierLessTravelBotRequestsFromState memoryMeasurement stateBefore =
    case stateBefore of
        StartConversationWithAgent ->
            ( Courier CheckRoute, [], "Start Conversation with Agent" )

        CheckRoute ->
            ( Courier AcceptMission, [], "Check that the route does not go through Low Sec." )

        AcceptMission ->
            ( Courier OpenInventory, [], "Accept mission." )

        OpenInventory ->
            ( Courier MoveMissionItemToTransportShipCargoHold, [], "Open inventory." )

        MoveMissionItemToTransportShipCargoHold ->
            ( Courier SetDestination, [], "Move Mission Item to Transport ship cargo hold as much as will fit." )

        SetDestination ->
            ( Courier Undock, [], "Set destination." )

        Undock ->
            ( Travel, [], "Undock." )


botRequestsWhenNotWaitingForShipManeuver :
    MemoryMeasurement
    -> InfoPanelRouteRouteElementMarker
    -> ( List BotRequest, String )
botRequestsWhenNotWaitingForShipManeuver memoryMeasurement infoPanelRouteFirstMarker =
    let
        openMenuAnnouncementAndEffect =
            ( [ EffectOnGameClientWindow
                    (effectMouseClickAtLocation
                        (infoPanelRouteFirstMarker.uiElement.region |> centerFromRegion)
                        Sanderling.MouseButtonRight
                    )
              ]
            , "I click on the route marker in the info panel to open the menu."
            )
    in
    case memoryMeasurement.menus |> List.head of
        Nothing ->
            openMenuAnnouncementAndEffect

        Just firstMenu ->
            let
                maybeMenuEntryToClick =
                    firstMenu.entries
                        |> List.filter
                            (\menuEntry ->
                                let
                                    textLowercase =
                                        menuEntry.text |> String.toLower
                                in
                                (textLowercase |> String.contains "dock")
                                    || (textLowercase |> String.contains "jump")
                            )
                        |> List.head
            in
            case maybeMenuEntryToClick of
                Nothing ->
                    openMenuAnnouncementAndEffect

                Just menuEntryToClick ->
                    ( [ EffectOnGameClientWindow (effectMouseClickAtLocation (menuEntryToClick.uiElement.region |> centerFromRegion) MouseButtonLeft) ]
                    , "I click on the menu entry '" ++ menuEntryToClick.text ++ "' to start the next ship maneuver."
                    )


infoPanelRouteFirstMarkerFromMemoryMeasurement : MemoryMeasurement -> Maybe InfoPanelRouteRouteElementMarker
infoPanelRouteFirstMarkerFromMemoryMeasurement =
    .infoPanelRoute
        >> Maybe.map .routeElementMarker
        >> Maybe.map (List.sortBy (\routeMarker -> routeMarker.uiElement.region.left + routeMarker.uiElement.region.top))
        >> Maybe.andThen List.head


isShipWarpingOrJumping : MemoryMeasurementShipUi -> Bool
isShipWarpingOrJumping =
    .indication
        >> Maybe.andThen .maneuverType
        >> Maybe.map
            (\maneuverType ->
                [ SanderlingMemoryMeasurement.Warp, SanderlingMemoryMeasurement.Jump ]
                    |> List.member maneuverType
            )
        -- If the ship is just floating in space, there might be no indication displayed.
        >> Maybe.withDefault False


type alias InterfaceBotState =
    SimpleSanderling.StateIncludingSetup State


interfaceToHost_initState : InterfaceBotState
interfaceToHost_initState =
    SimpleSanderling.initState initState


interfaceToHost_processEvent : String -> InterfaceBotState -> ( InterfaceBotState, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent (SimpleSanderling.processEvent processEvent)


interfaceToHost_serializeState : InterfaceBotState -> String
interfaceToHost_serializeState =
    always ""


interfaceToHost_deserializeState : String -> InterfaceBotState
interfaceToHost_deserializeState =
    always interfaceToHost_initState


{-| Define the Elm entry point. Don't change this function.
-}
main : Program Int InterfaceBotState String
main =
    InterfaceToHost.elmEntryPoint interfaceToHost_initState interfaceToHost_processEvent interfaceToHost_serializeState (interfaceToHost_deserializeState >> always interfaceToHost_initState)
