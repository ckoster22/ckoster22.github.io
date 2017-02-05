module Views.ContentArea.LTTable.LightningTalks.View exposing (viewWithFilter)

import Html exposing (Html, div, span, ul, li, text, input, button, h3, h4)
import Html.Attributes exposing (class, classList, value, title)
import Html.Events exposing (onClick, onInput)
import Model.Model as Model exposing (Data, Model(..), Modifier(..), Msg(..), Page(..), Timeslot)
import Views.Icons.Icon exposing (icon)
import Model.RoundModel as Round
import Model.LightningTalkModel as LightningTalk
import Helpers.DateHelper exposing (millisecondsInHour, getDateFromEpoch, convertMonthToString)
import Helpers.ModelHelper as ModelHelper
import Time exposing (Time)
import Date exposing (Date)


noThemePlaceholder : String
noThemePlaceholder =
    "(No Theme)"


viewWithFilter : (Date -> Round.Model -> Bool) -> Page -> Data -> Modifier -> Html Model.Msg
viewWithFilter filter page data modifier =
    div [ class "talk-list-container flex-auto" ]
        [ roundsContainer filter page data modifier
        ]


getFilteredRounds : (Date -> Round.Model -> Bool) -> Data -> List Round.Model
getFilteredRounds filter data =
    let
        curriedUpcomingRoundFilter : Round.Model -> Bool
        curriedUpcomingRoundFilter =
            filter data.initialTime
    in
        List.filter curriedUpcomingRoundFilter data.rounds


roundsContainer : (Date -> Round.Model -> Bool) -> Page -> Data -> Modifier -> Html Model.Msg
roundsContainer filter page data modifier =
    let
        maybeTimeslot =
            case ( page, modifier ) of
                ( UpcomingTalks, WithTimeslotSelected timeslot ) ->
                    Just timeslot

                ( PreviousTalks, WithTimeslotSelected timeslot ) ->
                    Just timeslot

                _ ->
                    Nothing

        curriedRoundElementFunc : Round.Model -> Html Msg
        curriedRoundElementFunc =
            roundElement page modifier maybeTimeslot

        roundsToDisplay =
            case page of
                PreviousTalks ->
                    getFilteredRounds filter data
                        |> List.sortBy .startDateTime
                        |> List.reverse

                _ ->
                    getFilteredRounds filter data
                        |> List.sortBy .startDateTime
    in
        div []
            (List.map curriedRoundElementFunc roundsToDisplay)


roundElement : Page -> Modifier -> Maybe Timeslot -> Round.Model -> Html Msg
roundElement page modifier maybeSelectedTimeslot round =
    let
        curriedTimeslotElementFunc : Maybe LightningTalk.Model -> Time -> Bool -> Html Msg
        curriedTimeslotElementFunc =
            timeslotElement round

        ( isEditingRoundTitle, roundToShow ) =
            case ( page, modifier ) of
                ( UpcomingTalks, WithRoundEditing editRound ) ->
                    if editRound.id == round.id then
                        ( True, editRound )
                    else
                        ( False, round )

                _ ->
                    ( False, round )

        slot1Div =
            if round.slot1 /= Nothing || page == UpcomingTalks then
                [ curriedTimeslotElementFunc round.slot1 0 (isSlotSelected maybeSelectedTimeslot round 0) ]
            else
                []

        slot2Div =
            if round.slot2 /= Nothing || page == UpcomingTalks then
                [ curriedTimeslotElementFunc round.slot2 600000 (isSlotSelected maybeSelectedTimeslot round 600000) ]
            else
                []

        slot3Div =
            if round.slot3 /= Nothing || page == UpcomingTalks then
                [ curriedTimeslotElementFunc round.slot3 1200000 (isSlotSelected maybeSelectedTimeslot round 1200000) ]
            else
                []

        slot4Div =
            if round.slot4 /= Nothing || page == UpcomingTalks then
                [ curriedTimeslotElementFunc round.slot4 1800000 (isSlotSelected maybeSelectedTimeslot round 1800000) ]
            else
                []
    in
        div []
            [ div [ class "round-title" ]
                [ span [] [ text <| getDateFromEpoch roundToShow.startDateTime ++ ": " ]
                , getRoundThemeHtml isEditingRoundTitle roundToShow
                ]
            , div []
                (slot4Div
                    |> List.append slot3Div
                    |> List.append slot2Div
                    |> List.append slot1Div
                )
            ]


isSlotSelected : Maybe Timeslot -> Round.Model -> Time -> Bool
isSlotSelected maybeTimeslot round expectedOffset =
    case maybeTimeslot of
        Just timeslot ->
            timeslot.round == round && timeslot.offset == expectedOffset

        Nothing ->
            False


timeslotElement : Round.Model -> Maybe LightningTalk.Model -> Time -> Bool -> Html Msg
timeslotElement round maybeTalk offset isSelected =
    case maybeTalk of
        Just talk ->
            div
                [ onClick (SelectTimeslot (Timeslot round offset maybeTalk))
                , classList
                    [ ( "selected", isSelected )
                    , ( "talk-item", True )
                    , ( "flex", True )
                    ]
                ]
                [ div [ class "flex-auto" ]
                    [ div [ class "talk-title" ] [ text talk.topic ]
                    , div [ class "talk-author" ] [ text talk.speakers ]
                    ]
                , div [ class "talk-time" ] [ text <| ModelHelper.getTalkStartTime (round.startDateTime + offset) ]
                ]

        Nothing ->
            div
                [ onClick (SelectTimeslot (Timeslot round offset maybeTalk))
                , classList
                    [ ( "selected", isSelected )
                    , ( "talk-item", True )
                    , ( "flex", True )
                    ]
                ]
                [ div [ class "flex-auto" ]
                    [ div [ class "talk-title-empty" ] [ text "Empty Time Slot" ]
                    ]
                , div [ class "talk-time" ] [ text <| ModelHelper.getTalkStartTime (round.startDateTime + offset) ]
                ]


getRoundThemeHtml : Bool -> Round.Model -> Html Msg
getRoundThemeHtml isEditingRoundTitle round =
    if isEditingRoundTitle then
        span []
            [ input [ value round.theme, onInput (\input -> UpdateRound ({ round | theme = input })) ] []
            , div [ class "round-update-button save", onClick UpdateRoundSubmit, title "Save changes" ] [ icon "icon-check" ]
            , div [ class "round-update-button cancel", onClick Cancel, title "Cancel" ] [ icon "icon-cancel" ]
            ]
    else if String.isEmpty round.theme then
        span [ onClick (SetRoundIsEditing round) ] [ text noThemePlaceholder ]
    else
        span [ onClick (SetRoundIsEditing round) ] [ text round.theme ]
