namespace Interlude.Features.Play.Practice

open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

[<RequireQualifiedAccess>]
type SyncMode =
    | AUDIO_OFFSET
    | HIT_POSITION
    | SCROLL_SPEED
    | VISUAL_OFFSET
    member this.Audio =
        match this with
        | AUDIO_OFFSET -> 2
        | VISUAL_OFFSET -> 0
        | HIT_POSITION
        | SCROLL_SPEED -> 1

type SyncSuggestions =
    {
        LooksAboutRight: bool
        VisualOffset: float32<ms / rate>
        HitPosition: float32
        ScrollSpeed: float32<rate / ms>
        AudioOffset: Time
    }

type PracticeState =
    {
        Chart: Chart
        SaveData: ChartSaveData
        Paused: Setting<bool>
        SyncMode: Setting<SyncMode>
        mutable SyncSuggestions: SyncSuggestions option
        PracticePoint: Setting.Bounded<Time>
    }

module PracticeState =

    let update_suggestions (scoring: ScoreProcessor) (state: PracticeState) : unit =

        let mutable sum = 0.0f<ms / rate>
        let mutable count = 1.0f

        for ev in scoring.Events do
            match ev.Action with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | Hold x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()

        let mean = sum / count

        let hit_position_suggestion =
            options.HitPosition.Value - mean * options.ScrollSpeed.Value

        let expected_pixels = (1080.0f - options.HitPosition.Value) * 0.6f
        let current_lead_time = expected_pixels / options.ScrollSpeed.Value
        let desired_lead_time = current_lead_time - mean
        let scroll_speed_suggestion = expected_pixels / desired_lead_time

        let visual_offset_suggestion =
            if options.AudioVolume.Value = 0.0 then
                options.VisualOffset.Value + mean
            else
                options.VisualOffset.Value

        let local_audio_offset_suggestion =
            let local_audio_offset = LocalOffset.offset_setting state.SaveData

            if options.AudioVolume.Value > 0.0 then
                local_audio_offset.Value - mean * SelectedChart.rate.Value
            else
                local_audio_offset.Value

        state.SyncSuggestions <-
            Some
                {
                    LooksAboutRight = abs mean < 5.0f<ms / rate>
                    AudioOffset = local_audio_offset_suggestion
                    VisualOffset = visual_offset_suggestion
                    HitPosition = hit_position_suggestion
                    ScrollSpeed = scroll_speed_suggestion
                }

    let accept_suggested_offset (state: PracticeState) : unit =
        match state.SyncSuggestions with
        | None -> ()
        | Some suggestions ->

        match state.SyncMode.Value with
        | SyncMode.AUDIO_OFFSET -> (LocalOffset.offset_setting state.SaveData).Set suggestions.AudioOffset
        | SyncMode.HIT_POSITION -> (options.HitPosition |> Setting.roundf 0).Set suggestions.HitPosition
        | SyncMode.SCROLL_SPEED -> (options.ScrollSpeed |> Setting.roundf_uom 2).Set suggestions.ScrollSpeed
        | SyncMode.VISUAL_OFFSET -> (options.VisualOffset |> Setting.roundf_uom 0).Set suggestions.VisualOffset

        state.SyncSuggestions <- None

    let reset_offset (state: PracticeState) : unit =
        match state.SyncMode.Value with
        | SyncMode.AUDIO_OFFSET -> (LocalOffset.offset_setting state.SaveData).Set 0.0f<ms>
        | SyncMode.HIT_POSITION -> (options.HitPosition |> Setting.roundf 0).Set 0.0f
        | SyncMode.SCROLL_SPEED -> (options.ScrollSpeed |> Setting.roundf_uom 2).Set 2.05f<rate / ms>
        | SyncMode.VISUAL_OFFSET -> (options.VisualOffset |> Setting.roundf_uom 0).Set 0.0f<ms / rate>

        state.SyncSuggestions <- None