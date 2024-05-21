namespace Interlude.Features.Play.Practice

open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Data
open Prelude.Gameplay
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
        VisualOffset: float32
        HitPosition: float32
        ScrollSpeed: float32
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

    let update_suggestions (scoring: ScoreMetric) (state: PracticeState) =

        let mutable sum = 0.0f<ms>
        let mutable count = 1.0f

        for ev in scoring.HitEvents do
            match ev.Guts with
            | Hit x when not x.Missed ->
                sum <- sum + x.Delta
                count <- count + 1.0f
            | _ -> ()

        let mean = sum / count * SelectedChart.rate.Value

        let hit_position_suggestion =
            options.HitPosition.Value - mean * options.ScrollSpeed.Value * 1.0f< / ms>

        let expected_pixels = (1080.0f - options.HitPosition.Value) * 0.6f
        let current_lead_time = expected_pixels / (options.ScrollSpeed.Value * 1.0f< / ms>)
        let desired_lead_time = current_lead_time - mean
        let scroll_speed_suggestion = expected_pixels / float32 desired_lead_time

        let visual_offset_suggestion =
            if options.AudioVolume.Value = 0.0 then
                options.VisualOffset.Value + mean / 1.0f<ms>
            else
                options.VisualOffset.Value

        let local_audio_offset_suggestion =
            let local_audio_offset = LocalAudioSync.offset_setting state.SaveData

            if options.AudioVolume.Value > 0.0 then
                local_audio_offset.Value - mean
            else
                local_audio_offset.Value

        state.SyncSuggestions <-
            Some
                {
                    LooksAboutRight = Time.abs mean < 5.0f<ms>
                    AudioOffset = local_audio_offset_suggestion
                    VisualOffset = visual_offset_suggestion
                    HitPosition = hit_position_suggestion
                    ScrollSpeed = scroll_speed_suggestion
                }

    let accept_suggestion (state: PracticeState) =
        match state.SyncSuggestions with
        | None -> ()
        | Some suggestions ->

        match state.SyncMode.Value with
        | SyncMode.AUDIO_OFFSET -> (LocalAudioSync.offset_setting state.SaveData).Set suggestions.AudioOffset
        | SyncMode.HIT_POSITION -> (options.HitPosition |> Setting.roundf 0).Set suggestions.HitPosition
        | SyncMode.SCROLL_SPEED -> (options.ScrollSpeed |> Setting.roundf 2).Set suggestions.ScrollSpeed
        | SyncMode.VISUAL_OFFSET -> (options.VisualOffset |> Setting.roundf 0).Set suggestions.VisualOffset

        state.SyncSuggestions <- None
