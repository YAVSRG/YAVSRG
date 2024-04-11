namespace Interlude.Features.Play.Practice

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Play

type private ModeButton(label: string, mode: SyncMode, state: PracticeState) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()

                if state.SyncMode.Value = mode then
                    PracticeState.accept_suggestion state
                else
                    state.SyncMode.Set mode
            )
        )

    let requires_audio = mode.Audio = 2

    override this.Init(parent) =
        this
        |+ Text(
            label,
            Color =
                fun () ->
                    if requires_audio <> (options.AudioVolume.Value > 0.0) then
                        Colors.text_greyout
                    elif this.Focused then
                        Colors.text_yellow_2
                    else
                        Colors.text
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if state.SyncMode.Value = mode then
            Draw.rect this.Bounds Colors.shadow_2.O3

        base.Draw()

type SyncSuggestionControls(state: PracticeState) =
    inherit
        SlideoutContent(
            FlowContainer.Vertical<ModeButton>(40.0f, Position = Position.SliceLeft(250.0f))
            |+ ModeButton(%"practice.localoffset.name", SyncMode.AUDIO_OFFSET, state)
            |+ ModeButton(%"gameplay.hitposition.name", SyncMode.HIT_POSITION, state)
            |+ ModeButton(%"gameplay.scrollspeed.name", SyncMode.SCROLL_SPEED, state)
            |+ ModeButton(%"system.visualoffset.name", SyncMode.VISUAL_OFFSET, state),
            160.0f
        )

    let local_audio_offset =
        LocalAudioSync.offset_setting state.SaveData
        |> Setting.bound -200.0f<ms> 200.0f<ms>

    let visual_offset = options.VisualOffset |> Setting.roundf 0
    let scroll_speed = options.ScrollSpeed |> Setting.roundf 2
    let hit_position = options.HitPosition |> Setting.roundf 0

    override this.Init(parent) =

        let suggestion_hint = %"practice.suggestions.hint"
        let about_right_hint = Icons.CHECK + " " + %"practice.suggestions.aboutright"

        let current = %"practice.suggestions.current"
        let suggested = %"practice.suggestions.suggested"

        this

        |+ Conditional(
            (fun () -> state.SyncMode.Value = SyncMode.HIT_POSITION),
            FlowContainer.Vertical<Widget>(50.0f, Spacing = 5.0f)
            |+ Text(fun () -> sprintf "%s: %.0f" current hit_position.Value)
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s -> sprintf "%s: %.0f" suggested s.HitPosition
                | None -> suggestion_hint
            )
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s when s.LooksAboutRight -> about_right_hint
                | _ -> ""
            ),
            Position = Position.Box(0.0f, 0.0f, 270.0f, 0.0f, 500.0f, 200.0f)
        )
        |+ Conditional(
            (fun () -> state.SyncMode.Value = SyncMode.SCROLL_SPEED),
            FlowContainer.Vertical<Widget>(50.0f, Spacing = 5.0f)
            |+ Text(fun () -> sprintf "%s: %.0f%%" current (100.0f * scroll_speed.Value))
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s -> sprintf "%s: %.0f%%" suggested (100.0f * s.ScrollSpeed)
                | None -> suggestion_hint
            )
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s when s.LooksAboutRight -> about_right_hint
                | _ -> ""
            ),
            Position = Position.Box(0.0f, 0.0f, 270.0f, 0.0f, 500.0f, 200.0f)
        )
        |+ Conditional(
            (fun () -> state.SyncMode.Value = SyncMode.VISUAL_OFFSET && options.AudioVolume.Value = 0.0),
            FlowContainer.Vertical<Widget>(50.0f, Spacing = 5.0f)
            |+ Text(fun () -> sprintf "%s: %.0f" current visual_offset.Value)
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s -> sprintf "%s: %.0f" suggested s.VisualOffset
                | None -> suggestion_hint
            )
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s when s.LooksAboutRight -> about_right_hint
                | _ -> ""
            ),
            Position = Position.Box(0.0f, 0.0f, 270.0f, 0.0f, 500.0f, 200.0f)
        )
        |+ Conditional(
            (fun () -> state.SyncMode.Value = SyncMode.AUDIO_OFFSET && options.AudioVolume.Value > 0.0),
            FlowContainer.Vertical<Widget>(50.0f, Spacing = 5.0f)
            |+ Text(fun () -> sprintf "%s: %.0f" current local_audio_offset.Value)
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s -> sprintf "%s: %.0f" suggested s.AudioOffset
                | None -> suggestion_hint
            )
            |+ Text(fun () ->
                match state.SyncSuggestions with
                | Some s when s.LooksAboutRight -> about_right_hint
                | _ -> ""
            ),
            Position = Position.Box(0.0f, 0.0f, 270.0f, 0.0f, 500.0f, 200.0f)
        )

        |+ Conditional(
            (fun () -> state.SyncMode.Value.Audio = 0 && options.AudioVolume.Value > 0.0),
            Callout.frame
                (Callout.Small.Icon(Icons.VOLUME_X).Body(%"practice.mute_mandatory_hint"))
                (fun (w, h) -> Position.Box(1.0f, 0.0f, -w - 20.0f, 20.0f, w, h))
        )
        |+ Conditional(
            (fun () -> state.SyncMode.Value.Audio = 1 && options.AudioVolume.Value > 0.0),
            Callout.frame
                (Callout.Small.Icon(Icons.VOLUME_X).Body(%"practice.mute_hint"))
                (fun (w, h) -> Position.Box(1.0f, 0.0f, -w - 20.0f, 20.0f, w, h))
        )
        |+ Conditional(
            (fun () -> state.SyncMode.Value.Audio = 2 && options.AudioVolume.Value = 0.0),
            Callout.frame
                (Callout.Small.Icon(Icons.VOLUME_2).Body(%"practice.unmute_hint"))
                (fun (w, h) -> Position.Box(1.0f, 0.0f, -w - 20.0f, 20.0f, w, h))
        )
        |* Dummy() // todo: button to reset offsets

        base.Init parent
