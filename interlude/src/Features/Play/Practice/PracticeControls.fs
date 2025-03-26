namespace Interlude.Features.Play.Practice

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Mods
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Play

type private PracticeToolsPage(state: PracticeState) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(%"play.localoffset", fun () -> state.SyncMode.Set SyncMode.AUDIO_OFFSET; Menu.Back()).Pos(0)
        |+ PageButton(%"gameplay.hitposition", fun () -> state.SyncMode.Set SyncMode.HIT_POSITION; Menu.Back()).Pos(2)
        |+ PageButton(%"gameplay.scrollspeed", fun () -> state.SyncMode.Set SyncMode.SCROLL_SPEED; Menu.Back()).Pos(4)
        |+ PageButton(%"system.visualoffset", fun () -> state.SyncMode.Set SyncMode.VISUAL_OFFSET; Menu.Back()).Pos(6)
        :> Widget

    override this.Title = sprintf "%s %s" Icons.SETTINGS (%"practice.tools")
    override this.OnClose() = ()

type SyncSuggestionControls(state: PracticeState) =
    inherit Container(NodeType.None)

    let local_audio_offset =
        LocalOffset.offset_setting state.SaveData
        |> Setting.bound (-200.0f<ms>, 200.0f<ms>)
    let visual_offset = options.VisualOffset |> Setting.roundf_uom 0
    let scroll_speed = options.ScrollSpeed |> Setting.roundf_uom 2
    let hit_position = options.HitPosition |> Setting.roundf 0

    override this.Init(parent) =

        let suggestion_hint = %"practice.suggestions.hint"

        let current = %"practice.suggestions.current"
        let suggested = %"practice.suggestions.suggested"
        let adjust = %"practice.adjust"

        let title () =
            match state.SyncMode.Value with
            | SyncMode.AUDIO_OFFSET -> sprintf "%s: %s" adjust %"play.localoffset"
            | SyncMode.HIT_POSITION -> sprintf "%s: %s" adjust %"gameplay.hitposition"
            | SyncMode.SCROLL_SPEED -> sprintf "%s: %s" adjust %"gameplay.scrollspeed"
            | SyncMode.VISUAL_OFFSET -> sprintf "%s: %s" adjust %"system.visualoffset"

        let current_value() =
            match state.SyncMode.Value with
            | SyncMode.AUDIO_OFFSET -> sprintf "%s: %.0fms" current local_audio_offset.Value
            | SyncMode.HIT_POSITION -> sprintf "%s: %.0f" current hit_position.Value
            | SyncMode.SCROLL_SPEED -> sprintf "%s: %.0f%%" current (100.0f * scroll_speed.Value)
            | SyncMode.VISUAL_OFFSET -> sprintf "%s: %.0fms" current visual_offset.Value

        let suggested_value() =
            match state.SyncSuggestions with
            | None -> suggestion_hint
            | Some s ->
            match state.SyncMode.Value with
            | SyncMode.AUDIO_OFFSET -> sprintf "%s: %.0fms" suggested s.AudioOffset
            | SyncMode.HIT_POSITION -> sprintf "%s: %.0f" suggested s.HitPosition
            | SyncMode.SCROLL_SPEED -> sprintf "%s: %.0f%%" suggested (100.0f * s.ScrollSpeed)
            | SyncMode.VISUAL_OFFSET -> sprintf "%s: %.0fms" suggested s.VisualOffset

        this
        |+ Text(title)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(60.0f).ShrinkX(25.0f))
        |+ Text(current_value)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(60.0f, 50.0f).ShrinkX(25.0f))
        |+ Text(suggested_value)
            .Color(fun () ->
                match state.SyncSuggestions with
                | Some v when v.LooksAboutRight -> Colors.text_green_2
                | _ -> Colors.text
            )
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(110.0f, 50.0f).ShrinkX(25.0f))
        |+ Text(%"practice.unmute_hint")
            .Color(Colors.text_red_2)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(160.0f, 50.0f).ShrinkX(25.0f))
            .Conditional(fun () -> state.SyncMode.Value.Audio = 2 && options.AudioVolume.Value = 0.0)
        |+ Text(%"practice.mute_mandatory_hint")
            .Color(Colors.text_red_2)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(160.0f, 50.0f).ShrinkX(25.0f))
            .Conditional(fun () -> state.SyncMode.Value.Audio = 0 && options.AudioVolume.Value > 0.0)
        |+ Text(%"practice.mute_hint")
            .Color(Colors.text_yellow_2)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(160.0f, 50.0f).ShrinkX(25.0f))
            .Conditional(fun () -> state.SyncMode.Value.Audio = 1 && options.AudioVolume.Value > 0.0)
        |+ Text(%"practice.mute_hint")
            .Position(Position.SliceT(160.0f, 50.0f).ShrinkX(25.0f))
            .Align(Alignment.LEFT)
            .Color(Colors.text_yellow_2)
            .Conditional(fun () -> state.SyncMode.Value.Audio = 1 && options.AudioVolume.Value > 0.0)
        |+ Text(sprintf "%s: %O" (%"practice.accept_suggestion") (%%"accept_offset"))
            .Color(Colors.text_cyan)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(160.0f, 50.0f).ShrinkX(25.0f))
            .Conditional(fun () -> (state.SyncMode.Value.Audio < 2) <> (options.AudioVolume.Value <> 0.0))
        |* Text(sprintf "%s: %O" (%"practice.reset_offset") (%%"reset_offset"))
            .Color(Colors.text_cyan)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(210.0f, 50.0f).ShrinkX(25.0f))
            .Conditional(fun () -> (state.SyncMode.Value.Audio < 2) <> (options.AudioVolume.Value <> 0.0))

        base.Init parent

type PracticeControls(state: PracticeState, with_mods: ModdedChart, on_seek: Time -> unit) =
    inherit Container(NodeType.None)

    let fade = Animation.Fade(1.0f)

    override this.Init(parent) =
        this
        |+ Text(Icons.TARGET + " " + (%"practice.title"))
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(90.0f).ShrinkX(25.0f).TranslateY(10.0f))
        |+ PageButton(
            sprintf "%s %s" Icons.SETTINGS (%"practice.tools"),
            (fun () -> PracticeToolsPage(state).Show())
        )
            .Position(Position.SliceT(50.0f).SliceL(500.0f).ShrinkX(25.0f).TranslateY(105.0f).Expand(Style.PADDING))
        |+ HotkeyListener("context_menu", fun () -> PracticeToolsPage(state).Show())
        |+ SyncSuggestionControls(state)
            .Position(Position.ShrinkT(160.0f))

        |+ Text(fun () -> sprintf "%s %.2fx" Icons.FAST_FORWARD SelectedChart.rate.Value)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(90.0f).ShrinkX(25.0f).TranslateY(10.0f))
        |+ Text(sprintf "%s: %O/%O" (%"replay.change_playback_rate") (%%"uprate") (%%"downrate"))
            .Color(Colors.text_cyan)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(110.0f, 50.0f).ShrinkX(25.0f))
        |+ Text(sprintf "%s: %O" (%"practice.play") (%%"pause"))
            .Color(Colors.text_cyan)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(160.0f, 50.0f).ShrinkX(25.0f))
        |+ Text(sprintf "%s: %O" (%"practice.restart") (%%"retry"))
            .Color(Colors.text_cyan)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(210.0f, 50.0f).ShrinkX(25.0f))

        |* Timeline(with_mods, on_seek, SelectedChart.rate)

        base.Init parent

    override this.Draw() =
        if fade.Alpha > 0 then
            let old_m = Render.alpha_multiplier_begin fade.Value
            base.Draw()
            Render.alpha_multiplier_restore old_m

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if state.Paused.Value && fade.Target <> 1.0f then
            fade.Target <- 1.0f
            Toolbar.show_cursor ()
        elif not state.Paused.Value && fade.Target = 1.0f then
            fade.Target <- 0.0f
            Toolbar.hide_cursor ()

        fade.Update elapsed_ms