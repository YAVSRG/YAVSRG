namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Common
open Prelude
open Prelude.Mods
open Interlude.UI
open Interlude.Features.Gameplay

type private ReplayControls(with_mods: ModdedChart, is_auto: bool, rate: Rate, on_seek: Time -> unit) =
    inherit Container(NodeType.None)

    let fade = Animation.Fade(1.0f)
    let mutable auto_hide_timer = 3000.0
    let mutable show_cooldown = 0.0

    let playback_speed =
        let setting = rate |> Setting.bounded (0.25f<rate>, 3.0f<rate>)
        { setting with
            Set = fun v ->

                if fixed_scroll_speed.Value then
                    SelectedChart.rate.Set v
                    setting.Set SelectedChart.rate.Value
                else
                    SelectedChart.rate.Set rate
                    Song.change_rate v
                    setting.Set v

                fade.Target <- 1.0f
                Toolbar.show_cursor ()
                auto_hide_timer <- 1500.0
        }

    let refresh_playback_speed () =
        playback_speed.Set playback_speed.Value

    override this.Init(parent) =
        this
        |+ Text(Icons.FILM + " " + (if is_auto then %"replay.title.autoplay" else %"replay.title"))
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(90.0f).ShrinkX(25.0f).TranslateY(10.0f))
        |+ PageButton(
            sprintf "%s %s" Icons.SETTINGS (%"replay.settings"),
            (fun () -> ReplayModeSettingsPage(refresh_playback_speed).Show())
        )
            .Position(Position.SliceT(50.0f).SliceL(500.0f).ShrinkX(25.0f).TranslateY(105.0f).Expand(Style.PADDING))
        |+ HotkeyListener("options", (fun () -> ReplayModeSettingsPage(refresh_playback_speed).Show()))
        |+ Text(sprintf "%s: %O" (%"replay.hide_overlay") (%%"hide_replay_overlay"))
            .Color(Colors.text_cyan)
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(160.0f))

        |+ Text(fun () -> sprintf "%s %.2fx" Icons.FAST_FORWARD playback_speed.Value)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(90.0f).ShrinkX(25.0f).TranslateY(10.0f))
        |+ Text(fun () -> sprintf "%s: %.2fx" (%"replay.original_rate") rate)
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(105.0f))
        |+ Text(sprintf "%s: %O/%O" (%"replay.change_playback_rate") (%%"uprate") (%%"downrate"))
            .Color(Colors.text_cyan)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(160.0f))
        |+ Text(sprintf "%s: %O" (%"replay.pause") (%%"pause"))
            .Color(Colors.text_cyan)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(210.0f))

        |* Timeline(with_mods, on_seek, SelectedChart.rate)

        base.Init parent

    override this.Draw() =
        if fade.Alpha > 0 then
            let old_m = Render.alpha_multiplier_begin fade.Value
            base.Draw()
            Render.alpha_multiplier_restore old_m

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        fade.Update elapsed_ms

        if show_cooldown <= 0.0 && Mouse.moved_recently () then
            fade.Target <- 1.0f
            Toolbar.show_cursor ()
            auto_hide_timer <- 1500.0

        elif fade.Target = 1.0f && not (Dialog.exists()) then
            auto_hide_timer <- auto_hide_timer - elapsed_ms

            if auto_hide_timer <= 0.0 then
                fade.Target <- 0.0f
                Toolbar.hide_cursor ()

            if (%%"hide_replay_overlay").Held() then
                fade.Target <- 0.0f
                Toolbar.hide_cursor ()
                show_cooldown <- 1000.0

        else show_cooldown <- show_cooldown - elapsed_ms

        if not (Mouse.held Mouse.LEFT) then
            let scroll = Mouse.scroll()
            if scroll <> 0.0f then
                if Song.playing() then
                    Song.pause()
                    Song.seek(Song.time() - scroll * 40.0f<ms>)
                    Song.resume()
                else
                    Song.seek(Song.time() - scroll * 40.0f<ms>)

        if (%%"difficulty_overlay").Pressed() then
            show_difficulty_overlay.Set (not show_difficulty_overlay.Value)
        elif (%%"pause").Pressed() || (%%"pause_music").Pressed() then
            if Song.playing () then
                (if Song.time () > 0.0f<ms> then Song.pause ())
            elif not (Mouse.held Mouse.LEFT) then Song.resume ()
        else
            SelectedChart.change_rate_hotkeys (fun change_by -> playback_speed.Value <- playback_speed.Value + change_by)