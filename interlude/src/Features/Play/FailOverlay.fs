namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Pacemaker

type FailButtonType =
    | NONE = 0
    | RETRY = 1
    | SCORE_SCREEN = 2
    | CONTINUE = 3

module FailButtonType =

    let mutable most_recently_used = FailButtonType.NONE

type FailButton(kind: FailButtonType, label: string, hotkey: Bind, action: unit -> unit, fade: Animation.Fade) =
    inherit StaticWidget(NodeType.Button(fun () -> FailButtonType.most_recently_used <- kind; Style.click.Play(); action()))

    override this.Draw() =
        let bounds = this.Bounds.TranslateY((1.0f - fade.Value) * 30.0f)
        let alpha = fade.Alpha
        Render.rect bounds (if this.Focused then Colors.yellow_accent.O1a alpha else Colors.shadow_2.O2a alpha)
        Render.rect (bounds.BorderR(10.0f).TranslateY(10.0f)) (Colors.black.O3a alpha)
        Render.rect (bounds.BorderB(10.0f).TranslateX(10.0f)) (Colors.black.O3a alpha)
        Text.fill_b(Style.font, hotkey.ToString(), bounds.Shrink(20.0f, 25.0f).TranslateY(22.0f), Colors.text_cyan, Alignment.CENTER)
        Text.fill_b(Style.font, label, bounds.Shrink(20.0f, 10.0f).TranslateY(-5.0f), (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        if Mouse.hover this.Bounds then
            if not this.Focused && Mouse.moved_recently() then
                this.Focus true
            if Mouse.left_clicked() then
                this.Select true
        if hotkey.Pressed() then
            this.Select false
        base.Update(elapsed_ms, moved)

    override this.OnFocus by_mouse = Style.hover.Play(); base.OnFocus by_mouse

type FailOverlay(pacemaker_state: PacemakerState, retry: unit -> unit, score_screen: unit -> unit, next_song: unit -> unit) =
    inherit Container(NodeType.None)

    let main_fade = Animation.Fade(0.0f, Target = 1.0f)
    let b1_fade = Animation.Fade(0.0f)
    let b2_fade = Animation.Fade(0.0f)
    let b3_fade = Animation.Fade(0.0f)

    let pacemaker_desc = PacemakerState.description pacemaker_state

    let animation =
        Animation.seq [
            Animation.Delay(50.0)
            Animation.Action(fun () -> b1_fade.Target <- 1.0f)
            Animation.Delay(100.0)
            Animation.Action(fun () -> b2_fade.Target <- 1.0f)
            Animation.Delay(100.0)
            Animation.Action(fun () -> b3_fade.Target <- 1.0f)
            Animation.Delay(500.0)
        ]

    do
        Background.set_parallax_amount 0.0f
        Song.set_low_pass 1.0f
        Toolbar.show_cursor()

    override this.Init(parent: Widget) =
        let b_retry = FailButton(FailButtonType.RETRY, %"failed.retry", %%"retry", retry, b1_fade)
        let b_score_screen = FailButton(FailButtonType.SCORE_SCREEN, %"failed.score_screen", %%"save_score", score_screen, b2_fade)
        let b_next_song = FailButton(FailButtonType.CONTINUE, %"failed.next_song", %%"next_song", next_song, b3_fade)
        let b_none = Dummy(NodeType.Leaf)

        NavigationContainer.Column()
            .Position(Position.SliceX(300.0f).ShrinkT(560.0f))
            .With(
                b_none,
                b_retry
                    .Position(Position.SliceT(0.0f, 70.0f)),
                b_score_screen
                    .Position(Position.SliceT(100.0f, 70.0f)),
                b_next_song
                    .Position(Position.SliceT(200.0f, 70.0f))
            )
        |> this.Add

        base.Init parent

        let initial_focus : Widget =
            match FailButtonType.most_recently_used with
            | FailButtonType.RETRY -> b_retry
            | FailButtonType.SCORE_SCREEN -> b_score_screen
            | FailButtonType.CONTINUE -> b_next_song
            | _ -> b_none
        GameThread.defer (fun () -> initial_focus.Focus false)

    override this.Draw() =
        let old_m = Render.alpha_multiplier_begin 1.0f

        let alpha = main_fade.Alpha

        let text_color = (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha)

        Render.rect_c this.Bounds (Quad.gradient_top_to_bottom (Colors.red.O4a 0) (Colors.red.O2a alpha))
        Render.rect (this.Bounds.SliceY(main_fade.Value * 200.0f).TranslateY(-200.0f)) Colors.shadow_2.O2

        Text.fill_b(Style.font, "MISSION FAILED", this.Bounds.SliceY(160.0f).TranslateY(-200.0f), text_color, Alignment.CENTER)
        Text.fill_b(Style.font, pacemaker_desc, this.Bounds.SliceY(70.0f).TranslateY(-50.0f - 50f * main_fade.Value), (Colors.yellow_accent.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

        base.Draw()

        Render.alpha_multiplier_restore old_m

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms
        main_fade.Update elapsed_ms
        b1_fade.Update elapsed_ms
        b2_fade.Update elapsed_ms
        b3_fade.Update elapsed_ms