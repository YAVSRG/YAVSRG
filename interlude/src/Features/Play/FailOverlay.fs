namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Prelude
open Interlude.UI

type FailButton(label: string, action: unit -> unit, fade: Animation.Fade) =
    inherit StaticWidget(NodeType.Button(fun () -> Style.click.Play(); action()))

    override this.Draw() =
        let bounds = this.Bounds.TranslateY((1.0f - fade.Value) * 30.0f)
        let alpha = fade.Alpha
        Draw.rect bounds (if this.Focused then Colors.yellow_accent.O1a alpha else Colors.shadow_2.O2a alpha)
        Draw.rect (bounds.BorderR(10.0f).TranslateY(10.0f)) (Colors.black.O3a alpha)
        Draw.rect (bounds.BorderB(10.0f).TranslateX(10.0f)) (Colors.black.O3a alpha)
        Text.fill_b(Style.font, label, bounds.Shrink(20.0f, 10.0f), (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        if not this.Focused && Mouse.moved_recently() && Mouse.hover this.Bounds then
            this.Focus true
        base.Update(elapsed_ms, moved)

    override this.OnFocus by_mouse = Style.hover.Play(); base.OnFocus by_mouse

type FailOverlay(retry, score_screen, next_song) =
    inherit Container(NodeType.None)

    let main_fade = Animation.Fade(0.0f, Target = 1.0f)
    let b1_fade = Animation.Fade(0.0f)
    let b2_fade = Animation.Fade(0.0f)
    let b3_fade = Animation.Fade(0.0f)

    let animation = 
        Animation.seq [
            Animation.Delay(50.0)
            Animation.Action(fun () -> b1_fade.Target <- 1.0f)
            Animation.Delay(100.0)
            Animation.Action(fun () -> b2_fade.Target <- 1.0f)
            Animation.Delay(100.0)
            Animation.Action(fun () -> b3_fade.Target <- 1.0f)
        ]

    do
        Background.set_parallax_amount 0.0f
        Song.set_low_pass 1.0f
        Toolbar.show_cursor()

    override this.Init(parent) =
        NavigationContainer.Column(Position = Position.SliceX(300.0f).ShrinkT(560.0f))
        |+ Dummy(NodeType.Leaf)
        |+ FailButton(%"failed.retry", retry, b1_fade, Position = Position.Row(0.0f, 70.0f))
        |+ FailButton(%"failed.score_screen", score_screen, b2_fade, Position = Position.Row(100.0f, 70.0f))
        |+ FailButton(%"failed.next_song", next_song, b3_fade, Position = Position.Row(200.0f, 70.0f))
        |> this.Add
        base.Init parent

    override this.Draw() =
        let alpha = main_fade.Alpha

        let text_color = (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha)

        Draw.untextured_quad this.Bounds.AsQuad (Quad.gradient_top_to_bottom (Colors.red.O4a 0) (Colors.red.O2a alpha))
        Draw.rect (this.Bounds.SliceY(main_fade.Value * 200.0f).TranslateY(-200.0f)) Colors.shadow_2.O2

        Text.fill_b(Style.font, "MISSION FAILED", this.Bounds.SliceY(160.0f).TranslateY(-200.0f), text_color, Alignment.CENTER)

        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        animation.Update elapsed_ms
        main_fade.Update elapsed_ms
        b1_fade.Update elapsed_ms
        b2_fade.Update elapsed_ms
        b3_fade.Update elapsed_ms