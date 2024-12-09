namespace Interlude.Features.MainMenu

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Online

type LoadingScreen() =
    inherit Screen()

    let mutable closing = false
    let audio_fade = Animation.Fade 0.0f
    let animation = Animation.Sequence()
    let background_fade = Animation.Delay(2200.0)

    override this.OnEnter(prev: Screen.Type) =
        Logo.move_center ()
        Toolbar.hide ()

        background_fade.Reset()

        match prev with
        | Screen.Type.SplashScreen ->
            animation.Add(Animation.Action(fun () -> Sounds.get("hello").Play()))
            animation.Add(Animation.Delay 1000.0)
            animation.Add(Animation.Action(fun () -> audio_fade.Target <- 1.0f))
            animation.Add(Animation.Delay 1200.0)

            animation.Add(
                Animation.Action(fun () -> Screen.change Screen.Type.MainMenu Transitions.UnderLogo |> ignore)
            )
        | _ ->
            closing <- true
            DiscordRPC.clear()
            audio_fade.Snap()
            animation.Add(Animation.Delay 1000.0)
            animation.Add(Animation.Action(fun () -> audio_fade.Target <- 0.0f))
            animation.Add(Animation.Delay 1200.0)
            animation.Add(Animation.Action(fun () -> Screen.back Transitions.Default |> ignore))

    override this.OnExit _ =
        if not closing then
            Devices.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        audio_fade.Update elapsed_ms
        animation.Update elapsed_ms
        background_fade.Update elapsed_ms

        Devices.change_volume (
            options.AudioVolume.Value,
            options.AudioVolume.Value * float audio_fade.Value
        )

    override this.Draw() =
        if closing then
            let alpha = 255.0 * (1.0 - background_fade.Time / background_fade.Interval) |> int
            Render.rect this.Bounds (Colors.black.O4a alpha)
        else
            let alpha = 255.0 * (background_fade.Time / background_fade.Interval) |> int
            Render.rect this.Bounds (Colors.black.O4a alpha)

        let (x, y) = this.Bounds.Center

        Text.draw_aligned_b (
            Style.font,
            (if closing then "Thank you for playing" else "Loading :)"),
            80.f,
            x,
            y - 500.0f,
            Colors.text,
            0.5f
        )

    override this.OnBack() =
        WindowThread.exit()
        None