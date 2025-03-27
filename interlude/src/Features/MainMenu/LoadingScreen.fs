namespace Interlude.Features.MainMenu

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Online

type LoadingScreen(post_init_thunk: unit -> unit) =
    inherit Screen()

    let mutable closing = false
    let audio_fade = Animation.Fade 0.0f
    let animation = Animation.Sequence()
    let background_fade = Animation.Delay(2200.0)

    let post_init () =
        async {
            try
                post_init_thunk ()
                Ok()
            with error ->
                Error error
            |>
            function
            | Ok () ->
                GameThread.defer
                <| fun () ->
                animation.Add(Animation.Delay 50.0)
                animation.Add(Animation.Action(fun () -> audio_fade.Target <- 1.0f))
                animation.Add(Animation.Delay 500.0)
                animation.Add(Animation.Action(fun () -> Screen.change ScreenType.MainMenu Transitions.UnderLogo |> ignore))
            | Error error ->
                GameThread.defer (fun () -> raise error)
        }
        |> Async.Start

    override this.Init(parent: Widget) =
        this
        |* LoadingIndicator.Strip(fun () -> not closing)
            .Position(Position.SliceT(165.0f, 10.0f).SliceX(400.0f))
        base.Init parent

    override this.OnEnter(prev: ScreenType) =
        Toolbar.hide ()

        background_fade.Reset()

        match prev with
        | ScreenType.SplashScreen ->
            animation.Add(Animation.Action(fun () -> Sounds.get("hello").Play()))
            animation.Add(Animation.Delay 100.0)
            animation.Add(Animation.Action(fun () -> Screen.logo.MoveCenter()))
            animation.Add(Animation.Delay 900.0)
            animation.Add(Animation.Action(post_init))
        | _ ->
            Screen.logo.MoveCenter()
            closing <- true
            DiscordRPC.clear()
            audio_fade.Snap()
            animation.Add(Animation.Delay 1000.0)
            animation.Add(Animation.Action(fun () -> audio_fade.Target <- 0.0f))
            animation.Add(Animation.Delay 1200.0)
            animation.Add(Animation.Action(fun () -> Screen.back Transitions.Default |> ignore))

    override this.OnExit _ =
        if not closing then
            Audio.change_volume (options.AudioVolume.Value, options.AudioVolume.Value)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        audio_fade.Update elapsed_ms
        animation.Update elapsed_ms
        background_fade.Update elapsed_ms

        Audio.change_volume (
            options.AudioVolume.Value,
            options.AudioVolume.Value * float audio_fade.Value
        )

    override this.Draw() =
        let alpha =
            if closing then
                255.0 * (1.0 - background_fade.Progress) |> int
            else
                255.0 * background_fade.Progress |> int
        Render.rect this.Bounds (Colors.black.O4a alpha)

        if closing then
            Text.draw_aligned_b (
                Style.font,
                "Thank you for playing",
                70.0f,
                this.Bounds.CenterX,
                40.0f,
                Colors.text,
                0.5f
            )
        else
            Text.draw_aligned_b (
                Style.font,
                "Loading :)",
                70.0f,
                this.Bounds.CenterX,
                40.0f,
                (Colors.white.O4a alpha, Colors.black.O4a alpha),
                0.5f
            )
            base.Draw()

    override this.OnBack() =
        WindowThread.exit()
        None