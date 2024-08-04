namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Play

type private ControlOverlay(with_mods: ModdedChart, state: ReplayState, on_seek: Time -> unit) =
    inherit SlideContainer(NodeType.None)

    let mutable show = true
    let mutable show_timeout = 3000.0

    let overlay_buttons =
        NavigationContainer.Column(Position = Position.SliceL(400.0f))
        |+ Button(
            (fun () ->
                sprintf "%s %s"
                    (if state.ShowInputOverlay.Value then
                            Icons.CHECK_CIRCLE
                        else
                            Icons.CIRCLE)
                    %"replay.input_overlay"
            ),
            (fun () -> Setting.app not state.ShowInputOverlay),
            Position = Position.SliceT(50.0f)
        )
        |+ Button(
            (fun () ->
                sprintf "%s %s"
                    (if state.ShowHitOverlay.Value then
                            Icons.CHECK_CIRCLE
                        else
                            Icons.CIRCLE)
                    %"replay.hit_overlay"
            ),
            (fun () -> Setting.app not state.ShowHitOverlay),
            Position = Position.SliceB(50.0f)
        )

    let dim_slider =
        Slider.Percent(
            state.PlayfieldDim,
            Position = Position.ShrinkL(400.0f).SliceL(400.0f).SliceB(50.0f).Shrink(5.0f)
        )

    let replay_controls = 
        SlideoutContent(NavigationContainer.Row() |+ overlay_buttons |+ dim_slider, 100.0f)
        |+ Text(
            sprintf "%s %s" Icons.PLAY (if state.IsAuto then %"replay.title.autoplay" else %"replay.title"),
            Color = K Colors.text,
            Align = Alignment.RIGHT,
            Position = Position.Shrink(30.0f, 20.0f)
        )
        |+ Text(
            %"replay.playfield_dim",
            Color =
                (fun () ->
                    if dim_slider.Focused then
                        Colors.text_yellow_2
                    elif state.ShowInputOverlay.Value || state.ShowHitOverlay.Value then
                        Colors.text
                    else
                        Colors.text_greyout
                ),
            Align = Alignment.CENTER,
            Position = Position.ShrinkL(400.0f).SliceL(400.0f).SliceT(50.0f)
        )

    let slideout = Slideout(replay_controls, AutoCloseWhen = K false)

    override this.Init(parent) =
        this |+ Timeline(with_mods, on_seek, SelectedChart.rate) |* slideout

        base.Init parent

        slideout.Open()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.moved_recently () then
            show <- true
            slideout.Open()
            this.Position <- Position.DEFAULT
            show_timeout <- 1500.0
            Toolbar.show_cursor ()

        elif show then
            show_timeout <- show_timeout - elapsed_ms

            if show_timeout < 0.0 then
                show <- false

                slideout.Close()

                this.Position <-
                    { Position.DEFAULT with
                        Bottom = 1.0f %+ 100.0f
                    }
                Toolbar.hide_cursor ()

        if show && not replay_controls.Focused then
            Screen.back Transitions.Default |> ignore