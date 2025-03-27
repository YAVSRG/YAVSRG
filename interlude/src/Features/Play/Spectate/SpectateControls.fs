namespace Interlude.Features.Play.Spectate

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.Features.Gameplay

type private Controls(who: unit -> string, cycle: unit -> unit) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(%"spectate.title")
            .Color(Colors.text_subheading)
            .Align(Alignment.CENTER)
            .Position(Position.SliceT(40.0f))
        |+ Text(who)
            .Color(Colors.text)
            .Align(Alignment.CENTER)
            .Position(Position.ShrinkT(40.0f))
        |* MouseListener().OnLeftClick(cycle)

        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds Colors.black.O2
        base.Draw()

type private ControlOverlay(info: LoadedChartInfo, on_seek: Time -> unit, who: unit -> string, cycle: unit -> unit) =
    inherit SlideContainer(NodeType.None)

    let mutable show = true
    let mutable show_timeout = 3000.0

    override this.Init(parent) =
        this
        |+ Timeline(info.WithMods, on_seek, SelectedChart.rate)
        |* Controls(who, cycle)
            .Position(Position.Box(0.0f, 0.0f, 30.0f, 70.0f, 440.0f, 100.0f))

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.moved_recently () then
            show <- true
            this.Position <- Position.DEFAULT
            show_timeout <- 1500.0
            Toolbar.show_cursor ()
        elif show then
            show_timeout <- show_timeout - elapsed_ms

            if show_timeout < 0.0 then
                show <- false

                this.Position <-
                    { Position.DEFAULT with
                        Top = 0.0f %- 300.0f
                        Bottom = 1.0f %+ 100.0f
                    }
                Toolbar.hide_cursor ()