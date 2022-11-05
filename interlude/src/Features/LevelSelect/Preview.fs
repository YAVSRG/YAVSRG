namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Scoring
open Interlude.Features
open Interlude.Features.Play

type Preview() =
    inherit Dialog()

    let renderer : Widget =
        match Gameplay.Chart.current with
        | Some chart -> 
            NoteRenderer(Metrics.createDummyMetric chart)
            |+ GameplayWidgets.LaneCover()
            :> Widget
        | None -> new Dummy()

    override this.Init(parent: Widget) =
        base.Init parent
        renderer.Init this

    override this.Draw() =
        renderer.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        renderer.Update(elapsedTime, moved)
        if (!|"preview").Released() || (!|"exit").Tapped() || Mouse.leftClick() then
            this.Close()