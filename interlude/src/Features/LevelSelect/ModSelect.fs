namespace Interlude.Features.LevelSelect

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Menu
open Interlude.Utils
open Interlude.Features.Gameplay

type private ModCard(id) as this =
    inherit Percyqaz.Flux.UI.Frame(
        NodeType.Button(fun () -> this.ToggleMod()),
        Border = (fun () -> if this.ModEnabled then Color.White elif this.Focused then !*Palette.LIGHTER else Color.Transparent),
        Fill = fun () -> if this.Focused then !*Palette.BASE else !*Palette.DARK
    )

    do
        this
        |+ Text(ModState.getModName id, Position = Position.SliceTop(50.0f).TrimLeft(5.0f))
        |+ Text(ModState.getModDesc id, Position = Position.TrimTop(50.0f).TrimLeft(5.0f))
        |* Clickable.Focus this

    member this.ModEnabled = if id = "auto" then autoplay else selectedMods.Value.ContainsKey id
    member this.ToggleMod() = if id = "auto" then autoplay <- not autoplay else Setting.app (ModState.cycleState id) selectedMods

type private ModSelectPage() as this =
    inherit Page()

    do
        let container = FlowContainer.Vertical<ModCard>(80.0f, Spacing = 5.0f, Position = Position.Margin(100.0f, 200.0f))
        container.Add(ModCard "auto")
        for id in modList.Keys do container.Add(ModCard id)
        this.Content container

    override this.Title = N"mods"
    override this.OnClose() = ()
    
type ModSelect() as this =
    inherit Widget1()

    do StylishButton((fun () -> Menu.ShowPage ModSelectPage), K "Mods", (fun () -> Style.color(100, 0.8f, 0.2f)), "mods") |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"autoplay").Tapped() then
            autoplay <- not autoplay