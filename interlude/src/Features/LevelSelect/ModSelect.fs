namespace Interlude.Features.LevelSelect

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Interlude.Features
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features.Gameplay

type private ModCard(name: string, desc: string, enabled: Setting<bool>) as this =
    inherit Frame(
        NodeType.Button(fun () -> this.ToggleMod()),
        Border = (fun () -> if this.Focused then Color.White elif this.ModEnabled then Style.highlight 100 () else Color.Transparent),
        Fill = fun () -> if this.ModEnabled then !*Palette.BASE else !*Palette.DARK
    )

    do
        this
        |+ Text(name, Position = Position.SliceTop(50.0f).TrimLeft(5.0f))
        |+ Text(desc, Position = Position.TrimTop(50.0f).TrimLeft(5.0f))
        |* Clickable.Focus this

    member this.ModEnabled = enabled.Value
    member this.ToggleMod() = enabled.Set true

type private ModSelectPage(onClose) as this =
    inherit Page()

    let mods = FlowContainer.Vertical<Widget>(PRETTYHEIGHT, Spacing = 15.0f, Position = { Position.Default with Right = 0.5f %+ 0.0f }.Margin(150.0f, 200.0f))
    do 
        mods
        |* ModCard(ModState.getModName "auto", ModState.getModDesc "auto",
            Setting.make (fun _ -> autoplay <- not autoplay) (fun _ -> autoplay))
        for id in modList.Keys do
            mods
            |* ModCard(ModState.getModName id, ModState.getModDesc id,
                Setting.make (fun _ -> Setting.app (ModState.cycleState id) selectedMods) (fun _ -> selectedMods.Value.ContainsKey id))
        mods
        |+ Dummy()
        |+ ModCard(L"options.gameplay.pacemaker.enable.name", L"options.gameplay.pacemaker.enable.tooltip",
            Setting.make (fun _ -> enablePacemaker <- not enablePacemaker) (fun _ -> enablePacemaker))
        |* PrettyButton("gameplay.pacemaker", fun () ->  Menu.ShowPage PacemakerPage)

    do
        this.Content(
            SwitchContainer.Row<Widget>()
            |+ mods
        )

    override this.Title = N"mods"
    override this.OnClose() = onClose()

type ModSelect(onClose) =
    inherit StylishButton((fun () -> Menu.ShowPage (ModSelectPage onClose)), K (sprintf "%s %s" Icons.mods (N"mods")), (fun () -> Style.color(100, 0.5f, 0.0f)), Hotkey = "mods")

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"autoplay").Tapped() then
            autoplay <- not autoplay