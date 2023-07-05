namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay.Mods
open Interlude.Features
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features.Gameplay

type private ModSelector(id, states: string[], current_state: unit -> int, action: unit -> unit) =
    inherit StaticContainer(NodeType.Button action)

    let fill = Animation.Color(Colors.grey_2.O4a 0)
    let selected = Animation.Fade(0.0f)

    override this.Init(parent) =
        this
        |+ Clickable.Focus this
        |+ Text(
            ModState.getModName id,
            Color = (fun () -> (if this.Focused then Colors.yellow_accent else Colors.white), Colors.shadow_1),
            Position = Position.SliceTop(50.0f),
            Align = Alignment.CENTER)
        |* Text(
            ModState.getModDesc id,
            Color = (fun () -> (if this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2),
            Position = Position.TrimTop(40.0f).TrimBottom(30.0f),
            Align = Alignment.CENTER)
        base.Init parent

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        let state = current_state()
        if state >= 0 then 
            selected.Target <- 1.0f
            fill.Target <- Colors.pink_shadow.O2
        else 
            selected.Target <- 0.0f
            if this.Focused then fill.Target <- Colors.yellow_accent.O1
            else fill.Target <- Colors.grey_2.O4a 0
        fill.Update elapsedTime
        selected.Update elapsedTime

    override this.Draw() =
        Draw.rect this.Bounds fill.Value
        let state = current_state()
        let edgeColor = if this.Focused then Colors.yellow_accent.O2 elif state >= 0 then Colors.pink_accent.O2 else Colors.grey_2.O1
        let bottom_edge = this.Bounds.Expand(0.0f, Style.padding).SliceBottom(Style.padding)
        Draw.rect bottom_edge edgeColor
        Draw.rect (Rect.Create(bottom_edge.CenterX - 100.0f - 150.0f * selected.Value, bottom_edge.Top, bottom_edge.CenterX + 100.0f + 150.0f * selected.Value, bottom_edge.Bottom)) edgeColor

        if state >= 0 then Text.drawFillB(Style.baseFont, states.[state], this.Bounds.SliceBottom(40.0f), Colors.text, Alignment.CENTER)
        base.Draw()

type private ModSelectPage(onClose) as this =
    inherit Page()

    let flow = FlowContainer.Vertical<Widget>(100.0f, Spacing = 5f, Position = Position.TrimTop(100.0f))

    do 
        flow
        |* ModSelector("auto",
            [|Icons.check|],
            (fun _ -> if autoplay then 0 else -1),
            (fun _ -> autoplay <- not autoplay))

        for id in modList.Keys do
            flow
            |* ModSelector(id,
                [|Icons.check|],
                (fun _ -> if selectedMods.Value.ContainsKey id then selectedMods.Value.[id] else -1),
                (fun _ -> Setting.app (ModState.cycleState id) selectedMods))

        flow
        |+ ModSelector("pacemaker",
            [|Icons.check|],
            (fun _ -> if enablePacemaker then 0 else -1),
            (fun _ -> enablePacemaker <- not enablePacemaker))
        |* SwapContainer(Current =
                PageButton("gameplay.pacemaker", fun () -> PacemakerPage().Show())
                .Pos(15.0f)
                .Tooltip(Tooltip.Info("gameplay.pacemaker"))
            )

    do
        this.Content(
            SwitchContainer.Row<Widget>()
            |+ flow
        )

    override this.Title = L"mods.name"
    override this.OnClose() = onClose()

type ModSelect(onClose) =
    inherit StylishButton((fun () -> ModSelectPage(onClose).Show()), K (sprintf "%s %s" Icons.mods (L"levelselect.mods.name")), (fun () -> Style.color(100, 0.5f, 0.0f)), Hotkey = "mods")

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"autoplay").Tapped() then
            autoplay <- not autoplay