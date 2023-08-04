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

    let TOP_HEIGHT = 70.0f

    override this.Init(parent) =
        this
        |+ Clickable.Focus this
        |+ Text(
            ModState.getModName id,
            Color = (fun () -> (if this.Focused then Colors.yellow_accent else Colors.white), Colors.shadow_1),
            Position = Position.SliceTop(TOP_HEIGHT).Margin(20.0f, 0.0f),
            Align = Alignment.LEFT)
        |* Text(
            ModState.getModDesc id,
            Color = (fun () -> (if this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2),
            Position = Position.TrimTop(TOP_HEIGHT).Margin(20.0f, 0.0f),
            Align = Alignment.LEFT)
        base.Init parent

    override this.Draw() =
        let state = current_state()
        Draw.rect (this.Bounds.SliceTop(TOP_HEIGHT)) (if state >= 0 then Colors.pink.O3 else Colors.shadow_2.O3)
        Draw.rect (this.Bounds.TrimTop(TOP_HEIGHT)) (if state >= 0 then Colors.pink_shadow.O3 else Colors.black.O3)

        if state >= 0 then Text.drawFillB(Style.font, states.[state], this.Bounds.SliceTop(TOP_HEIGHT).Shrink(20.0f, 0.0f), Colors.text, Alignment.RIGHT)
        base.Draw()

type private ModSelectPage(onClose) as this =
    inherit Page()

    let grid = GridContainer<Widget>(100.0f, 3, Spacing = (30f, 30f), Position = Position.Margin(100.0f, 200.0f), WrapNavigation = false)

    do 
        grid
        |* ModSelector("auto",
            [|Icons.check|],
            (fun _ -> if autoplay then 0 else -1),
            (fun _ -> autoplay <- not autoplay))

        for id in modList.Keys do
            grid
            |* ModSelector(id,
                [|Icons.check|],
                (fun _ -> if selectedMods.Value.ContainsKey id then selectedMods.Value.[id] else -1),
                (fun _ -> Setting.app (ModState.cycleState id) selectedMods))

        grid
        |* ModSelector("pacemaker",
            [|Icons.check|],
            (fun _ -> if enablePacemaker then 0 else -1),
            (fun _ -> enablePacemaker <- not enablePacemaker))

    do
        this.Content(
            SwitchContainer.Column<Widget>()
            |+ grid
            |+ PageButton("gameplay.pacemaker", fun () -> PacemakerPage().Show())
                .Pos(500.0f)
                .Tooltip(Tooltip.Info("gameplay.pacemaker"))
        )

    override this.Title = L"mods.name"
    override this.OnClose() = onClose()

type ModSelect(onClose) =
    inherit StylishButton((fun () -> ModSelectPage(onClose).Show()), K (sprintf "%s %s" Icons.mods (L"levelselect.mods.name")), (fun () -> Palette.color(100, 0.5f, 0.0f)), Hotkey = "mods")

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"autoplay").Tapped() then
            autoplay <- not autoplay