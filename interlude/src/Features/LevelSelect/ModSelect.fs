namespace Interlude.Features.LevelSelect

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Prelude.Scoring
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Gameplay

type private ModCard(id) as this =
    inherit Frame(
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

    let mods =
        let container = FlowContainer.Vertical<ModCard>(80.0f, Spacing = 5.0f, Position = { Position.Default with Left = 0.5f %+ 0.0f }.Margin(150.0f, 200.0f))
        container.Add(ModCard "auto")
        for id in modList.Keys do container.Add(ModCard id)
        container
        
    let rulesetId = Gameplay.rulesetId
    let enable = Setting.make (fun b -> enablePacemaker <- b) (fun () -> enablePacemaker)
    let existing = if options.Pacemakers.ContainsKey rulesetId then options.Pacemakers.[Gameplay.rulesetId] else Pacemaker.Default

    let utype =
        match existing with
        | Pacemaker.Accuracy _ -> 0
        | Pacemaker.Lamp _ -> 1
        |> Setting.simple
    let accuracy =
        match existing with
        | Pacemaker.Accuracy a -> a
        | Pacemaker.Lamp _ -> 0.95
        |> Setting.simple
        |> Setting.bound 0.0 1.0
        |> Setting.round 3
    let lamp =
        match existing with
        | Pacemaker.Accuracy _ -> 0
        | Pacemaker.Lamp l -> l
        |> Setting.simple

    let pacemaker =
        let lamps = 
            ruleset.Grading.Lamps
            |> Array.indexed
            |> Array.map (fun (i, l) -> (i, l.Name))

        let container = 
            column()
            |+ PrettySetting("gameplay.pacemaker.enable", Selector<_>.FromBool enable).Pos(200.0f)
            |+ PrettySetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.ScaveScoreIfUnderPace).Pos(280.0f)
            |+ CaseSelector("gameplay.pacemaker.type", 
                [|N"gameplay.pacemaker.accuracy"; N"gameplay.pacemaker.lamp"|],
                [|
                    [| PrettySetting("gameplay.pacemaker.accuracy", Slider<_>.Percent(accuracy, 0.01f)).Pos(460.0f) |]
                    [| PrettySetting("gameplay.pacemaker.lamp", Selector(lamps, lamp)).Pos(460.0f) |]
                |], utype).Pos(380.0f)

        container.Position <- { Position.Default with Right = 0.5f %+ 0.0f }
        container

    do
        this.Content(
            SwitchContainer.Row<Widget>()
            |+ pacemaker
            |+ mods
            |+ WIP()
        )

    override this.Title = N"mods"
    override this.OnClose() =
        match utype.Value with
        | 0 -> options.Pacemakers.[rulesetId] <- Pacemaker.Accuracy accuracy.Value
        | 1 -> options.Pacemakers.[rulesetId] <- Pacemaker.Lamp lamp.Value
        | _ -> failwith "impossible"
    
type ModSelect() =
    inherit StylishButton((fun () -> Menu.ShowPage ModSelectPage), K (sprintf "%s %s" Icons.mods (N"mods")), (fun () -> Style.color(100, 0.5f, 0.0f)), Hotkey = "mods")

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"autoplay").Tapped() then
            autoplay <- not autoplay