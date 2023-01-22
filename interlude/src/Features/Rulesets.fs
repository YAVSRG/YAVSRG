namespace Interlude.Features

open Percyqaz.Common
open Prelude.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu

module Rulesets = Interlude.Content.Rulesets

module Rulesets = 

    type private RulesetButton(id: string, name, selected, action) =
        inherit StaticContainer(NodeType.Button (fun _ -> action()))
            
        override this.Init(parent: Widget) =
            this
            |+ Text(
                K (sprintf "%s  %s" name (if selected then Icons.selected else "")),
                Color = ( 
                    fun () -> ( 
                        (if this.Focused <> (id.StartsWith '*') then Style.color(255, 1.0f, 0.5f) else Color.White),
                        Color.Black
                    )
                ),
                Align = Alignment.LEFT,
                Position = Position.Margin Style.padding)
            |* Clickable.Focus this
            base.Init parent
            
        override this.Draw() =
            if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

    type FavouritesPage() as this =
        inherit Page()

        let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
        let rec refresh() =
            container.Clear()

            for id, rs in Rulesets.list() do
                let selected = List.contains id options.FavouriteRulesets.Value
                container 
                |* RulesetButton(id, rs.Name, 
                    List.contains id options.FavouriteRulesets.Value,
                    (fun () -> 
                        if selected then Setting.app (List.except [id]) options.FavouriteRulesets
                        else Setting.app (fun l -> id :: l) options.FavouriteRulesets
                        sync refresh
                    ) )

            if container.Focused then container.Focus()

        do
            refresh()

            this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 150.0f)) )

        override this.Title = N"gameplay.rulesets"
        override this.OnClose() = ()
    
    type QuickSwitcher(setting: Setting<string>) =
        inherit StaticContainer(NodeType.None)
    
        override this.Init(parent: Widget) =
            this
            |+ Clickable(ignore, OnRightClick = fun () -> Menu.ShowPage FavouritesPage)
            |+ HotkeyAction("ruleset_picker", fun () -> Menu.ShowPage FavouritesPage)
            |* StylishButton(
                ( fun () -> this.ToggleDropdown() ),
                ( fun () -> Rulesets.current.Name ),
                Style.main 100,
                TiltRight = false,
                Hotkey = "ruleset_switch")
            base.Init parent
    
        member this.ToggleDropdown() =
            match this.Dropdown with
            | Some _ -> this.Dropdown <- None
            | _ ->
                let rulesets = Rulesets.list() |> Map.ofSeq
                let favouriteRulesets = 
                    options.FavouriteRulesets.Value
                    |> List.choose (fun id -> match rulesets.TryFind id with Some rs -> Some id | None -> None)
                    |> function [] -> [Rulesets.DEFAULT] | xs -> xs
                let d = Dropdown.Selector favouriteRulesets (fun id -> rulesets.[id].Name) (fun id -> setting.Set id) (fun () -> this.Dropdown <- None)
                d.Position <- Position.SliceBottom(d.Height + 60.0f).TrimBottom(60.0f).Margin(Style.padding, 0.0f)
                d.Init this
                this.Dropdown <- Some d
    
        member val Dropdown : Dropdown option = None with get, set
    
        override this.Draw() =
            base.Draw()
            match this.Dropdown with
            | Some d -> d.Draw()
            | None -> ()
    
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            match this.Dropdown with
            | Some d -> d.Update(elapsedTime, moved)
            | None -> ()

type PacemakerPage() as this =
    inherit Page()

    let rulesetId = Rulesets.current_hash
    let existing = if options.Pacemakers.ContainsKey rulesetId then options.Pacemakers.[rulesetId] else Pacemaker.Default

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

    do 
        let lamps = 
            Rulesets.current.Grading.Lamps
            |> Array.indexed
            |> Array.map (fun (i, l) -> (i, l.Name))
        this.Content(
            column()
            |+ PrettySetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.SaveScoreIfUnderPace).Pos(200.0f)
            |+ CaseSelector("gameplay.pacemaker.type", 
                [|N"gameplay.pacemaker.accuracy"; N"gameplay.pacemaker.lamp"|],
                [|
                    [| PrettySetting("gameplay.pacemaker.accuracy", Slider<_>.Percent(accuracy, 0.01f)).Pos(380.0f) |]
                    [| PrettySetting("gameplay.pacemaker.lamp", Selector(lamps, lamp)).Pos(380.0f) |]
                |], utype).Pos(300.0f)
            |+ Text(L"options.gameplay.pacemaker.hint", Align = Alignment.CENTER, Position = Position.SliceBottom(100.0f).TrimBottom(40.0f))
        )

    override this.Title = N"gameplay.pacemaker"
    override this.OnClose() = 
        match utype.Value with
        | 0 -> options.Pacemakers.[rulesetId] <- Pacemaker.Accuracy accuracy.Value
        | 1 -> options.Pacemakers.[rulesetId] <- Pacemaker.Lamp lamp.Value
        | _ -> failwith "impossible"