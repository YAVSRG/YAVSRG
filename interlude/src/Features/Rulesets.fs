namespace Interlude.Features

open Percyqaz.Common
open Prelude
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu

module Rulesets = Interlude.Content.Rulesets

module Rulesets = 

    type private RulesetButton(id: string, ruleset: Gameplay.Ruleset) =
        inherit StaticContainer(NodeType.Button (fun _ -> Style.click.Play(); options.SelectedRuleset.Set id))
            
        override this.Init(parent: Widget) =
            this
            |+ Text(
                (fun () -> sprintf "%s  %s" ruleset.Name (if options.SelectedRuleset.Value = id then Icons.selected else "")),
                Color = ( 
                    fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text
                ),
                Align = Alignment.LEFT,
                Position = Position.SliceTop(PRETTYHEIGHT).Margin Style.PADDING)
            |+ Text(
                ruleset.Description,
                Color = K Colors.text,
                Align = Alignment.LEFT,
                Position = Position.TrimTop(PRETTYHEIGHT - 10.0f).Margin Style.PADDING)
            |* Clickable.Focus this
            base.Init parent

        override this.OnFocus() = Style.hover.Play(); base.OnFocus()
            
        override this.Draw() =
            if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
            base.Draw()

    type FavouritesPage() as this =
        inherit Page()

        do
            let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT * 1.5f)
            for id, rs in Rulesets.list() do
                container |* RulesetButton(id, rs)
            this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 150.0f)) )

        override this.Title = L"gameplay.rulesets.name"
        override this.OnClose() = ()
    
    type QuickSwitcher(setting: Setting<string>) =
        inherit StaticContainer(NodeType.None)
    
        override this.Init(parent: Widget) =
            this
            |+ HotkeyAction("ruleset_picker", fun () -> Menu.ShowPage FavouritesPage)
            |* StylishButton(
                ( fun () -> this.ToggleDropdown() ),
                ( fun () -> Rulesets.current.Name ),
                !%Palette.MAIN_100,
                TiltRight = false,
                Hotkey = "ruleset_switch")
            base.Init parent
    
        member this.ToggleDropdown() =
            match this.Dropdown with
            | Some _ -> this.Dropdown <- None
            | _ ->
                let rulesets = Rulesets.list()
                let d = Dropdown.Selector rulesets (fun (id, rs) -> rs.Name) (fun (id, rs) -> setting.Set id) (fun () -> this.Dropdown <- None)
                d.Position <- Position.SliceBottom(d.Height + 60.0f).TrimBottom(60.0f).Margin(Style.PADDING, 0.0f)
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
            |+ PageSetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.SaveScoreIfUnderPace)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("gameplay.pacemaker.saveunderpace"))
            |+ CaseSelector("gameplay.pacemaker.type", 
                [|L"gameplay.pacemaker.accuracy.name"; L"gameplay.pacemaker.lamp.name"|],
                [|
                    [| PageSetting("gameplay.pacemaker.accuracy", Slider.Percent(accuracy |> Setting.f32)).Pos(370.0f) |]
                    [| PageSetting("gameplay.pacemaker.lamp", Selector(lamps, lamp)).Pos(370.0f) |]
                |], utype).Pos(300.0f)
            |+ Text(L"gameplay.pacemaker.hint", Align = Alignment.CENTER, Position = Position.SliceBottom(100.0f).TrimBottom(40.0f))
        )

    override this.Title = L"gameplay.pacemaker.name"
    override this.OnClose() = 
        match utype.Value with
        | 0 -> options.Pacemakers.[rulesetId] <- Pacemaker.Accuracy accuracy.Value
        | 1 -> options.Pacemakers.[rulesetId] <- Pacemaker.Lamp lamp.Value
        | _ -> failwith "impossible"