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

    type QuickSwitcher(setting: Setting<string>) =
        inherit StaticContainer(NodeType.None)

        override this.Init(parent: Widget) =
            this
            |* StylishButton(
                (fun () -> this.ToggleDropdown()),
                (fun () -> Rulesets.current.Name),
                !%Palette.MAIN_100,
                TiltRight = false,
                Hotkey = "ruleset_switch"
            )

            base.Init parent

        member this.ToggleDropdown() =
            match this.Dropdown with
            | Some _ -> this.Dropdown <- None
            | _ ->
                let rulesets = Rulesets.list ()

                let d =
                    Dropdown.Selector
                        rulesets
                        (fun (id, rs) -> rs.Name)
                        (fun (id, rs) -> setting.Set id)
                        (fun () -> this.Dropdown <- None)

                d.Position <-
                    Position
                        .SliceBottom(d.Height + 60.0f)
                        .TrimBottom(60.0f)
                        .Margin(Style.PADDING, 0.0f)

                d.Init this
                this.Dropdown <- Some d

        member val Dropdown: Dropdown option = None with get, set

        override this.Draw() =
            base.Draw()

            match this.Dropdown with
            | Some d -> d.Draw()
            | None -> ()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            match this.Dropdown with
            | Some d -> d.Update(elapsed_ms, moved)
            | None -> ()

type PacemakerPage() as this =
    inherit Page()

    let rulesetId = Rulesets.current_hash

    let existing =
        if options.Pacemakers.ContainsKey rulesetId then
            options.Pacemakers.[rulesetId]
        else
            Pacemaker.Default

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
            column ()
            |+ PageSetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.SaveScoreIfUnderPace)
                .Pos(200.0f)
                .Tooltip(Tooltip.Info("gameplay.pacemaker.saveunderpace"))
            |+ CaseSelector(
                "gameplay.pacemaker.type",
                [| %"gameplay.pacemaker.accuracy.name"; %"gameplay.pacemaker.lamp.name" |],
                [|
                    [|
                        PageSetting("gameplay.pacemaker.accuracy", Slider.Percent(accuracy |> Setting.f32))
                            .Pos(370.0f)
                    |]
                    [| PageSetting("gameplay.pacemaker.lamp", Selector(lamps, lamp)).Pos(370.0f) |]
                |],
                utype
            )
                .Pos(300.0f)
            |+ Text(
                %"gameplay.pacemaker.hint",
                Align = Alignment.CENTER,
                Position = Position.SliceBottom(100.0f).TrimBottom(40.0f)
            )
        )

    override this.Title = %"gameplay.pacemaker.name"

    override this.OnClose() =
        match utype.Value with
        | 0 -> options.Pacemakers.[rulesetId] <- Pacemaker.Accuracy accuracy.Value
        | 1 -> options.Pacemakers.[rulesetId] <- Pacemaker.Lamp lamp.Value
        | _ -> failwith "impossible"
