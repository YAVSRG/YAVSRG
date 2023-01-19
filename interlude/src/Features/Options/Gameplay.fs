namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Scoring
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Utils

module Gameplay = 

    type PacemakerPage() as this =
        inherit Page()

        let rulesetId = Ruleset.hash Rulesets.current
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
                |+ PrettySetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.ScaveScoreIfUnderPace).Pos(200.0f)
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

    type RulesetsPage() as this =
        inherit Page()

        do 
            this.Content(
                column()
                |+ PrettySetting("gameplay.rulesets",
                        //Grid.create Rulesets.list (
                        //    Grid.Config.Default
                        //        .WithColumn(fun (_, rs: Prelude.Scoring.Ruleset) -> rs.Name)
                        //        .WithSelection(
                        //            (fun (id, _) -> CycleList.contains id options.Rulesets.Value),
                        //            (fun ((id, rs), selected) ->
                        //                if selected then Setting.app (CycleList.add id) options.Rulesets
                        //                else Setting.app (CycleList.delete id) options.Rulesets
                        //            ))
                        //)
                        Dummy()
                    ).Pos(200.0f, PRETTYWIDTH, 800.0f)
                )

        override this.Title = N"gameplay.rulesets"
        override this.OnClose() = ()

    type ScreencoverPage() as this =
        inherit Page()

        let preview = Themes.NoteskinPreview 0.35f

        do
            this.Content(
                column()
                |+ PrettySetting("gameplay.lanecover.enabled", Selector<_>.FromBool options.LaneCover.Enabled).Pos(200.0f)
                |+ PrettySetting("gameplay.lanecover.hidden", Slider<_>.Percent(options.LaneCover.Hidden, 0.01f)).Pos(350.0f)
                |+ PrettySetting("gameplay.lanecover.sudden", Slider<_>.Percent(options.LaneCover.Sudden, 0.01f)).Pos(450.0f)
                |+ PrettySetting("gameplay.lanecover.fadelength", Slider(options.LaneCover.FadeLength, 0.01f)).Pos(550.0f)
                |+ PrettySetting("gameplay.lanecover.color", ColorPicker(options.LaneCover.Color, true)).Pos(650.0f, PRETTYWIDTH, PRETTYHEIGHT * 2.0f)
                |+ preview
            )
        override this.Title = N"gameplay.lanecover"
        override this.OnDestroy() = preview.Destroy()
        override this.OnClose() = ()

    type GameplayPage() as this =
        inherit Page()

        do
            this.Content(
                column()
                |+ PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Pos(200.0f)
                |+ PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f)).Pos(280.0f)
                |+ PrettySetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll).Pos(360.0f)
                |+ PrettySetting("gameplay.backgrounddim", Slider<_>.Percent(options.BackgroundDim, 0.01f)).Pos(440.0f)
                |+ PrettyButton("gameplay.lanecover", fun() -> Menu.ShowPage ScreencoverPage).Pos(520.0f)
                |+ PrettyButton("gameplay.pacemaker", fun () ->  Menu.ShowPage PacemakerPage).Pos(670.0f)
                |+ PrettyButton("gameplay.rulesets", fun () -> Menu.ShowPage RulesetsPage).Pos(750.0f)
            )
        override this.Title = N"gameplay"
        override this.OnClose() = ()
