namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Scoring
open Interlude.Content
open Interlude.Options
open Interlude.UI.Menu

module Gameplay = 

    type PacemakerPage() as this =
        inherit Page()

        let rulesetId = Ruleset.hash Rulesets.current
        let existing = if options.Pacemakers.ContainsKey rulesetId then options.Pacemakers.[rulesetId] else Pacemaker.Default

        let utype =
            match existing with
            | Accuracy _ -> 0
            | Lamp _ -> 1
            |> Setting.simple
        let accuracy =
            match existing with
            | Accuracy a -> a
            | Lamp _ -> 0.95
            |> Setting.simple
            |> Setting.bound 0.0 1.0
            |> Setting.round 3
        let lamp =
            match existing with
            | Accuracy _ -> 0
            | Lamp l -> l
            |> Setting.simple

        do 
            let lamps = 
                Rulesets.current.Grading.Lamps
                |> Array.indexed
                |> Array.map (fun (i, l) -> (i, l.Name))
            this.Content(
                CaseSelector("gameplay.pacemaker.type", 
                    [|"ACCURACY"; "LAMP"|],
                    [|
                        [| PrettySetting("gameplay.pacemaker.accuracy", Slider<_>.Percent(accuracy, 0.01f)).Pos(300.0f) |]
                        [| PrettySetting("gameplay.pacemaker.lamp", Selector(lamps, lamp)).Pos(300.0f) |]
                    |], utype)
                )

        override this.Title = N"gameplay.pacemaker"
        override this.OnClose() = 
            match utype.Value with
            | 0 -> options.Pacemakers.[rulesetId] <- Accuracy accuracy.Value
            | 1 -> options.Pacemakers.[rulesetId] <- Lamp lamp.Value
            | _ -> failwith "impossible"

    type RulesetsPage() as this =
        inherit Page()

        do 
            this.Content(
                column()
                |+ PrettySetting("gameplay.rulesets",
                        Grid.create Rulesets.list (
                            Grid.Config.Default
                                .WithColumn(fun (_, rs: Prelude.Scoring.Ruleset) -> rs.Name)
                                .WithSelection(
                                    (fun (id, _) -> CycleList.contains id options.Rulesets.Value),
                                    (fun ((id, rs), selected) ->
                                        if selected then Setting.app (CycleList.add id) options.Rulesets
                                        else Setting.app (CycleList.delete id) options.Rulesets
                                    ))
                        )
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
                |+ PrettySetting("gameplay.screencover.enabled", Selector<_>.FromBool options.ScreenCover.Enabled).Pos(200.0f)
                |+ PrettySetting("gameplay.screencover.hidden", Slider<_>.Percent(options.ScreenCover.Hidden, 0.01f)).Pos(350.0f)
                |+ PrettySetting("gameplay.screencover.sudden", Slider<_>.Percent(options.ScreenCover.Sudden, 0.01f)).Pos(450.0f)
                |+ PrettySetting("gameplay.screencover.fadelength", Slider(options.ScreenCover.FadeLength, 0.01f)).Pos(550.0f)
                |+ PrettySetting("gameplay.screencover.color", ColorPicker(options.ScreenCover.Color, true)).Pos(650.0f, PRETTYWIDTH, PRETTYHEIGHT * 2.0f)
                |+ preview
            )
        override this.Title = N"gameplay.screencover"
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
                |+ PrettyButton("gameplay.screencover", fun() -> Menu.ShowPage ScreencoverPage).Pos(520.0f)
                |+ PrettyButton("gameplay.pacemaker", fun () ->  Menu.ShowPage PacemakerPage).Pos(670.0f)
                |+ PrettyButton("gameplay.rulesets", fun () -> Menu.ShowPage RulesetsPage).Pos(750.0f)
            )
        override this.Title = N"gameplay"
        override this.OnClose() = ()
