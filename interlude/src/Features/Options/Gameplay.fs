namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Scoring
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Compound
open Interlude.UI.Components.Selection.Menu

module Gameplay = 

    type PacemakerPage() as this =
        inherit Page()

        let utype =
            match options.Pacemaker.Value with
            | Accuracy _ -> 0
            | Lamp _ -> 1
            |> Setting.simple
        let accuracy =
            match options.Pacemaker.Value with
            | Accuracy a -> a
            | Lamp _ -> 0.95
            |> Setting.simple
            |> Setting.bound 0.0 1.0
            |> Setting.round 3
        let lamp =
            match options.Pacemaker.Value with
            | Accuracy _ -> 0
            | Lamp l -> l
            |> Setting.simple

        do 
            this.Content(
                CaseSelector("gameplay.pacemaker.type", 
                    [|"ACCURACY"; "LAMP"|],
                    [|
                        [| PrettySetting("gameplay.pacemaker.accuracy", Slider(accuracy, 0.01f)).Pos(300.0f) |]
                        [| PrettySetting("gameplay.pacemaker.accuracy", Slider(accuracy, 0.01f)).Pos(350.0f) |] // todo: this is broken
                    |], utype)
                )

        override this.Title = N"gameplay.pacemaker"
        override this.OnClose() = 
            match utype.Value with
            | 0 -> options.Pacemaker.Value <- Accuracy accuracy.Value
            | 1 -> options.Pacemaker.Value <- Lamp lamp.Value
            | _ -> failwith "impossible"

    type RulesetsPage() as this =
        inherit Page()

        do 
            this.Content(
                column()
                |+ PrettySetting("gameplay.rulesets",
                        Grid.create Interlude.Content.Rulesets.list (
                            Grid.Config.Default
                                .WithColumn(fun (_, rs: Prelude.Scoring.Ruleset) -> rs.Name)
                                .WithSelection(
                                    (fun (id, _) -> WatcherSelection.contains id options.Rulesets.Value),
                                    (fun ((id, rs), selected) ->
                                        if selected then Setting.app (WatcherSelection.add id) options.Rulesets
                                        else Setting.app (WatcherSelection.delete id) options.Rulesets
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
