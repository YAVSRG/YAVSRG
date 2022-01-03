namespace Interlude.UI.OptionsMenu

open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Common
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Compound
open Interlude.UI.Components.Selection.Menu

module Gameplay = 

    let pacemaker() : SelectionPage =
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
            | Accuracy _ -> Lamp.SDCB
            | Lamp l -> l
            |> Setting.simple
        {
            Content = fun add ->
                column [
                    PrettySetting("PacemakerType",
                        refreshChoice
                            [|"ACCURACY"; "LAMP"|]
                            [|
                                [| PrettySetting("PacemakerAccuracy", Slider(accuracy, 0.01f)).Position(300.0f) |]
                                [| PrettySetting("PacemakerLamp", Selector.FromEnum lamp).Position(300.0f) |]
                            |] utype
                    ).Position(200.0f)
                ] :> Selectable
            Callback = fun () ->
                match utype.Value with
                | 0 -> options.Pacemaker.Value <- Accuracy accuracy.Value
                | 1 -> options.Pacemaker.Value <- Lamp lamp.Value
                | _ -> failwith "impossible"
        }

    let editAccuracySystem (index, sys) =
        let utype =
            match sys with
            | SC _ -> 0
            | SCPlus _ -> 1
            | Wife _ -> 2
            | OM _ -> 3
            | _ -> 0 //nyi
            |> Setting.simple

        let judge =
            match sys with
            | SC (judge, rd)
            | SCPlus (judge, rd)
            | Wife (judge, rd) -> judge
            | _ -> 4
            |> Setting.simple
            |> Setting.bound 1 9
        let judgeEdit = PrettySetting("Judge", Slider(judge, 0.1f)).Position(300.0f)

        let od =
            match sys with
            | OM od -> od
            | _ -> 8.0f
            |> Setting.simple
            |> Setting.bound 0.0f 10.0f
            |> Setting.roundf 1
        let odEdit = PrettySetting("OverallDifficulty", Slider(od, 0.01f)).Position(300.0f)

        let ridiculous =
            match sys with
            | SC (judge, rd)
            | SCPlus (judge, rd)
            | Wife (judge, rd) -> rd
            | _ -> false
            |> Setting.simple
        let ridiculousEdit = PrettySetting("EnableRidiculous", Selector.FromBool ridiculous).Position(400.0f)

        {
            Content = fun add ->
                column [
                    PrettySetting("ScoreSystemType",
                        refreshChoice
                            [|"SC"; "SC+"; "Wife3"; "osu!mania"|]
                            [|
                                [| judgeEdit; ridiculousEdit |]
                                [| judgeEdit; ridiculousEdit |]
                                [| judgeEdit; ridiculousEdit |]
                                [| odEdit |]
                            |] utype
                    ).Position(200.0f)
                ] :> Selectable
            Callback = fun () ->
                let value =
                    match utype.Value with
                    | 0 -> SC (judge.Value, ridiculous.Value)
                    | 1 -> SCPlus (judge.Value, ridiculous.Value)
                    | 2 -> Wife (judge.Value, ridiculous.Value)
                    | 3 -> OM od.Value
                    | _ -> failwith "impossible"
                Setting.app (WatcherSelection.replace index value) options.AccSystems
        }

    let scoreSystems() : SelectionPage =
        {
            Content = fun add ->
                column [
                    let setting =
                        Setting.make ignore ( fun () -> WatcherSelection.indexed options.AccSystems.Value )
                    PrettySetting("ScoreSystems",
                        CardSelect.Selector(
                            setting,
                            { CardSelect.Config.Default with
                                NameFunc = fun (_, s) -> s.ToString()
                                DuplicateFunc = Some (fun (_, s) -> Setting.app (WatcherSelection.add s) options.AccSystems)
                                EditFunc = Some (fun (i, s) -> editAccuracySystem (i, s))
                                DeleteFunc = Some (fun (_, s) -> Setting.app (WatcherSelection.delete s) options.AccSystems)
                                MarkFunc = fun ((_, s), b) -> if b then Setting.app (WatcherSelection.moveToTop s) options.AccSystems
                            },
                            add
                        )
                    ).Position(200.0f, PRETTYWIDTH, 800.0f)
                ] :> Selectable
            Callback = ignore
        }

    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettySetting("ScrollSpeed", Slider(options.ScrollSpeed, 0.005f)).Position(200.0f)
                    PrettySetting("HitPosition", Slider(options.HitPosition, 0.005f)).Position(280.0f)
                    PrettySetting("Upscroll", Selector.FromBool options.Upscroll).Position(360.0f)
                    PrettySetting("BackgroundDim", Slider(options.BackgroundDim, 0.01f)).Position(440.0f)
                    PrettyButton("ScreenCover", 
                        fun() ->
                            add("ScreenCover",
                                {
                                    Content = fun add ->
                                        column [
                                            PrettySetting("ScreenCoverEnabled", Selector.FromBool options.ScreenCover.Enabled).Position(200.0f)
                                            PrettySetting("ScreenCoverHidden", Slider(options.ScreenCover.Hidden, 0.01f)).Position(350.0f)
                                            PrettySetting("ScreenCoverSudden", Slider(options.ScreenCover.Sudden, 0.01f)).Position(450.0f)
                                            PrettySetting("ScreenCoverFadeLength", Slider(options.ScreenCover.FadeLength, 0.01f)).Position(550.0f)
                                            Themes.NoteskinPreview 0.35f
                                        ] :> Selectable
                                    Callback = ignore
                                }
                            )
                    ).Position(520.0f)
                    PrettyButton("Pacemaker", fun () -> add("Pacemaker", pacemaker())).Position(670.0f)
                    PrettyButton("ScoreSystems", fun () -> add("ScoreSystems", scoreSystems())).Position(750.0f)
                ] :> Selectable
            Callback = ignore
        }
