namespace Interlude.UI.OptionsMenu

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
            | Accuracy _ -> 0
            | Lamp l -> l
            |> Setting.simple
        {
            Content = fun add ->
                column [
                    PrettySetting("gameplay.pacemaker.type",
                        refreshChoice
                            [|"ACCURACY"; "LAMP"|]
                            [|
                                [| PrettySetting("gameplay.pacemaker.accuracy", Slider(accuracy, 0.01f)).Position(300.0f) |]
                                [| PrettySetting("gameplay.pacemaker.lamp", Selector.FromEnum lamp).Position(300.0f) |] // broken
                            |] utype
                    ).Position(200.0f)
                ] :> Selectable
            Callback = fun () ->
                match utype.Value with
                | 0 -> options.Pacemaker.Value <- Accuracy accuracy.Value
                | 1 -> options.Pacemaker.Value <- Lamp lamp.Value
                | _ -> failwith "impossible"
        }

    let rulesets() : SelectionPage =
        {
            Content = fun add ->
                column [
                    let setting =
                        Setting.make ignore
                            ( fun () ->
                                Interlude.Content.Rulesets.list()
                                |> Seq.map (fun (key, value) -> (key, value), WatcherSelection.contains key options.Rulesets.Value)
                            )
                    PrettySetting("gameplay.rulesets",
                        CardSelect.Selector(
                            setting,
                            { CardSelect.Config.Default with
                                NameFunc = fun s -> (snd s).Name
                                MarkFunc = 
                                    fun (s, b) -> 
                                        if b then Setting.app (WatcherSelection.add (fst s)) options.Rulesets
                                        else Setting.app (WatcherSelection.delete (fst s)) options.Rulesets
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
                    PrettySetting("gameplay.scrollspeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Position(200.0f)
                    PrettySetting("gameplay.hitposition", Slider(options.HitPosition, 0.005f)).Position(280.0f)
                    PrettySetting("gameplay.upscroll", Selector<_>.FromBool options.Upscroll).Position(360.0f)
                    PrettySetting("gameplay.backgrounddim", Slider<_>.Percent(options.BackgroundDim, 0.01f)).Position(440.0f)
                    PrettyButton("gameplay.screencover", 
                        fun() ->
                            add( N"gameplay.screencover",
                                {
                                    Content = fun add ->
                                        column [
                                            PrettySetting("gameplay.screencover.enabled", Selector<_>.FromBool options.ScreenCover.Enabled).Position(200.0f)
                                            PrettySetting("gameplay.screencover.hidden", Slider<_>.Percent(options.ScreenCover.Hidden, 0.01f)).Position(350.0f)
                                            PrettySetting("gameplay.screencover.sudden", Slider<_>.Percent(options.ScreenCover.Sudden, 0.01f)).Position(450.0f)
                                            PrettySetting("gameplay.screencover.fadelength", Slider(options.ScreenCover.FadeLength, 0.01f)).Position(550.0f)
                                            Themes.NoteskinPreview 0.35f
                                        ] :> Selectable
                                    Callback = ignore
                                }
                            )
                    ).Position(520.0f)
                    //PrettyButton("Pacemaker", fun () -> add("Pacemaker", pacemaker())).Position(670.0f)
                    PrettyButton("gameplay.rulesets", fun () -> add(N"gameplay.rulesets", rulesets())).Position(750.0f)
                ] :> Selectable
            Callback = ignore
        }
