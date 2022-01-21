namespace Interlude.UI.OptionsMenu

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
            | Accuracy _ -> 0
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
                                [| PrettySetting("PacemakerLamp", Selector.FromEnum lamp).Position(300.0f) |] // broken
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
                                seq { 
                                    for id in Interlude.Content.Themes.rulesets.Keys do
                                        yield ((id, Interlude.Content.Themes.rulesets.[id]), WatcherSelection.contains id options.Rulesets.Value)
                                }
                            )
                    PrettySetting("Rulesets",
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
                    PrettySetting("ScrollSpeed", Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Position(200.0f)
                    PrettySetting("HitPosition", Slider(options.HitPosition, 0.005f)).Position(280.0f)
                    PrettySetting("Upscroll", Selector.FromBool options.Upscroll).Position(360.0f)
                    PrettySetting("BackgroundDim", Slider<_>.Percent(options.BackgroundDim, 0.01f)).Position(440.0f)
                    PrettyButton("ScreenCover", 
                        fun() ->
                            add("ScreenCover",
                                {
                                    Content = fun add ->
                                        column [
                                            PrettySetting("ScreenCoverEnabled", Selector.FromBool options.ScreenCover.Enabled).Position(200.0f)
                                            PrettySetting("ScreenCoverHidden", Slider<_>.Percent(options.ScreenCover.Hidden, 0.01f)).Position(350.0f)
                                            PrettySetting("ScreenCoverSudden", Slider<_>.Percent(options.ScreenCover.Sudden, 0.01f)).Position(450.0f)
                                            PrettySetting("ScreenCoverFadeLength", Slider(options.ScreenCover.FadeLength, 0.01f)).Position(550.0f)
                                            Themes.NoteskinPreview 0.35f
                                        ] :> Selectable
                                    Callback = ignore
                                }
                            )
                    ).Position(520.0f)
                    //PrettyButton("Pacemaker", fun () -> add("Pacemaker", pacemaker())).Position(670.0f)
                    PrettyButton("Rulesets", fun () -> add("Rulesets", rulesets())).Position(750.0f)
                ] :> Selectable
            Callback = ignore
        }
