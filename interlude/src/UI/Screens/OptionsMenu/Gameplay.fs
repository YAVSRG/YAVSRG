namespace Interlude.UI.OptionsMenu

open Percyqaz.Common
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Compound
open Interlude.UI.Components.Selection.Menu

module Gameplay = 

    //type PacemakerPage(m) as this =
    //    inherit Page(m)

    //    let utype =
    //        match options.Pacemaker.Value with
    //        | Accuracy _ -> 0
    //        | Lamp _ -> 1
    //        |> Setting.simple
    //    let accuracy =
    //        match options.Pacemaker.Value with
    //        | Accuracy a -> a
    //        | Lamp _ -> 0.95
    //        |> Setting.simple
    //        |> Setting.bound 0.0 1.0
    //        |> Setting.round 3
    //    let lamp =
    //        match options.Pacemaker.Value with
    //        | Accuracy _ -> 0
    //        | Lamp l -> l
    //        |> Setting.simple

    //    do 
    //        this |*
    //            page_content m [
    //                PrettySetting("gameplay.pacemaker.type",
    //                    refreshChoice
    //                        [|"ACCURACY"; "LAMP"|]
    //                        [|
    //                            [| PrettySetting("gameplay.pacemaker.accuracy", Slider(accuracy, 0.01f)).Position(300.0f) |]
    //                            [| PrettySetting("gameplay.pacemaker.lamp", Selector.FromEnum lamp).Position(300.0f) |] // broken
    //                        |] utype
    //                ).Pos(200.0f)
    //            ]

    //    override this.Title = N"gameplay.pacemaker"
    //    override this.OnClose() = 
    //        match utype.Value with
    //        | 0 -> options.Pacemaker.Value <- Accuracy accuracy.Value
    //        | 1 -> options.Pacemaker.Value <- Lamp lamp.Value
    //        | _ -> failwith "impossible"

    type RulesetsPage(m) as this =
        inherit Page(m)

        do
            this |*
                page_content m [
                    //let setting =
                    //    Setting.make ignore
                    //        ( fun () ->
                    //            Interlude.Content.Rulesets.list()
                    //            |> Seq.map (fun (key, value) -> (key, value), WatcherSelection.contains key options.Rulesets.Value)
                    //        )
                    //PrettySetting("gameplay.rulesets",
                    //    CardSelect.Selector(
                    //        setting,
                    //        { CardSelect.Config.Default with
                    //            NameFunc = fun s -> (snd s).Name
                    //            MarkFunc = 
                    //                fun (s, b) -> 
                    //                    if b then Setting.app (WatcherSelection.add (fst s)) options.Rulesets
                    //                    else Setting.app (WatcherSelection.delete (fst s)) options.Rulesets
                    //        },
                    //        add
                    //    )
                    //).Pos(200.0f, PRETTYWIDTH, 800.0f)
                ]

        override this.Title = N"gameplay.rulesets"
        override this.OnClose() = ()

    type ScreencoverPage(m) as this =
        inherit Page(m)

        do
            this |*
                page_content m [
                    PrettySetting("gameplay.screencover.enabled", Percyqaz.Flux.UI.Selector<_>.FromBool options.ScreenCover.Enabled).Pos(200.0f)
                    PrettySetting("gameplay.screencover.hidden", Percyqaz.Flux.UI.Slider<_>.Percent(options.ScreenCover.Hidden, 0.01f)).Pos(350.0f)
                    PrettySetting("gameplay.screencover.sudden", Percyqaz.Flux.UI.Slider<_>.Percent(options.ScreenCover.Sudden, 0.01f)).Pos(450.0f)
                    PrettySetting("gameplay.screencover.fadelength", Percyqaz.Flux.UI.Slider(options.ScreenCover.FadeLength, 0.01f)).Pos(550.0f)
                    Themes.NoteskinPreview 0.35f
                ]
        override this.Title = N"gameplay.screencover"
        override this.OnClose() = ()

    type GameplayPage(m) as this =
        inherit Page(m)

        do
            this |*
                page_content m [
                    PrettySetting("gameplay.scrollspeed", Percyqaz.Flux.UI.Slider<_>.Percent(options.ScrollSpeed, 0.0025f)).Pos(200.0f)
                    PrettySetting("gameplay.hitposition", Percyqaz.Flux.UI.Slider(options.HitPosition, 0.005f)).Pos(280.0f)
                    PrettySetting("gameplay.upscroll", Percyqaz.Flux.UI.Selector<_>.FromBool options.Upscroll).Pos(360.0f)
                    PrettySetting("gameplay.backgrounddim", Percyqaz.Flux.UI.Slider<_>.Percent(options.BackgroundDim, 0.01f)).Pos(440.0f)
                    PrettyButton("gameplay.screencover", fun() -> m.ChangePage ScreencoverPage).Pos(520.0f)
                    //PrettyButton("Pacemaker", fun () -> add("Pacemaker", pacemaker())).Pos(670.0f)
                    PrettyButton("gameplay.rulesets", fun () -> m.ChangePage RulesetsPage).Pos(750.0f)
                ]
        override this.Title = N"gameplay"
        override this.OnClose() = ()
