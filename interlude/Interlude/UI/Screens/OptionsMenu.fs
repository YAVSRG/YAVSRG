namespace Interlude.UI

open System
open System.Drawing
open OpenTK
open Prelude.Gameplay.NoteColors
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Selection
open Interlude.UI.Screens.LevelSelect

(*
    Actual options menu structure/design data
*)

module OptionsMenu =
    
    let system(add) =
        column [
            PrettySetting("AudioOffset",
                { new Slider<float>(options.AudioOffset, 0.01f)
                    with override this.OnDeselect() = Audio.globalOffset <- float32 options.AudioOffset.Value * 1.0f<ms> }
            ).Position(200.0f)
            PrettySetting("AudioVolume",
                { new Slider<float>(options.AudioVolume, 0.01f)
                    with override this.OnDeselect() = Audio.changeVolume(options.AudioVolume.Value) }
            ).Position(300.0f)
            PrettySetting("WindowMode", Selector.FromEnum(config.WindowMode, Options.applyOptions)).Position(400.0f)
            //todo: way to edit resolution settings?
            PrettySetting(
                "FrameLimiter",
                { new Selector(
                    [|"UNLIMITED"; "30"; "60"; "90"; "120"; "240"|],
                    int(config.FrameLimiter.Value / 30.0) |> min(5),
                    (let e = [|0.0; 30.0; 60.0; 90.0; 120.0; 240.0|] in fun (i, _) -> config.FrameLimiter.Value <- e.[i]) )
                    with override this.OnDeselect() = base.OnDeselect(); Options.applyOptions() }
            ).Position(500.0f)
        ]

    let themeChanger(add, refresh) =
        Themes.refreshAvailableThemes()
        column [
            PrettySetting("ChooseTheme",
                ListOrderedSelect.ListOrderedSelector(
                    { new ISettable<_>() with
                        override this.Value
                            with set(v) =
                                options.EnabledThemes.Clear()
                                options.EnabledThemes.AddRange(v)
                                Themes.loadThemes(options.EnabledThemes)
                                Themes.changeNoteSkin(options.NoteSkin.Value)
                                refresh()
                            and get() = options.EnabledThemes }, Themes.availableThemes )
            ).Position(200.0f, PRETTYWIDTH, 500.0f)
            Divider().Position(750.0f)
            PrettyButton("OpenThemeFolder",
                fun () ->
                    //todo: move this to utils
                    let target = System.Diagnostics.ProcessStartInfo("file://" + System.IO.Path.GetFullPath(getDataPath("Themes")), UseShellExecute = true)
                    System.Diagnostics.Process.Start target |> ignore).Position(800.0f)
            PrettyButton("NewTheme", fun () -> ScreenGlobals.addDialog <| TextInputDialog(Render.bounds, "Enter theme name", Themes.createNew)).Position(900.0f)
        ]

    let themes(add) =
        let mutable keycount = options.KeymodePreference.Value
        
        let g keycount i =
            let k = if options.ColorStyle.Value.UseGlobalColors then 0 else keycount - 2
            { new ISettable<_>() with
                override this.Value
                    with set(v) = options.ColorStyle.Value.Colors.[k].[i] <- v
                    and get() = options.ColorStyle.Value.Colors.[k].[i] }

        let colors, refreshColors =
            refreshRow
                (fun () -> colorCount keycount options.ColorStyle.Value.Style)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    ColorPicker(g keycount i)
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))

        let noteskins = PrettySetting("Noteskin", Selectable())
        let refreshNoteskins() =
            let ns = Themes.noteskins() |> Seq.toArray
            let ids = ns |> Array.map fst
            let names = ns |> Array.map (fun (id, data) -> data.Config.Name)
            Selector(names, Math.Max(0, Array.IndexOf(ids, Themes.currentNoteSkin)), (fun (i, _) -> let id = ns.[i] |> fst in options.NoteSkin.Value <- id; Themes.changeNoteSkin id; refreshColors()))
            |> noteskins.Refresh
        refreshNoteskins()

        column [
            PrettyButton("ChangeTheme", fun () -> add("ChangeTheme", themeChanger(add, fun () -> refreshColors(); refreshNoteskins()))).Position(200.0f)
            PrettyButton("EditTheme", ignore).Position(300.0f)
            PrettySetting("Keymode",
                Selector.FromKeymode(
                    { new ISettable<int>() with
                        override this.Value
                            with set(v) = keycount <- v + 3
                            and get() = keycount - 3 }, refreshColors)
            ).Position(450.0f)
            PrettySetting(
                "ColorStyle",
                Selector.FromEnum(
                    { new ISettable<ColorScheme>() with
                        override this.Value
                            with set(v) = options.ColorStyle.Apply(fun x -> { x with Style = v })
                            and get() = options.ColorStyle.Value.Style }, refreshColors)
            ).Position(550.0f)
            PrettySetting("NoteColors", colors).Position(650.0f, Render.vwidth - 200.0f, 120.0f)
            noteskins.Position(800.0f)
            PrettyButton("EditNoteskin", ignore).Position(900.0f)
        ]

    let pacemaker(add) =
        column [
            PrettySetting("PacemakerType",
                DUEditor(
                    [|"ACCURACY"; "LAMP"|],
                    (match options.Pacemaker.Value with
                    | Accuracy _ -> 0
                    | Lamp _ -> 1),
                    (fun (i, s) ->
                        if i <> 0 then options.Pacemaker.Value <- Lamp Lamp.SDCB
                        else options.Pacemaker.Value <- Accuracy 0.95),
                    [|
                        [| PrettySetting("PacemakerAccuracy",
                            Slider(
                                { new FloatSetting(0.95, 0.0, 1.0) with
                                    override this.Value
                                        with get() = match options.Pacemaker.Value with Accuracy v -> v | Lamp l -> 0.0
                                        and set(v) = base.Value <- v; options.Pacemaker.Value <- Accuracy base.Value }, 0.01f) ).Position(300.0f) |]
                        [| PrettySetting("PacemakerLamp",
                            Selector.FromEnum(
                                { new ISettable<Lamp>() with
                                    override this.Value
                                        with get() = match options.Pacemaker.Value with Accuracy v -> Lamp.NONE | Lamp l -> l
                                        and set(v) = options.Pacemaker.Value <- Lamp v }, ignore) ).Position(300.0f) |] |] )
            ).Position(200.0f)
        ]

    let scoreSystems(add) =
        let judge (s: ISettable<AccuracySystemConfig>) =
            PrettySetting("Judge",
                Slider(
                    { new IntSetting(4, 1, 9) with
                        override this.Value
                            with get() = match s.Value with SC (j, _) | SCPlus (j, _) | Wife (j, _) -> j | _ -> 4
                            and set(v) =
                                let v = Math.Clamp(v, 1, 9)
                                s.Value <- 
                                    match s.Value with
                                    | SC (_, r) -> SC (v, r)
                                    | SCPlus (_, r) -> SCPlus (v, r)
                                    | Wife (_, r) -> Wife (v, r)
                                    | _ -> SC (v, false) }, 0.1f)
            ).Position(300.0f)
        
        let ridiculous (s: ISettable<AccuracySystemConfig>) =
            PrettySetting("EnableRidiculous",
                Selector.FromBool(
                    { new Setting<bool>(false) with
                        override this.Value
                            with get() = match s.Value with SC (_, r) | SCPlus (_, r) | Wife (_, r) -> r | _ -> false
                            and set(r) =
                                s.Value <- 
                                    match s.Value with
                                    | SC (j, _) -> SC (j, r)
                                    | SCPlus (j, _) -> SCPlus (j, r)
                                    | Wife (j, _) -> Wife (j, r)
                                    | _ -> SC (4, r) })
            ).Position(400.0f)

        let overallDifficulty (s: ISettable<AccuracySystemConfig>) =
            PrettySetting("OverallDifficulty",
                Slider(
                    { new FloatSetting(8.0, 0.0, 10.0) with
                        override this.Value
                            with get() = match s.Value with OM od -> float od | _ -> 8.0
                            and set(v) = base.Value <- v; s.Value <- base.Value |> float32 |> OM }, 0.01f)
            ).Position(300.0f)

        let editor (s: ISettable<AccuracySystemConfig>) =
            let judge = judge s
            let ridiculous = ridiculous s
            let overallDifficulty = overallDifficulty s
            column [
                PrettySetting("ScoreSystemType",
                    DUEditor(
                        [|"SC"; "SC+"; "WIFE"; "OSU!MANIA"|],
                        (match s.Value with SC _ -> 0 | SCPlus _ -> 1 | Wife _ -> 2 | OM _ -> 3 | _ -> 0),
                        (fun (i, _) ->
                            s.Value <- 
                                match s.Value with
                                | SC (j, r) | SCPlus (j, r) | Wife (j, r) ->
                                    match i with
                                    | 1 -> SCPlus (j, r)
                                    | 2 -> Wife (j, r)
                                    | 3 -> OM 8.0f
                                    | _ -> SC (j, r)
                                | _ ->
                                    match i with
                                    | 1 -> SCPlus (4, false)
                                    | 2 -> Wife (4, false)
                                    | 3 -> OM 8.0f
                                    | _ -> SC (4, false)),
                        [|
                            [|judge; ridiculous|]; [|judge; ridiculous|]; [|judge; ridiculous|]
                            [|overallDifficulty|]
                        |]
                )).Position(200.0f)
            ] :> Selectable

        column [
            PrettySetting("ScoreSystems",
                WatcherSelect.WatcherSelector(options.AccSystems, editor, (fun o -> o.ToString()), add)
            ).Position(200.0f, PRETTYWIDTH, 800.0f)
        ]

    let gameplay(add: string * Selectable -> unit) =
        column [
            PrettySetting("ScrollSpeed", Slider(options.ScrollSpeed :?> FloatSetting, 0.005f)).Position(200.0f)
            PrettySetting("HitPosition", Slider(options.HitPosition :?> IntSetting, 0.005f)).Position(280.0f)
            PrettySetting("Upscroll", Selector.FromBool(options.Upscroll)).Position(360.0f)
            PrettySetting("BackgroundDim", Slider(options.BackgroundDim :?> FloatSetting, 0.01f)).Position(440.0f)
            PrettyButton("ScreenCover", 
                fun() ->
                    //todo: preview of what screencover looks like
                    add("ScreenCover",
                        column [
                            PrettySetting("ScreenCoverUp", Slider(options.ScreenCoverUp :?> FloatSetting, 0.01f)).Position(200.0f)
                            PrettySetting("ScreenCoverDown", Slider(options.ScreenCoverDown :?> FloatSetting, 0.01f)).Position(300.0f)
                            PrettySetting("ScreenCoverFadeLength", Slider(options.ScreenCoverFadeLength :?> IntSetting, 0.01f)).Position(400.0f)
                        ])
            ).Position(520.0f)
            PrettyButton("Pacemaker", fun () -> add("Pacemaker", pacemaker(add))).Position(670.0f)
            PrettyButton("ScoreSystems", fun () -> add("ScoreSystems", scoreSystems(add))).Position(750.0f)
            PrettyButton("LifeSystems", ignore).Position(830.0f)
        ]

    let keybinds(add) =
        let mutable keycount = options.KeymodePreference.Value
    
        let f keycount i =
            let k = keycount - 3
            { new ISettable<_>() with
                override this.Value
                    with set(v) = options.GameplayBinds.[k].[i] <- v
                    and get() = options.GameplayBinds.[k].[i] }

        let binds, refreshBinds =
            refreshRow
                (fun () -> keycount)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    { new KeyBinder(f keycount i, false) with
                        override this.OnDeselect() =
                            base.OnDeselect()
                            if i + 1 < k then
                                match this.SParent.Value with
                                | :? ListSelectable as s -> s.Synchronized(fun () -> s.Next(); s.HoverChild.Value.Selected <- true)
                                | _ -> failwith "impossible"
                    }
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))

        column [
            PrettySetting("Keymode",
                Selector.FromKeymode(
                    { new ISettable<int>() with
                        override this.Value
                            with set(v) = keycount <- v + 3
                            and get() = keycount - 3 }, refreshBinds)
            ).Position(200.0f)
            PrettySetting("GameplayBinds", binds).Position(280.0f, Render.vwidth - 200.0f, 120.0f)
            PrettyButton("Hotkeys", ignore).Position(400.0f)
        ]

    let debug(add) =
        column [
            PrettyButton("RebuildCache", fun () -> BackgroundTask.Create TaskFlags.LONGRUNNING "Rebuilding Cache" Gameplay.cache.RebuildCache |> ignore).Position(200.0f)
            PrettyButton("DownloadUpdate",
                fun () ->
                    if Interlude.Utils.AutoUpdate.updateAvailable then
                        Interlude.Utils.AutoUpdate.applyUpdate(fun () -> ScreenGlobals.addNotification(Localisation.localise "notification.UpdateInstalled", NotificationType.System))
            ).Position(300.0f)
        ]

    let topLevel(add: string * Selectable -> unit) =
        row [
            BigButton(localiseOption "System", 0, fun () -> add("System", system(add))) |> positionWidget(-790.0f, 0.5f, -150.0f, 0.5f, -490.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localiseOption "Themes", 1, fun () -> add("Themes", themes(add))) |> positionWidget(-470.0f, 0.5f, -150.0f, 0.5f, -170.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localiseOption "Gameplay", 2, fun () -> add("Gameplay", gameplay(add))) |> positionWidget(-150.0f, 0.5f, -150.0f, 0.5f, 150.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localiseOption "Keybinds", 3, fun () -> add("Keybinds", keybinds(add))) |> positionWidget(170.0f, 0.5f, -150.0f, 0.5f, 470.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localiseOption "Debug", 4, fun () -> add("Debug", debug(add))) |> positionWidget(490.0f, 0.5f, -150.0f, 0.5f, 790.0f, 0.5f, 150.0f, 0.5f);
        ] :> Selectable

    let quickplay(add: string * Selectable -> unit) =
        let firstNote = Gameplay.currentChart.Value.Notes.First |> Option.map Prelude.Charts.Interlude.offsetOf |> Option.defaultValue 0.0f<ms>
        let offset = Gameplay.chartSaveData.Value.Offset
        column [
            //offset changer!
            PrettySetting("SongAudioOffset",
                Slider(
                    { new FloatSetting(float (offset.Value - firstNote), -200.0, 200.0) with
                        override this.Value
                            with get() = base.Value
                            and set(v) = base.Value <- v; offset.Value <- toTime v + firstNote; Audio.localOffset <- toTime v }, 0.01f)
            ).Position(200.0f)
            PrettySetting("ScrollSpeed", Slider(options.ScrollSpeed :?> FloatSetting, 0.005f)).Position(280.0f)
            PrettySetting("HitPosition", Slider(options.HitPosition :?> IntSetting, 0.005f)).Position(360.0f)
            PrettySetting("Upscroll", Selector.FromBool(options.Upscroll)).Position(440.0f)
            //PrettySetting("BackgroundDim", Slider(options.BackgroundDim :?> FloatSetting, 0.01f)).Position(440.0f)
            //PrettyButton("ScoreSystems", fun () -> add("ScoreSystems", scoreSystems(add))).Position(560.0f)
            //PrettyButton("LifeSystems", ignore).Position(640.0f)
        ] :> Selectable

(*
    Options dialog which manages the scrolling effect
*)

    type SelectionMenu with

        static member Options() = SelectionMenu(topLevel)
        static member QuickPlay() =
            { new SelectionMenu(quickplay) with
                override this.OnClose() = base.OnClose(); Audio.playLeadIn() }

