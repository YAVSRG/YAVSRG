namespace Interlude.UI

open System
open System.Drawing
open OpenTK
open Prelude.Gameplay.NoteColors
open Prelude.Gameplay.Score
open Prelude.Common
open Interlude.Options
open Interlude.Render
open Interlude
open Interlude.Utils
open Interlude.UI.Components
open Interlude.UI.OptionsMenuFramework

(*
    Actual options menu structure/design data
*)

module OptionsMenuTabs =

    let PRETTYTEXTWIDTH = 500.0f
    let PRETTYHEIGHT = 80.0f
    let PRETTYWIDTH = 1200.0f

    type Divider() =
        inherit Widget()

        member this.Position(y) =
            this |> positionWidget(100.0f, 0.0f, y - 5.0f, 0.0f, 100.0f + PRETTYWIDTH, 0.0f, y + 5.0f, 0.0f)

        override this.Draw() =
            base.Draw()
            Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

    type PrettySetting(name, widget: Selectable) as this =
        inherit Selectable()

        let mutable widget = widget

        do
            this.Add(widget |> positionWidgetA(PRETTYTEXTWIDTH, 0.0f, 0.0f, 0.0f))
            this.Add(TextBox(K (localise name + ":"), (fun () -> ((if this.Selected then Screens.accentShade(255, 1.0f, 0.2f) else Color.White), Color.Black)), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, PRETTYTEXTWIDTH, 0.0f, PRETTYHEIGHT, 0.0f))
    
        member this.Position(y, width, height) =
            this |> positionWidget(100.0f, 0.0f, y, 0.0f, 100.0f + width, 0.0f, y + height, 0.0f)
    
        member this.Position(y, width) = this.Position(y, width, PRETTYHEIGHT)
        member this.Position(y) = this.Position(y, PRETTYWIDTH)

        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (Color.FromArgb(180, 0, 0, 0)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 0, 0, 0)) Sprite.Default
            base.Draw()
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if widget.Hover && not widget.Selected && this.Selected then this.HoverChild <- None; this.Hover <- true
        
        override this.OnSelect() = if not widget.Hover then widget.Selected <- true
        override this.OnDehover() = base.OnDehover(); widget.OnDehover()

        member this.Refresh(w: Selectable) =
            widget.Destroy()
            widget <- w
            this.Add(widget |> positionWidgetA(PRETTYTEXTWIDTH, 0.0f, 0.0f, 0.0f))

    type PrettyButton(name, action) as this =
        inherit Selectable()
        do
            this.Add(TextBox(K (localise name + "  >"), (fun () -> ((if this.Hover then Screens.accentShade(255, 1.0f, 0.5f) else Color.White), Color.Black)), 0.0f))
            this.Add(Clickable((fun () -> this.Selected <- true), (fun b -> if b then this.Hover <- true)))
        override this.OnSelect() = action(); this.Selected <- false
        override this.Draw() =
            if this.Hover then Draw.rect this.Bounds (Color.FromArgb(120, 0, 0, 0)) Sprite.Default
            base.Draw()
        member this.Position(y) = this |> positionWidget(100.0f, 0.0f, y, 0.0f, 100.0f + PRETTYWIDTH, 0.0f, y + PRETTYHEIGHT, 0.0f)
    
    let system(add) =
        column [
            PrettySetting("AudioOffset",
                { new Slider<float>(options.AudioOffset, 0.01f)
                    with override this.OnDeselect() = Audio.globalOffset <- float32 (options.AudioOffset.Get()) * 1.0f<ms> }
            ).Position(200.0f)
            PrettySetting("AudioVolume",
                { new Slider<float>(options.AudioVolume, 0.01f)
                    with override this.OnDeselect() = Audio.changeVolume(options.AudioVolume.Get()) }
            ).Position(300.0f)
            PrettySetting("WindowMode", Selector.FromEnum(config.WindowMode, Options.applyOptions)).Position(400.0f)
            //todo: way to edit resolution settings?
            PrettySetting(
                "FrameLimiter",
                { new Selector(
                    [|"UNLIMITED"; "30"; "60"; "90"; "120"; "240"|],
                    int(config.FrameLimiter.Get() / 30.0) |> min(5),
                    (let e = [|0.0; 30.0; 60.0; 90.0; 120.0; 240.0|] in fun (i, _) -> config.FrameLimiter.Set(e.[i])) )
                    with override this.OnDeselect() = base.OnDeselect(); Options.applyOptions() }
            ).Position(500.0f)
        ]

    let themeChanger(add, refreshColors) =
        Themes.refreshAvailableThemes()
        column [
            PrettySetting("ChooseTheme",
                ListOrderedSelect.ListOrderedSelector(
                    { new ISettable<_>() with 
                        override this.Set(v) =
                            options.EnabledThemes.Clear()
                            options.EnabledThemes.AddRange(v)
                            Themes.loadThemes(options.EnabledThemes)
                            Themes.changeNoteSkin(options.NoteSkin.Get())
                            refreshColors()
                        override this.Get() = options.EnabledThemes }, Themes.availableThemes )
            ).Position(200.0f, PRETTYWIDTH, 500.0f)
            Divider().Position(750.0f)
            PrettyButton("OpenThemeFolder", ignore).Position(800.0f)
            PrettyButton("NewTheme", ignore).Position(900.0f)
        ]

    let themes(add) =
        let mutable keycount = options.KeymodePreference.Get()
        
        let g keycount i =
            let k = if options.ColorStyle.Get().UseGlobalColors then 0 else keycount - 2
            { new ISettable<_>() with
                override this.Set(v) = options.ColorStyle.Get().Colors.[k].[i] <- v
                override this.Get() = options.ColorStyle.Get().Colors.[k].[i] }

        let colors, refreshColors =
            refreshRow
                (fun () -> options.ColorStyle.Get().Style |> colorCount keycount)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    ColorPicker(g keycount i)
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))

        let noteskins = PrettySetting("Noteskin", Selectable())
        let refreshNoteskins() =
            let ns = Themes.noteskins() |> Seq.toArray
            let ids = ns |> Array.map fst
            let names = ns |> Array.map (fun (id, data) -> data.Name)
            Selector(names, Math.Max(0, Array.IndexOf(ids, Themes.currentNoteSkin)), (fun (i, _) -> let id = ns.[i] |> fst in options.NoteSkin.Set(id); Themes.changeNoteSkin(id); refreshColors()))
            |> noteskins.Refresh
        refreshNoteskins()

        column [
            PrettyButton("ChangeTheme", fun () -> add("ChangeTheme", themeChanger(add, refreshColors))).Position(200.0f)
            PrettyButton("EditTheme", ignore).Position(300.0f)
            PrettySetting("Keymode",
                Selector.FromKeymode(
                    { new ISettable<int>() with
                        override this.Set(v) = keycount <- v + 3
                        override this.Get() = keycount - 3 }, refreshColors)
            ).Position(450.0f)
            PrettySetting(
                "ColorStyle",
                Selector.FromEnum(
                    { new ISettable<ColorScheme>() with
                    override this.Set(v) = options.ColorStyle.Set({options.ColorStyle.Get() with Style = v})
                    override this.Get() = options.ColorStyle.Get().Style }, refreshColors)
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
                    (match options.Pacemaker.Get() with
                    | Accuracy _ -> 0
                    | Lamp _ -> 1),
                    (fun (i, s) ->
                        if i <> 0 then options.Pacemaker.Set(Lamp Lamp.SDCB)
                        else options.Pacemaker.Set(Accuracy 0.95)),
                    [|
                        [|PrettySetting("PacemakerAccuracy",
                            Slider(
                                { new FloatSetting(0.95, 0.0, 1.0) with
                                    override this.Get() = match options.Pacemaker.Get() with Accuracy v -> v | Lamp l -> 0.0
                                    override this.Set(v) = base.Set(v); options.Pacemaker.Set(Accuracy <| base.Get()) }, 0.01f) ).Position(300.0f) |]
                        [|PrettySetting("PacemakerLamp",
                            Selector.FromEnum(
                                { new ISettable<Lamp>() with
                                    override this.Get() = match options.Pacemaker.Get() with Accuracy v -> Lamp.NONE | Lamp l -> l
                                    override this.Set(v) = options.Pacemaker.Set(Lamp v) }, ignore) ).Position(300.0f) |] |] )
            ).Position(200.0f)
        ]

    let scoreSystems(add) =
        let judge (s: ISettable<AccuracySystemConfig>) =
            PrettySetting("Judge",
                Slider(
                    { new IntSetting(4, 1, 9) with
                        override this.Get() = match s.Get() with SC (j, _) | SCPlus (j, _) | Wife (j, _) | DP (j, _) -> j | _ -> 4
                        override this.Set(v) =
                            let v = Math.Clamp(v, 1, 9)
                            match s.Get() with
                            | SC (_, r) -> SC (v, r)
                            | SCPlus (_, r) -> SCPlus (v, r)
                            | Wife (_, r) -> Wife (v, r)
                            | DP (_, r) -> DP (v, r)
                            | _ -> SC (v, false)
                            |> s.Set }, 0.1f)
            ).Position(300.0f)
        
        let ridiculous (s: ISettable<AccuracySystemConfig>) =
            PrettySetting("EnableRidiculous",
                Selector.FromBool(
                    { new Setting<bool>(false) with
                        override this.Get() = match s.Get() with SC (_, r) | SCPlus (_, r) | Wife (_, r) | DP (_, r) -> r | _ -> false
                        override this.Set(r) =
                            match s.Get() with
                            | SC (j, _) -> SC (j, r)
                            | SCPlus (j, _) -> SCPlus (j, r)
                            | Wife (j, _) -> Wife (j, r)
                            | DP (j, _) -> DP (j, r)
                            | _ -> SC (4, r) 
                            |> s.Set })
            ).Position(400.0f)

        let overallDifficulty (s: ISettable<AccuracySystemConfig>) =
            PrettySetting("OverallDifficulty",
                Slider(
                    { new FloatSetting(8.0, 0.0, 10.0) with
                        override this.Get() = match s.Get() with OM od -> float od | _ -> 8.0
                        override this.Set(v) = base.Set(v); s.Set(base.Get() |> float32 |> OM) }, 0.01f)
            ).Position(300.0f)

        let editor (s: ISettable<AccuracySystemConfig>) =
            let judge = judge s
            let ridiculous = ridiculous s
            let overallDifficulty = overallDifficulty s
            column [
                PrettySetting("ScoreSystemType",
                    DUEditor(
                        [|"SC"; "SC+"; "WIFE"; "DP"; "OSU!MANIA"|],
                        (match s.Get() with SC _ -> 0 | SCPlus _ -> 1 | Wife _ -> 2 | DP _ -> 3 | OM _ -> 4 | _ -> 0),
                        (fun (i, _) ->
                            match s.Get() with
                            | SC (j, r) | SCPlus (j, r) | Wife (j, r) | DP (j, r) ->
                                match i with
                                | 1 -> SCPlus (j, r)
                                | 2 -> Wife (j, r)
                                | 3 -> DP (j, r)
                                | 4 -> OM 8.0f
                                | _ -> SC (j, r)
                            | _ ->
                                match i with
                                | 1 -> SCPlus (4, false)
                                | 2 -> Wife (4, false)
                                | 3 -> DP (4, false)
                                | 4 -> OM 8.0f
                                | _ -> SC (4, false)
                            |> s.Set),
                        [|
                            [|judge; ridiculous|]; [|judge; ridiculous|]; [|judge; ridiculous|]; [|judge; ridiculous|]
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
        let mutable keycount = options.KeymodePreference.Get()
    
        let f keycount i =
            let k = keycount - 3
            { new ISettable<_>() with
                override this.Set(v) = options.GameplayBinds.[k].[i] <- v
                override this.Get() = options.GameplayBinds.[k].[i] }

        let binds, refreshBinds =
            refreshRow
                (fun () -> keycount)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    KeyBinder(f keycount i, false)
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))

        column [
            PrettySetting("Keymode",
                Selector.FromKeymode(
                    { new ISettable<int>() with
                        override this.Set(v) = keycount <- v + 3
                        override this.Get() = keycount - 3 }, refreshBinds)
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
                        Interlude.Utils.AutoUpdate.applyUpdate(fun () -> Screens.addNotification(Localisation.localise "notification.UpdateInstalled", NotificationType.System))
            ).Position(300.0f)
        ]

    let topLevel(add: string * Selectable -> unit) =
        row [
            BigButton(localise "System", 0, fun () -> add("System", system(add))) |> positionWidget(-790.0f, 0.5f, -150.0f, 0.5f, -490.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localise "Themes", 1, fun () -> add("Themes", themes(add))) |> positionWidget(-470.0f, 0.5f, -150.0f, 0.5f, -170.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localise "Gameplay", 2, fun () -> add("Gameplay", gameplay(add))) |> positionWidget(-150.0f, 0.5f, -150.0f, 0.5f, 150.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localise "Keybinds", 3, fun () -> add("Keybinds", keybinds(add))) |> positionWidget(170.0f, 0.5f, -150.0f, 0.5f, 470.0f, 0.5f, 150.0f, 0.5f);
            BigButton(localise "Debug", 4, fun () -> add("Debug", debug(add))) |> positionWidget(490.0f, 0.5f, -150.0f, 0.5f, 790.0f, 0.5f, 150.0f, 0.5f);
        ]

open OptionsMenuTabs

(*
    Options dialog which manages the scrolling effect
*)

type OptionsMenu() as this =
    inherit Dialog()

    let stack: Selectable option array = Array.create 10 None
    let mutable namestack = []
    let mutable name = ""
    let body = Widget()

    let add(label, widget) =
        let n = (List.length namestack)
        namestack <- localise label :: namestack
        name <- String.Join(" > ", List.rev namestack)
        let w = wrapper widget
        match stack.[n] with
        | None -> ()
        | Some x -> x.Destroy()
        stack.[n] <- Some w
        body.Add(w)
        let n = float32 n + 1.0f
        w.Reposition(0.0f, Render.vheight * n, 0.0f, Render.vheight * n)
        body.Move(0.0f, -Render.vheight * n, 0.0f, -Render.vheight * n)

    let back() =
        namestack <- List.tail namestack
        name <- String.Join(" > ", List.rev namestack)
        let n = List.length namestack
        stack.[n].Value.Dispose()
        let n = float32 n
        body.Move(0.0f, -Render.vheight * n, 0.0f, -Render.vheight * n)

    do
        this.Add(body)
        this.Add(TextBox((fun () -> name), K (Color.White, Color.Black), 0.0f)
            |> positionWidget(20.0f, 0.0f, 20.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f))
        add("Options", topLevel(add))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        match List.length namestack with
        | 0 -> this.Close()
        | n -> if stack.[n - 1].Value.SelectedChild.IsNone then back()

    override this.OnClose() = ScreenLevelSelect.refresh <- true

