namespace Interlude.UI

open System
open System.Drawing
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.UI.Selection
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.OptionsMenu
open Interlude.Utils
open Interlude.Input

// Toolbar widgets

module Notifications =

    let taskBox (t: BackgroundTask.ManagedTask) = 
        let w = Frame()

        TextBox(t.get_Name, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f)
        |> w.Add

        TextBox(t.get_Info, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(0.0f, 0.0f, 50.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
        |> w.Add

        Clickable(
            (fun () ->
                match t.Status with
                | Threading.Tasks.TaskStatus.RanToCompletion -> w.Destroy()
                | _ -> t.Cancel(); w.Destroy()), ignore)
        |> w.Add

        w |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 90.0f, 0.0f)

    let taskBoxes =
        let f = FlowContainer()
        Logging.Debug "Subscribed to background tasks"
        BackgroundTask.Subscribe(fun t -> if t.Visible then f.Add(taskBox t))
        f |> positionWidget(-500.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)

    type TaskDisplayDialog() as this = 
        inherit SlideDialog(SlideDialog.Direction.Left, 500.0f)
        do this.Add taskBoxes

        override this.Draw() =
            Draw.rect taskBoxes.Bounds (ScreenGlobals.accentShade(180, 0.4f, 0.0f)) Sprite.Default
            base.Draw()

        override this.OnClose() = this.Remove taskBoxes

    type NotificationDisplay() as this =
        inherit Widget()
        let items = ResizeArray<Color * string * AnimationFade>()
        let slider = new AnimationFade 0.0f
        let notifWidth = 400.0f
        let notifHeight = 35.0f

        do
            this.Animation.Add slider
            ScreenGlobals.addNotification <-
                fun (str: string, t: NotificationType) ->
                    this.Parent.Value.Synchronized(
                        fun () -> 
                            let c =
                                match t with
                                | NotificationType.Info -> Color.Blue
                                | NotificationType.System -> Color.Green
                                | NotificationType.Task -> Color.Purple
                                | NotificationType.Error -> Color.Red
                                | _ -> Color.Black
                            slider.Target <- slider.Target + 1.0f
                            let f = new AnimationFade((if items.Count = 0 then 0.0f else 1.0f), Target = 1.0f)
                            this.Animation.Add f
                            let i = (c, str, f)
                            items.Add i
                            this.Animation.Add(
                                Animation.Serial(
                                    AnimationTimer 4000.0,
                                    AnimationAction(fun () -> f.Target <- 0.0f),
                                    AnimationTimer 1500.0,
                                    AnimationAction(fun () -> slider.Target <- slider.Target - 1.0f; slider.Value <- slider.Value - 1.0f; f.Stop(); items.Remove i |> ignore)
                                )) )

        override this.Draw() =
            if items.Count > 0 then
                Stencil.create false
                Draw.rect this.Bounds Color.Transparent Sprite.Default
                Stencil.draw()
                let struct (_, _, _, b) = this.Bounds
                let m = Rect.centerX this.Bounds
                let mutable y = b - notifHeight * slider.Value
                for (c, s, f) in items do
                    let r = Rect.create (m - notifWidth) y (m + notifWidth) (y + notifHeight)
                    let f = f.Value * 255.0f |> int
                    Draw.rect r (Color.FromArgb(f / 2, c)) Sprite.Default
                    Text.drawFill(Themes.font(), s, r, Color.FromArgb(f, Color.White), 0.5f)
                    y <- y + notifHeight
                Stencil.finish()

type TooltipHandler() as this =
    inherit Widget()
    let mutable active = false
    let mutable bind = Dummy
    let mutable text = [||]
    let mutable timeLeft = 0.0
    let mutable action = ignore

    let SCALE = 30.0f

    let fade = AnimationFade(0.0f)

    do
        this.Animation.Add(fade)
        ScreenGlobals.addTooltip <- 
            fun (b, str, time, callback) ->
                if not active then
                    active <- true
                    fade.Target <- 1.0f
                    bind <- b
                    text <- str.Split("\n")
                    timeLeft <- time
                    action <- callback

    override this.Update(elapsedTime, bounds) =
        if active then
            timeLeft <- timeLeft - elapsedTime
            if timeLeft <= 0.0 then (action(); active <- false; fade.Target <- 0.0f)
            elif bind.Released() then (active <- false; fade.Target <- 0.0f)
        base.Update(elapsedTime, bounds)

    override this.Draw() =
        if fade.Value > 0.01f then
            let x = Mouse.X()
            let mutable y = Mouse.Y() + 50.0f
            //todo: y-clamping
            for str in text do
                let w = Text.measure(Themes.font(), str) * SCALE
                //todo: x-clamping
                Text.drawB(Themes.font(), str, SCALE, x - w * 0.5f, y, (Color.FromArgb(int(255.0f * fade.Value), Color.White), Color.FromArgb(int(255.0f * fade.Value), Color.Black)))
                y <- y + SCALE
        base.Draw()

type Jukebox() as this =
    inherit Widget()
    //todo: right click to seek/tools to pause and play music
    let fade = new AnimationFade 0.0f
    let slider = new AnimationFade 0.0f
    do
        this.Animation.Add fade
        this.Animation.Add slider

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Options.options.Hotkeys.Volume.Value.Pressed() then
            fade.Target <- 1.0f
            Setting.app ((+) (float (Mouse.Scroll()) * 0.02)) Options.options.AudioVolume
            Audio.changeVolume Options.options.AudioVolume.Value
            slider.Target <- float32 Options.options.AudioVolume.Value
        else fade.Target <- 0.0f

    override this.Draw() =
        let r = Rect.sliceBottom 5.0f this.Bounds
        Draw.rect r (ScreenGlobals.accentShade(int (255.0f * fade.Value), 0.4f, 0.0f)) Sprite.Default
        Draw.rect (Rect.sliceLeft(slider.Value * Rect.width r) r) (ScreenGlobals.accentShade(int (255.0f * fade.Value), 1.0f, 0.0f)) Sprite.Default

// Toolbar

type Toolbar() as this =
    inherit Widget()

    let HEIGHT = 70.0f

    let barSlider = new AnimationFade 1.0f
    let notifSlider = new AnimationFade 0.0f

    let mutable userCollapse = false
    let mutable forceCollapse = true
    
    do
        this.Animation.Add barSlider
        this.Animation.Add notifSlider

        TextBox(K version, K (Color.White, Color.Black), 1.0f)
        |> positionWidget(-300.0f, 1.0f, 0.0f, 1.0f, 0.0f, 1.0f, HEIGHT * 0.5f, 1.0f)
        |> this.Add

        TextBox((fun () -> System.DateTime.Now.ToString()), K (Color.White, Color.Black), 1.0f)
        |> positionWidget(-300.0f, 1.0f, HEIGHT * 0.5f, 1.0f, 0.0f, 1.0f, HEIGHT, 1.0f)
        |> this.Add

        Button((fun () -> ScreenGlobals.back(ScreenTransitionFlag.UnderLogo)), "⮜ Back  ", Options.options.Hotkeys.Exit, Sprite.Default)
        |> positionWidget(0.0f, 0.0f, 0.0f, 1.0f, 200.0f, 0.0f, HEIGHT, 1.0f)
        |> this.Add
        
        Button((fun () -> if ScreenGlobals.currentType <> ScreenType.Play then ScreenGlobals.addDialog(SelectionMenu.Options())), "Options", Options.options.Hotkeys.Options, Sprite.Default)
        |> positionWidget(0.0f, 0.0f, -HEIGHT, 0.0f, 200.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button((fun () -> ScreenGlobals.changeScreen(ScreenType.Import, ScreenTransitionFlag.Default)), "Import", Options.options.Hotkeys.Import, Sprite.Default)
        |> positionWidget(200.0f, 0.0f, -HEIGHT, 0.0f, 400.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button(MarkdownReader.help, "Help", Options.options.Hotkeys.Help, Sprite.Default)
        |> positionWidget(400.0f, 0.0f, -HEIGHT, 0.0f, 600.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button((fun () -> ScreenGlobals.addDialog(new Notifications.TaskDisplayDialog())), "Tasks", Options.options.Hotkeys.Tasks, Sprite.Default)
        |> positionWidget(600.0f, 0.0f, -HEIGHT, 0.0f, 800.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Jukebox() |> this.Add
        Notifications.NotificationDisplay() |> this.Add

        ScreenGlobals.setToolbarCollapsed <- fun b -> forceCollapse <- b

    override this.Draw() = 
        let struct (l, t, r, b) = this.Bounds
        Draw.rect(Rect.create l (t - HEIGHT) r t) (ScreenGlobals.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        Draw.rect(Rect.create l b r (b + HEIGHT)) (ScreenGlobals.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        if barSlider.Value > 0.01f then
            let s = (r - l) / 48.0f
            for i in 0 .. 47 do
                let level = System.Math.Min((Audio.waveForm.[i] + 0.01f) * barSlider.Value * 0.4f, HEIGHT)
                Draw.rect(Rect.create (l + float32 i * s + 2.0f) (t - HEIGHT) (l + (float32 i + 1.0f) * s - 2.0f) (t - HEIGHT + level)) (ScreenGlobals.accentShade(int level, 1.0f, 0.5f)) Sprite.Default
                Draw.rect(Rect.create (r - (float32 i + 1.0f) * s + 2.0f) (b + HEIGHT - level) (r - float32 i * s - 2.0f) (b + HEIGHT)) (ScreenGlobals.accentShade(int level, 1.0f, 0.5f)) Sprite.Default
        base.Draw()

    override this.Update(elapsedTime, bounds) =
        if (not forceCollapse) && Options.options.Hotkeys.Toolbar.Value.Tapped() then
            userCollapse <- not userCollapse
            barSlider.Target <- if userCollapse then 0.0f else 1.0f
        base.Update(elapsedTime, Rect.expand (0.0f, -HEIGHT * if forceCollapse then 0.0f else barSlider.Value) bounds)