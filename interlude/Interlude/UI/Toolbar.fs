namespace Interlude.UI

open System
open System.Drawing
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Animation
open Interlude.UI.OptionsMenu
open Interlude.Utils
open Interlude.Input

// Toolbar widgets

module TaskDisplay =

    let private taskBox (t: BackgroundTask.ManagedTask) = 
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

    let private taskBoxes =
        let f = FlowContainer()
        BackgroundTask.Subscribe(fun t -> if t.Visible then f.Add(taskBox t))
        f |> positionWidget(-500.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)

    let init () = BackgroundTask.Subscribe(fun t -> if t.Visible then taskBoxes.Add(taskBox t))

    type Dialog() as this = 
        inherit SlideDialog(SlideDialog.Direction.Left, 500.0f)
        do this.Add taskBoxes

        override this.Draw() =
            Draw.rect taskBoxes.Bounds (Style.accentShade(180, 0.4f, 0.0f)) Sprite.Default
            base.Draw()

        override this.OnClose() = this.Remove taskBoxes

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
        Draw.rect r (Style.accentShade(int (255.0f * fade.Value), 0.4f, 0.0f)) Sprite.Default
        Draw.rect (Rect.sliceLeft(slider.Value * Rect.width r) r) (Style.accentShade(int (255.0f * fade.Value), 1.0f, 0.0f)) Sprite.Default

// Toolbar implementation

type Toolbar() as this =
    inherit Widget()

    let HEIGHT = 70.0f

    let barSlider = new AnimationFade 1.0f
    let notifSlider = new AnimationFade 0.0f

    let mutable userCollapse = false
    
    do
        this.Animation.Add barSlider
        this.Animation.Add notifSlider

        TextBox(K version, K (Color.White, Color.Black), 1.0f)
        |> positionWidget(-300.0f, 1.0f, 0.0f, 1.0f, 0.0f, 1.0f, HEIGHT * 0.5f, 1.0f)
        |> this.Add

        TextBox((fun () -> System.DateTime.Now.ToString()), K (Color.White, Color.Black), 1.0f)
        |> positionWidget(-300.0f, 1.0f, HEIGHT * 0.5f, 1.0f, 0.0f, 1.0f, HEIGHT, 1.0f)
        |> this.Add

        Button((fun () -> Screen.back Screen.TransitionFlag.UnderLogo), "⮜ Back  ", Options.options.Hotkeys.Exit, Sprite.Default)
        |> positionWidget(0.0f, 0.0f, 0.0f, 1.0f, 200.0f, 0.0f, HEIGHT, 1.0f)
        |> this.Add
        
        Button((fun () -> if Screen.currentType <> Screen.Type.Play then Dialog.add (SelectionMenu(mainOptionsMenu()))), "Options", Options.options.Hotkeys.Options, Sprite.Default)
        |> positionWidget(0.0f, 0.0f, -HEIGHT, 0.0f, 200.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button((fun () -> Screen.change Screen.Type.Import Screen.TransitionFlag.Default), "Import", Options.options.Hotkeys.Import, Sprite.Default)
        |> positionWidget(200.0f, 0.0f, -HEIGHT, 0.0f, 400.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button(MarkdownReader.help, "Help", Options.options.Hotkeys.Help, Sprite.Default)
        |> positionWidget(400.0f, 0.0f, -HEIGHT, 0.0f, 600.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button((fun () -> Dialog.add (TaskDisplay.Dialog())), "Tasks", Options.options.Hotkeys.Tasks, Sprite.Default)
        |> positionWidget(600.0f, 0.0f, -HEIGHT, 0.0f, 800.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Jukebox() |> this.Add
        Notifications.display |> this.Add

    override this.Draw() = 
        let struct (l, t, r, b) = this.Bounds
        Draw.rect(Rect.create l (t - HEIGHT) r t) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        Draw.rect(Rect.create l b r (b + HEIGHT)) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        if barSlider.Value > 0.01f then
            let s = (r - l) / 48.0f
            for i in 0 .. 47 do
                let level = System.Math.Min((Audio.waveForm.[i] + 0.01f) * barSlider.Value * 0.4f, HEIGHT)
                Draw.rect(Rect.create (l + float32 i * s + 2.0f) (t - HEIGHT) (l + (float32 i + 1.0f) * s - 2.0f) (t - HEIGHT + level)) (Style.accentShade(int level, 1.0f, 0.5f)) Sprite.Default
                Draw.rect(Rect.create (r - (float32 i + 1.0f) * s + 2.0f) (b + HEIGHT - level) (r - float32 i * s - 2.0f) (b + HEIGHT)) (Style.accentShade(int level, 1.0f, 0.5f)) Sprite.Default
        base.Draw()

    override this.Update(elapsedTime, bounds) =
        if (not Screen.toolbar) && Options.options.Hotkeys.Toolbar.Value.Tapped() then
            userCollapse <- not userCollapse
            barSlider.Target <- if userCollapse then 0.0f else 1.0f
        base.Update(elapsedTime, Rect.expand (0.0f, -HEIGHT * if Screen.toolbar then 0.0f else barSlider.Value) bounds)