namespace Interlude.UI.Toolbar

open System.Drawing
open Interlude
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.OptionsMenu
open Interlude.Utils

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

        Button((fun () -> Screen.back Screen.TransitionFlag.UnderLogo), sprintf "%s %s  " Icons.back (L"menu.back"), Options.options.Hotkeys.Exit)
        |> positionWidget(0.0f, 0.0f, 0.0f, 1.0f, 200.0f, 0.0f, HEIGHT, 1.0f)
        |> this.Add
        
        Button(( fun () -> if Screen.currentType <> Screen.Type.Play && Screen.currentType <> Screen.Type.Replay then OptionsMenuRoot.show() ), L"menu.options", Options.options.Hotkeys.Options)
        |> positionWidget(0.0f, 0.0f, -HEIGHT, 0.0f, 200.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button((fun () -> Screen.change Screen.Type.Import Screen.TransitionFlag.Default), L"menu.import", Options.options.Hotkeys.Import)
        |> positionWidget(200.0f, 0.0f, -HEIGHT, 0.0f, 400.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button(MarkdownReader.help, L"menu.help", Options.options.Hotkeys.Help)
        |> positionWidget(400.0f, 0.0f, -HEIGHT, 0.0f, 600.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Button((fun () -> TaskDisplay.Dialog().Show()), L"menu.tasks", Options.options.Hotkeys.Tasks)
        |> positionWidget(600.0f, 0.0f, -HEIGHT, 0.0f, 800.0f, 0.0f, 0.0f, 0.0f)
        |> this.Add

        Jukebox() |> this.Add

    override this.VisibleBounds = this.Parent.Value.VisibleBounds

    override this.Draw() = 
        let struct (l, t, r, b) = this.Bounds
        Draw.rect(Rect.create l (t - HEIGHT) r t) (Style.main 100 ()) Sprite.Default
        Draw.rect(Rect.create l b r (b + HEIGHT)) (Style.main 100 ()) Sprite.Default
        if barSlider.Value > 0.01f then
            let s = (r - l) / 48.0f
            for i in 0 .. 47 do
                let level = System.Math.Min((Audio.waveForm.[i] + 0.01f) * barSlider.Value * 0.4f, HEIGHT)
                Draw.rect(Rect.create (l + float32 i * s + 2.0f) (t - HEIGHT) (l + (float32 i + 1.0f) * s - 2.0f) (t - HEIGHT + level)) (Style.accentShade(int level, 1.0f, 0.5f)) Sprite.Default
                Draw.rect(Rect.create (r - (float32 i + 1.0f) * s + 2.0f) (b + HEIGHT - level) (r - float32 i * s - 2.0f) (b + HEIGHT)) (Style.accentShade(int level, 1.0f, 0.5f)) Sprite.Default
        base.Draw()
        Terminal.draw()

    override this.Update(elapsedTime, bounds) =
        if (not Screen.toolbar) && Options.options.Hotkeys.Toolbar.Value.Tapped() then
            userCollapse <- not userCollapse
            barSlider.Target <- if userCollapse then 0.0f else 1.0f
        Terminal.update()
        base.Update(elapsedTime, Rect.expand (0.0f, -HEIGHT * if Screen.toolbar then 0.0f else barSlider.Value) bounds)