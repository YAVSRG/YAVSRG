namespace Interlude.UI.Toolbar

open System.Drawing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.OptionsMenu
open Interlude.Utils

type Toolbar() as this =
    inherit Widget1()

    let HEIGHT = 70.0f

    let barSlider = Animation.Fade 1.0f
    let notifSlider = Animation.Fade 0.0f

    let shown() = not Screen.hideToolbar

    let mutable userCollapse = false
    
    do
        this
        |-* barSlider
        |-* notifSlider
        |-+ TextBox(K version, K (Color.White, Color.Black), 1.0f)
            .Position (Position.Box (1.0f, 1.0f, -305.0f, 0.0f, 300.0f, HEIGHT * 0.5f))
        |-+ TextBox((fun () -> System.DateTime.Now.ToString()), K (Color.White, Color.Black), 1.0f)
            .Position( Position.Box (1.0f, 1.0f, -305.0f, HEIGHT * 0.5f, 300.0f, HEIGHT * 0.5f) )
        |-+ Button(
                (fun () -> Screen.back Screen.TransitionFlag.UnderLogo),
                sprintf "%s %s  " Icons.back (L"menu.back"),
                "exit" )
            .Position( Position.Box (0.0f, 1.0f, 200.0f, HEIGHT) )
        |-+ Button(
                ( fun () -> if shown() && Screen.currentType <> Screen.Type.Play && Screen.currentType <> Screen.Type.Replay then OptionsMenuRoot.show() ),
                L"menu.options",
                "options" )
            .Position( Position.Box(0.0f, 0.0f, 0.0f, -HEIGHT, 200.0f, HEIGHT) )
        |-+ Button(
                ( fun () -> if shown() then Screen.change Screen.Type.Import Screen.TransitionFlag.Default ),
                L"menu.import",
                "import" )
            .Position( Position.Box(0.0f, 0.0f, 200.0f, -HEIGHT, 200.0f, HEIGHT) )
        |-+ Button(
                ( fun () -> if shown() then MarkdownReader.help() ),
                L"menu.help",
                "help" )
            .Position( Position.Box(0.0f, 0.0f, 400.0f, -HEIGHT, 200.0f, HEIGHT) )
        |-+ Button(
                ( fun () -> if shown() then TaskDisplay.Dialog().Show() ),
                L"menu.tasks",
                "tasks" )
            .Position( Position.Box(0.0f, 0.0f, 600.0f, -HEIGHT, 200.0f, HEIGHT) )
        |=+ Jukebox()

    override this.VisibleBounds = Viewport.bounds

    override this.Draw() = 
        let { Rect.Left = l; Top = t; Right = r; Bottom = b } = this.Bounds
        Draw.rect (Rect.Create(l, t - HEIGHT, r, t)) (Style.main 100 ())
        Draw.rect (Rect.Create(l, b, r, b + HEIGHT)) (Style.main 100 ())
        if barSlider.Value > 0.01f then
            let s = this.Bounds.Width / 48.0f
            for i in 0 .. 47 do
                let level = System.Math.Min((Devices.waveForm.[i] + 0.01f) * barSlider.Value * 0.4f, HEIGHT)
                Draw.rect (Rect.Create(l + float32 i * s + 2.0f, t - HEIGHT, l + (float32 i + 1.0f) * s - 2.0f, t - HEIGHT + level)) (Style.accentShade(int level, 1.0f, 0.5f))
                Draw.rect (Rect.Create(r - (float32 i + 1.0f) * s + 2.0f, b + HEIGHT - level, r - float32 i * s - 2.0f, b + HEIGHT)) (Style.accentShade(int level, 1.0f, 0.5f))
        base.Draw()
        Terminal.draw()

    override this.Update(elapsedTime, bounds) =
        if shown() && (!|"toolbar").Tapped() then
            userCollapse <- not userCollapse
            barSlider.Target <- if userCollapse then 0.0f else 1.0f
        Terminal.update()
        base.Update(elapsedTime, bounds.Expand (0.0f, -HEIGHT * if Screen.hideToolbar then 0.0f else barSlider.Value))