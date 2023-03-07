namespace Interlude.Features.Toolbar

open System
open System.IO
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data
open Interlude.Content
open Interlude.UI
open Interlude.Utils
open Interlude.UI.Screen.Toolbar
open Interlude.Features
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.Printerlude

type Toolbar() =
    inherit Widget(NodeType.None)

    let shown() = not hidden
    let mutable userCollapse = false

    let container = StaticContainer(NodeType.None)

    do
        container
        |+ Text(version, Align = Alignment.RIGHT,
            Position = Position.Box (1.0f, 1.0f, -305.0f, -HEIGHT, 300.0f, HEIGHT * 0.5f))
        |+ Text((fun () -> System.DateTime.Now.ToString()), Align = Alignment.RIGHT,
            Position = Position.Box (1.0f, 1.0f, -305.0f, -HEIGHT * 0.5f, 300.0f, HEIGHT * 0.5f))
        |+ IconButton(L"menu.back",
            Icons.back, HEIGHT,
            (fun () -> Screen.back Transitions.Flags.UnderLogo),
            Hotkey = "exit",
            Position = Position.Box(0.0f, 1.0f, 0.0f, -HEIGHT, 160.0f, HEIGHT - 10.0f))
        |+ (
            FlowContainer.LeftToRight(200.0f, AllowNavigation = false, Position = Position.SliceTop (HEIGHT - 10.0f))
            |+ IconButton(L"menu.options",
                Icons.options, HEIGHT,
                ( fun () -> if shown() && Screen.currentType <> Screen.Type.Play && Screen.currentType <> Screen.Type.Replay then OptionsMenuRoot.show() ),
                Hotkey = "options")
            |+ IconButton(L"menu.import",
                Icons.import, HEIGHT,
                ( fun () -> if shown() then Screen.change Screen.Type.Import Transitions.Flags.Default ),
                Hotkey = "import")
            |+ IconButton(L"menu.wiki",
                Icons.wiki, HEIGHT,
                ( fun () -> if shown() then Wiki.show() ),
                Hotkey = "wiki",
                HoverIcon = Icons.wiki2)
            )
        |+ if Interlude.Options.options.EnableConsole.Value then Online.Status(Position = Position.SliceTop(HEIGHT).SliceRight(300.0f)) :> Widget else Dummy()
        |+ HotkeyAction("screenshot", fun () ->
            let id =  DateTime.Now.ToString("yyyy'-'MM'-'dd'.'HH'_'mm'_'ss.fffffff") + ".png"
            let path = Path.Combine(getDataPath "Screenshots", id)
            let img = Render.screenshot()
            ImageServices.save_image.Request((img, path), img.Dispose)
            Notifications.notif("Screenshot saved: " + id, NotificationType.Info) )
        |+ HotkeyAction("reload_themes", fun () -> 
            first_init <- true
            Noteskins.load()
            Themes.load()
            first_init <- false
            Notifications.notif("Reloaded themes & noteskins", NotificationType.Info) )
        |* Jukebox(Position = Position.Margin(0.0f, HEIGHT))

    override this.Draw() = 
        let { Rect.Left = l; Top = t; Right = r; Bottom = b } = this.Bounds
        Draw.rect (Rect.Create(l, t, r, t + HEIGHT)) (Style.main 100 ())
        Draw.rect (Rect.Create(l, b - HEIGHT, r, b)) (Style.main 100 ())
        if expandAmount.Value > 0.01f then
            let s = this.Bounds.Width / 48.0f
            for i in 0 .. 47 do
                let level = System.Math.Min((Devices.waveForm.[i] + 0.01f) * expandAmount.Value * 0.4f, HEIGHT)
                Draw.rect (Rect.Create(l + float32 i * s + 2.0f, t, l + (float32 i + 1.0f) * s - 2.0f, t + level)) (Style.color(int level, 1.0f, 0.5f))
                Draw.rect (Rect.Create(r - (float32 i + 1.0f) * s + 2.0f, b - level, r - float32 i * s - 2.0f, b)) (Style.color(int level, 1.0f, 0.5f))
        container.Draw()
        Terminal.draw()

    override this.Update(elapsedTime, moved) =
        let moved = if wasHidden <> hidden then wasHidden <- hidden; true else moved || expandAmount.Moving
        if shown() && (!|"toolbar").Tapped() then
            userCollapse <- not userCollapse
            expandAmount.Target <- if userCollapse then 0.0f else 1.0f
        Terminal.update()
        if moved then 
            this.Bounds <- if hidden then this.Parent.Bounds.Expand(0.0f, HEIGHT) else this.Parent.Bounds.Expand(0.0f, HEIGHT * (1.0f - expandAmount.Value))
            this.VisibleBounds <- if hidden then this.Parent.Bounds else this.Parent.Bounds.Expand(0.0f, HEIGHT * 2.0f)
        container.Update(elapsedTime, moved)

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- if hidden then this.Parent.Bounds.Expand(0.0f, HEIGHT) else this.Parent.Bounds.Expand(0.0f, HEIGHT * (1.0f - expandAmount.Value))
        this.VisibleBounds <- if hidden then this.Parent.Bounds else this.Parent.Bounds.Expand(0.0f, HEIGHT * 2.0f)
        container.Init this
    
    override this.Position with set _ = failwith "Position can not be set for toolbar"