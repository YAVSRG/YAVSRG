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
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Screen.Toolbar
open Interlude.Features
open Interlude.Features.Stats
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.Printerlude

type Toolbar() =
    inherit Widget(NodeType.None)

    let shown () = not hidden
    let mutable userCollapse = false

    let container = StaticContainer(NodeType.None)
    let volume = Volume(Position = Position.Margin(0.0f, HEIGHT))

    do
        container
        |+ Text(
            version,
            Align = Alignment.RIGHT,
            Position = Position.Box(1.0f, 1.0f, -305.0f, -HEIGHT, 300.0f, HEIGHT * 0.5f)
        )
        |+ Text(
            (fun () -> System.DateTime.Now.ToString()),
            Align = Alignment.RIGHT,
            Position = Position.Box(1.0f, 1.0f, -305.0f, -HEIGHT * 0.5f, 300.0f, HEIGHT * 0.5f)
        )
        |+ IconButton(
            %"menu.back",
            Icons.back,
            HEIGHT,
            (fun () -> Screen.back Transitions.Flags.UnderLogo),
            Position = Position.Box(0.0f, 1.0f, 0.0f, -HEIGHT, 160.0f, HEIGHT - 10.0f)
        )
        |+ (FlowContainer.LeftToRight(
                180.0f,
                Spacing = 10.0f,
                AllowNavigation = false,
                Position = Position.SliceTop(HEIGHT).TrimLeft(20.0f)
            )
            |+ InlaidButton(
                %"menu.options.name",
                (fun () ->
                    if
                        shown ()
                        && Screen.currentType <> Screen.Type.Play
                        && Screen.currentType <> Screen.Type.Replay
                    then
                        OptionsMenuRoot.show ()
                ),
                Icons.options,
                Hotkey = "options"
            )
                .Tooltip(Tooltip.Info("menu.options").Hotkey("options"))
            |+ InlaidButton(
                %"menu.import.name",
                (fun () ->
                    if shown () then
                        Screen.change Screen.Type.Import Transitions.Flags.Default
                ),
                Icons.import,
                Hotkey = "import"
            )
                .Tooltip(Tooltip.Info("menu.import").Hotkey("import"))
            |+ InlaidButton(
                %"menu.wiki.name",
                (fun () ->
                    if shown () then
                        Wiki.show ()
                ),
                Icons.wiki,
                HoverIcon = Icons.wiki2,
                Hotkey = "wiki"
            )
                .Tooltip(Tooltip.Info("menu.wiki").Hotkey("wiki"))
            |+ InlaidButton(
                %"menu.stats.name",
                (fun () ->
                    if shown () then
                        Screen.changeNew StatsScreen Screen.Type.Stats Transitions.Flags.Default
                ),
                Icons.stats_2
            )
                .Tooltip(Tooltip.Info("menu.stats")))
        |+ NetworkStatus(Position = Position.SliceTop(HEIGHT).SliceRight(300.0f))
        |+ HotkeyAction(
            "edit_noteskin",
            fun () ->
                if
                    shown ()
                    && Screen.currentType <> Screen.Type.Play
                    && Screen.currentType <> Screen.Type.Replay
                    && (not Interlude.Content.Noteskins.Current.instance.IsEmbedded)
                then
                    Noteskins.EditNoteskinPage(true).Show()
        )
        |+ HotkeyAction(
            "screenshot",
            fun () ->
                let id = DateTime.Now.ToString("yyyy'-'MM'-'dd'.'HH'_'mm'_'ss.fffffff") + ".png"
                let path = Path.Combine(get_game_folder "Screenshots", id)
                let img = Render.screenshot ()
                ImageServices.save_image_jpg.Request((img, path), img.Dispose)

                Notifications.action_feedback_button (
                    Icons.screenshot,
                    %"notification.screenshot",
                    id,
                    %"notification.screenshot.open_folder",
                    fun () -> open_directory (get_game_folder "Screenshots")
                )
        )
        |+ HotkeyAction(
            "reload_themes",
            fun () ->
                first_init <- true
                Noteskins.load ()
                Themes.load ()
                first_init <- false
                Notifications.action_feedback (Icons.system_notification, %"notification.reload_themes", "")
        )
        |+ HotkeyAction(
            "preset1",
            fun () ->
                match options.Preset1.Value with
                | Some s when Screen.currentType <> Screen.Type.Play ->
                    Presets.load s
                    Notifications.action_feedback (Icons.system_notification, %"notification.preset_loaded", s.Name)
                | _ -> ()
        )
        |+ HotkeyAction(
            "preset2",
            fun () ->
                match options.Preset2.Value with
                | Some s when Screen.currentType <> Screen.Type.Play ->
                    Presets.load s
                    Notifications.action_feedback (Icons.system_notification, %"notification.preset_loaded", s.Name)
                | _ -> ()
        )
        |+ HotkeyAction(
            "preset3",
            fun () ->
                match options.Preset3.Value with
                | Some s when Screen.currentType <> Screen.Type.Play ->
                    Presets.load s
                    Notifications.action_feedback (Icons.system_notification, %"notification.preset_loaded", s.Name)
                | _ -> ()
        )
        |+ Conditional(
            (fun () -> AutoUpdate.update_available),
            Updater(Position = Position.Box(0.5f, 1.0f, -250.0f, -HEIGHT, 500.0f, HEIGHT))
        )
        |* volume

    override this.Draw() =
        if hidden then volume.Draw() else
        let {
                Rect.Left = l
                Top = t
                Right = r
                Bottom = b
            } =
            this.Bounds

        Draw.rect (Rect.Create(l, t, r, t + HEIGHT)) !*Palette.MAIN_100
        Draw.rect (Rect.Create(l, b - HEIGHT, r, b)) !*Palette.MAIN_100

        if expandAmount.Value > 0.01f then
            let s = this.Bounds.Width / 48.0f

            for i in 0..47 do
                let level =
                    System.Math.Min((Devices.waveform.[i] + 0.01f) * expandAmount.Value * 0.4f, HEIGHT)

                Draw.rect
                    (Rect.Create(l + float32 i * s + 2.0f, t, l + (float32 i + 1.0f) * s - 2.0f, t + level))
                    (Palette.color (int level, 1.0f, 0.5f))

                Draw.rect
                    (Rect.Create(r - (float32 i + 1.0f) * s + 2.0f, b - level, r - float32 i * s - 2.0f, b))
                    (Palette.color (int level, 1.0f, 0.5f))

        container.Draw()
        Terminal.draw ()

    override this.Update(elapsed_ms, moved) =
        Stats.session.GameTime <- Stats.session.GameTime + elapsed_ms

        let moved =
            if wasHidden <> hidden then
                wasHidden <- hidden
                true
            else
                moved || expandAmount.Moving

        if shown () && (%%"toolbar").Tapped() then
            userCollapse <- not userCollapse
            expandAmount.Target <- if userCollapse then 0.0f else 1.0f

        Terminal.update ()

        if moved then
            this.Bounds <-
                if hidden then
                    this.Parent.Bounds.Expand(0.0f, HEIGHT)
                else
                    this.Parent.Bounds.Expand(0.0f, HEIGHT * (1.0f - expandAmount.Value))

            this.VisibleBounds <-
                if hidden then
                    this.Parent.Bounds
                else
                    this.Parent.Bounds.Expand(0.0f, HEIGHT * 2.0f)

        container.Update(elapsed_ms, moved)

    override this.Init(parent: Widget) =
        base.Init parent

        this.Bounds <-
            if hidden then
                this.Parent.Bounds.Expand(0.0f, HEIGHT)
            else
                this.Parent.Bounds.Expand(0.0f, HEIGHT * (1.0f - expandAmount.Value))

        this.VisibleBounds <-
            if hidden then
                this.Parent.Bounds
            else
                this.Parent.Bounds.Expand(0.0f, HEIGHT * 2.0f)

        container.Init this

    override this.Position
        with set _ = failwith "Position can not be set for toolbar"
