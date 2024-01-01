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
open Interlude.Features
open Interlude.Features.Stats
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.Printerlude

type Toolbar() =
    inherit Widget(NodeType.None)

    let HEIGHT = Toolbar.HEIGHT

    let mutable collapsed_by_user = false

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
            Icons.ARROW_LEFT_CIRCLE,
            HEIGHT,
            (fun () -> Screen.back Transitions.Flags.UnderLogo |> ignore),
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
                        not Toolbar.hidden
                        && Screen.current_type <> Screen.Type.Play
                        && Screen.current_type <> Screen.Type.Replay
                    then
                        OptionsMenuRoot.show ()
                ),
                Icons.SETTINGS,
                Hotkey = "options"
            )
                .Tooltip(Tooltip.Info("menu.options").Hotkey("options"))
            |+ InlaidButton(
                %"menu.import.name",
                (fun () ->
                    if not Toolbar.hidden then
                        Screen.change Screen.Type.Import Transitions.Flags.Default |> ignore
                ),
                Icons.DOWNLOAD,
                Hotkey = "import"
            )
                .Tooltip(Tooltip.Info("menu.import").Hotkey("import"))
            |+ InlaidButton(
                %"menu.wiki.name",
                (fun () ->
                    if not Toolbar.hidden then
                        Wiki.show ()
                ),
                Icons.BOOK,
                HoverIcon = Icons.BOOK_OPEN,
                Hotkey = "wiki"
            )
                .Tooltip(Tooltip.Info("menu.wiki").Hotkey("wiki"))
            |+ InlaidButton(
                %"menu.stats.name",
                (fun () ->
                    if not Toolbar.hidden then
                        Screen.change_new StatsScreen Screen.Type.Stats Transitions.Flags.Default
                        |> ignore
                ),
                Icons.TRENDING_UP
            )
                .Tooltip(Tooltip.Info("menu.stats")))
        |+ NetworkStatus(Position = Position.SliceTop(HEIGHT).SliceRight(300.0f))
        |+ HotkeyAction(
            "edit_noteskin",
            fun () ->
                if
                    not Toolbar.hidden
                    && Screen.current_type <> Screen.Type.Play
                    && Screen.current_type <> Screen.Type.Replay
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
                    Icons.IMAGE,
                    %"notification.screenshot",
                    id,
                    %"notification.screenshot.open_folder",
                    fun () -> open_directory (get_game_folder "Screenshots")
                )
        )
        |+ HotkeyAction(
            "reload_themes",
            fun () ->
                if not Toolbar.hidden then
                    first_init <- true
                    Noteskins.load ()
                    Themes.load ()
                    first_init <- false
                    Gameplay.Chart.recolor()
                    Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.reload_themes", "")
        )
        |+ HotkeyAction(
            "preset1",
            fun () ->
                if not Toolbar.hidden then
                    match Presets.load 1 with
                    | Some success_name ->
                        Gameplay.Chart.recolor()
                        Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_loaded", success_name)
                    | None -> ()
        )
        |+ HotkeyAction(
            "preset2",
            fun () ->
                if not Toolbar.hidden then
                    match Presets.load 2 with
                    | Some success_name ->
                        Gameplay.Chart.recolor()
                        Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_loaded", success_name)
                    | None -> ()
        )
        |+ HotkeyAction(
            "preset3",
            fun () ->
                if not Toolbar.hidden then
                    match Presets.load 3 with
                    | Some success_name ->
                        Gameplay.Chart.recolor()
                        Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_loaded", success_name)
                    | None -> ()
        )
        |+ Conditional(
            (fun () -> AutoUpdate.update_available),
            Updater(Position = Position.Box(0.5f, 1.0f, -250.0f, -HEIGHT, 500.0f, HEIGHT))
        )
        |* volume

    override this.Draw() =
        if Toolbar.hidden then
            volume.Draw()
        else
            let {
                    Rect.Left = l
                    Top = t
                    Right = r
                    Bottom = b
                } =
                this.Bounds

            Draw.rect (Rect.Create(l, t, r, t + HEIGHT)) !*Palette.MAIN_100
            Draw.rect (Rect.Create(l, b - HEIGHT, r, b)) !*Palette.MAIN_100

            if Toolbar.slideout_amount.Value > 0.01f then
                let s = this.Bounds.Width / 48.0f

                for i in 0..47 do
                    let level =
                        System.Math.Min((Devices.waveform.[i] + 0.01f) * Toolbar.slideout_amount.Value * 0.4f, HEIGHT)

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
            if Toolbar.was_hidden <> Toolbar.hidden then
                Toolbar.was_hidden <- Toolbar.hidden
                true
            else
                moved || Toolbar.slideout_amount.Moving

        if not Toolbar.hidden && (%%"toolbar").Tapped() then
            collapsed_by_user <- not collapsed_by_user
            Toolbar.slideout_amount.Target <- if collapsed_by_user then 0.0f else 1.0f

        Terminal.update ()

        if moved then
            this.Bounds <-
                if Toolbar.hidden then
                    this.Parent.Bounds.Expand(0.0f, HEIGHT)
                else
                    this.Parent.Bounds.Expand(0.0f, HEIGHT * (1.0f - Toolbar.slideout_amount.Value))

            this.VisibleBounds <-
                if Toolbar.hidden then
                    this.Parent.Bounds
                else
                    this.Parent.Bounds.Expand(0.0f, HEIGHT * 2.0f)

        container.Update(elapsed_ms, moved)

    override this.Init(parent: Widget) =
        base.Init parent

        this.Bounds <-
            if Toolbar.hidden then
                this.Parent.Bounds.Expand(0.0f, HEIGHT)
            else
                this.Parent.Bounds.Expand(0.0f, HEIGHT * (1.0f - Toolbar.slideout_amount.Value))

        this.VisibleBounds <-
            if Toolbar.hidden then
                this.Parent.Bounds
            else
                this.Parent.Bounds.Expand(0.0f, HEIGHT * 2.0f)

        container.Init this

    override this.Position
        with set _ = failwith "Position can not be set for toolbar"
