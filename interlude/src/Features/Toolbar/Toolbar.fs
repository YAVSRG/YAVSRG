namespace Interlude.Features.Toolbar

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats
open Interlude.Content
open Interlude.UI
open Interlude
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Stats
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.OptionsMenu.Library
open Interlude.Features.Printerlude

type Toolbar() =
    inherit Widget(NodeType.None)

    let HEIGHT = Toolbar.HEIGHT

    let mutable collapsed_by_user = false

    let container = Container(NodeType.None)
    let volume_when_collapsed = Volume(Position = Position.Shrink(0.0f, HEIGHT))

    let load_preset (i: int) =
        match Presets.load i with
        | Some success_name ->
            SelectedChart.recolor ()
            Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_loaded", success_name)
        | None -> ()

    let import_button =
        InlaidButton(
            %"menu.import",
            (fun () -> ImportsMenuPage().Show()),
            Icons.DOWNLOAD,
            Hotkey = "import"
        )
        |+ LoadingIndicator.Strip(
            Imports.import_in_progress,
            Position = Position.BorderB(Style.PADDING)
        )

    override this.Init(parent) =
        container
        |+ Text(
            Updates.version,
            Align = Alignment.RIGHT,
            Position = Position.Box(1.0f, 1.0f, -305.0f, -HEIGHT, 300.0f, HEIGHT * 0.5f)
        )
        |+ Text(
            (fun () -> System.DateTime.Now.ToString()),
            Align = Alignment.RIGHT,
            Position = Position.Box(1.0f, 1.0f, -305.0f, -HEIGHT * 0.5f, 300.0f, HEIGHT * 0.5f)
        )
        |+ InlaidButton(
            %"menu.back",
            (fun () -> Screen.back Transitions.UnderLogo |> ignore),
            Icons.ARROW_LEFT_CIRCLE,
            Position = Position.Box(0.0f, 1.0f, 10.0f, -HEIGHT + 7.5f, 180.0f, InlaidButton.HEIGHT)
        )
        |+ InlaidButton(
            Icons.MENU,
            (fun () -> QuickMenuPage().Show()),
            "",
            Position = Position.SliceT(InlaidButton.HEIGHT).ShrinkL(10.0f).SliceL(HEIGHT)
        )
            .Help(Help.Info("menu.quick").Hotkey("quick_menu"))
        |+ (FlowContainer.LeftToRight(
                180.0f,
                Spacing = 10.0f,
                AllowNavigation = false,
                Position = Position.SliceT(InlaidButton.HEIGHT).ShrinkL(HEIGHT + 20.0f)
            )
            |+ InlaidButton(
                %"menu.options",
                (fun () -> OptionsMenuPage().Show()),
                Icons.SETTINGS
            )
            |+ import_button
            |+ InlaidButton(
                %"menu.wiki",
                WikiBrowserPage.Show,
                Icons.BOOK_OPEN,
                Hotkey = "wiki"
            )
            |+ InlaidButton(
                %"menu.stats",
                (fun () -> StatsPage().Show()),
                Icons.TRENDING_UP,
                Hotkey = "stats"
            )
        )
        |+ NetworkStatus(Position = Position.SliceT(HEIGHT).SliceR(300.0f))
        |+ HotkeyAction(
            "reload_content",
            fun () ->
                if not (Dialog.exists()) then
                    Themes.reload_current ()
                    Skins.load()
                    Rulesets.load()
                    SelectedChart.recolor ()
                    Notifications.action_feedback (Icons.CHECK, %"notification.reload_content", "")
        )
        |+ HotkeyAction("preset1", fun () -> load_preset 1)
        |+ HotkeyAction("preset2", fun () -> load_preset 2)
        |+ HotkeyAction("preset3", fun () -> load_preset 3)
        |+ Updater(Position = Position.Box(1.0f, 1.0f, -600.0f, -HEIGHT, 300.0f, HEIGHT))
            .Conditional(fun () -> Updates.update_available)
        |+ Jukebox(Position = { Position.SliceB(55.0f).Translate(200.0f, -7.5f) with Right = 0.4f %- 10.0f })
        |* Volume(Position = Position.Shrink(0.0f, HEIGHT))

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

        volume_when_collapsed.Init this
        container.Init this

    override this.Draw() =
        if Toolbar.hidden then
            volume_when_collapsed.Draw()
        else
            let {
                    Rect.Left = l
                    Top = t
                    Right = r
                    Bottom = b
                } =
                this.Bounds

            Render.rect (Rect.Create(l, t, r, t + HEIGHT)) !*Palette.MAIN_100
            Render.rect (Rect.Create(l, b - HEIGHT, r, b)) !*Palette.MAIN_100

            if Toolbar.slideout_amount.Value > 0.01f then
                let s = this.Bounds.Width / 48.0f

                for i in 0..47 do
                    let level =
                        System.Math.Min((Audio.waveform.[i] + 0.01f) * Toolbar.slideout_amount.Value * 0.4f, HEIGHT)

                    Render.rect
                        (Rect.Create(l + float32 i * s + 2.0f, t, l + (float32 i + 1.0f) * s - 2.0f, t + level))
                        (Palette.color (int level, 1.0f, 0.5f))

                    Render.rect
                        (Rect.Create(r - (float32 i + 1.0f) * s + 2.0f, b - level, r - float32 i * s - 2.0f, b))
                        (Palette.color (int level, 1.0f, 0.5f))

            container.Draw()
            Terminal.draw ()

    override this.Update(elapsed_ms, moved) =
        if Screen.current_type <> Screen.Type.SplashScreen then
            CURRENT_SESSION.GameTime <- CURRENT_SESSION.GameTime + elapsed_ms

        let moved =
            if Toolbar.was_hidden <> Toolbar.hidden then
                Toolbar.was_hidden <- Toolbar.hidden
                true
            else
                moved || Toolbar.slideout_amount.Moving

        if not Toolbar.hidden && (%%"toolbar").Tapped() then
            collapsed_by_user <- not collapsed_by_user
            Toolbar.slideout_amount.Target <- if collapsed_by_user then 0.0f else 1.0f

        if Screen.current_type <> Screen.Type.Score && (%%"screenshot").Tapped() then
            Toolbar.take_screenshot()

        if (Screen.current_type = Screen.Type.Score || not Toolbar.hidden) && (%%"options").Tapped() then
            OptionsMenuPage().Show()
        if (Screen.current_type = Screen.Type.Score || not Toolbar.hidden) && (%%"quick_menu").Tapped() then
            QuickMenuPage().Show()

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

        if Toolbar.hidden then
            volume_when_collapsed.Update(elapsed_ms, moved)
        else
            container.Update(elapsed_ms, moved)

    override this.Position
        with set _ = failwith "Position can not be set for toolbar"