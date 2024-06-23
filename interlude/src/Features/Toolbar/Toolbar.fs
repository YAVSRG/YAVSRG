namespace Interlude.Features.Toolbar

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Stats
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.Printerlude
open Interlude.Features.Noteskins.Edit
open Interlude.Features.Import.osu
open Interlude.Features.Import.Etterna
open Interlude.Features.Tables.Browser
open Interlude.Features.Rulesets

type Toolbar() =
    inherit Widget(NodeType.None)

    let HEIGHT = Toolbar.HEIGHT

    let mutable collapsed_by_user = false

    let container = Container(NodeType.None)
    let volume_when_collapsed = Volume(Position = Position.Margin(0.0f, HEIGHT))

    let load_preset (i: int) =
        match Presets.load i with
        | Some success_name ->
            SelectedChart.recolor ()
            Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_loaded", success_name)
        | None -> ()

    let import_button =
        let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceTop(d.Height).Translate(0.0f, HEIGHT).SliceLeft(370.0f))
        let container = 
            InlaidButton(
                %"menu.import",
                (fun () ->
                    dropdown_wrapper.Toggle(fun () ->
                        DropdownMenu
                            {
                                Items = 
                                    [|
                                        (fun () -> BeatmapBrowserPage().Show()), Icons.DOWNLOAD_CLOUD + " " + %"beatmap_browser"
                                        (fun () -> EtternaPacksBrowserPage().Show()), Icons.DOWNLOAD_CLOUD + " " + %"etterna_pack_browser"
                                        (fun () -> AddRulesetsPage().Show()), Icons.SLIDERS + " " + %"imports.rulesets"
                                        (fun () -> TableBrowserPage().Show()), Icons.SIDEBAR + " " + %"tables.browser"
                                    |]
                            }
                    )
                ),
                Icons.DOWNLOAD,
                Hotkey = "import"
            )
            |+ LoadingIndicator.Strip(
                Imports.import_in_progress,
                Position = Position.SliceBottom(15.0f).SliceTop(Style.PADDING)
            )
            |+ dropdown_wrapper
        container.Tooltip(Tooltip.Info("menu.import").Hotkey("import"))

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
            Position = Position.Box(0.0f, 1.0f, 10.0f, -HEIGHT + 7.5f, 180.0f, HEIGHT)
        )
        |+ (FlowContainer.LeftToRight(
                180.0f,
                Spacing = 10.0f,
                AllowNavigation = false,
                Position = Position.SliceTop(HEIGHT).TrimLeft(20.0f)
            )
            |+ InlaidButton(
                %"menu.options",
                OptionsMenuPage.Show,
                Icons.SETTINGS
            )
                .Tooltip(Tooltip.Info("menu.options").Hotkey("options"))
            |+ import_button
            |+ InlaidButton(
                %"menu.wiki",
                WikiBrowserPage.Show,
                Icons.BOOK,
                HoverIcon = Icons.BOOK_OPEN,
                Hotkey = "wiki"
            )
                .Tooltip(Tooltip.Info("menu.wiki").Hotkey("wiki"))
            |+ InlaidButton(
                %"menu.stats",
                (fun () ->
                    Screen.change_new StatsScreen Screen.Type.Stats Transitions.Default
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
                    Screen.current_type <> Screen.Type.Play
                    && Screen.current_type <> Screen.Type.Replay
                    && (not Content.Noteskin.IsEmbedded)
                then
                    EditNoteskinPage(true).Show()
        )
        |+ HotkeyAction(
            "reload_themes",
            fun () ->
                if not (Dialog.exists()) then
                    Themes.reload_current ()
                    Noteskins.reload_current ()
                    SelectedChart.recolor ()
                    Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.reload_themes", "")
        )
        |+ HotkeyAction("preset1", fun () -> load_preset 1)
        |+ HotkeyAction("preset2", fun () -> load_preset 2)
        |+ HotkeyAction("preset3", fun () -> load_preset 3)
        |+ Updater(Position = Position.Box(1.0f, 1.0f, -600.0f, -HEIGHT, 300.0f, HEIGHT))
            .Conditional(fun () -> Updates.update_available)
        |+ Jukebox(Position = Position.Box(0.0f, 1.0f, 200.0f, -62.5f, 560.0f, 55.0f))
        |* Volume(Position = Position.Margin(0.0f, HEIGHT))

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

        if Screen.current_type <> Screen.Type.Score && (%%"screenshot").Tapped() then
            Toolbar.take_screenshot()
        
        if (Screen.current_type = Screen.Type.Score || not Toolbar.hidden) && (%%"options").Tapped() then
            OptionsMenuPage.Show()

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
