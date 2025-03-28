namespace Interlude.Features.MainMenu

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats
open Interlude
open Interlude.Utils
open Interlude.Content
open Interlude.UI
open Interlude.Features.Online
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.LevelSelect
open Interlude.Features.Import

type ConfirmQuitPage(on_confirm: unit -> unit) =
    inherit Page()

    override this.Content() =
        if TaskTracking.in_progress() then
            page_container()
            |+ PageButton.Once(%"confirm.no", Menu.Back)
                .Pos(7)
            |+ PageButton.Once(%"confirm.yes", fork on_confirm Menu.Back)
                .Pos(5)
            |+ Text(%"menu.exit_prompt")
                .Align(Alignment.LEFT)
                .Pos(0, 2, PageWidth.Full)
            |+ Text(%"menu.exit_task_warning")
                .Color(Colors.text_red_2)
                .Align(Alignment.LEFT)
                .Pos(2, 2, PageWidth.Full)
        else
            page_container()
            |+ PageButton.Once(%"confirm.yes", fork on_confirm Menu.Back)
                .Pos(3)
            |+ PageButton.Once(%"confirm.no", Menu.Back)
                .Pos(5)
            |+ Text(%"menu.exit_prompt")
                .Align(Alignment.LEFT)
                .Pos(0, 2, PageWidth.Full)
        :> Widget

    override this.Title = %"confirm"
    override this.OnClose() = ()

type private MenuButton(label: string, on_click: unit -> unit, position: Position) =
    inherit
        SlideContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    override this.Init(parent) =
        this
            .With(
                MouseListener().Button(this),
                Text(label)
                    .Align(Alignment.CENTER)
                    .Color(fun () ->
                        if this.Focused then Colors.text_yellow_2
                        else Colors.text
                    )
                    .Position(Position.SlicePercentR(0.3f).ShrinkY(10.0f).ShrinkB(10.0f))
            )
            .Position(position)
            .Hide()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        Render.quad (Quad.parallelogram 0.5f (this.Bounds.Expand 5.0f)) (!*Palette.HIGHLIGHT_100)

        Render.quad (Quad.parallelogram 0.5f this.Bounds) (!*Palette.MAIN_100)
        base.Draw()

    member this.Hide() =
        this.Position(position.SliceL(-100.0f)).SnapPosition()

    member this.Show() =
        this.Hide()
        this.Position <- position

// todo: cool redesign with news feed and stuff

type MainMenuScreen() =
    inherit Screen()

    let mutable confirmed_quit = false
    let confirm_quit () =
        ConfirmQuitPage(
            fun () ->
                confirmed_quit <- true
                Screen.back Transitions.UnderLogo |> ignore
                GameThread.defer (fun () -> Song.set_low_pass 1.0f)
        )
            .Show()

    let play_action () =
        Screen.change ScreenType.LevelSelect Transitions.Default |> ignore

    let play_button =
        MenuButton(
            %"menu.play",
            play_action,
            Position.Box(0.0f, 0.5f, -300.0f, -200.0f, 1500.0f, 100.0f)
        )

    let options_button =
        MenuButton(
            %"menu.options",
            (fun () -> OptionsMenuPage().Show()),
            Position.Box(0.0f, 0.5f, -300.0f, -50.0f, 1430.0f, 100.0f)
        )

    let quit_button =
        MenuButton(
            %"menu.quit",
            (fun () ->
                if Screen.back Transitions.UnderLogo then
                    Screen.logo.MoveCenter ()
            ),
            Position.Box(0.0f, 0.5f, -300.0f, 100.0f, 1360.0f, 100.0f)
        )

    let choose_splash =
        splash_message_picker "MenuSplashes.txt"
        >> fun s -> s.Split '¬'
        >> fun l -> if l.Length > 1 then l.[0], l.[1] else l.[0], ""

    let mutable splash_text = "", ""
    let splash_fade = Animation.Fade 0.0f
    let splash_subtitle_fade = Animation.Fade 0.0f
    let button_sequence = Animation.Group()

    override this.Init (parent: Widget): unit =
        this
            .Add(
                play_button,
                options_button,
                quit_button,

                AngledButton(
                    Icons.STAR + " " + %"menu.changelog",
                    WikiBrowserPage.ShowChangelog,
                    Palette.MAIN_100
                )
                    .LeanRight(false)
                    .Position(Position.SliceB(AngledButton.HEIGHT).SliceR(300.0f)),

                AngledButton(
                    Icons.MESSAGE_SQUARE + " " + %"menu.discord",
                    (fun () -> open_url ("https://discord.gg/tA22tWR")),
                    Palette.DARK_100
                )
                    .Position(Position.SliceB(AngledButton.HEIGHT).SliceR(300.0f).TranslateX(-325.0f))
            )
        base.Init(parent)

    override this.OnEnter(prev: ScreenType) =
        if Updates.update_available then
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.update_available.title",
                %"notification.update_available.body"
            )

        if prev = ScreenType.SplashScreen then

            if Data.Maintenance.Patterns.recalculate_needed Content.Charts then
                Library.Library.recalculate_patterns()

            if TOTAL_STATS.NotesHit = 0 then
                // todo: quick start wizard instead
                WikiBrowserPage.Show()

        splash_text <- choose_splash ()
        Screen.logo.MoveMenu()
        Background.dim 0.0f
        Toolbar.show ()
        Song.on_finish <-
            SongFinishAction.Custom (fun () ->
                LevelSelect.random_chart ()
                splash_text <- choose_splash ()
            )

        button_sequence.Add
        <| Animation.seq
            [
                Animation.Action play_button.Show
                Animation.Delay 50.0
                Animation.Action options_button.Show
                Animation.Delay 50.0
                Animation.Action quit_button.Show
                Animation.Delay 200.0
                Animation.Action(fun () -> splash_fade.Target <- 1.0f)
            ]

        DiscordRPC.in_menus ("Main menu")

    override this.OnExit(next: ScreenType) =
        if next <> ScreenType.SplashScreen then
            Screen.logo.MoveOffscreen()

        splash_fade.Target <- 0.0f
        splash_fade.Snap()
        play_button.Hide()
        options_button.Hide()
        quit_button.Hide()
        Background.dim 0.7f

    override this.OnBack() : ScreenType option =
        if confirmed_quit then Some ScreenType.SplashScreen
        else confirm_quit(); None

    override this.Draw() =
        let c = this.Bounds.CenterX
        let (splash, subsplash) = splash_text
        let a1 = splash_subtitle_fade.Value * splash_fade.Value * 255.0f |> int
        let a2 = splash_fade.Alpha

        let heading_width = Text.measure (Style.font, splash) * 40.0f

        Render.rect_size
            (c - heading_width * 0.5f - 20.0f)
            (this.Bounds.Top - 25.0f + 40.0f * splash_fade.Value)
            (heading_width + 40.0f)
            70.0f
            (Colors.shadow_2.O1a a2)

        Text.draw_aligned_b (
            Style.font,
            subsplash,
            20.0f,
            c,
            this.Bounds.Top + 50.0f + 30.0f * splash_subtitle_fade.Value,
            (Colors.white.O4a a1, Palette.color (a1, 0.5f, 0.0f)),
            Alignment.CENTER
        )

        Text.draw_aligned_b (
            Style.font,
            splash,
            40.0f,
            c,
            this.Bounds.Top - 20.0f + 40.0f * splash_fade.Value,
            (Colors.white.O4a a2, Palette.color (a2, 0.5f, 0.0f)),
            Alignment.CENTER
        )

        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        splash_fade.Update elapsed_ms
        splash_subtitle_fade.Update elapsed_ms
        button_sequence.Update elapsed_ms

        splash_subtitle_fade.Target <-
            if Mouse.hover (this.Bounds.ShrinkX(400.0f).SliceT(100.0f)) then
                1.0f
            else
                0.0f

        if (%%"select").Pressed() then
            play_action ()