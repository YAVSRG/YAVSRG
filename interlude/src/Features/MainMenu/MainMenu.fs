namespace Interlude.Features.MainMenu

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Caching
open Interlude.Options
open Interlude
open Interlude.Utils
open Interlude.Content
open Interlude.UI
open Interlude.Features.Online
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu
open Interlude.Features.LevelSelect

type private MenuButton(on_click, label: string, pos: Position) =
    inherit
        SlideContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    override this.Init(parent) =
        this 
        |+ Clickable.Focus this
        |* Text(
            label,
            Align = Alignment.CENTER,
            Color = (fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text),
            Position =
                {
                    Left = 0.7f %+ 0.0f
                    Top = 0.0f %+ 10.0f
                    Right = 1.0f %+ 0.0f
                    Bottom = 1.0f %- 20.0f
                }
        )

        this.Position <- pos
        base.Init parent
        this.Hide()

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        Draw.untextured_quad (Quad.parallelogram 0.5f (this.Bounds.Expand 5.0f)) (!*Palette.HIGHLIGHT_100).AsQuad

        Draw.untextured_quad (Quad.parallelogram 0.5f this.Bounds) (!*Palette.MAIN_100).AsQuad
        base.Draw()

    member this.Hide() =
        this.Position <-
            { pos with
                Right = 0.0f %- 100.0f
            }

        this.SnapPosition()

    member this.Show() =
        this.Position <-
            { pos with
                Right = 0.0f %- 100.0f
            }

        this.SnapPosition()
        this.Position <- pos

// Menu screen
// todo: cool redesign with news feed and stuff

type MainMenuScreen() as this =
    inherit Screen.T()

    let play_action () =
        Screen.change Screen.Type.LevelSelect Transitions.Flags.Default |> ignore

    let play =
        MenuButton(play_action, %"menu.play", Position.Box(0.0f, 0.5f, -300.0f, -200.0f, 1500.0f, 100.0f))

    let options =
        MenuButton(
            OptionsMenuRoot.show,
            %"menu.options",
            Position.Box(0.0f, 0.5f, -300.0f, -50.0f, 1430.0f, 100.0f)
        )

    let quit =
        MenuButton(
            (fun () ->
                if Screen.back Transitions.Flags.UnderLogo then
                    Logo.move_center ()
            ),
            %"menu.quit",
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

    let mutable confirmed_exit = false

    do
        this
        |+ play
        |+ options
        |+ quit
        |+ (StylishButton(
            Wiki.show_changelog,
            K(Icons.STAR + " " + %"menu.changelog"),
            !%Palette.MAIN_100,
            TiltRight = false,
            Position = Position.SliceBottom(50.0f).SliceRight(300.0f)
        ))
        |* StylishButton(
            (fun () -> open_url ("https://discord.gg/tA22tWR")),
            K(Icons.MESSAGE_SQUARE + " " + %"menu.discord"),
            !%Palette.DARK_100,
            Position = Position.SliceBottom(50.0f).SliceRight(300.0f).Translate(-325.0f, 0.0f)
        )

    override this.OnEnter prev =
        if Updates.update_available then
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.update_available.title",
                %"notification.update_available.body"
            )

        if prev = Screen.Type.SplashScreen then
            if 
                Cache.cache_patterns_if_needed 
                    Content.Cache 
                    (fun () ->
                        Notifications.system_feedback (
                            Icons.ALERT_OCTAGON,
                            %"notification.pattern_cache_complete.title",
                            ""
                        )
                    )
            then
                Notifications.system_feedback (
                    Icons.ALERT_OCTAGON,
                    %"notification.pattern_cache_started.title",
                    %"notification.pattern_cache_started.body"
                )
            if first_launch then
                Wiki.show ()

        splash_text <- choose_splash ()
        Logo.move_menu ()
        Background.dim 0.0f
        Toolbar.show ()
        Song.on_finish <- 
            SongFinishAction.Custom (fun () -> 
                Suggestions.random_chart()
                splash_text <- choose_splash ()
            )

        button_sequence.Add
        <| Animation.seq
            [
                Animation.Action play.Show
                Animation.Delay 50.0
                Animation.Action options.Show
                Animation.Delay 50.0
                Animation.Action quit.Show
                Animation.Delay 200.0
                Animation.Action(fun () -> splash_fade.Target <- 1.0f)
            ]

        DiscordRPC.in_menus ("Main menu")

    override this.OnExit next =
        if next <> Screen.Type.SplashScreen then
            Logo.move_offscreen ()

        splash_fade.Target <- 0.0f
        splash_fade.Snap()
        play.Hide()
        options.Hide()
        quit.Hide()
        Background.dim 0.7f

    override this.OnBack() =
        if not Interlude.Options.options.ConfirmExit.Value || confirmed_exit then
            Some Screen.Type.SplashScreen
        else
            Menu
                .ConfirmPage(
                    %"menu.exit_prompt",
                    fun () ->
                        confirmed_exit <- true
                        Screen.back Transitions.Flags.UnderLogo |> ignore
                )
                .Show()

            None

    override this.Draw() =
        let c = this.Bounds.CenterX
        let (splash, subsplash) = splash_text
        let a1 = splash_subtitle_fade.Value * splash_fade.Value * 255.0f |> int
        let a2 = splash_fade.Alpha

        let heading_width = Text.measure (Style.font, splash) * 40.0f

        Draw.rect
            (Rect.Box(
                c - heading_width * 0.5f - 20.0f,
                this.Bounds.Top - 25.0f + 40.0f * splash_fade.Value,
                heading_width + 40.0f,
                70.0f
            ))
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
            if Mouse.hover (this.Bounds.Expand(-400.0f, 0.0f).SliceTop(100.0f)) then
                1.0f
            else
                0.0f

        if (%%"select").Tapped() then
            play_action ()
