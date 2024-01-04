﻿namespace Interlude.Features.MainMenu

open System
open Percyqaz.Common
open Prelude.Common
open Interlude.Features
open Interlude.Options
open Interlude.Utils
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.UI.Components
open Interlude.Features.Online
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu

type private MenuButton(on_click, label: string, pos) =
    inherit
        DynamicContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    override this.Init(parent) =
        this |+ Clickable.Focus this
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

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        Draw.untextured_quad (Quad.parallelogram 0.5f (this.Bounds.Expand 5.0f)) (Quad.color !*Palette.HIGHLIGHT_100)

        Draw.untextured_quad (Quad.parallelogram 0.5f this.Bounds) (Quad.color !*Palette.MAIN_100)
        base.Draw()

    member this.Hide() =
        this.Position <-
            { pos with
                Right = 0.0f %- Viewport.vwidth
            }

        this.SnapPosition()

    member this.Show() =
        this.Position <-
            { pos with
                Right = 0.0f %- Viewport.vwidth
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
        MenuButton(play_action, %"menu.play.name", Position.Box(0.0f, 0.5f, -300.0f, -200.0f, 1500.0f, 100.0f))

    let options =
        MenuButton(
            OptionsMenuRoot.show,
            %"menu.options.name",
            Position.Box(0.0f, 0.5f, -300.0f, -50.0f, 1430.0f, 100.0f)
        )

    let quit =
        MenuButton(
            (fun () ->
                if Screen.back Transitions.Flags.UnderLogo then
                    Logo.move_center ()
            ),
            %"menu.quit.name",
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
            Position = Position.Box(1.0f, 1.0f, 300.0f, 50.0f).Translate(-300.0f, -50.0f)
        ))
        |* StylishButton(
            (fun () -> open_url ("https://discord.gg/tA22tWR")),
            K(Icons.MESSAGE_SQUARE + " " + %"menu.discord"),
            !%Palette.DARK_100,
            Position = Position.Box(1.0f, 1.0f, 300.0f, 50.0f).Translate(-625.0f, -50.0f)
        )

    override this.OnEnter prev =
        if AutoUpdate.update_available then
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.update_available.title",
                %"notification.update_available.body"
            )

        if prev = Screen.Type.SplashScreen && first_launch then
            Wiki.show ()

        splash_text <- choose_splash ()
        Logo.move_menu ()
        Background.dim 0.0f
        Toolbar.show ()
        Song.on_finish <- SongFinishAction.LoopFromBeginning
        splash_fade.Target <- 1.0f

        button_sequence.Add
        <| Animation.seq
            [
                Animation.Action play.Show
                Animation.Delay 50.0
                Animation.Action options.Show
                Animation.Delay 50.0
                Animation.Action quit.Show
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

    override this.OnBack() = Some Screen.Type.SplashScreen

    override this.Draw() =
        let c = this.Bounds.CenterX
        let (s, ss) = splash_text
        let a1 = splash_subtitle_fade.Value * splash_fade.Value * 255.0f |> int
        let a2 = splash_fade.Alpha

        Text.draw_aligned_b (
            Style.font,
            ss,
            20.0f,
            c,
            this.Bounds.Top + 50.0f + 30.0f * splash_subtitle_fade.Value,
            (Colors.white.O4a a1, Palette.color (a1, 0.5f, 0.0f)),
            Alignment.CENTER
        )

        Text.draw_aligned_b (
            Style.font,
            s,
            40.0f,
            c,
            this.Bounds.Top - 60.0f + 80.0f * splash_fade.Value,
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
