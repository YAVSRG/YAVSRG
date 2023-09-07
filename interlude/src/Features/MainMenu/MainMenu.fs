namespace Interlude.Features.MainMenu

open System
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
open Interlude.Features.Wiki
open Interlude.Features.OptionsMenu

type private MenuButton(onClick, label: string, pos) as this =
    inherit DynamicContainer(NodeType.Button(fun () -> Style.click.Play(); onClick()))

    do
        this
        |+ Clickable.Focus this
        |* Text(label,
            Align = Alignment.CENTER,
            Color = (fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text),
            Position = { Left = 0.7f %+ 0.0f; Top = 0.0f %+ 10.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %- 20.0f })
        this.Position <- pos

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    override this.Draw() =
        Draw.quad (Quad.parallelogram 0.5f (this.Bounds.Expand 5.0f)) (Quad.colorOf !*Palette.HIGHLIGHT_100) Sprite.DefaultQuad
        Draw.quad (Quad.parallelogram 0.5f this.Bounds) (Quad.colorOf !*Palette.MAIN_100) Sprite.DefaultQuad
        base.Draw()

    member this.Pop() =
        this.Position <- { pos with Right = 0.0f %- Viewport.vwidth }
        this.SnapPosition()
        this.Position <- pos

// Menu screen

type MainMenuScreen() as this =
    inherit Screen.T()

    let playFunc() = Screen.change Screen.Type.LevelSelect Transitions.Flags.Default

    let play = MenuButton (playFunc, L"menu.play.name", Position.Box(0.0f, 0.5f, -100.0f, -200.0f, 1300.0f, 100.0f))
    let options = MenuButton (OptionsMenuRoot.show, L"menu.options.name", Position.Box(0.0f, 0.5f, -100.0f, -50.0f, 1230.0f, 100.0f))
    let quit = MenuButton ((fun () -> Screen.back Transitions.Flags.UnderLogo), L"menu.quit.name", Position.Box(0.0f, 0.5f, -100.0f, 100.0f, 1160.0f, 100.0f))

    let newSplash =
        randomSplash "MenuSplashes.txt"
        >> fun s -> s.Split '¬'
        >> fun l -> if l.Length > 1 then l.[0], l.[1] else l.[0], ""
    let mutable splashText = "", ""
    let splashAnim = Animation.Fade 0.0f
    let splashSubAnim = Animation.Fade 0.0f

    do
        this
        |+ play
        |+ options
        |+ quit
        |+ Text((fun () -> if AutoUpdate.updateDownloaded then L"menu.updatehint" else ""),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT,
            Position = Position.Box(1.0f, 1.0f, 490.0f, 40.0f).Translate(-500.0f, -95.0f))
        |+ (
            let b = StylishButton(
                Wiki.show_changelog,
                K (Icons.star + " " + L"menu.changelog"),
                !%Palette.MAIN_100,
                TiltRight = false,
                Position = Position.Box(1.0f, 1.0f, 300.0f, 50.0f).Translate(-300.0f, -50.0f) )
            let c = b.TextColor
            b.TextColor <- fun () -> if AutoUpdate.updateAvailable then Color.Yellow, Color.Black else c()
            b
        )
        |* StylishButton(
            (fun () -> openUrl("https://discord.gg/tA22tWR")),
            K (Icons.comment + " " + L"menu.discord"),
            !%Palette.DARK_100,
            Position = Position.Box(1.0f, 1.0f, 300.0f, 50.0f).Translate(-625.0f, -50.0f) )
        
    override this.OnEnter prev =
        if AutoUpdate.updateAvailable then Notifications.system_feedback (Icons.system_notification, L"notification.update_available.title", L"notification.update_available.body")
        if prev = Screen.Type.SplashScreen && firstLaunch then Wiki.show()
        splashText <- newSplash()
        Logo.moveMenu()
        Background.dim 0.0f
        Screen.Toolbar.show()
        Song.onFinish <- SongFinishAction.Loop
        splashAnim.Target <- 1.0f
        play.Pop(); options.Pop(); quit.Pop()

    override this.OnExit next =
        if next <> Screen.Type.SplashScreen then Logo.moveOffscreen()
        splashAnim.Target <- 0.0f; splashAnim.Snap()
        Background.dim 0.7f

    override this.OnBack() = Some Screen.Type.SplashScreen

    override this.Draw() =
        let c = this.Bounds.CenterX
        let (s, ss) = splashText
        let a1 = splashSubAnim.Value * splashAnim.Value * 255.0f |> int
        let a2 = splashAnim.Alpha
        Text.drawJustB (Style.font, ss, 20.0f, c, this.Bounds.Top + 50.0f + 30.0f * splashSubAnim.Value, (Colors.white.O4a a1, Palette.color (a1, 0.5f, 0.0f)), Alignment.CENTER)
        Text.drawJustB (Style.font, s, 40.0f, c, this.Bounds.Top - 60.0f + 80.0f * splashAnim.Value, (Colors.white.O4a a2, Palette.color (a2, 0.5f, 0.0f)), Alignment.CENTER)
        base.Draw()

    override this.Update (elapsedTime, moved) =
        base.Update (elapsedTime, moved)
        splashAnim.Update elapsedTime
        splashSubAnim.Update elapsedTime
        splashSubAnim.Target <- if Mouse.hover (this.Bounds.Expand(-400.0f, 0.0f).SliceTop(100.0f)) then 1.0f else 0.0f
        if (!|"select").Tapped() then playFunc()