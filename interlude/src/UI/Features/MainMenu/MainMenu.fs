namespace Interlude.UI.Features.MainMenu

open System
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.Utils
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.OptionsMenu

type private MenuButton(onClick, label) as this =
    inherit Widget1()

    let color = Animation.Fade 0.3f
    do
        this
        |-+ Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f)
        |-+ TextBox(K label, K (Color.White, Color.Black), 0.5f)
            .Position { Left = 0.7f %+ 0.0f; Top = 0.0f %+ 10.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %- 20.0f }
        |=* color

    override this.Draw() =
        Draw.quad (Quad.parallelogram 0.5f (this.Bounds.Expand 5.0f)) (Quad.colorOf (Style.highlightF 127 color.Value)) Sprite.DefaultQuad
        Draw.quad (Quad.parallelogram 0.5f this.Bounds) (Quad.colorOf (Style.main 80 ())) Sprite.DefaultQuad
        base.Draw()

    member this.Pop() =
        let (_, _, r, _) = this.Anchors
        r.Value <- -Viewport.vwidth

// Menu screen

type MainMenuScreen() as this =
    inherit Screen.T()

    let playFunc() =
        Logo.moveOffscreen()
        Screen.change Screen.Type.LevelSelect Screen.TransitionFlags.UnderLogo

    let play = MenuButton (playFunc, L"menu.play")
    let options = MenuButton (OptionsMenuRoot.show, L"menu.options")
    let quit = MenuButton ((fun () -> Screen.back Screen.TransitionFlags.UnderLogo), L"menu.quit")

    let newSplash =
        randomSplash "MenuSplashes.txt"
        >> fun s -> s.Split '¬'
        >> fun l -> if l.Length > 1 then l.[0], l.[1] else l.[0], ""
    let mutable splashText = "", ""
    let splashAnim = Animation.Fade 0.0f
    let splashSubAnim = Animation.Fade 0.0f

    do
        this
        |-+ play.Position( Position.Box(0.0f, 0.5f, -100.0f, -200.0f, 1300.0f, 100.0f) )
        |-+ options.Position( Position.Box(0.0f, 0.5f, -100.0f, -50.0f, 1230.0f, 100.0f) )
        |-+ quit.Position( Position.Box(0.0f, 0.5f, -100.0f, 100.0f, 1160.0f, 100.0f) )
        |-* splashAnim
        |=* splashSubAnim
        
    override this.OnEnter prev =
        if AutoUpdate.updateAvailable then Notification.add (L"notification.update.available", NotificationType.System)
        if prev = Screen.Type.SplashScreen && firstLaunch then QuickStartGuide.help()
        splashText <- newSplash()
        Logo.moveMenu()
        Background.dim 0.0f
        Screen.Toolbar.show()
        Song.onFinish <- SongFinishAction.Loop
        splashAnim.Target <- 1.0f
        play.Pop(); options.Pop(); quit.Pop()

    override this.OnExit next =
        Logo.moveOffscreen()
        splashAnim.Target <- 0.0f
        Background.dim 0.7f

    override this.Draw() =
        let c = this.Bounds.CenterX
        let (s, ss) = splashText
        let a1 = splashSubAnim.Value * splashAnim.Value * 255.0f |> int
        let a2 = splashAnim.Alpha
        Text.drawJustB (Content.font, ss, 20.0f, c, this.Bounds.Top + 50.0f + 30.0f * splashSubAnim.Value, (Color.FromArgb (a1, Color.White), Style.color (a1, 0.5f, 0.0f)), 0.5f)
        Text.drawJustB (Content.font, s, 40.0f, c, this.Bounds.Top - 60.0f + 80.0f * splashAnim.Value, (Color.FromArgb (a2, Color.White), Style.color (a2, 0.5f, 0.0f)), 0.5f)
        base.Draw()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        splashSubAnim.Target <- if Mouse.hover (bounds.Expand(-400.0f, 0.0f).SliceTop(100.0f)) then 1.0f else 0.0f
        if (!|"select").Tapped() then playFunc()