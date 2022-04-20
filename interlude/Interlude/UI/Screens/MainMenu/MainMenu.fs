namespace Interlude.UI.Screens.MainMenu

open System
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.Utils
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.UI.OptionsMenu
open Interlude.Input

type private MenuButton(onClick, label) as this =
    inherit Widget()

    let color = AnimationFade 0.3f
    do
        this.Animation.Add color
        this.Add (new Clickable(onClick, fun b -> color.Target <- if b then 0.7f else 0.3f))
        this.Add (new TextBox(K label, K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.7f, 10.0f, 0.0f, 0.0f, 1.0f, -20.0f, 1.0f))

    override this.Draw() =
        Draw.quad (Quad.parallelogram 0.5f (Rect.expand (5.0f, 5.0f) this.Bounds)) (Quad.colorOf (Style.highlightF 127 color.Value)) Sprite.DefaultQuad
        Draw.quad (Quad.parallelogram 0.5f this.Bounds) (Quad.colorOf (Style.main 80 ())) Sprite.DefaultQuad
        base.Draw()

    member this.Pop() =
        let (_, _, r, _) = this.Anchors
        r.Value <- -Render.vwidth

// Menu screen

type Screen() as this =
    inherit Screen.T()

    let playFunc() =
        Logo.moveOffscreen()
        Screen.change Screen.Type.LevelSelect Screen.TransitionFlag.UnderLogo

    //todo: localise these buttons
    let play = MenuButton (playFunc, L"menu.play")
    let options = MenuButton (OptionsMenuRoot.show, L"menu.options")
    let quit = MenuButton ((fun () -> Screen.back Screen.TransitionFlag.UnderLogo), L"menu.quit")

    let newSplash =
        randomSplash "MenuSplashes.txt"
        >> fun s -> s.Split '¬'
        >> fun l -> if l.Length > 1 then l.[0], l.[1] else l.[0], ""
    let mutable splashText = "", ""
    let splashAnim = AnimationFade 0.0f
    let splashSubAnim = AnimationFade 0.0f

    do
        this.Add(play |> positionWidget (-100.0f, 0.0f, -200.0f, 0.5f, 1200.0f, 0.0f, -100.0f, 0.5f))
        this.Add(options |> positionWidget (-100.0f, 0.0f, -50.0f, 0.5f, 1130.0f, 0.0f, 50.0f, 0.5f))
        this.Add(quit |> positionWidget (-100.0f, 0.0f, 100.0f, 0.5f, 1060.0f, 0.0f, 200.0f, 0.5f))
        this.Animation.Add splashAnim
        this.Animation.Add splashSubAnim

    override this.OnEnter prev =
        if AutoUpdate.updateAvailable then Notification.add (L"notification.update.available", NotificationType.System)
        if prev = Screen.Type.SplashScreen && firstLaunch then MarkdownReader.help()
        splashText <- newSplash()
        Logo.moveMenu()
        Screen.backgroundDim.Target <- 0.0f
        Screen.hideToolbar <- false
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Loop
        splashAnim.Target <- 1.0f
        play.Pop(); options.Pop(); quit.Pop()

    override this.OnExit next =
        Logo.moveOffscreen()
        splashAnim.Target <- 0.0f
        Screen.backgroundDim.Target <- 0.7f

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let c = (right + left) * 0.5f
        let (s, ss) = splashText
        let a1 = splashSubAnim.Value * splashAnim.Value * 255.0f |> int
        let a2 = splashAnim.Value * 255.0f |> int
        Text.drawJustB (Content.font, ss, 20.0f, c, top + 50.0f + 30.0f * splashSubAnim.Value, (Color.FromArgb (a1, Color.White), Style.accentShade (a1, 0.5f, 0.0f)), 0.5f)
        Text.drawJustB (Content.font, s, 40.0f, c, top - 60.0f + 80.0f * splashAnim.Value, (Color.FromArgb (a2, Color.White), Style.accentShade (a2, 0.5f, 0.0f)), 0.5f)
        base.Draw()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        splashSubAnim.Target <- if Mouse.Hover (bounds |> Rect.expand (-400.0f, 0.0f) |> Rect.sliceTop 100.0f) then 1.0f else 0.0f
        if (!|Hotkey.Select).Tapped() then playFunc()