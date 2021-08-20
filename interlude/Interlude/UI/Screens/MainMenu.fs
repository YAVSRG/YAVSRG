namespace Interlude.UI

open System
open System.Drawing
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.UI.Selection
open Interlude.UI.Animation
open Interlude.UI.Components
open Interlude.Utils
open Interlude.UI.OptionsMenu
open Interlude.Input

// Loading screen

type LoadingScreen() as this =
    inherit Screen()

    let mutable closing = false
    let fade = new AnimationFade 1.0f
    do
        this.Animation.Add fade

    override this.OnEnter (prev: ScreenType) =
        fade.Value <- 0.0f
        ScreenGlobals.logo.Move (-400.0f, -400.0f, 400.0f, 400.0f)
        ScreenGlobals.setToolbarCollapsed true
        match prev with
        | ScreenType.MainMenu ->
            closing <- true
            let s = AnimationSequence()
            s.Add (AnimationTimer 1500.0)
            s.Add (AnimationAction (fun () -> ScreenGlobals.back ScreenTransitionFlag.Default))
            this.Animation.Add s
        | _ -> 
            let s = AnimationSequence()
            s.Add (AnimationTimer 1500.0)
            s.Add (AnimationAction(fun () -> ScreenGlobals.changeScreen (ScreenType.MainMenu, ScreenTransitionFlag.UnderLogo)))
            this.Animation.Add s

    override this.OnExit _ = ()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        Audio.changeVolume (Options.options.AudioVolume.Value * float (if closing then 1.0f - fade.Value else fade.Value))
        
    override this.Draw() =
        let (x, y) = Rect.center this.Bounds
        Text.drawJust (Themes.font(), (if closing then "Bye o/" else "Loading :)"), 80.f, x, y - 500.0f, Color.White, 0.5f)

type MenuButton(onClick, label) as this =
    inherit Widget()

    let color = AnimationFade 0.3f
    do
        this.Animation.Add color
        this.Add (new Clickable(onClick, fun b -> color.Target <- if b then 0.3f else 0.3f))
        this.Add (new TextBox(K label, K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.7f, 10.0f, 0.0f, 0.0f, 1.0f, -20.0f, 1.0f))

    override this.Draw() =
        Draw.quad (Quad.parallelogram 0.5f this.Bounds) (Quad.colorOf (ScreenGlobals.accentShade (200, 1.0f, color.Value))) Sprite.DefaultQuad
        base.Draw()

    member this.Pop() =
        let (_, _, r, _) = this.Anchors
        r.Value <- -Render.vwidth

// Menu screen

type MainMenu() as this =
    inherit Screen()

    let playFunc() =
        ScreenGlobals.logo.Move (-Render.vwidth * 0.5f - 600.0f, -300.0f, -Render.vwidth * 0.5f, 300.0f)
        ScreenGlobals.changeScreen (ScreenType.LevelSelect, ScreenTransitionFlag.UnderLogo)

    //todo: localise these buttons
    let play = MenuButton (playFunc, "Play")
    let options = MenuButton ((fun () -> ScreenGlobals.addDialog (SelectionMenu.Options())), "Options")
    let quit = MenuButton ((fun () -> ScreenGlobals.back ScreenTransitionFlag.UnderLogo), "Quit")

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
        Utils.AutoUpdate.checkForUpdates()

    override this.OnEnter prev =
        if Utils.AutoUpdate.updateAvailable then ScreenGlobals.addNotification (Localisation.localise "notification.UpdateAvailable", NotificationType.System)
        if prev = ScreenType.SplashScreen && Options.firstLaunch then MarkdownReader.help()
        splashText <- newSplash()
        ScreenGlobals.logo.Move (-Render.vwidth * 0.5f, -400.0f, 800.0f - Render.vwidth * 0.5f, 400.0f)
        ScreenGlobals.backgroundDim.Target <- 0.0f
        ScreenGlobals.setToolbarCollapsed false
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Loop
        splashAnim.Target <- 1.0f
        play.Pop(); options.Pop(); quit.Pop()

    override this.OnExit next =
        ScreenGlobals.logo.Move (-Render.vwidth * 0.5f - 600.0f, -300.0f, -Render.vwidth * 0.5f, 300.0f)
        splashAnim.Target <- 0.0f
        ScreenGlobals.backgroundDim.Target <- 0.7f

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let c = (right + left) * 0.5f
        let (s, ss) = splashText
        let a1 = splashSubAnim.Value * splashAnim.Value * 255.0f |> int
        let a2 = splashAnim.Value * 255.0f |> int
        Text.drawJustB (Themes.font(), ss, 20.0f, c, top + 50.0f + 30.0f * splashSubAnim.Value, (Color.FromArgb (a1, Color.White), ScreenGlobals.accentShade (a1, 0.5f, 0.0f)), 0.5f)
        Text.drawJustB (Themes.font(), s, 40.0f, c, top - 60.0f + 80.0f * splashAnim.Value, (Color.FromArgb (a2, Color.White), ScreenGlobals.accentShade (a2, 0.5f, 0.0f)), 0.5f)
        base.Draw()

    override this.Update (elapsedTime, bounds) =
        base.Update (elapsedTime, bounds)
        splashSubAnim.Target <- if Mouse.Hover (bounds |> Rect.expand (-400.0f, 0.0f) |> Rect.sliceTop 100.0f) then 1.0f else 0.0f
        if Options.options.Hotkeys.Select.Value.Tapped() then playFunc()