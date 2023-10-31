namespace Interlude.UI

open Percyqaz.Flux.Input
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI

module Screen =

    type Type =
        | SplashScreen = 0
        | MainMenu = 1
        | Import = 2
        | Lobby = 3
        | LevelSelect = 4
        | Play = 5
        | Practice = 6
        | Replay = 7
        | Score = 8
        | Stats = 9

    [<AbstractClass>]
    type T() =
        inherit StaticContainer(NodeType.None)
        abstract member OnEnter: Type -> unit
        abstract member OnExit: Type -> unit
        abstract member OnBack: unit -> Type option

    module Toolbar =
        let HEIGHT = 70.0f
        let expandAmount = Animation.Fade 1.0f
        let mutable hidden = false
        let mutable wasHidden = false

        let hide () = hidden <- true
        let show () = hidden <- false

        let moving () =
            wasHidden <> hidden || expandAmount.Moving

    let globalAnimation = Animation.fork [ Palette.accent_color; Toolbar.expandAmount ]

    let logo = Logo.display

    let mutable private current = Unchecked.defaultof<T>
    let private screens: T array = Array.zeroCreate 5

    let init (_screens: T array) =
        assert (_screens.Length = 5)

        for i = 0 to 4 do
            screens.[i] <- _screens.[i]

        current <- screens.[0]

    let mutable exit = false
    let mutable currentType = Type.SplashScreen

    type ScreenContainer() =
        inherit Widget(NodeType.None)

        override this.Position
            with set _ = failwith "Not permitted"

        override this.Update(elapsedTime, moved) =
            let moved = moved || Toolbar.moving ()

            if moved then
                this.Bounds <-
                    if Toolbar.hidden then
                        Viewport.bounds
                    else
                        Viewport.bounds.Shrink(0.0f, Toolbar.HEIGHT * Toolbar.expandAmount.Value)

                this.VisibleBounds <- Viewport.bounds

            current.Update(elapsedTime, moved)

        override this.Init(parent: Widget) =
            base.Init parent

            this.Bounds <-
                if Toolbar.hidden then
                    Viewport.bounds
                else
                    Viewport.bounds.Shrink(0.0f, Toolbar.HEIGHT * Toolbar.expandAmount.Value)

            this.VisibleBounds <- Viewport.bounds
            current.Init this

        override this.Draw() = current.Draw()

    let screenContainer = ScreenContainer()

    // todo: return a bool indicating success
    let changeNew (thunk: unit -> #T) (screenType: Type) (flags: Transitions.Flags) =
        if not Song.loading && (screenType <> currentType || screenType = Type.Play) then
            Transitions.tryStart (
                (fun () ->
                    let s = thunk ()
                    current.OnExit screenType

                    if not s.Initialised then
                        s.Init screenContainer
                    else
                        s.Update(0.0, true)

                    s.OnEnter currentType
                    currentType <- screenType
                    current <- s
                ),
                flags
            )
            |> globalAnimation.Add

    let change (screenType: Type) (flags: Transitions.Flags) =
        changeNew (K screens.[int screenType]) screenType flags

    let back (flags: Transitions.Flags) =
        match current.OnBack() with
        | Some t -> change t flags
        | None -> ()

    type ScreenRoot(toolbar: Widget) =
        inherit Root()

        let perf = PerformanceMonitor()

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)

            perf.Update(elapsedTime, moved)

            Background.update elapsedTime

            if currentType <> Type.Play || Dialog.exists () then
                Notifications.display.Update(elapsedTime, moved)

            if Viewport.vwidth > 0.0f then
                let x, y = Mouse.pos ()
                Background.setParallaxPos (x / Viewport.vwidth, y / Viewport.vheight)

            Palette.accent_color.Target <- Content.accentColor
            Dialog.display.Update(elapsedTime, moved)

            toolbar.Update(elapsedTime, moved)
            globalAnimation.Update elapsedTime
            logo.Update(elapsedTime, moved)
            screenContainer.Update(elapsedTime, moved)

            if (!| "exit").Tapped() then
                back Transitions.Flags.UnderLogo

            if exit then
                this.ShouldExit <- true

        override this.Draw() =
            Background.drawWithDim (this.Bounds, Color.White, 1.0f)
            screenContainer.Draw()
            logo.Draw()
            toolbar.Draw()

            if Transitions.active then
                Transitions.draw this.Bounds

                if (Transitions.flags &&& Transitions.Flags.UnderLogo = Transitions.Flags.UnderLogo) then
                    logo.Draw()

            Dialog.display.Draw()

            if currentType <> Type.Play || Dialog.exists () then
                Notifications.display.Draw()
                let x, y = Mouse.pos ()

                Draw.sprite
                    (Rect.Box(x, y, Content.themeConfig().CursorSize, Content.themeConfig().CursorSize))
                    (if Notifications.tooltip_available then
                         Colors.white
                     else
                         Palette.color (255, 1.0f, 0.5f))
                    (Content.getTexture "cursor")

            perf.Draw()

        override this.Init() =
            base.Init()

            Logo.display.Init this
            toolbar.Init this
            Notifications.display.Init this
            Dialog.display.Init this
            screenContainer.Init this
            perf.Init this
            current.OnEnter Type.SplashScreen

type Screen = Screen.T
