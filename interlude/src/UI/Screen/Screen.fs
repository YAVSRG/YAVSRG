namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.UI

module Toolbar =
    let HEIGHT = 70.0f
    let slideout_amount = Animation.Fade 1.0f
    let mutable hidden = false
    let mutable was_hidden = false

    let hide () = hidden <- true
    let show () = hidden <- false

    let moving () =
        was_hidden <> hidden || slideout_amount.Moving

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

    let animations = Animation.fork [ Palette.accent_color; Toolbar.slideout_amount ]

    let logo = Logo.display

    let mutable timescale = 1.0
    let mutable exit = false
    let mutable current_type = Type.SplashScreen
    let mutable private current = Unchecked.defaultof<T>
    let private screens: T array = Array.zeroCreate 5

    let init_window (_screens: T array) =
        assert (_screens.Length = 5)

        for i = 0 to 4 do
            screens.[i] <- _screens.[i]

        current <- screens.[0]

    type ScreenContainer() =
        inherit Widget(NodeType.None)

        override this.Position
            with set _ = failwith "Not permitted"

        override this.Update(elapsed_ms, moved) =
            let moved = moved || Toolbar.moving ()

            if moved then
                this.Bounds <-
                    if Toolbar.hidden then
                        Viewport.bounds
                    else
                        Viewport.bounds.Shrink(0.0f, Toolbar.HEIGHT * Toolbar.slideout_amount.Value)

                this.VisibleBounds <- Viewport.bounds

            current.Update(elapsed_ms, moved)

        override this.Init(parent: Widget) =
            base.Init parent

            this.Bounds <-
                if Toolbar.hidden then
                    Viewport.bounds
                else
                    Viewport.bounds.Shrink(0.0f, Toolbar.HEIGHT * Toolbar.slideout_amount.Value)

            this.VisibleBounds <- Viewport.bounds
            current.Init this

        override this.Draw() = current.Draw()

    let screen_container = ScreenContainer()

    let change_new (thunk: unit -> #T) (screen_type: Type) (flags: Transitions.Flags) : bool =
        if
            not Song.loading
            && (screen_type <> current_type || screen_type = Type.Play)
            && not Transitions.active
        then
            Transitions.animate (
                (fun () ->
                    let s = thunk ()
                    current.OnExit screen_type

                    if not s.Initialised then
                        s.Init screen_container
                    else
                        s.Update(0.0, true)

                    s.OnEnter current_type
                    current_type <- screen_type
                    current <- s
                ),
                flags
            )
            |> animations.Add

            true
        else
            false

    let change (screen_type: Type) (flags: Transitions.Flags) =
        change_new (K screens.[int screen_type]) screen_type flags

    let back (flags: Transitions.Flags) : bool =
        match current.OnBack() with
        | Some t -> change t flags
        | None -> false

    type ScreenRoot(toolbar: Widget) =
        inherit Root()

        let perf = PerformanceMonitor()

        override this.Update(elapsed_ms, moved) =
            let elapsed_ms = elapsed_ms * timescale
            base.Update(elapsed_ms, moved)

            perf.Update(elapsed_ms, moved)

            Background.update elapsed_ms

            if current_type <> Type.Play || Dialog.exists () then
                Notifications.display.Update(elapsed_ms, moved)

            if Viewport.vwidth > 0.0f then
                let x, y = Mouse.pos ()
                Background.set_parallax_pos (x / Viewport.vwidth, y / Viewport.vheight)

            Palette.accent_color.Target <- Content.accent_color
            Dialog.display.Update(elapsed_ms, moved)

            toolbar.Update(elapsed_ms, moved)
            animations.Update elapsed_ms
            logo.Update(elapsed_ms, moved)
            screen_container.Update(elapsed_ms, moved)

            if (%%"exit").Tapped() then
                back Transitions.Flags.UnderLogo |> ignore

            if exit then
                this.ShouldExit <- true

        override this.Draw() =
            if current_type <> Type.Play || Options.options.BackgroundDim.Value < 1.0f then
                Background.draw_with_dim (this.Bounds, Color.White, 1.0f)

            screen_container.Draw()
            logo.Draw()
            toolbar.Draw()

            if Transitions.active then
                Transitions.draw this.Bounds

                if (Transitions.flags &&& Transitions.Flags.UnderLogo = Transitions.Flags.UnderLogo) then
                    logo.Draw()

            Dialog.display.Draw()

            if current_type <> Type.Play || Dialog.exists () then
                Notifications.display.Draw()
                let x, y = Mouse.pos ()

                Draw.sprite
                    (Rect.Box(x, y, Content.theme_config().CursorSize, Content.theme_config().CursorSize))
                    (if Notifications.tooltip_available then
                         Colors.white
                     else
                         Palette.color (255, 1.0f, 0.5f))
                    (Content.get_texture "cursor")

            perf.Draw()

        override this.Init() =
            base.Init()

            Logo.display.Init this
            toolbar.Init this
            Notifications.display.Init this
            Dialog.display.Init this
            screen_container.Init this
            perf.Init this
            current.OnEnter Type.SplashScreen

type Screen = Screen.T
