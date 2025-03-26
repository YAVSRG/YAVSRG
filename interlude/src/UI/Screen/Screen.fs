namespace Interlude.UI

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data
open Interlude.Content
open Interlude.UI

module Toolbar =

    let HEIGHT = 70.0f
    let slideout_amount = Animation.Fade 1.0f
    let mutable hidden = false
    let mutable cursor_hidden = false
    let mutable was_hidden = false

    let hide () : unit = hidden <- true
    let show () : unit = hidden <- false

    let hide_cursor() : unit = cursor_hidden <- true
    let show_cursor() : unit = cursor_hidden <- false

    let moving () : bool =
        was_hidden <> hidden || slideout_amount.Moving

    let mutable screenshot_queued = false
    let take_screenshot_internal () : unit =
        let id = DateTime.Now.ToString("yyyy'-'MM'-'dd'.'HH'_'mm'_'ss.fffffff") + ".png"
        let path = Path.Combine(get_game_folder "Screenshots", id)
        let img = Render.take_screenshot ()
        ClipboardWin32.set_image img |> ignore
        ImageServices.save_image_jpg.Request((img, path), img.Dispose)

        Notifications.action_feedback_button (
            Icons.IMAGE,
            %"notification.screenshot",
            id,
            %"notification.screenshot.open_folder",
            fun () -> open_directory (get_game_folder "Screenshots")
        )

        screenshot_queued <- false

    /// Called during Update(), but it actually "queues" the screenshot to be taken midway through Draw()
    /// Allows screenshotting to not include certain overlays:
    ///  Screenshot is taken after most of the screen has rendered but before any debug overlays or notifications
    let take_screenshot() : unit =
        screenshot_queued <- true

type ScreenType =
    | SplashScreen = 0
    | MainMenu = 1
    | Lobby = 2
    | LevelSelect = 3
    | Play = 4
    | Practice = 5
    | EditHud = 6
    | Replay = 7
    | Score = 8

[<AbstractClass>]
type Screen() =
    inherit Container(NodeType.None)
    abstract member OnEnter: ScreenType -> unit
    abstract member OnExit: ScreenType -> unit
    abstract member OnBack: unit -> ScreenType option

module Screen =

    let animations = Animation.fork [ Palette.accent_color ]
    let logo = Logo()

    let mutable enable_background = true
    let mutable timescale = 1.0
    let mutable current_type = ScreenType.SplashScreen
    let mutable private current = Unchecked.defaultof<Screen>
    let private screens: Screen array = Array.zeroCreate 5

    let init (_screens: Screen array) : unit =
        assert (_screens.Length = 4)

        for i = 0 to 3 do
            screens.[i] <- _screens.[i]

        current <- screens.[0]

    type private ScreenContainer() =
        inherit Widget(NodeType.None)

        override this.Position
            with set _ = failwith "Not permitted"

        override this.Update(elapsed_ms, moved) =
            let moved = moved || Toolbar.moving ()

            if moved then
                this.Bounds <-
                    if Toolbar.hidden then
                        Render.bounds()
                    else
                        Render.bounds().Shrink(0.0f, Toolbar.HEIGHT * Toolbar.slideout_amount.Value)

                this.VisibleBounds <- Render.bounds()

            current.Update(elapsed_ms, moved)

        override this.Init(parent: Widget) =
            base.Init parent

            this.Bounds <-
                if Toolbar.hidden then
                    Render.bounds()
                else
                    Render.bounds().Shrink(0.0f, Toolbar.HEIGHT * Toolbar.slideout_amount.Value)

            this.VisibleBounds <- Render.bounds()
            current.Init this

        override this.Draw() = current.Draw()

    let private screen_container = ScreenContainer()

    /// Returns true if the transition could be triggered and is now in motion
    /// false if the requested change was rejected (transition already in progress)
    let change_new (thunk: unit -> #Screen) (screen_type: ScreenType) (transition_type: Transitions.Transition) : bool =
        if
            not Song.loading
            && (screen_type <> current_type || screen_type = ScreenType.Play)
            && not (Transitions.in_progress())
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
                transition_type
            )
            |> animations.Add

            true
        else
            false

    /// Returns true if the transition could be triggered and is now in motion
    /// false if the requested change was rejected (transition already in progress)
    let change (screen_type: ScreenType) (transition_type: Transitions.Transition) : bool =
        change_new (K screens.[int screen_type]) screen_type transition_type

    /// Returns true if the transition could be triggered and is now in motion
    /// false if the requested change was rejected (transition already in progress or the current Screen denied it)
    let back (transition_type: Transitions.Transition) : bool =
        match current.OnBack() with
        | Some t -> change t transition_type
        | None -> false

    type ScreenRoot(toolbar: Widget) =
        inherit Root()

        let perf = PerformanceMonitor()

        override this.Update(elapsed_ms, moved) =
            let elapsed_ms = elapsed_ms * timescale
            base.Update(elapsed_ms, moved)

            perf.Update(elapsed_ms, moved)

            Background.update elapsed_ms
            HelpOverlay.display.Update(elapsed_ms, moved)

            if current_type <> ScreenType.Play || Dialog.exists () then
                Notifications.display.Update(elapsed_ms, moved)

            assert(Render.width() > 0.0f)
            let x, y = Mouse.pos ()
            Background.set_parallax_pos (x / Render.width(), y / Render.height())

            Dialog.display.Update(elapsed_ms, moved)

            toolbar.Update(elapsed_ms, moved)
            animations.Update elapsed_ms
            logo.Update(elapsed_ms, moved)
            screen_container.Update(elapsed_ms, moved)

            if (%%"exit").Pressed() then
                back Transitions.UnderLogo |> ignore

        override this.Draw() =
            if enable_background && current_type <> ScreenType.SplashScreen then
                if
                    (current_type <> ScreenType.Play || Background.dim_percent.Value < 1.0f)
                then
                    Background.draw_with_dim (this.Bounds, Color.White, 1.0f)
                else Render.rect this.Bounds Color.Black

            screen_container.Draw()
            logo.Draw()
            toolbar.Draw()

            match Transitions.current with
            | Some transition_type ->
                Transitions.draw transition_type this.Bounds

                if transition_type = Transitions.UnderLogo then
                    logo.Draw()
            | None -> ()

            Dialog.display.Draw()
            HelpOverlay.display.Draw()

            if Toolbar.screenshot_queued then Toolbar.take_screenshot_internal()

            if not Toolbar.cursor_hidden || Dialog.exists () then
                Notifications.display.Draw()
                if Mouse.in_bounds() then
                    let x, y = Mouse.pos ()

                    Render.sprite
                        (Rect.FromSize(x, y, Content.ThemeConfig.CursorSize, Content.ThemeConfig.CursorSize))
                        (Palette.color (255, 1.0f, 0.5f))
                        (Content.Texture "cursor")

            perf.Draw()

        override this.Init() =
            base.Init()

            logo.Init this
            toolbar.Init this
            Notifications.display.Init this
            HelpOverlay.display.Init this
            Dialog.display.Init this
            screen_container.Init this
            perf.Init this
            current.OnEnter ScreenType.SplashScreen