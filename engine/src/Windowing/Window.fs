﻿namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open FSharp.NativeInterop
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics

#nowarn "9"

type MonitorDetails =
    {
        Id: int
        FriendlyName: string
        DisplayModes: FullscreenVideoMode array
    }

module private WindowEvents =

    let on_load = Event<unit>()
    let after_init = Event<unit>()
    let on_unload = Event<unit>()
    let on_file_drop = Event<string>()
    let on_resize = Event<unit>()

module Window =

    let on_load = WindowEvents.on_load.Publish
    let after_init = WindowEvents.after_init.Publish
    let on_unload = WindowEvents.on_unload.Publish
    let on_file_drop = WindowEvents.on_file_drop.Publish
    let on_resize = WindowEvents.on_resize.Publish

    let internal LOCK_OBJ = obj ()
    let mutable internal action_queue = []

    type WindowAction =
        | ApplyConfig of Config
        | EnableResize of callback: ((int * int) -> unit)
        | DisableResize
        | DisableWindowsKey
        | EnableWindowsKey

    let defer (a: WindowAction) =
        lock (LOCK_OBJ) (fun () -> action_queue <- action_queue @ [ a ])

    let mutable internal _monitors: MonitorDetails list = []

    let get_monitors () = lock (LOCK_OBJ) (fun () -> _monitors)


[<Sealed>]
type Window(config: Config, title: string, ui_root: Root) as this =
    inherit
        NativeWindow(
            NativeWindowSettings(
                StartVisible = false,
                NumberOfSamples = (if OperatingSystem.IsMacOS() then 0 else 24),
                Flags = ContextFlags.ForwardCompatible,
                Profile = ContextProfile.Core
            )
        )

    let render_thread =
        RenderThread(
            this, 
            config.AudioDevice.Value,
            config.AudioDevicePeriod.Value,
            config.AudioDevicePeriod.Value * config.AudioDeviceBufferLengthMultiplier.Value,
            ui_root,
            WindowEvents.after_init.Trigger
        )

    let mutable resize_callback = fun (w, h) -> ()
    let mutable refresh_rate = 60
    let mutable was_fullscreen = false
    let mutable input_cpu_saver = false

    let mutable disable_windows_key = false

    do
        base.Title <- title
        base.VSync <- VSyncMode.Off

        base.CursorState <-
            if OperatingSystem.IsMacOS() then
                CursorState.Grabbed
            else
                CursorState.Hidden

    let should_disable_windows_key() = 
        disable_windows_key 
        && render_thread.IsFocused 
        && not input_cpu_saver 
        && OperatingSystem.IsWindows()

    member this.ApplyConfig(config: Config) =

        let monitor_list = Monitors.GetMonitors()

        input_cpu_saver <- config.InputCPUSaver.Value

        Window._monitors <-
            monitor_list
            |> Seq.indexed
            |> Seq.map (fun (i, m) ->
                {
                    Id = i
                    FriendlyName = sprintf "%i: %s" (i + 1) m.Name
                    DisplayModes =
                        GLFW.GetVideoModes(m.Handle.ToUnsafePtr<Monitor>())
                        |> Array.map (fun glfw_mode ->
                            {
                                Width = glfw_mode.Width
                                Height = glfw_mode.Height
                                RefreshRate = glfw_mode.RefreshRate
                            }
                        )
                }
            )
            |> List.ofSeq

        let monitor =
            if config.WindowMode.Value <> WindowType.Windowed then
                try
                    monitor_list.[config.Display.Value]
                with err ->
                    Logging.Error(sprintf "Failed to get display info for monitor %i" config.Display.Value)
                    Monitors.GetMonitorFromWindow(this)
            else
                Monitors.GetMonitorFromWindow(this)

        render_thread.RenderMode <- config.RenderMode.Value

        let monitor_ptr = monitor.Handle.ToUnsafePtr<Monitor>()

        if was_fullscreen then

            match config.WindowMode.Value with

            | WindowType.Windowed ->
                let width, height = config.WindowResolution.Value

                GLFW.SetWindowMonitor(
                    this.WindowPtr,
                    NativePtr.nullPtr<Monitor>,
                    monitor.ClientArea.Min.X,
                    monitor.ClientArea.Min.Y,
                    width,
                    height,
                    0
                )

                base.CenterWindow()
                base.WindowBorder <- WindowBorder.Fixed
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Borderless ->
                GLFW.SetWindowMonitor(
                    this.WindowPtr,
                    NativePtr.nullPtr<Monitor>,
                    monitor.ClientArea.Min.X - 1,
                    monitor.ClientArea.Min.Y - 1,
                    monitor.ClientArea.Size.X + 1,
                    monitor.ClientArea.Size.Y + 1,
                    0
                )

                base.WindowBorder <- WindowBorder.Hidden
                GLFW.HideWindow(this.WindowPtr)
                GLFW.MaximizeWindow(this.WindowPtr)
                GLFW.ShowWindow(this.WindowPtr)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.``Borderless Fullscreen`` ->
                GLFW.SetWindowMonitor(
                    this.WindowPtr,
                    NativePtr.nullPtr<Monitor>,
                    monitor.ClientArea.Min.X,
                    monitor.ClientArea.Min.Y,
                    monitor.ClientArea.Size.X,
                    monitor.ClientArea.Size.Y,
                    0
                )

                base.WindowBorder <- WindowBorder.Hidden
                base.CenterWindow()
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Fullscreen ->
                let requested_mode = config.FullscreenVideoMode.Value

                GLFW.SetWindowMonitor(
                    this.WindowPtr,
                    monitor_ptr,
                    0,
                    0,
                    requested_mode.Width,
                    requested_mode.Height,
                    requested_mode.RefreshRate
                )

                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | _ -> Logging.Error "Tried to change to invalid window mode"

        else

            match config.WindowMode.Value with

            | WindowType.Windowed ->
                base.WindowBorder <- WindowBorder.Fixed
                let width, height = config.WindowResolution.Value
                GLFW.SetWindowSize(this.WindowPtr, width, height)
                base.CenterWindow()
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Borderless ->
                base.WindowBorder <- WindowBorder.Hidden
                GLFW.HideWindow(this.WindowPtr)
                GLFW.SetWindowPos(this.WindowPtr, monitor.ClientArea.Min.X - 1, monitor.ClientArea.Min.Y - 1)
                GLFW.SetWindowSize(this.WindowPtr, monitor.ClientArea.Size.X + 1, monitor.ClientArea.Size.Y + 1)
                GLFW.MaximizeWindow(this.WindowPtr)
                GLFW.ShowWindow(this.WindowPtr)
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.``Borderless Fullscreen`` ->
                base.WindowBorder <- WindowBorder.Hidden
                GLFW.SetWindowPos(this.WindowPtr, monitor.ClientArea.Min.X, monitor.ClientArea.Min.Y)
                GLFW.SetWindowSize(this.WindowPtr, monitor.ClientArea.Size.X, monitor.ClientArea.Size.Y)
                base.CenterWindow()
                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | WindowType.Fullscreen ->
                let requested_mode = config.FullscreenVideoMode.Value

                GLFW.SetWindowMonitor(
                    this.WindowPtr,
                    monitor_ptr,
                    0,
                    0,
                    requested_mode.Width,
                    requested_mode.Height,
                    requested_mode.RefreshRate
                )

                refresh_rate <- NativePtr.read(GLFW.GetVideoMode(monitor_ptr)).RefreshRate

            | _ -> Logging.Error "Tried to change to invalid window mode"

        was_fullscreen <- config.WindowMode.Value = WindowType.Fullscreen

        if OperatingSystem.IsWindows() then
            FrameTimeStrategies.VBlankThread.switch (1000.0 / float refresh_rate) (GLFW.GetWin32Adapter monitor_ptr) (GLFW.GetWin32Monitor monitor_ptr)

        defer
        <| fun () ->
            render_thread.RenderModeChanged(
                config.WindowMode.Value = WindowType.Fullscreen
                || config.WindowMode.Value = WindowType.``Borderless Fullscreen``
            )

            anti_jitter <- config.SmartCapAntiJitter.Value
            tearline_position <- config.SmartCapTearlinePosition.Value
            framerate_multiplier <- config.SmartCapFramerateMultiplier.Value

    member this.EnableResize(callback) =
        if base.WindowState = WindowState.Normal && base.WindowBorder = WindowBorder.Fixed then
            base.WindowBorder <- WindowBorder.Resizable

        resize_callback <- callback

    member this.DisableResize() =
        if
            base.WindowState = WindowState.Normal
            && base.WindowBorder = WindowBorder.Resizable
        then
            base.WindowBorder <- WindowBorder.Fixed

    override this.OnResize e =
        base.OnResize e
        let client_size = this.ClientSize
        defer (fun () ->
            if client_size.X <> 0 && client_size.Y <> 0 then
                if this.WindowBorder = WindowBorder.Resizable then
                    resize_callback (client_size.X, client_size.Y)

                render_thread.OnResize(client_size.X, client_size.Y)
        )

    override this.OnFocusedChanged e =
        render_thread.IsFocused <- e.IsFocused
        if should_disable_windows_key() then 
            WindowsKey.disable()
        else WindowsKey.enable()

    override this.OnFileDrop e =
        Array.iter WindowEvents.on_file_drop.Trigger e.FileNames

    member this.Run() =
        this.OnLoad()
        this.OnResize(ResizeEventArgs(this.Size))

        this.Context.MakeNoneCurrent()
        render_thread.Start()

        if input_cpu_saver && OperatingSystem.IsWindows() then
            Thread.CurrentThread.Priority <- ThreadPriority.Highest

        while not (GLFW.WindowShouldClose this.WindowPtr) do
            lock
                Window.LOCK_OBJ
                (fun () ->
                    for a in Window.action_queue do
                        match a with
                        | Window.ApplyConfig c -> this.ApplyConfig c
                        | Window.EnableResize c -> this.EnableResize c
                        | Window.DisableResize -> this.DisableResize()
                        | Window.DisableWindowsKey ->
                            disable_windows_key <- true
                            if should_disable_windows_key() then
                                WindowsKey.disable()
                        | Window.EnableWindowsKey ->
                            disable_windows_key <- false
                            WindowsKey.enable()

                    Window.action_queue <- []
                )

            if input_cpu_saver then
                GLFW.WaitEventsTimeout(0.5)
            else
                GLFW.PollEvents()

        this.OnUnload()
        this.Close()

        WindowsKey.enable()
        if render_thread.HasFatalError then Error() else Ok()

    member this.OnLoad() =
        this.ApplyConfig config
        Fonts.init ()
        Input.init this
        WindowEvents.on_load.Trigger()
        base.IsVisible <- true

    member this.OnUnload() = WindowEvents.on_unload.Trigger()
