namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open OpenTK
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Common

type RenderThread(window: NativeWindow, audioDevice: int, root: Root, afterInit: unit -> unit) =
    
    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let this_frame_timer = Stopwatch()
    member val RenderFrequency = 0.0 with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        resized <- true

    member private this.Loop() =
        window.Context.MakeCurrent()
        this.Init()
        fps_timer.Start()
        try
            while not (GLFW.WindowShouldClose window.WindowPtr) do
                let timeUntilNextFrameMs = this.DispatchFrame()
                if timeUntilNextFrameMs > 0.5 then Thread.Sleep(TimeSpan.FromMilliseconds(timeUntilNextFrameMs - 0.5))
        with fatal_err -> Logging.Critical("Fatal crash in UI thread", fatal_err); window.Close()

    member this.Start() =
        let thread = Thread(this.Loop)
        Percyqaz.Flux.Utils.UITHREAD <- thread.ManagedThreadId
        thread.Start()
        
    member this.DispatchFrame() =

        let frameTime = if this.RenderFrequency = 0.0 then 0.0 else 1000.0 / this.RenderFrequency
        let elapsedTime = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()
        
        // Update
        Input.begin_frame_events()
        ROOT_ANIMATION.Update elapsedTime
        root.Update (elapsedTime, resized)
        resized <- false
        Input.finish_frame_events()
        Devices.update(elapsedTime)
        if root.ShouldExit then window.Close()
        
        // Draw
        Render.start()
        if Viewport.rheight > 0 then root.Draw()
        Render.finish()
        window.Context.SwapBuffers()
        
        // Timing
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks
        if time > 10_000_000L then
            Render.FPS <- (fps_count, time)
            fps_timer.Restart()
            fps_count <- 0

        let elapsedTime = this_frame_timer.Elapsed.TotalMilliseconds
        this_frame_timer.Restart()

        if this.RenderFrequency = 0.0 then 0.0 else frameTime - elapsedTime

    member this.Init() =
        Devices.init audioDevice
        Render.init()
        Render.resize(window.ClientSize.X, window.ClientSize.Y)
        root.Init()
        afterInit()