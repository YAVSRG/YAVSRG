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

type RenderThread(window: NativeWindow, root: Root) =
    
    let mutable resized = false
    let mutable renderFrequency = 0.0
    let frame_timer = Stopwatch()
    member val RenderFrequency = 0.0 with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        resized <- true

    member private this.Loop() =
        window.Context.MakeCurrent()
        this.Init()
        while not (GLFW.WindowShouldClose window.WindowPtr) do
            let timeUntilNextFrame = this.DispatchFrame()
            if timeUntilNextFrame > 0.0 then Thread.Sleep(Math.Floor(timeUntilNextFrame * 1000.0) |> int)

    member this.Start() =
        Thread(this.Loop).Start()
        
    member this.DispatchFrame() =

        let frameTime = if renderFrequency = 0.0 then 0.0 else 1.0 / renderFrequency
        let elapsedTime = frame_timer.Elapsed.TotalMilliseconds
        frame_timer.Restart()
        
        // Update
        Input.update()
        root.Animation.Update elapsedTime
        root.Update (elapsedTime, resized)
        resized <- false
        Input.finish_frame_events()
        Track.update()
        if root.ShouldExit then window.Close()
        
        // Draw
        Render.start()
        if Viewport.rheight > 0 then root.Draw()
        Render.finish()
        window.Context.SwapBuffers()
        
        // Timing
        if renderFrequency = 0.0 then 0.0 else frameTime - frame_timer.Elapsed.TotalSeconds

    member this.Init() =
        Render.init()
        Render.resize(window.ClientSize.X, window.ClientSize.Y)
        root.Init()