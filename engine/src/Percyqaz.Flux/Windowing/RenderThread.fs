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
    let between_frame_timer = Stopwatch()
    let current_frame_timer = Stopwatch()
    member val RenderFrequency = 0.0 with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        root.Bounds <- Render.bounds
        resized <- true

    member private this.Loop() =
        window.Context.MakeCurrent()
        this.Init()
        between_frame_timer.Start()
        while not (GLFW.WindowShouldClose window.WindowPtr) do
            let timeUntilNextFrame = this.DispatchFrame()
            if timeUntilNextFrame > 0.0 then Thread.Sleep(Math.Floor(timeUntilNextFrame * 1000.0) |> int)

    member this.Start() =
        Thread(this.Loop).Start()
        
    member this.DispatchFrame() =
        let between_frames = between_frame_timer.Elapsed.TotalSeconds
        let frameTime = if renderFrequency = 0.0 then 0.0 else 1.0 / renderFrequency
        current_frame_timer.Restart()
        
        // Update
        Input.update()
        if Render.rheight > 0 then root.Update(between_frames * 1000.0, resized)
        resized <- false
        Input.finish_frame_events()
        Track.update()
        if root.ShouldExit then window.Close()
        
        // Draw
        Render.start()
        if Render.rheight > 0 then root.Draw()
        Render.finish()
        window.Context.SwapBuffers()
        
        // Timing
        between_frame_timer.Restart()
        if renderFrequency = 0.0 then 0.0 else frameTime - current_frame_timer.Elapsed.TotalSeconds

    member this.Init() =
        Render.init()
        root.Init()