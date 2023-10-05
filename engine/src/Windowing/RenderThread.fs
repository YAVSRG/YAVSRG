namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open System.Runtime.InteropServices
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

module Scanline =

    let mutable private _hAdapter = 0
    let mutable private _VinPnSourceId = 0

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME =
        {
            [<MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)>] pDeviceName : string
            mutable hAdapter: int32
            mutable AdapterLuid: int32
            mutable VinPnSourceId: int32
        }

    [<DllImport("gdi32.dll")>]
    extern uint D3DKMTOpenAdapterFromGdiDisplayName(D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME& info)

    let open_adapter(gdi_name: string) =
        let mutable info = { Unchecked.defaultof<D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME> with pDeviceName = gdi_name }
        if D3DKMTOpenAdapterFromGdiDisplayName &info <> 0u then Logging.Error("Error getting adapter handle by GDI name")
        _hAdapter <- info.hAdapter
        _VinPnSourceId <- info.VinPnSourceId

    let close_adapter() = "nyi"

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type D3DKMT_GETSCANLINE =
        {
            hAdapter: int32
            VinPnSourceId: int32
            mutable InVerticalBlank: bool
            mutable ScanLine: uint
        }

    [<DllImport("gdi32.dll")>]
    extern uint D3DKMTGetScanLine(D3DKMT_GETSCANLINE& info)

    let get() =
        let mutable info = { Unchecked.defaultof<D3DKMT_GETSCANLINE> with hAdapter = _hAdapter; VinPnSourceId = _VinPnSourceId }
        if D3DKMTGetScanLine &info <> 0u then Logging.Error("Error getting scanline from video adapter")
        info.ScanLine, info.InVerticalBlank
    
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type D3DKMT_WAITFORVERTICALBLANKEVENT =
        {
            hAdapter: int32
            hDevice: int32
            VinPnSourceId: int32
        }
    
    [<DllImport("gdi32.dll")>]
    extern uint D3DKMTWaitForVerticalBlankEvent(D3DKMT_WAITFORVERTICALBLANKEVENT& info)

    let wait() =
        let mutable info = { hAdapter = _hAdapter; hDevice = 0; VinPnSourceId = _VinPnSourceId }
        if D3DKMTWaitForVerticalBlankEvent &info <> 0u then Logging.Error("Error waiting for vblank event")

module Timing =

    [<DllImport("winmm.dll")>]
    extern uint private timeBeginPeriod(int msec)
    [<DllImport("winmm.dll")>]
    extern uint private timeEndPeriod(int msec)

    let NATIVE_SLEEP_TRUST_THRESHOLD_MS =
        if OperatingSystem.IsWindows() then 1.0 else 0.125

    let spin_wait = SpinWait()

    /// Sleep the thread until a stopwatch time is reached
    /// Strategy is to natively sleep the thread as far as it can be trusted, then spin-wait until it's time
    /// Native sleep can still wake up much later if threads are busy
    /// CPU intensive
    let sleep_accurate : Stopwatch * float -> unit =
        if OperatingSystem.IsWindows() then

            fun (timer: Stopwatch, until: float) ->
                let delta = until - timer.Elapsed.TotalMilliseconds

                if delta > NATIVE_SLEEP_TRUST_THRESHOLD_MS then
                    timeBeginPeriod 1 |> ignore
                    Thread.Sleep(TimeSpan.FromMilliseconds (delta - NATIVE_SLEEP_TRUST_THRESHOLD_MS))
                    timeEndPeriod 1 |> ignore

                while timer.Elapsed.TotalMilliseconds < until do
                    spin_wait.SpinOnce -1

        else

            fun (timer: Stopwatch, until: float) ->
                let delta = until - timer.Elapsed.TotalMilliseconds

                if delta > NATIVE_SLEEP_TRUST_THRESHOLD_MS then
                    Thread.Sleep(TimeSpan.FromMilliseconds (delta - NATIVE_SLEEP_TRUST_THRESHOLD_MS))

                while timer.Elapsed.TotalMilliseconds < until do
                    spin_wait.SpinOnce -1


type RenderThread(window: NativeWindow, audioDevice: int, root: Root, afterInit: unit -> unit) =
    
    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable next_frame_time = 0.0
    let mutable refresh_period = 1000.0 / 60.0
    let mutable is_focused = true

    member this.IsFocused with set v = is_focused <- v
    member val RenderMode = FrameLimit.Smart with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        resized <- true

    member this.RenderModeChanged(refresh_rate: int) =
        refresh_period <- 1000.0 / float refresh_rate

    member private this.Loop() =
        window.Context.MakeCurrent()
        this.Init()
        fps_timer.Start()
        try
            while not (GLFW.WindowShouldClose window.WindowPtr) do this.DispatchFrame()
        with fatal_err -> Logging.Critical("Fatal crash in UI thread", fatal_err); window.Close()

    member this.Start() =
        let thread = Thread(this.Loop)
        Percyqaz.Flux.Utils.UITHREAD <- thread.ManagedThreadId
        thread.Start()
        
    member this.DispatchFrame() =
    
        if this.RenderMode = FrameLimit.Smart then Scanline.wait()

        let elapsed_time = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()
        
        // Update
        let before_update = total_frame_timer.Elapsed.TotalMilliseconds
        Input.begin_frame_events()
        ROOT_ANIMATION.Update elapsed_time
        root.Update (elapsed_time, resized)
        resized <- false
        Input.finish_frame_events()
        Devices.update elapsed_time
        let after_update = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.update_time <- after_update - before_update
        if root.ShouldExit then window.Close()
        
        // Draw
        let before_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.start()
        if Viewport.rheight > 0 then root.Draw()
        Render.finish()
        let after_draw = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.draw_time <- after_draw - before_draw

        let before_swap = total_frame_timer.Elapsed.TotalMilliseconds
        if not root.ShouldExit then window.Context.SwapBuffers()
        let after_swap = total_frame_timer.Elapsed.TotalMilliseconds
        Render.Performance.swap_time <- after_swap - before_swap

        if this.RenderMode = FrameLimit.Smart then 
            Render.Performance.visual_latency <- after_swap - next_frame_time
            next_frame_time <- before_update + refresh_period
        else
            while next_frame_time < after_swap do
                next_frame_time <- next_frame_time + refresh_period
            Render.Performance.visual_latency <- 0.0
        
        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks
        if time > Stopwatch.Frequency then
            Render.Performance.framecount_tickcount <- (fps_count, time)

            fps_timer.Restart()
            fps_count <- 0
        Render.Performance.elapsed_time <- elapsed_time

    member this.Init() =
        Devices.init audioDevice
        Render.init()
        Render.resize(window.ClientSize.X, window.ClientSize.Y)
        Render.Performance.frame_compensation <- 
            fun () -> 
                if this.RenderMode = FrameLimit.Smart then
                    float32 (next_frame_time - total_frame_timer.Elapsed.TotalMilliseconds) * 1.0f<ms>
                else float32 (next_frame_time - total_frame_timer.Elapsed.TotalMilliseconds) * 1.0f<ms>
        root.Init()
        afterInit()