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

module FrameTimeStrategies =
    
    type MonitorEnumProc = delegate of IntPtr * IntPtr * IntPtr * IntPtr -> bool

    [<DllImport("user32.dll")>]
    extern bool EnumDisplayMonitors(IntPtr hdc, IntPtr lprcClip, MonitorEnumProc lpfnEnum, IntPtr dwData)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME =
        {
            [<MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)>] pDeviceName : string
            mutable hAdapter: int32
            mutable AdapterLuid: int32
            mutable VinPnSourceId: int32
        }

    [<DllImport("gdi32.dll", SetLastError = true)>]
    extern int32 CreateDC(string lpszDriver, string lpszDevice, string lpszOutput, IntPtr lpInitData)
    [<DllImport("gdi32.dll", SetLastError = true)>]
    extern bool DeleteDC(IntPtr hdc)

    [<DllImport("gdi32.dll")>]
    extern uint D3DKMTOpenAdapterFromGdiDisplayName(D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME& info)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type D3DKMT_OPENADAPTERFROMHDC =
        {
            hDc: IntPtr
            mutable hAdapter: int32
            mutable AdapterLuid: int32
            mutable VinPnSourceId: int32
        }
        
    [<DllImport("gdi32.dll")>]
    extern uint D3DKMTOpenAdapterFromHdc(D3DKMT_OPENADAPTERFROMHDC& info)

    [<DllImport("gdi32.dll")>]
    extern uint D3DKMTCloseAdapter(int32 hAdapter)

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

    [<DllImport("winmm.dll")>]
    extern uint private timeBeginPeriod(int msec)
    [<DllImport("winmm.dll")>]
    extern uint private timeEndPeriod(int msec)
    
    (* GET DISPLAY HANDLE INFO *)

    let get_display_handle(glfw_monitor_name: string) =

        let mutable handles = List.empty
        let mutable i = 0
        let f = MonitorEnumProc(fun h _ _ _ -> i <- i + 1; handles <- ((sprintf "\\\\.\\DISPLAY%i" i), h) :: handles; true)
        EnumDisplayMonitors(IntPtr.Zero, IntPtr.Zero, f, IntPtr.Zero) |> ignore

        match handles |> List.tryFind (fun (name, h) -> glfw_monitor_name.StartsWith name) with
        | Some (name, h) -> 
            let hDc = CreateDC(null, name, null, 0)
            if hDc = 0 then Logging.Error("Failed to get hDC for monitor")
            hDc, 0
        | None -> 0, 0

    let mutable private _hAdapter = 0
    let mutable private _VinPnSourceId = 0
    let mutable private _hDevice = 0

    let close_adapter() = 
        if _hAdapter <> 0 then 
            if D3DKMTCloseAdapter(_hAdapter) <> 0u then Logging.Error("Error closing adapter after use")

    let open_adapter (gdi_adapter_name: string) (glfw_monitor_name: string) =
        close_adapter()
        let hDc, hDevice = get_display_handle glfw_monitor_name
        if hDc = 0 then
            let mutable info = { Unchecked.defaultof<D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME> with pDeviceName = gdi_adapter_name }
            if D3DKMTOpenAdapterFromGdiDisplayName &info <> 0u then Logging.Error("Error getting adapter handle by GDI name")
            _hAdapter <- info.hAdapter
            _VinPnSourceId <- info.VinPnSourceId
            _hDevice <- hDevice
        else
            let mutable info = { Unchecked.defaultof<D3DKMT_OPENADAPTERFROMHDC> with hDc = hDc }
            if D3DKMTOpenAdapterFromHdc &info <> 0u then Logging.Error("Error getting adapter handle by hDc")
            _hAdapter <- info.hAdapter
            _VinPnSourceId <- info.VinPnSourceId
            _hDevice <- hDevice

            if not (DeleteDC hDc) then Logging.Error("Error deleting hDc after use")

    (* USING OPEN ADAPTER TO GET SYNC INFORMATION *)

    let get_scanline() =
        let mutable info = { Unchecked.defaultof<D3DKMT_GETSCANLINE> with hAdapter = _hAdapter; VinPnSourceId = _VinPnSourceId }
        if D3DKMTGetScanLine &info <> 0u then Logging.Error("Error getting scanline from video adapter")
        info.ScanLine, info.InVerticalBlank

    let wait_for_vblank() =
        let mutable info = { hAdapter = _hAdapter; hDevice = _hDevice; VinPnSourceId = _VinPnSourceId }
        let status = D3DKMTWaitForVerticalBlankEvent &info
        if status <> 0u then
            Logging.Error(sprintf "Error waiting for vblank (%i)" status)
            printfn "%A" (get_scanline())
            false
        else true

    (* THREAD SLEEPING STRATEGIES *)

    let NATIVE_SLEEP_TRUST_THRESHOLD_MS =
        if OperatingSystem.IsWindows() then 1.0 else 0.125

    let spin_wait = SpinWait()

    /// Sleep the thread until a stopwatch time is reached
    /// Strategy is to natively sleep the thread as far as it can be trusted, then spin-wait until it's time
    /// Native sleep can still wake up much later if threads are busy
    /// CPU intensive
    /// Works on all operating systems
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

type Strategy =
    | Unlimited
    | DriverSync
    | SleepSpin

type RenderThread(window: NativeWindow, audioDevice: int, root: Root, afterInit: unit -> unit) =
    
    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable next_frame_time = 0.0
    let mutable refresh_period = 1000.0 / 60.0
    let mutable is_focused = true
    let mutable strategy = DriverSync

    member this.IsFocused with set v = is_focused <- v
    member val RenderMode = FrameLimit.Smart with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize(newSize.X, newSize.Y)
        resized <- true

    member this.RenderModeChanged(refresh_rate: int) =
        refresh_period <- 1000.0 / float refresh_rate
        strategy <- if this.RenderMode = FrameLimit.Smart then DriverSync else Unlimited

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
    
        if strategy = DriverSync && not (FrameTimeStrategies.wait_for_vblank()) then
            strategy <- SleepSpin

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