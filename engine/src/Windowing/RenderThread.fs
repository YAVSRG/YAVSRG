namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open System.Runtime.InteropServices
open OpenTK.Mathematics
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Common

module private FrameTimeStrategies =

    type MonitorEnumProc = delegate of IntPtr * IntPtr * IntPtr * IntPtr -> bool

    [<DllImport("user32.dll")>]
    extern bool EnumDisplayMonitors(IntPtr hdc, IntPtr lprcClip, MonitorEnumProc lpfnEnum, IntPtr dwData)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type _LUID =
        {
            mutable LowPart: uint32
            mutable HighPart: int32
        }

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME =
        {
            [<MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)>]
            pDeviceName: string
            mutable hAdapter: uint
            mutable AdapterLuid: _LUID
            mutable VinPnSourceId: uint32
        }

    [<DllImport("gdi32.dll", SetLastError = true)>]
    extern IntPtr CreateDC(string lpszDriver, string lpszDevice, string lpszOutput, IntPtr lpInitData)

    [<DllImport("gdi32.dll", SetLastError = true)>]
    extern bool DeleteDC(IntPtr hdc)

    [<DllImport("gdi32.dll")>]
    extern int64 D3DKMTOpenAdapterFromGdiDisplayName(D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME& info)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type D3DKMT_OPENADAPTERFROMHDC =
        {
            hDc: IntPtr
            mutable hAdapter: uint
            mutable AdapterLuid: _LUID
            mutable VinPnSourceId: uint
        }

    [<DllImport("gdi32.dll")>]
    extern int64 D3DKMTOpenAdapterFromHdc(D3DKMT_OPENADAPTERFROMHDC& info)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type D3DKMT_CLOSEADAPTER = { hAdapter: uint }

    [<DllImport("gdi32.dll")>]
    extern int64 D3DKMTCloseAdapter(D3DKMT_CLOSEADAPTER& info)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type D3DKMT_GETSCANLINE =
        {
            hAdapter: uint32
            VinPnSourceId: uint32
            mutable InVerticalBlank: uint32 // actually only 1 byte but alignment
            mutable ScanLine: uint32
        }

    [<DllImport("gdi32.dll")>]
    extern int64 D3DKMTGetScanLine(D3DKMT_GETSCANLINE& info)

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential)>]
    type D3DKMT_WAITFORVERTICALBLANKEVENT =
        {
            hAdapter: uint
            hDevice: uint
            VinPnSourceId: uint
        }

    [<DllImport("gdi32.dll")>]
    extern int64 D3DKMTWaitForVerticalBlankEvent(D3DKMT_WAITFORVERTICALBLANKEVENT& info)

    [<DllImport("winmm.dll")>]
    extern int64 private timeBeginPeriod(int msec)

    [<DllImport("winmm.dll")>]
    extern int64 private timeEndPeriod(int msec)

    (* GET DISPLAY HANDLE INFO *)

    let get_display_handle (glfw_monitor_name: string) =

        let mutable handles = List.empty
        let mutable i = 0

        let f =
            MonitorEnumProc(fun h _ _ _ ->
                i <- i + 1
                handles <- ((sprintf "\\\\.\\DISPLAY%i" i), h) :: handles
                true
            )

        EnumDisplayMonitors(IntPtr.Zero, IntPtr.Zero, f, IntPtr.Zero) |> ignore

        match handles |> List.tryFind (fun (name, h) -> glfw_monitor_name.StartsWith name) with
        | Some(name, h) ->
            let hDc = CreateDC(null, name, null, 0)

            if hDc = 0 then
                Logging.Error("Failed to get hDC for monitor")

            hDc
        | None -> 0

    (* THREAD SLEEPING STRATEGIES *)

    let NATIVE_SLEEP_TRUST_THRESHOLD_MS =
        if OperatingSystem.IsWindows() then 1.0 else 0.125

    let spin_wait = SpinWait()

    /// Sleep the thread until a stopwatch time is reached
    /// Strategy is to natively sleep the thread as far as it can be trusted, then spin-wait until it's time
    /// Native sleep can still wake up much later if threads are busy
    /// CPU intensive
    /// Works on all operating systems
    let sleep_accurate: Stopwatch * float -> unit =
        if OperatingSystem.IsWindows() then

            fun (timer: Stopwatch, until: float) ->
                let delta = until - timer.Elapsed.TotalMilliseconds

                if delta > NATIVE_SLEEP_TRUST_THRESHOLD_MS then
                    timeBeginPeriod 1 |> ignore
                    Thread.Sleep(TimeSpan.FromMilliseconds(delta - NATIVE_SLEEP_TRUST_THRESHOLD_MS))
                    timeEndPeriod 1 |> ignore

                while timer.Elapsed.TotalMilliseconds < until do
                    spin_wait.SpinOnce -1

        else

            fun (timer: Stopwatch, until: float) ->
                let delta = until - timer.Elapsed.TotalMilliseconds

                if delta > NATIVE_SLEEP_TRUST_THRESHOLD_MS then
                    Thread.Sleep(TimeSpan.FromMilliseconds(delta - NATIVE_SLEEP_TRUST_THRESHOLD_MS))

                while timer.Elapsed.TotalMilliseconds < until do
                    spin_wait.SpinOnce -1

    module VBlankThread =

        let mutable private _hAdapter = 0u
        let mutable private _VinPnSourceId = 0u

        let private close_adapter () =
            if _hAdapter <> 0u then
                let mutable info = { hAdapter = _hAdapter }

                if D3DKMTCloseAdapter(&info) <> 0l then
                    Logging.Error("Error closing adapter after use")

        let private open_adapter (gdi_adapter_name: string) (glfw_monitor_name: string) =
            close_adapter ()
            let hDc = get_display_handle glfw_monitor_name

            if hDc = 0 then
                let mutable info =
                    { Unchecked.defaultof<D3DKMT_OPENADAPTERFROMGDIDISPLAYNAME> with
                        pDeviceName = gdi_adapter_name
                    }

                if D3DKMTOpenAdapterFromGdiDisplayName &info <> 0l then
                    Logging.Error("Error getting adapter handle by GDI name")

                _hAdapter <- info.hAdapter
                _VinPnSourceId <- info.VinPnSourceId
            else
                let mutable info =
                    { Unchecked.defaultof<D3DKMT_OPENADAPTERFROMHDC> with
                        hDc = hDc
                    }

                if D3DKMTOpenAdapterFromHdc &info <> 0l then
                    Logging.Error("Error getting adapter handle by hDc")

                _hAdapter <- info.hAdapter
                _VinPnSourceId <- info.VinPnSourceId

                if not (DeleteDC hDc) then
                    Logging.Error("Error deleting hDc after use")

        (* USING OPEN ADAPTER TO GET SYNC INFORMATION *)

        let private get_scanline () =
            let mutable info =
                { Unchecked.defaultof<D3DKMT_GETSCANLINE> with
                    hAdapter = _hAdapter
                    VinPnSourceId = _VinPnSourceId
                }

            if D3DKMTGetScanLine &info <> 0l then
                Logging.Error("Error getting scanline from video adapter")
                None
            else
                Some(info.ScanLine, info.InVerticalBlank)

        let private wait_for_vblank () =
            let mutable info =
                {
                    hAdapter = _hAdapter
                    hDevice = 0u
                    VinPnSourceId = _VinPnSourceId
                }

            let status = D3DKMTWaitForVerticalBlankEvent &info

            if status <> 0l then
                Logging.Error(sprintf "Error waiting for vblank (%i)" status)
                false
            else
                true

        (* LOOP *)

        let private LOCK_OBJ = obj()
        let mutable private _last_time = 0.0 // in thread
        let mutable private _vblank_number = 0uL
        let mutable private last_time = 0.0 // shared
        let mutable private vblank_number = 0uL // shared
        let mutable private est_period = 0.0
        let mutable private sync_broken = true
        let mutable private action_queue : (unit -> unit) list = []
        let mutable private looping = true

        let private loop (sw: Stopwatch) =
            while looping do
                if not sync_broken then
                    let before = sw.Elapsed.TotalMilliseconds
                    if not (wait_for_vblank()) then sync_broken <- true
                    _last_time <- sw.Elapsed.TotalMilliseconds
                    if _vblank_number > 240uL && abs (before + est_period - _last_time) > 1.0 then
                        _last_time <- before + est_period
                    else
                        est_period <- est_period * 0.95 + (_last_time - before) * 0.05
                    _vblank_number <- _vblank_number + 1uL
                lock LOCK_OBJ <| fun () ->
                    last_time <- _last_time
                    vblank_number <- _vblank_number
                    for action in action_queue do action()
                    action_queue <- []

        let start (sw: Stopwatch) =
            Thread(fun () ->
                loop sw
            ).Start()

        let stop () = looping <- false

        let switch (period: float) (gdi_adapter_name: string) (glfw_monitor_name: string) =
            lock LOCK_OBJ <| fun () ->
                action_queue <- (fun () -> _vblank_number <- 0uL; sync_broken <- false; est_period <- period; open_adapter gdi_adapter_name glfw_monitor_name) :: action_queue

        let get (sw: Stopwatch) =
            lock LOCK_OBJ <| fun () ->
            if sync_broken then 0uL, sw.Elapsed.TotalMilliseconds, est_period
            else vblank_number, last_time, est_period
            
type Strategy =
    | Unlimited
    | CpuTimingWindowed
    | CpuTimingFullscreen

[<AutoOpen>]
module SmartCapConstants =
    
    let mutable screen_tearing_offset = -1.0
    let mutable anti_jitter = true

type private RenderThread(window: NativeWindow, audio_device: int, ui_root: Root, after_init: unit -> unit) =

    let mutable resized = false
    let mutable fps_count = 0
    let fps_timer = Stopwatch()
    let last_frame_timer = Stopwatch()
    let total_frame_timer = Stopwatch.StartNew()
    let mutable est_present_of_next_frame = 0.0
    let mutable present_of_last_frame = 0.0
    let mutable swap_of_last_frame = 0.0
    let mutable start_of_frame = 0.0
    let mutable frame_is_ready = 0.0
    let mutable monitor_y = 1080.0
    let mutable est_refresh_period = 1000.0 / 60.0
    let mutable is_focused = true
    let mutable strategy = Unlimited
    let mutable cpu_timing_compensation = 0.0

    let now () =
        total_frame_timer.Elapsed.TotalMilliseconds

    member this.IsFocused
        with set v = is_focused <- v

    member val RenderMode = FrameLimit.Smart with get, set

    member this.OnResize(newSize: Vector2i) =
        Render.resize (newSize.X, newSize.Y)
        resized <- true

    member this.RenderModeChanged(refresh_rate: int, monitor_height: int, fullscreen: bool) =
        est_refresh_period <- 1000.0 / float refresh_rate
        cpu_timing_compensation <- 0.0
        monitor_y <- float monitor_height

        strategy <-
            if this.RenderMode = FrameLimit.Smart then
                if fullscreen then CpuTimingFullscreen else CpuTimingWindowed
            else
                Unlimited

    member private this.Loop() =
        window.Context.MakeCurrent()
        this.Init()
        fps_timer.Start()

        try
            while not (GLFW.WindowShouldClose window.WindowPtr) do
                this.DispatchFrame()
        with fatal_err ->
            Logging.Critical("Fatal crash in UI thread", fatal_err)
            window.Close()
        FrameTimeStrategies.VBlankThread.stop ()

    member this.Start() =
        let thread = Thread(this.Loop)
        Percyqaz.Flux.Utils.UI_THREAD <- thread.ManagedThreadId
        thread.Start()

    member this.DispatchFrame() =

        match strategy with

        | Unlimited ->
            present_of_last_frame <- now ()
            Performance.visual_latency_lo <- 0.0
            Performance.visual_latency_hi <- 1.0

        | CpuTimingFullscreen ->
            let frame, last_vblank, est_refresh_period = FrameTimeStrategies.VBlankThread.get total_frame_timer
            present_of_last_frame <- swap_of_last_frame
            est_present_of_next_frame <- last_vblank + est_refresh_period + screen_tearing_offset
            Performance.visual_latency_lo <- est_present_of_next_frame - present_of_last_frame
            Performance.visual_latency_hi <- est_present_of_next_frame - present_of_last_frame

            // apply correction
            let how_late = present_of_last_frame - last_vblank
            let how_early = est_present_of_next_frame - present_of_last_frame
            printfn "late: %.2f early: %.2f" how_late how_early
            if how_late < how_early then
                cpu_timing_compensation <- cpu_timing_compensation - how_late * 0.001
                printfn "%.2f" cpu_timing_compensation
            else
                cpu_timing_compensation <- cpu_timing_compensation + how_early * 0.001
                printfn "%.2f" cpu_timing_compensation

            FrameTimeStrategies.sleep_accurate (total_frame_timer, est_present_of_next_frame + cpu_timing_compensation)
        
        | CpuTimingWindowed ->
            let frame, last_vblank, est_refresh_period = FrameTimeStrategies.VBlankThread.get total_frame_timer
            present_of_last_frame <- swap_of_last_frame
            est_present_of_next_frame <- last_vblank + est_refresh_period
            Performance.visual_latency_lo <- est_present_of_next_frame - present_of_last_frame
            Performance.visual_latency_hi <- est_present_of_next_frame - present_of_last_frame

            FrameTimeStrategies.sleep_accurate (total_frame_timer, est_present_of_next_frame)

        let elapsed_ms = last_frame_timer.Elapsed.TotalMilliseconds
        last_frame_timer.Restart()

        // Update
        start_of_frame <- now ()
        Input.begin_frame_events ()
        ROOT_ANIMATION.Update elapsed_ms
        ui_root.Update(elapsed_ms, resized)
        resized <- false
        Input.finish_frame_events ()
        Devices.update elapsed_ms
        Performance.update_time <- now () - start_of_frame

        if ui_root.ShouldExit then
            window.Close()

        // Draw
        let before_draw = now ()
        Render.start ()

        if Viewport.rheight > 0 then
            ui_root.Draw()

        Render.finish ()
        frame_is_ready <- now ()
        Performance.draw_time <- frame_is_ready - before_draw

        if not ui_root.ShouldExit then
            window.Context.SwapBuffers()
            swap_of_last_frame <- now ()

        // Performance profiling
        fps_count <- fps_count + 1
        let time = fps_timer.ElapsedTicks

        if time > Stopwatch.Frequency then
            Performance.framecount_tickcount <- (fps_count, time)
            fps_timer.Restart()
            fps_count <- 0

        Performance.elapsed_ms <- elapsed_ms

    member this.Init() =
        Devices.init audio_device
        Render.init ()
        Render.resize (window.ClientSize.X, window.ClientSize.Y)

        FrameTimeStrategies.VBlankThread.start total_frame_timer

        Performance.frame_compensation <-
            fun () ->
                if strategy <> Unlimited && anti_jitter then
                    float32 (est_present_of_next_frame - now ()) * 1.0f<ms>
                else
                    0.0f<ms>

        ui_root.Init()
        after_init ()