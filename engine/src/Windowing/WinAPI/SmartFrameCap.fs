namespace Percyqaz.Flux.Windowing

open System
open System.Threading
open System.Diagnostics
open System.Runtime.InteropServices
open Percyqaz.Flux.Graphics
open Percyqaz.Common

module private FrameTimeStrategies =

    //https://github.com/glfw/glfw/issues/1157

    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type _LUID =
        {
            mutable LowPart: uint32
            mutable HighPart: int32
        }

    type MonitorEnumProc = delegate of IntPtr * IntPtr * IntPtr * IntPtr -> bool
    [<DllImport("user32.dll")>]
    extern bool EnumDisplayMonitors(IntPtr hdc, IntPtr lprcClip, MonitorEnumProc lpfnEnum, IntPtr dwData)

    [<DllImport("gdi32.dll", SetLastError = true)>]
    extern IntPtr CreateDC(string lpszDriver, string lpszDevice, string lpszOutput, IntPtr lpInitData)

    [<DllImport("gdi32.dll", SetLastError = true)>]
    extern bool DeleteDC(IntPtr hdc)

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

    // https://stackoverflow.com/questions/49244480/correct-way-to-wait-for-vblank-on-windows-10-in-windowed-mode
    [<DllImport("dwmapi.dll")>]
    extern int64 DwmFlush()

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

    let spin_wait = SpinWait()
    let sleep_accurate (timer: Stopwatch, until: float) =

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
                Logging.Error "Error waiting for vblank (%i)" status
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

        let ENABLED =
            OperatingSystem.IsWindows() &&
            Render.detect_gpu_vendor() <> Render.GpuVendor.Intel

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
            if ENABLED then
                Thread(fun () ->
                    loop sw
                ).Start()

        let stop () = looping <- false

        let switch (period: float) (gdi_adapter_name: string) (glfw_monitor_name: string) =
            if ENABLED then
                lock LOCK_OBJ <| fun () ->
                    action_queue <- (fun () ->
                        _vblank_number <- 0uL
                        sync_broken <- false
                        est_period <- period
                        open_adapter gdi_adapter_name glfw_monitor_name
                    ) :: action_queue
            else
                _vblank_number <- 0uL
                sync_broken <- true
                est_period <- period

        let get (sync_adjustment: float, sw: Stopwatch) =
            lock LOCK_OBJ <| fun () ->
            if sync_broken then sw.Elapsed.TotalMilliseconds, est_period
            else
                let adjusted_last_time = last_time + sync_adjustment * est_period
                if sw.Elapsed.TotalMilliseconds < adjusted_last_time then
                    adjusted_last_time - est_period, est_period
                else adjusted_last_time, est_period