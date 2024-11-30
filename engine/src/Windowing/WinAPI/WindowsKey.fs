namespace Percyqaz.Flux.Windowing

open System
open System.Diagnostics
open System.Runtime.InteropServices

module WindowsKey =

    type private LowLevelKeyboardProc = delegate of nCode: int * wParam: IntPtr * lParam: IntPtr -> IntPtr

    [<DllImport("user32.dll", CharSet = CharSet.Auto)>]
    extern IntPtr private SetWindowsHookEx(int32 idHook, LowLevelKeyboardProc lpfn, IntPtr hMod, uint dwThreadId)

    [<DllImport("user32.dll", CharSet = CharSet.Auto)>]
    extern bool private UnhookWindowsHookEx(IntPtr hhk)
    
    [<DllImport("user32.dll", CharSet = CharSet.Auto)>]
    extern IntPtr private CallNextHookEx(IntPtr hhk, int nCode, IntPtr wParam, IntPtr lParam)

    [<DllImport("kernel32.dll", CharSet = CharSet.Auto)>]
    extern IntPtr private GetModuleHandle(string lpModuleName)

    let mutable private hook = -1n

    let WM_KEYDOWN = 0x0100n
    let VK_LWIN = 0x5B
    let VK_RWIN = 0x5C

    let private hook_callback (nCode: int) (wParam: IntPtr) (lParam: IntPtr) : IntPtr =
        let vkCode = Marshal.ReadInt32(lParam)
        if nCode >= 0 && wParam = WM_KEYDOWN && (vkCode = VK_LWIN || vkCode = VK_RWIN) then
            1n
        else 
            CallNextHookEx(hook, nCode, wParam, lParam)
    let private hook_callback_delegate = LowLevelKeyboardProc(hook_callback)

    let disable () : unit =
        assert(OperatingSystem.IsWindows())
        if hook = -1n then
            use current_process = Process.GetCurrentProcess()
            use current_module = current_process.MainModule
            let hMod = GetModuleHandle(current_module.ModuleName)
            hook <- SetWindowsHookEx(13, hook_callback_delegate, hMod, 0u)

    let enable () : unit =
        if hook <> -1n then 
            UnhookWindowsHookEx(hook) |> ignore
            hook <- -1n