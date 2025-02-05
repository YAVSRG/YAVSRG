namespace Percyqaz.Flux.Windowing

open System
open System.IO
open System.Runtime.InteropServices
open FSharp.NativeInterop
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open Percyqaz.Common

#nowarn "9"

module ClipboardWin32 =

    [<DllImport("User32.dll")>]
    [<return: MarshalAs(UnmanagedType.Bool)>]
    extern bool private IsClipboardFormatAvailable(uint format);

    [<DllImport("User32.dll")>]
    extern IntPtr private GetClipboardData(uint uFormat);

    [<DllImport("user32.dll")>]
    extern IntPtr private SetClipboardData(uint uFormat, IntPtr hMem);

    [<DllImport("User32.dll")>]
    [<return: MarshalAs(UnmanagedType.Bool)>]
    extern bool private OpenClipboard(IntPtr hWndNewOwner);

    [<DllImport("user32.dll")>]
    extern bool private EmptyClipboard();

    [<DllImport("User32.dll")>]
    [<return: MarshalAs(UnmanagedType.Bool)>]
    extern bool private CloseClipboard();

    [<DllImport("Kernel32.dll")>]
    extern IntPtr private GlobalLock(IntPtr hMem);

    [<DllImport("kernel32.dll")>]
    extern IntPtr private GlobalAlloc(uint uFlags, UIntPtr dwBytes);

    [<DllImport("Kernel32.dll")>]
    [<return: MarshalAs(UnmanagedType.Bool)>]
    extern bool private GlobalUnlock(IntPtr hMem);

    [<DllImport("Kernel32.dll")>]
    extern int private GlobalSize(IntPtr hMem);

    [<DllImport("kernel32.dll")>]
    extern IntPtr private GlobalFree(IntPtr hMem);

    // GLFW manages most clipboard stuff, but as of 3.3.8 doesn't have a cross-platform API for reading/writing images to the clipboard
    // So for now enjoy this windows-only code

    let private set_clipboard(ptr: nativeint, length: int, format: uint32, clear: bool) =
        let mutable success = false
        try
            try
                if OpenClipboard(IntPtr.Zero) |> not then
                    failwith "OpenClipboard"
                else

                if clear && EmptyClipboard() |> not then
                    failwith "EmptyClipboard"
                else

                let mutable hGlobal = GlobalAlloc(0x02u ||| 0x40u, UIntPtr(uint32 length))

                try

                    let mutable target = GlobalLock(hGlobal)
                    if target = IntPtr.Zero then
                        failwith "GlobalLock"
                    else

                    try
                        NativePtr.copyBlock (NativePtr.ofNativeInt<byte> target) (NativePtr.ofNativeInt<byte> ptr) length
                    finally
                        if target <> IntPtr.Zero then
                            GlobalUnlock(target) |> ignore

                        Marshal.FreeHGlobal(ptr)

                    if SetClipboardData(format, hGlobal) |> int64 <> 0 then
                        hGlobal <- IntPtr.Zero
                        success <- true
                finally
                    if hGlobal <> IntPtr.Zero then
                        GlobalFree(hGlobal) |> ignore
            finally
                CloseClipboard() |> ignore
        with err ->
            Logging.Error "Error copying image to clipboard: %s" err.Message

        success

    // https://learn.microsoft.com/en-us/windows/win32/dataxchg/standard-clipboard-formats
    let CF_DIB = 8u
    let CF_UNICODETEXT = 13u

    let set_image (image: Image<Rgba32>) =
        if OperatingSystem.IsWindows() |> not then
            false
        else

        use stream = new MemoryStream()
        image.SaveAsBmp(stream)

        let bytes = int (stream.Length - 14L)
        let ptr = Marshal.AllocHGlobal(bytes)
        Marshal.Copy(stream.GetBuffer(), 14, ptr, bytes)

        set_clipboard (ptr, bytes, CF_DIB, true)

    let set_image_and_text (image: Image<Rgba32>, text: string) =
        if set_image image then
            let bytes = (text.Length + 1) * 2
            let source = Marshal.StringToHGlobalUni(text)
            set_clipboard (source, bytes, CF_UNICODETEXT, false) |> ignore
            true
        else 
            false