namespace Percyqaz.Flux.Audio

open System
open System.Diagnostics
open System.Runtime.CompilerServices
open ManagedBass
open Percyqaz.Common

type BassErrorExtensions =

    // todo: use this in more places, it's extremely useful!
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    static member Stacktrace(depth: int) : string =
        let fmt_method (frame: StackFrame) =
            if frame.HasMethod() then
                let m = frame.GetMethod()
                sprintf "\r\n   %s.%s::%s" m.DeclaringType.Namespace m.DeclaringType.Name m.Name
            else
                ""

        StackTrace(2, true).GetFrames()
        |> Array.truncate depth
        |> Array.map(fun frame ->
            $"at {frame.GetFileName()}:{frame.GetFileLineNumber()}:{frame.GetFileColumnNumber()}{fmt_method(frame)}"
        )
        |> String.concat "\r\n"

    [<Extension>]
    static member LogBassError(return_value: bool) : unit =
        if not return_value then
            Logging.Debug "Bass Error: %O\r\n%s" Bass.LastError (BassErrorExtensions.Stacktrace(5))

    [<Extension>]
    static member LogBassError(return_handle: int) : unit =
        if return_handle = 0 then
            Logging.Debug "Bass Handle Error: %O\r\n%s" Bass.LastError (BassErrorExtensions.Stacktrace(5))
