namespace Percyqaz.Flux.Graphics

open System.Diagnostics
open System.Runtime.CompilerServices
open OpenTK.Graphics.OpenGL
open Percyqaz.Common

type private GLError =

    static let mutable first_error = true

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

    static member Check() : unit =
        let error = GL.GetError()

        if error <> ErrorCode.NoError && first_error then
            first_error <- false
            Logging.Error "GL Error: %O\r\n%s" error (GLError.Stacktrace(5))