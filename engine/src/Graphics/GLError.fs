namespace Percyqaz.Flux.Graphics

open System
open OpenTK.Graphics.OpenGL

type private GLError =

    static let mutable first_error = true

    static member Check() : unit =
        let error = GL.GetError()

        if error <> ErrorCode.NoError && first_error then
            first_error <- false
            Percyqaz.Common.Logging.Error "%O\n%s" error Environment.StackTrace
