namespace Percyqaz.Flux.Graphics

open System
open OpenTK.Graphics.OpenGL

[<AutoOpen>]
module private Helpers =

    let mutable first_error = true

    let check_gl_error () =
        let error = GL.GetError()

        if error <> ErrorCode.NoError && first_error then
            first_error <- false
            Percyqaz.Common.Logging.Error "%O\n%s" error Environment.StackTrace
