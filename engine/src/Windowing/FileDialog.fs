namespace Percyqaz.Flux.Windowing

open System.IO
open NativeFileDialogSharp

module FileDialog =
    
    let pick_file(path: string) =
        let result = Dialog.FileOpen("osz,osu,qp,qua,zip,sm;osk,isk;ruleset", Path.GetFullPath(path))
        if result.IsOk then printfn "Ok %s" result.Path
        elif result.IsError then printfn "Error %A" result.Path
        elif result.IsCancelled then printfn "Cancelled %A" result.Path