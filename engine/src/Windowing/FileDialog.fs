namespace Percyqaz.Flux.Windowing

open System
open System.IO
open NativeFileDialogSharp

[<RequireQualifiedAccess>]
type FileDialogResult =
    | Ok of string
    | Cancelled
    | Error

[<RequireQualifiedAccess>]
type FileDialogMultipleResult =
    | Ok of string array
    | Cancelled
    | Error

module FileDialog =

    let pick_file (path: string | null, filters: string, callback: FileDialogResult -> unit) : unit =

        let show_dialog () : FileDialogResult =
            let result =
                Dialog.FileOpen(filters, if path = null then null else Path.GetFullPath(path))

            if result.IsOk then FileDialogResult.Ok(result.Path)
            elif result.IsError then FileDialogResult.Error
            else FileDialogResult.Cancelled

        if OperatingSystem.IsMacOS() then
            WindowThread.ACTION_QUEUE.EnsureCurrent(fun () ->
                let result = show_dialog()
                GameThread.ACTION_QUEUE.Defer(fun () -> callback result)
            )
        else
            GameThread.ACTION_QUEUE.EnsureCurrent(fun () -> callback(show_dialog()))

    let pick_files (path: string | null, filters: string, callback: FileDialogMultipleResult -> unit) : unit =

        let show_dialog () : FileDialogMultipleResult =
            let result =
                Dialog.FileOpenMultiple(filters, if path = null then null else Path.GetFullPath(path))

            if result.IsOk then FileDialogMultipleResult.Ok(result.Paths |> Seq.toArray)
            elif result.IsError then FileDialogMultipleResult.Error
            else FileDialogMultipleResult.Cancelled

        if OperatingSystem.IsMacOS() then
            WindowThread.ACTION_QUEUE.EnsureCurrent(fun () ->
                let result = show_dialog()
                GameThread.ACTION_QUEUE.Defer(fun () -> callback result)
            )
        else
            GameThread.ACTION_QUEUE.EnsureCurrent(fun () -> callback(show_dialog()))

    let pick_folder (path: string | null, callback: FileDialogResult -> unit) : unit =

        let show_dialog () : FileDialogResult =
            let result =
                Dialog.FolderPicker(if path = null then null else Path.GetFullPath(path))

            if result.IsOk then FileDialogResult.Ok(result.Path)
            elif result.IsError then FileDialogResult.Error
            else FileDialogResult.Cancelled

        if OperatingSystem.IsMacOS() then
            WindowThread.ACTION_QUEUE.EnsureCurrent(fun () ->
                let result = show_dialog()
                GameThread.ACTION_QUEUE.Defer(fun () -> callback result)
            )
        else
            GameThread.ACTION_QUEUE.EnsureCurrent(fun () -> callback(show_dialog()))
