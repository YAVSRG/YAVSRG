namespace Interlude.UI.Screens.Import

open System.IO
open Percyqaz.Json
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Web
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Screens.LevelSelect

[<Json.AllRequired>]
type EOPackAttrs = {
    name: string
    average: float
    download: string
    mirror: string
    size: int64
} with static member Default = { name = ""; average = 0.0; download = ""; mirror = ""; size = 0L }

[<Json.AllRequired>]
type EOPack = {
    ``type``: string
    id: int
    attributes: EOPackAttrs
} with static member Default = {``type`` = "pack"; id = 0; attributes = EOPackAttrs.Default }

type private SMImportCard(data: EOPackAttrs) as this =
    inherit Frame((fun () -> Style.accentShade(120, (if this.Downloaded then 0.7f else 0.5f), 0.0f)), (fun () -> Style.accentShade(200, 0.7f, 0.2f)))
    let mutable downloaded = 
        let path = Path.Combine(getDataPath "Songs", data.name)
        Directory.Exists path && not ( Seq.isEmpty (Directory.EnumerateDirectories path) )
    let download() =
        let target = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString() + ".zip")
        Notification.add (Localisation.localiseWith [data.name] "notification.download.pack", NotificationType.Task)
        BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.name)
            (BackgroundTask.Chain
                [
                    downloadFile(data.download, target)
                    (Library.Imports.autoConvert target
                        |> BackgroundTask.Callback( fun b -> 
                            LevelSelect.refresh <- LevelSelect.refresh || b
                            Notification.add (Localisation.localiseWith [data.name] "notification.install.pack", NotificationType.Task)
                            File.Delete target ))
                ]) |> ignore
        downloaded <- true
    do
        TextBox(K data.name, K (Color.White, Color.Black), 0.0f)
        |> positionWidgetA(5.0f, 0.0f, -400.0f, -30.0f)
        |> this.Add
        TextBox(
            K (sprintf "%.1fMB" (float data.size / 1000000.0)),
            K (Color.White, Color.Black), 1.0f )
        |> positionWidgetA(5.0f, 0.0f, -5.0f, -30.0f)
        |> this.Add
        TextBox(
            (fun () -> if downloaded then "Downloaded" else ""),
            K (Color.Aqua, Color.Black), 1.0f )
        |> positionWidgetA(5.0f, 50.0f, -5.0f, 0.0f)
        |> this.Add
        TextBox(
            K (sprintf "Average difficulty (MSD): %.2f" data.average),
            K (Color.White, Color.Black), 0.0f )
        |> positionWidgetA(5.0f, 50.0f, -400.0f, 0.0f)
        |> this.Add

        this.Add(new Clickable((fun () -> if not downloaded then download()), ignore))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f)

    member this.Downloaded = downloaded

    member this.Data = data

    static member Filter(filter: Filter) =
        fun (c: Widget) ->
            match c with
            | :? SMImportCard as c ->
                List.forall (
                    function
                    | Impossible -> false
                    | String str -> c.Data.name.ToLower().Contains(str)
                    | _ -> true
                ) filter
            | _ -> true