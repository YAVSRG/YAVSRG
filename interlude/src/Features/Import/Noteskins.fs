namespace Interlude.UI.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Themes.Noteskin
open Prelude.Web
open Prelude.Data.Charts.Sorting
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.Content

type NoteskinCard(data: RepoEntry) as this =
    inherit Frame((fun () -> Style.color(120, (if this.Downloaded then 0.7f else 0.5f), 0.0f)), (fun () -> Style.color(200, 0.7f, 0.2f)))
            
    let mutable downloaded = Noteskins.list() |> Array.map snd |> Array.contains data.Name
    let download() =
        let target = Path.Combine(getDataPath("Noteskins"), System.Guid.NewGuid().ToString() + ".isk")
        Notification.add (Localisation.localiseWith [data.Name] "notification.download.noteskin", NotificationType.Task)
        BackgroundTask.Create TaskFlags.LONGRUNNING ("Installing " + data.Name)
            ( BackgroundTask.Callback(fun b -> Noteskins.load()) (downloadFile(data.Download, target)) ) |> ignore
        downloaded <- true

    let mutable preview = Sprite.Default
    do
        this.Position( Position.SliceTop 300.0f )
        |-+ TextBox(K data.Name, K (Color.White, Color.Black), 0.0f)
            .Position( Position.Margin(5.0f, 0.0f).TrimTop(240.0f) )
        |-+ TextBox(
                (fun () -> if downloaded then "Downloaded" else ""),
                K (Color.Aqua, Color.Black), 1.0f )
            .Position( Position.Margin(5.0f, 0.0f).TrimTop(240.0f) )
        |=+ Clickable((fun () -> if not downloaded then download()), ignore)

    override this.Draw() =
        base.Draw()
        Draw.sprite ( Rect.Box(this.Bounds.Left, this.Bounds.Top, 320.0f, 240.0f) ) Color.White preview

    member this.LoadPreview(img: Bitmap) =
        preview <- Sprite.upload(img, 1, 1, true)

    override this.Dispose() =
        if preview <> Sprite.Default then Sprite.destroy preview

    member this.Name = data.Name
    member this.Downloaded = downloaded

    static member Filter(filter: Filter) =
        fun (c: Widget1) ->
            match c with
            | :? NoteskinCard as c ->
                List.forall (
                    function
                    | Impossible -> false
                    | String str -> c.Name.ToLower().Contains(str)
                    | _ -> true
                ) filter
            | _ -> true
        
module Noteskins =

    let source = "https://raw.githubusercontent.com/YAVSRG/Interlude.Noteskins/main/index.json"

    let image_loader =
        { new Async.ManyWorker<(string * NoteskinCard), Bitmap>() with
            member this.Handle( (url, _) ) =
                 Async.RunSynchronously(downloadImage url)
            member this.Callback((_, card), img) =
                card.Synchronized(fun () -> card.LoadPreview img)
        }