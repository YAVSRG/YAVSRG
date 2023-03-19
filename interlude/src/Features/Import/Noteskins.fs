namespace Interlude.Features.Import

open System.IO
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Themes.Noteskin
open Prelude.Data
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.Utils
open Interlude.Content

type NoteskinCard(data: RepoEntry) as this =
    inherit Frame(NodeType.None, Fill = (fun () -> Style.color(120, (if this.Downloaded then 0.7f else 0.5f), 0.0f)), Border = (fun () -> Style.color(200, 0.7f, 0.2f)))
            
    let mutable downloaded = Noteskins.list() |> Array.map snd |> Array.contains data.Name
    let download() =
        let target = Path.Combine(getDataPath "Noteskins", System.Guid.NewGuid().ToString() + ".isk")
        WebServices.download_file.Request((data.Download, target, ignore), 
            fun success -> 
                if success then 
                    sync Noteskins.load
                    Notifications.task_feedback (Icons.download, L"notification.install_noteskin", data.Name)
        )
        downloaded <- true

    let mutable preview : Sprite option = None
    let imgFade = Animation.Fade 0.0f
    do
        this
        |+ Text(data.Name,
            Align = Alignment.LEFT,
            Position = Position.Margin(5.0f, 0.0f).TrimTop(240.0f))
        |+ Text(
            (fun () -> if downloaded then "Downloaded" else ""),
            Align = Alignment.RIGHT,
            Position = Position.Margin(5.0f, 0.0f).TrimTop(240.0f) )
        |* Clickable((fun () -> if not downloaded then download()))

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        imgFade.Update elapsedTime

    override this.Draw() =
        base.Draw()
        match preview with
        | Some p -> 
            Draw.sprite ( Rect.Box(this.Bounds.Left, this.Bounds.Top, 320.0f, 240.0f) ) (Color.FromArgb(imgFade.Alpha, Color.White)) p
        | None -> ()

    member this.LoadPreview(img: Bitmap) =
        preview <- Some <| Sprite.upload(img, 1, 1, true)
        imgFade.Target <- 1.0f

    member this.Name = data.Name
    member this.Downloaded = downloaded

    static member Filter(filter: Filter) =
        fun (c: Widget) ->
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

    let tab =
        SearchContainer(
            (fun searchContainer callback -> 
                WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Interlude.Noteskins/main/index.json",
                    fun data ->
                    match data with
                    | Some (d: Themes.Noteskin.Repo) -> 
                        sync( fun () -> 
                            for ns in d.Noteskins do
                                let nc = NoteskinCard ns
                                ImageServices.get_cached_image.Request(ns.Preview, fun img -> sync(fun () -> nc.LoadPreview img))
                                searchContainer.Items.Add nc
                        )
                    | None -> ()
                    callback()
                )
            ),
            (fun searchContainer filter -> searchContainer.Items.Filter <- NoteskinCard.Filter filter),
            300.0f
        )