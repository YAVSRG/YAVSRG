namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Themes.Noteskin
open Prelude.Data
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Content

type NoteskinCard(data: RepoEntry) as this =
    inherit Frame(NodeType.Button (fun () -> this.Download()),
        Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.pink_accent else Colors.grey_2.O3))
            
    let mutable downloaded = Noteskins.list() |> Array.map snd |> Array.contains data.Name

    let mutable preview : Sprite option = None
    let imgFade = Animation.Fade 0.0f
    do
        this
        |+ Text(data.Name,
            Align = Alignment.CENTER,
            Position = Position.Margin(Style.padding).SliceTop(70.0f))
        |* Clickable.Focus this

    member this.Download() =
        let target = Path.Combine(getDataPath "Noteskins", System.Guid.NewGuid().ToString() + ".isk")
        WebServices.download_file.Request((data.Download, target, ignore), 
            fun success -> 
                if success then 
                    sync Noteskins.load
                    Notifications.task_feedback (Icons.download, L"notification.install_noteskin", data.Name)
        )
        downloaded <- true

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        imgFade.Update elapsedTime

    override this.Draw() =
        base.Draw()
        Draw.rect (this.Bounds.SliceBottom(60.0f).Shrink(50.0f, 0.0f)) Colors.shadow_2.O2
        match preview with
        | Some p -> 
            let img_bounds = Rect.Box(this.Bounds.CenterX - 160.0f, this.Bounds.Top + 75.0f, 320.0f, 240.0f)
            Draw.sprite img_bounds (Colors.white.O4a imgFade.Alpha) p
        | None -> ()
        Text.drawFillB(
            Style.baseFont, 
            (if downloaded then Icons.check + " Downloaded" else Icons.download + " Download"),
            this.Bounds.SliceBottom(60.0f).Shrink(Style.padding),
            (if downloaded then Colors.text_green else Colors.text),
            Alignment.CENTER)

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

    type NoteskinSearch() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))
    
        let grid = GridContainer<Widget>(380.0f, 2, Spacing = (15.0f, 15.0f))
        let scroll = ScrollContainer.Grid(grid, Margin = Style.padding, Position = Position.TrimTop 70.0f)
        let mutable failed = false
    
        do
            WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Interlude.Noteskins/main/index.json",
                fun data ->
                match data with
                | Some (d: Themes.Noteskin.Repo) -> 
                    sync( fun () -> 
                        for ns in d.Noteskins do
                            let nc = NoteskinCard ns
                            ImageServices.get_cached_image.Request(ns.Preview, fun img -> sync(fun () -> nc.LoadPreview img))
                            grid.Add nc
                    )
                | None -> failed <- true
            )
            this
            |+ (SearchBox(Setting.simple "", (fun (f: Filter) -> grid.Filter <- NoteskinCard.Filter f), Position = Position.SliceTop 60.0f ))
            |* scroll
    
        member this.Items = grid

    let tab = NoteskinSearch()