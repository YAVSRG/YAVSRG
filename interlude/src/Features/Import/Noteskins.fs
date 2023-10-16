namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Content.Noteskin
open Prelude.Data
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Content

type NoteskinCard(data: RepoEntry) as this =
    inherit Frame(NodeType.Button (fun () -> Style.click.Play(); this.Download()),
        Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.pink_accent else Colors.grey_2.O3))
            
    let mutable status = 
        if Noteskins.list() |> Seq.map snd |> Seq.map (fun ns -> ns.Config.Name) |> Seq.contains data.Name then Installed else NotDownloaded

    let mutable preview : Sprite option = None
    let imgFade = Animation.Fade 0.0f
    do
        this
        |+ Text(data.Name,
            Align = Alignment.CENTER,
            Position = Position.Margin(Style.PADDING).SliceTop(70.0f))
        |* Clickable.Focus this

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    member this.Download() =
        if status = NotDownloaded || status = DownloadFailed then
            status <- Downloading
            let target = Path.Combine(getDataPath "Noteskins", System.Text.RegularExpressions.Regex("[^a-zA-Z0-9_-]").Replace(data.Name, "") + ".isk")
            WebServices.download_file.Request((data.Download, target, ignore), 
                fun success -> 
                    if success then 
                        sync Noteskins.load
                        Notifications.task_feedback (Icons.download, L"notification.install_noteskin", data.Name)
                        status <- Installed
                    else status <- DownloadFailed
            )

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
            Style.font, 
            (
                match status with
                | NotDownloaded -> Icons.download + " Download"
                | Downloading -> Icons.download + " Downloading .."
                | DownloadFailed -> Icons.x + " Error"
                | Installed -> Icons.check + " Downloaded"
            ),
            this.Bounds.SliceBottom(60.0f).Shrink(Style.PADDING),
            (
                match status with
                | NotDownloaded -> if this.Focused then Colors.text_yellow_2 else Colors.text
                | Downloading -> Colors.text_yellow_2
                | DownloadFailed -> Colors.text_red
                | Installed -> Colors.text_green
            ),
            Alignment.CENTER)

    member this.LoadPreview(img: Bitmap) =
        preview <- Some <| Sprite.upload(img, 1, 1, true)
        imgFade.Target <- 1.0f

    member this.Name = data.Name

    static member Filter(filter: Filter) =
        fun (c: NoteskinCard) ->
            List.forall (
                function
                | Impossible -> false
                | String str -> c.Name.ToLower().Contains(str)
                | _ -> true
            ) filter
        
module Noteskins =

    type NoteskinSearch() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))
    
        let grid = GridFlowContainer<NoteskinCard>(380.0f, 3, Spacing = (15.0f, 15.0f), WrapNavigation = false)
        let scroll = ScrollContainer.Grid(grid, Margin = Style.PADDING, Position = Position.TrimTop(70.0f).TrimBottom(65.0f))
        let mutable failed = false
    
        override this.Init(parent) =
            WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Interlude.Noteskins/main/index.json",
                fun data ->
                match data with
                | Some (d: Content.Noteskin.Repo) -> 
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
            |+ Text(L"imports.noteskins.hint", Position = Position.SliceBottom 55.0f)
            |* scroll
            base.Init parent

        override this.Focusable = grid.Focusable 
    
        member this.Items = grid

    let tab = NoteskinSearch()