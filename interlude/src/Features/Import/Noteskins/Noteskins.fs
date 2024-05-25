namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins.Repo
open Prelude.Data
open Prelude.Data.Library.Sorting
open Interlude.UI

type NoteskinGroupCard(data: NoteskinGroup) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                NoteskinGroupPage(data).Show()
            ),
            Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
            Border =
                (fun () ->
                    if this.Focused then
                        Colors.pink_accent
                    else
                        Colors.grey_2.O3
                )
        )

    let mutable preview: Sprite option = None
    let preview_fade = Animation.Fade 0.0f

    do
        this
        |+ Text(data.Name, Align = Alignment.CENTER, Position = Position.Margin(Style.PADDING).SliceTop(70.0f))
        |* Clickable.Focus this

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        preview_fade.Update elapsed_ms

    override this.Draw() =
        base.Draw()

        match preview with
        | Some p ->
            let img_bounds =
                Rect.Box(this.Bounds.CenterX - 160.0f, this.Bounds.Top + 85.0f, 320.0f, 240.0f)

            Draw.sprite img_bounds (Colors.white.O4a preview_fade.Alpha) p
        | None -> ()

    member this.LoadPreview(img: Bitmap) =
        preview <-
            Some
            <| Sprite.upload_one false true (SpriteUpload.OfImage("NOTESKIN_PREVIEW", img))

        preview_fade.Target <- 1.0f

    member this.Name = data.Name

    static member Filter(filter: Filter) =
        fun (c: NoteskinGroupCard) ->
            List.forall
                (function
                | Impossible -> false
                | String str -> c.Name.ToLower().Contains(str)
                | _ -> true)
                filter

module Noteskins =

    type NoteskinSearch() as this =
        inherit Container(NodeType.Container(fun _ -> Some this.Items))

        let grid =
            GridFlowContainer<NoteskinGroupCard>(340.0f, 3, Spacing = (15.0f, 15.0f), WrapNavigation = false)

        let scroll =
            ScrollContainer(grid, Margin = Style.PADDING, Position = Position.TrimTop(70.0f).TrimBottom(110.0f))

        let mutable loading = true
        let mutable failed = false

        override this.Init(parent) =
            WebServices.download_json (
                sprintf "https://raw.%s.com/YAVSRG/YAVSRG/main/backbeat/noteskins/index.json" "githubusercontent",
                fun data ->
                    match data with
                    | Some(d: NoteskinRepo) ->
                        defer (fun () ->
                            for ns in d.Noteskins do
                                let nc = NoteskinGroupCard ns

                                ImageServices.get_cached_image.Request(
                                    ns.Versions.[0].Preview,
                                    function
                                    | Some img -> defer (fun () -> nc.LoadPreview img)
                                    | None -> Logging.Warn("Failed to load noteskin preview", ns.Versions.[0].Preview)
                                )

                                grid.Add nc

                            loading <- false
                        )
                    | None ->
                        defer (fun () ->
                            failed <- true
                            loading <- false
                        )
            )

            this
            |+ (SearchBox(
                    Setting.simple "",
                    (fun (f: Filter) -> grid.Filter <- NoteskinGroupCard.Filter f),
                    Position = Position.SliceTop 60.0f
                )
                |+ LoadingIndicator.Border(fun () -> loading))
            |+ EmptyState(Icons.X, "Couldn't connect to noteskins repository").Conditional(fun () -> failed)
            |+ Text(%"imports.noteskins.hint_a", Position = Position.TrimLeft(300.0f).SliceBottom(100.0f).SliceTop(50.0f), Align = Alignment.RIGHT)
            |+ Text(%"imports.noteskins.hint_b", Position = Position.TrimLeft(300.0f).SliceBottom(50.0f), Align = Alignment.RIGHT)
            |+ Button(Icons.DOWNLOAD + " " + %"osu_skin_import.list_skins", 
                (fun () -> FromOsu.ImportSkins.OsuSkinsListPage().Show()),
                Position = Position.SliceLeft(300.0f).SliceBottom(70.0f).Translate(0.0f, -15.0f)
            )
            |* scroll

            base.Init parent

        override this.Focusable = grid.Focusable

        member this.Items = grid

    let tab = NoteskinSearch()
