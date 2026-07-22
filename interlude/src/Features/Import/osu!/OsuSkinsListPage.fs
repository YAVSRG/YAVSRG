namespace Interlude.Features.Import.Osu

open System.IO
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Interlude.UI
open Interlude.Options

type OsuSkinsListPage(on_select: string -> unit) =
    inherit Page()

    let osu_song_folder =
        match options.OsuMount.Value with
        | Some mount -> mount.SourceFolder
        | None -> OSU_SONG_FOLDER
    let osu_skin_folder =
        Path.Combine(Path.GetDirectoryName(osu_song_folder), "Skins")

    let osu_skin_paths =
        if Directory.Exists(osu_skin_folder) then
            Directory.EnumerateDirectories(osu_skin_folder)
            |> List.ofSeq
        else []

    override this.Content() =
        if List.isEmpty osu_skin_paths then
            Container(NodeType.Leaf)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))
            |+ EmptyState(Icons.IMAGE, %"osu_skin_import.no_skins_found")
            :> Widget
        else
            ScrollContainer(
                FlowContainer.Vertical<_>(PAGE_ITEM_HEIGHT)
                |+ seq {
                    for path in osu_skin_paths do
                        yield PageButton(
                            Path.GetFileName(path),
                            fun () -> on_select(path)
                        )
                }
            )
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y)) :> Widget

    override this.Title = %"skins.import_from_osu"