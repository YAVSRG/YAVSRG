namespace Interlude.Features.Import.Osu

open System.IO
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Imports
open Interlude.UI
open Interlude.Options

type OsuSkinsListPage(on_select: string -> unit) =
    inherit Page()

    let osu_song_folder =
        match options.OsuMount.Value with
        | Some mount -> mount.SourceFolder
        | None -> MountedChartSource.DEFAULT_OSU_SONGS_LOCATION

    let osu_skin_folder = Path.Combine(Path.GetDirectoryName(osu_song_folder), "Skins")

    let osu_skin_paths =
        if Directory.Exists(osu_skin_folder) then Directory.EnumerateDirectories(osu_skin_folder) |> List.ofSeq else []

    override this.Content() : Widget =
        if List.isEmpty osu_skin_paths then
            Container(NodeType.Leaf)
                .With(EmptyState(Icons.IMAGE, %"osu_skin_import.no_skins_found"))
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))
            :> Widget
        else
            ScrollContainer(
                FlowContainer
                    .Vertical<_>(PAGE_ITEM_HEIGHT)
                    .With(
                        seq {
                            for path in osu_skin_paths do
                                yield PageButton(Path.GetFileName(path), fun () -> on_select(path))
                        }
                    )
            )
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))
            :> Widget

    override this.Title = %"skins.import_from_osu"
