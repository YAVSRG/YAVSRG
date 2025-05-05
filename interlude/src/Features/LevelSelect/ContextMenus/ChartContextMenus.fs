namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Collections
open Interlude.Features.Play
open Interlude.Features.Export

type ChartDeleteMenu(chart_meta: ChartMeta, context: LibraryContext, is_submenu: bool) =
    inherit Page()

    let delete_from_everywhere() =
        ChartDatabase.delete chart_meta Content.Charts
        LevelSelect.refresh_all ()

        if is_submenu then
            Menu.Back()

    override this.Content() =
        page_container()
        |+ seq {
            match context with
            | LibraryContext.Pack p when chart_meta.Packs.Count > 1 ->

                let delete_from_pack() =
                    ChartDatabase.delete_from_pack chart_meta p Content.Charts
                    LevelSelect.refresh_all ()

                    if is_submenu then
                        Menu.Back()

                yield PageButton.Once([p] %> "chart.delete.from_pack", fun () -> delete_from_pack(); Menu.Back()).Pos(3)
                yield PageButton.Once([chart_meta.Packs.Count.ToString()] %> "chart.delete.from_everywhere", fun () -> delete_from_everywhere(); Menu.Back()).Pos(5)
                yield PageButton.Once(%"confirm.no", Menu.Back).Pos(7)
            | _ ->
                yield PageButton.Once(
                    (if chart_meta.Packs.Count > 1 then [chart_meta.Packs.Count.ToString()] %> "chart.delete.from_everywhere" else %"confirm.yes"),
                    fun () -> delete_from_everywhere(); Menu.Back()).Pos(3)
                yield PageButton.Once(%"confirm.no", Menu.Back).Pos(5)
        }
        |+ Text([ sprintf "%s [%s]" chart_meta.Title chart_meta.DifficultyName ] %> "misc.confirmdelete")
            .Align(Alignment.LEFT)
            .Pos(0, 2, PageWidth.Full)
        :> Widget

    override this.Title = %"chart.delete"

#nowarn "40"

type ChartContextMenu(chart_meta: ChartMeta, context: LibraryContext) =
    inherit Page()

    let rec like_button =
        PageButton(
            %"chart.add_to_likes",
            fun () -> CollectionActions.toggle_liked chart_meta; like_button_swap.Current <- unlike_button)
            .Icon(Icons.HEART)
            .Hotkey("like")
    and unlike_button =
        PageButton(
            %"chart.remove_from_likes",
            fun () -> CollectionActions.toggle_liked chart_meta; like_button_swap.Current <- like_button)
            .Icon(Icons.FOLDER_MINUS)
            .Hotkey("like")
    and like_button_swap : SwapContainer = SwapContainer(if CollectionActions.is_liked chart_meta then unlike_button else like_button)

    override this.Content() =
        let content =
            FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)
                .Position(page_position(0, PAGE_BOTTOM, PageWidth.Normal).Translate(PAGE_MARGIN_X, PAGE_MARGIN_Y))
            |+ like_button_swap
            |+ PageButton(
                %"chart.add_to_collection",
                fun () -> AddToCollectionPage(chart_meta).Show()
            )
                .Icon(Icons.FOLDER_PLUS)

        match context with
        | LibraryContext.None
        | LibraryContext.Likes
        | LibraryContext.Pack _
        | LibraryContext.Table _ -> ()
        | LibraryContext.Folder name ->
            content
            |* PageButton(
                [ name ] %> "chart.remove_from_collection",
                fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, chart_meta, context) then
                        Menu.Back()
            )
                .Icon(Icons.FOLDER_MINUS)
        | LibraryContext.Playlist(index, name, _) ->
            content
            |+ PageButton(
                %"chart.move_up_in_playlist",
                fun () ->
                    if CollectionActions.reorder_up context then
                        Menu.Back()
            )
                .Disabled((index = 0))
                .Icon(Icons.ARROW_UP_CIRCLE)
                .Hotkey("move_up_in_playlist")
            |+ PageButton(
                %"chart.move_down_in_playlist",
                fun () ->
                    if CollectionActions.reorder_down context then
                        Menu.Back()
            )
                .Disabled((index + 1 = Content.Collections.GetPlaylist(name).Value.Charts.Count))
                .Icon(Icons.ARROW_DOWN_CIRCLE)
                .Hotkey("move_down_in_playlist")
            |+ PageButton(
                [ name ] %> "chart.remove_from_collection",
                fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, chart_meta, context) then
                        Menu.Back()
            )
                .Icon(Icons.FOLDER_MINUS)
            |+ PageButton.Once(
                %"playlist.play",
                fun () ->
                    LevelSelect.start_playlist (name, Content.Collections.GetPlaylist(name).Value)
            )
                .Disabled(Network.lobby.IsSome)
                .Icon(Icons.PLAY)
            |* PageButton.Once(
                %"playlist.play_shuffled",
                fun () ->
                    LevelSelect.start_playlist_shuffled (name, Content.Collections.GetPlaylist(name).Value)
            )
                .Disabled(Network.lobby.IsSome)
                .Icon(Icons.SHUFFLE)

        if Some chart_meta = SelectedChart.CACHE_DATA then
            content
            |+ PageButton(%"chart.change_offset", fun () ->
                match SelectedChart.CHART, SelectedChart.SAVE_DATA with
                | Some chart, Some save_data ->
                    LocalOffsetPage(LocalOffset.get_recent_suggestion chart save_data, LocalOffset.offset_setting save_data)
                        .Show()
                | _ -> ()
            )
                .Icon(Icons.SPEAKER)
            |+ PageButton.Once(%"chart.practice", fun () ->
                Menu.Exit()
                SelectedChart.when_loaded
                    true
                    (fun info ->
                        Screen.change_new
                            (fun () -> PracticeScreen.Create(info, 0.0f<ms>))
                            ScreenType.Practice
                            Transitions.Default
                        |> ignore
                    )
            )
                .Icon(Icons.TARGET)
                .Hotkey("practice_mode")
            |* PageButton.Once(%"chart.export_osz", fun () ->
                match SelectedChart.CHART, SelectedChart.WITH_MODS with
                | Some c, Some m ->
                    OsuExportOptionsPage(
                        %"chart.export_osz",
                        m.ModsApplied,
                        function
                        | true -> OsuExport.export_chart_with_mods m chart_meta
                        | false -> OsuExport.export_chart_without_mods c chart_meta
                    )
                        .Show()
                | _ -> ()
            )
                .Icon(Icons.UPLOAD)

        content
        |+ PageButton(%"chart.delete", fun () -> ChartDeleteMenu(chart_meta, context, true).Show())
            .TextColor(Colors.red_accent)
            .Icon(Icons.TRASH)
            .Hotkey("delete")
        :> Widget

    override this.Title = chart_meta.Title