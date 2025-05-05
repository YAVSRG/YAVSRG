namespace Interlude.Features.Import.osu

open System
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data
open Interlude.UI

type BeatmapStatusToggle =
    static member Create(label: string, status: int, color: Color, statuses: Setting<Set<int>>) =
        AngledButton(
            (fun () ->
                if statuses.Value.Contains status then Icons.CHECK + " " + label
                else Icons.X + " " + label
            ),
            (fun () ->
                if statuses.Value.Contains status then
                    Setting.app (Set.remove status) statuses
                else
                    Setting.app (Set.add status) statuses
            ),
            (fun () -> if statuses.Value.Contains status then color.O3 else color.O1)
        )

type BeatmapBrowserPage() =
    inherit Page()

    let WIDTH = 1400.0f
    let MARGIN_TOP = 20.0f

    let items = FlowContainer.Vertical<BeatmapImportCard>(80.0f, Spacing = 15.0f)
    let scroll_container = ScrollContainer(items, Margin = Style.PADDING)

    let search_text = Setting.simple ""
    let query_order = Setting.simple "submitted_date"
    let descending_order = Setting.simple true
    let statuses = Setting.simple (Set.singleton 1)
    let mutable when_at_bottom: (unit -> unit) option = None
    let mutable loading = false

    let json_downloader =
        { new Async.CancelQueue<string * (unit -> unit), MinoBeatmapSearch option * (unit -> unit)>() with
            override this.Process((url, action_at_bottom)) =
                async {
                    match! WebServices.download_json_async(url) with
                    | WebResult.Ok (data: MinoBeatmapSearch) -> return Some data, action_at_bottom
                    | WebResult.HttpError 429 ->
                        Logging.Error("Mino rate limit hit")
                        Threading.Thread.Sleep(60000)
                        return None, action_at_bottom
                    | WebResult.HttpError code ->
                        Logging.Error "Mino API returned code %i" code
                        return None, action_at_bottom
                    | WebResult.Exception err ->
                        Logging.Error "Error getting Mino data from '%s': %O" url err
                        return None, action_at_bottom
                }

            override this.Handle((data: MinoBeatmapSearch option, action_at_bottom)) =
                match data with
                | Some d ->
                    for p in d do
                        items.Add(BeatmapImportCard p)

                    if d.Length >= 50 then
                        when_at_bottom <- Some action_at_bottom

                    loading <- false
                | None -> ()
        }

    let rec search (query: string) (page: int) : unit =
        loading <- true
        when_at_bottom <- None

        let status_string =
            (if statuses.Value.Contains 0 then "status=-2&status=-1&" else "")
            + (statuses.Value |> Seq.map (sprintf "status=%i") |> String.concat "&")

        let url =
            sprintf "https://catboy.best/api/v2/search?query=%s&mode=3&sort=%s:%s&limit=50&offset=%i&%s"
                (Uri.EscapeDataString query)
                query_order.Value
                (if descending_order.Value then "desc" else "asc")
                (page * 50)
                status_string

        json_downloader.Request(url, (fun () -> search query (page + 1)))

    let begin_search (query: string) : unit =
        search query 0
        items.Clear()

    let statuses = statuses |> Setting.trigger (fun _ -> begin_search search_text.Value)

    let search_results =
        NavigationContainer.Column()
            .Position(Position.ShrinkT(MARGIN_TOP + AngledButton.HEIGHT + SearchBox.HEIGHT + Style.PADDING * 2.0f).SliceX(WIDTH).ShrinkB(70.0f))
            .With(
                Dummy(NodeType.Leaf),
                scroll_container,
                EmptyState(Icons.SEARCH, %"beatmap_browser.no_results")
                    .Conditional(fun () -> not loading && items.Count = 0)
            )

    let sort_filter_buttons =
        NavigationContainer.Row()
            .With(
                BeatmapStatusToggle.Create("Ranked", 1, Colors.cyan, statuses)
                    .LeanLeft(false)
                    .Position(Position.ShrinkPercentR(0.28f).GridX(1, 4, AngledButton.LEAN_AMOUNT)),
                BeatmapStatusToggle.Create("Qualified", 3, Colors.green, statuses)
                    .Position(Position.ShrinkPercentR(0.28f).GridX(2, 4, AngledButton.LEAN_AMOUNT)),
                BeatmapStatusToggle.Create("Loved", 4, Colors.pink, statuses)
                    .Position(Position.ShrinkPercentR(0.28f).GridX(3, 4, AngledButton.LEAN_AMOUNT)),
                BeatmapStatusToggle.Create("Unranked", 0, Colors.grey_2, statuses)
                    .Position(Position.ShrinkPercentR(0.28f).GridX(4, 4, AngledButton.LEAN_AMOUNT)),

                SortingDropdown.Create(
                    [
                        "play_count", "Play count"
                        "submitted_date", "Date"
                        "beatmaps.difficulty_rating", "Difficulty"
                        "favourite_count", "Favourites"
                    ],
                    "Sort",
                    query_order |> Setting.trigger (fun _ -> begin_search search_text.Value; search_results.Focus false),
                    descending_order |> Setting.trigger (fun _ -> begin_search search_text.Value; search_results.Focus false),
                    "sort_mode"
                )
                    .Position(Position.SlicePercentR(0.28f).ShrinkL(AngledButton.LEAN_AMOUNT))
            )

    let header =
        NavigationContainer.Column()
            .WrapNavigation(false)
            .Position(Position.SliceT(MARGIN_TOP, AngledButton.HEIGHT + SearchBox.HEIGHT + Style.PADDING).SliceX(WIDTH))
            .With(
                SearchBox(search_text, fun (query: string) ->
                    GameThread.defer (fun () -> begin_search query)
                )
                    .Fill(Colors.cyan.O3)
                    .Border(Colors.cyan_accent)
                    .TextColor(Colors.text_cyan)
                    .Position(Position.SliceT(SearchBox.HEIGHT))
                    .With(LoadingIndicator.Border(fun () -> loading)),

                sort_filter_buttons
                    .Position(Position.SliceB(AngledButton.HEIGHT))
            )

    // page parts are rotated around to give correct draw order for the dropdowns in the header
    override this.Content() = header

    override this.Footer() =
        begin_search search_text.Value
        search_results

    override this.Header() =
        Text(%"beatmap_browser.disclaimer")
            .Align(Alignment.CENTER)
            .Position(Position.SliceB(55.0f).Translate(0.0f, -10.0f))

    override this.Update(elapsed_ms, moved) =
        json_downloader.Join()
        base.Update(elapsed_ms, moved)

        if when_at_bottom.IsSome && scroll_container.PositionPercent > 0.9f then
            when_at_bottom.Value()
            when_at_bottom <- None

    override this.Title = %"beatmap_browser"