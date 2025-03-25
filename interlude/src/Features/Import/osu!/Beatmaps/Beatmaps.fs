namespace Interlude.Features.Import.osu

open System
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data
open Interlude.UI

type BeatmapBrowserPage() =
    inherit Page()

    let items = FlowContainer.Vertical<BeatmapImportCard>(80.0f, Spacing = 15.0f)
    let scroll_container = ScrollContainer(items, Margin = Style.PADDING)

    let mutable filter: FilterPart list = []
    let query_order = Setting.simple "submitted_date"
    let descending_order = Setting.simple true
    let mutable statuses = Set.singleton 1
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

    let rec search (filter: FilterPart list) (page: int) : unit =
        loading <- true
        when_at_bottom <- None

        let mutable query = ""

        let mutable invalid = false

        List.iter
            (function
            | Impossible -> invalid <- true
            | String s ->
                query <- if query = "" then s else query + " " + s
            | _ -> ())
            filter

        let status_string =
            (if statuses.Contains 0 then "status=-2&status=-1&" else "")
            + (statuses |> Seq.map (sprintf "status=%i") |> String.concat "&")

        let url =
            sprintf "https://catboy.best/api/v2/search?query=%s&mode=3&sort=%s:%s&limit=50&offset=%i&%s"
                (Uri.EscapeDataString query)
                query_order.Value
                (if descending_order.Value then "desc" else "asc")
                (page * 50)
                status_string

        json_downloader.Request(url, (fun () -> search filter (page + 1)))

    let begin_search (filter: FilterPart list) : unit =
        search filter 0
        items.Clear()

    let status_button (label: string, status: int, color: Color) : LeaningButton =
        LeaningButton(
            (fun () ->
                if statuses.Contains status then
                    Icons.CHECK + " " + label
                else
                    Icons.X + " " + label
            ),
            (fun () ->
                if statuses.Contains status then
                    statuses <- Set.remove status statuses
                else
                    statuses <- Set.add status statuses

                begin_search filter
            ),
            (fun () -> if statuses.Contains status then color.O3 else color.O1)
        )

    let search_results =
        NavigationContainer.Column(Position = Position.ShrinkT(140.0f).SliceX(1400.0f).ShrinkB(70.0f))
        |+ Dummy(NodeType.Leaf)
        |+ scroll_container
        |>> Container
        |+ EmptyState(Icons.SEARCH, %"beatmap_browser.no_results", Position = Position.ShrinkT(135.0f))
            .Conditional(fun () -> not loading && items.Count = 0)
        :> Widget

    let header =
        NavigationContainer.Row(Position = Position.SliceB(LeaningButton.HEIGHT))
        |+ status_button("Ranked", 1, Colors.cyan)
            .LeanLeft(false)
            .Position(Position.ShrinkPercentR(0.28f).GridX(1, 4, LeaningButton.LEAN_AMOUNT))
        |+ status_button("Qualified", 3, Colors.green)
            .Position(Position.ShrinkPercentR(0.28f).GridX(2, 4, LeaningButton.LEAN_AMOUNT))
        |+ status_button("Loved", 4, Colors.pink)
            .Position(Position.ShrinkPercentR(0.28f).GridX(3, 4, LeaningButton.LEAN_AMOUNT))
        |+ status_button("Unranked", 0, Colors.grey_2)
            .Position(Position.ShrinkPercentR(0.28f).GridX(4, 4, LeaningButton.LEAN_AMOUNT))
        // todo: this should not use accent color and should be keyboard navigatable
        |+ SortingDropdown(
            [
                "play_count", "Play count"
                "submitted_date", "Date"
                "beatmaps.difficulty_rating", "Difficulty"
                "favourite_count", "Favourites"
            ],
            "Sort",
            query_order |> Setting.trigger (fun _ -> begin_search filter; search_results.Focus false),
            descending_order |> Setting.trigger (fun _ -> begin_search filter; search_results.Focus false),
            "sort_mode",
            Position =
                Position
                    .SlicePercentR(0.28f)
                    .ShrinkL(LeaningButton.LEAN_AMOUNT)
        )
        |>> (fun nt -> Container(nt, Position = Position.SliceT(20.0f, 115.0f).SliceX(1400.0f)))
        |+ (SearchBox(
                Setting.simple "",
                (fun (f: FilterPart list) ->
                    filter <- f
                    GameThread.defer (fun () -> begin_search filter)
                ),
                Position = Position.SliceT 60.0f,
                Fill = K Colors.cyan.O3,
                Border = K Colors.cyan_accent,
                TextColor = K Colors.text_cyan
            )
            |+ LoadingIndicator.Border(fun () -> loading)
        )

    // page parts are rotated around to give correct draw order for the dropdowns in the header
    override this.Content() = header

    override this.Footer() =
        begin_search filter
        search_results

    override this.Header() =
        Text(%"beatmap_browser.disclaimer", Align = Alignment.CENTER, Position = Position.SliceB(55.0f).Translate(0.0f, -10.0f))

    override this.Update(elapsed_ms, moved) =
        json_downloader.Join()
        base.Update(elapsed_ms, moved)

        if when_at_bottom.IsSome && scroll_container.PositionPercent > 0.9f then
            when_at_bottom.Value()
            when_at_bottom <- None

    override this.Title = %"beatmap_browser"
    override this.OnClose() = ()