namespace Interlude.Features.Import.osu

open System
open System.Text.RegularExpressions
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Sorting
open Prelude.Data
open Interlude.UI
open Interlude.UI.Menu
        
 type BeatmapsBrowser() as this =
    inherit Container(NodeType.Container(fun _ -> Some this.Items))

    let items = FlowContainer.Vertical<BeatmapImportCard>(80.0f, Spacing = 15.0f)

    let scroll =
        ScrollContainer(items, Margin = Style.PADDING, Position = Position.TrimTop(120.0f).TrimBottom(65.0f))

    let mutable filter: Filter = []
    let query_order = Setting.simple "updated"
    let descending_order = Setting.simple true
    let mutable statuses = Set.singleton "Ranked"
    let mutable when_at_bottom: (unit -> unit) option = None
    let mutable loading = false

    let json_downloader =
        { new Async.SwitchService<string * (unit -> unit), NeriNyanBeatmapSearch option * (unit -> unit)>() with
            override this.Process((url, action_at_bottom)) =
                async {
                    match! WebServices.download_string.RequestAsync(url) with
                    | Some bad_json ->
                        let fixed_json = Regex.Replace(bad_json, @"[^\u0000-\u007F]+", "")

                        match JSON.FromString(fixed_json) with 
                        | Ok data ->
                            return Some data, action_at_bottom
                        | Error err ->
                            Logging.Error("Failed to parse json data from " + url, err)
                            return None, action_at_bottom
                    | None -> return None, action_at_bottom
                }

            override this.Handle((data: NeriNyanBeatmapSearch option, action_at_bottom)) =
                match data with
                | Some d ->
                    for p in d do
                        items.Add(BeatmapImportCard p)

                    if d.Length >= 50 then
                        when_at_bottom <- Some action_at_bottom

                    loading <- false
                | None -> ()
        }

    let rec search (filter: Filter) (page: int) =
        loading <- true
        when_at_bottom <- None

        let mutable request =
            {
                m = "mania"
                page = page
                query = ""
                ranked = (String.concat "," statuses).ToLower()
                sort = query_order.Value + if descending_order.Value then "_desc" else "_asc"
                cs = {| min = 3.0; max = 10.0 |}
            }

        let mutable invalid = false

        List.iter
            (function
            | Impossible -> invalid <- true
            | String s ->
                request <-
                    { request with
                        query =
                            match request.query with
                            | "" -> s
                            | t -> request.query + " " + s
                    }
            | Equals("k", n)
            | Equals("key", n)
            | Equals("keys", n) ->
                match Int32.TryParse n with
                | (true, i) ->
                    request <-
                        { request with
                            cs = {| min = float i; max = float i |}
                        }
                | _ -> ()
            | _ -> ())
            filter

        let url =
            "https://api.nerinyan.moe/search?b64="
            + (request
                |> JSON.ToString
                |> fun s ->
                    s.Replace("\n", "")
                    |> System.Text.Encoding.UTF8.GetBytes
                    |> Convert.ToBase64String
                    |> Uri.EscapeDataString)
            + "&ps=50"

        json_downloader.Request(url, (fun () -> search filter (page + 1)))

    let begin_search (filter: Filter) =
        search filter 0
        items.Clear()

    let status_button (status: string) (position: Position) (color: Color) =
        StylishButton(
            (fun () ->
                if statuses.Contains status then
                    statuses <- Set.remove status statuses
                else
                    statuses <- Set.add status statuses

                begin_search filter
            ),
            (fun () ->
                if statuses.Contains status then
                    Icons.CHECK + " " + status
                else
                    Icons.X + " " + status
            ),
            (fun () -> if statuses.Contains status then color.O3 else color.O1),
            Position = position
        )

    override this.Focusable = items.Focusable

    override this.Init(parent) =
        begin_search filter

        this
        |+ (SearchBox(
                Setting.simple "",
                (fun (f: Filter) ->
                    filter <- f
                    defer (fun () -> begin_search filter)
                ),
                Position = Position.SliceTop 60.0f
            )
            |+ LoadingIndicator.Border(fun () -> loading))
        |+ Text(%"imports.osu.disclaimer", Position = Position.SliceBottom 55.0f)
        |+ scroll
        |+ (let r =
                status_button
                    "Ranked"
                    { Position.TrimTop(65.0f).SliceTop(50.0f) with
                        Right = 0.18f %- 25.0f
                    }
                    Colors.cyan

            r.TiltLeft <- false
            r)
        |+ status_button
            "Qualified"
            { Position.TrimTop(65.0f).SliceTop(50.0f) with
                Left = 0.18f %+ 0.0f
                Right = 0.36f %- 25.0f
            }
            Colors.green
        |+ status_button
            "Loved"
            { Position.TrimTop(65.0f).SliceTop(50.0f) with
                Left = 0.36f %+ 0.0f
                Right = 0.54f %- 25.0f
            }
            Colors.pink
        |+ status_button
            "Unranked"
            { Position.TrimTop(65.0f).SliceTop(50.0f) with
                Left = 0.54f %+ 0.0f
                Right = 0.72f %- 25.0f
            }
            Colors.grey_2
        |+ EmptyState(Icons.SEARCH, %"imports.osu.no_results", Position = Position.TrimTop(120.0f))
            .Conditional(fun () -> not loading && items.Count = 0)
        |* SortingDropdown(
            [
                "plays", "Play count"
                "updated", "Date"
                "difficulty", "Difficulty"
                "favourites", "Favourites"
            ],
            "Sort",
            query_order |> Setting.trigger (fun _ -> begin_search filter),
            descending_order |> Setting.trigger (fun _ -> begin_search filter),
            "sort_mode",
            Position =
                {
                    Left = 0.72f %+ 0.0f
                    Top = 0.0f %+ 65.0f
                    Right = 1.0f %- 0.0f
                    Bottom = 0.0f %+ 115.0f
                }
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        json_downloader.Join()
        base.Update(elapsed_ms, moved)

        if when_at_bottom.IsSome && scroll.PositionPercent > 0.9f then
            when_at_bottom.Value()
            when_at_bottom <- None

    member private this.Items = items

type BeatmapsBrowserPage() =
    inherit Page()

    override this.Content() =
        NavigationContainer.Column(Position = Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
        |+ Dummy(NodeType.Leaf)
        |+ BeatmapsBrowser()
        :> Widget

    override this.Title = %"imports.osu"
    override this.OnClose() = ()