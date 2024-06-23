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
        
type BeatmapBrowserPage() =
    inherit Page()

    let items = FlowContainer.Vertical<BeatmapImportCard>(80.0f, Spacing = 15.0f)
    let scroll_container = ScrollContainer(items, Margin = Style.PADDING)

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

    let search_results =
        NavigationContainer.Column(Position = Position.TrimTop(140.0f).CenterX(1400.0f).TrimBottom(70.0f))
        |+ Dummy(NodeType.Leaf)
        |+ scroll_container
        |>> Container
        |+ EmptyState(Icons.SEARCH, %"beatmap_browser.no_results", Position = Position.TrimTop(135.0f))
            .Conditional(fun () -> not loading && items.Count = 0)
        :> Widget

    let header =
        NavigationContainer.Row(Position = Position.SliceBottom(50.0f))
        |+ (let r =
                status_button
                    "Ranked"
                    { Position.Default with
                        Right = 0.18f %- 25.0f
                    }
                    Colors.cyan

            r.TiltLeft <- false
            r)
        |+ status_button
            "Qualified"
            { Position.Default with
                Left = 0.18f %+ 0.0f
                Right = 0.36f %- 25.0f
            }
            Colors.green
        |+ status_button
            "Loved"
            { Position.Default with
                Left = 0.36f %+ 0.0f
                Right = 0.54f %- 25.0f
            }
            Colors.pink
        |+ status_button
            "Unranked"
            { Position.Default with
                Left = 0.54f %+ 0.0f
                Right = 0.72f %- 25.0f
            }
            Colors.grey_2
        // todo: this should not use accent color and should be keyboard navigatable
        |+ SortingDropdown(
            [
                "plays", "Play count"
                "updated", "Date"
                "difficulty", "Difficulty"
                "favourites", "Favourites"
            ],
            "Sort",
            query_order |> Setting.trigger (fun _ -> begin_search filter; search_results.Focus false),
            descending_order |> Setting.trigger (fun _ -> begin_search filter; search_results.Focus false),
            "sort_mode",
            Position =
                { Position.Default with
                    Left = 0.72f %+ 0.0f
                }
        )
        |>> (fun nt -> Container(nt, Position = Position.Row(20.0f, 115.0f).CenterX(1400.0f)))
        |+ (SearchBox(
                Setting.simple "",
                (fun (f: Filter) ->
                    filter <- f
                    defer (fun () -> begin_search filter)
                ),
                Position = Position.SliceTop 60.0f,
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
        Text(%"beatmap_browser.disclaimer", Align = Alignment.CENTER, Position = Position.SliceBottom(55.0f).Translate(0.0f, -10.0f))

    override this.Update(elapsed_ms, moved) =
        json_downloader.Join()
        base.Update(elapsed_ms, moved)

        if when_at_bottom.IsSome && scroll_container.PositionPercent > 0.9f then
            when_at_bottom.Value()
            when_at_bottom <- None

    override this.Title = %"beatmap_browser"
    override this.OnClose() = ()