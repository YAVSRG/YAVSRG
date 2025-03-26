namespace Interlude.Features.Import.Etterna

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data
open Interlude.UI

type EtternaPacksBrowserPage() =
    inherit Page()

    let items = FlowContainer.Vertical<EtternaPackCard>(80.0f, Spacing = 15.0f)
    let scroll_container = ScrollContainer(items, Margin = Style.PADDING)

    let mutable filter: FilterPart list = []
    let query_order = Setting.simple "popularity"
    let descending_order = Setting.simple true
    let mutable when_at_bottom: (unit -> unit) option = None
    let mutable loading = false
    let mutable failed = false

    let json_downloader =
        { new Async.CancelQueue<string * (unit -> unit), EtternaOnlineApiResponse option * (unit -> unit)>() with
            override this.Process((url, action_at_bottom)) =
                async {
                    match! WebServices.download_json_async<EtternaOnlineApiResponse>(url) with
                    | WebResult.Ok data ->
                        return Some data, action_at_bottom
                    | _ -> return None, action_at_bottom
                }

            override this.Handle((data: EtternaOnlineApiResponse option, action_at_bottom)) =
                match data with
                | Some d ->
                    for p in d.data do
                        items.Add(EtternaPackCard p)

                    if d.meta.current_page < d.meta.last_page then
                        when_at_bottom <- Some action_at_bottom

                    loading <- false
                | None ->
                    failed <- true
                    loading <- false
        }

    let rec search (filter: FilterPart list) (page: int) : unit =
        loading <- true
        when_at_bottom <- None
        let mutable search_string = ""

        let mutable invalid = false

        List.iter
            (function
            | Impossible -> invalid <- true
            | String s ->
                search_string <-
                    match search_string with
                    | "" -> s
                    | t -> search_string + " " + s
            | _ -> ())
            filter

        let url =
            sprintf "https://api.etternaonline.com/api/packs?page=%i&limit=36&sort=%s%s%s"
                (page + 1)
                (if descending_order.Value then "-" else "")
                (query_order.Value)
                (if search_string = "" then "" else "&filter[search]=" + System.Net.WebUtility.UrlEncode search_string)

        json_downloader.Request(url, (fun () -> search filter (page + 1)))

    let begin_search (filter: FilterPart list) : unit =
        search filter 0
        items.Clear()

    override this.Focusable = items.Focusable

    override this.Content() =
        begin_search filter

        NavigationContainer.Column().Position(Position.SliceX(1400.0f).ShrinkT(90.0f).ShrinkB(70.0f))
        |+ Dummy(NodeType.Leaf)
        |+ scroll_container
        |+ EmptyState(Icons.X, %"etterna_pack_browser.error").Position(Position.ShrinkT(120.0f))
            .Conditional(fun () -> failed)
        |+ EmptyState(Icons.SEARCH, %"etterna_pack_browser.no_results").Position(Position.ShrinkT(120.0f))
            .Conditional(fun () -> not failed && not loading && items.Count = 0)
        :> Widget

    override this.Header() =
        SearchBox(
            Setting.simple "",
            (fun (f: FilterPart list) ->
                filter <- f
                GameThread.defer (fun () -> begin_search filter)
            )).Position(Position.SliceX(1400.0f).SliceT(60.0f).Translate(0.0f, 20.0f))
        |+ LoadingIndicator.Border(fun () -> loading)
        :> Widget

    override this.Footer() =
        Text(%"etterna_pack_browser.disclaimer")
            .Align(Alignment.CENTER)
            .Position(Position.SliceB(55.0f).Translate(0.0f, -10.0f))

    override this.Update(elapsed_ms, moved) =
        json_downloader.Join()
        base.Update(elapsed_ms, moved)

        if when_at_bottom.IsSome && scroll_container.PositionPercent > 0.9f then
            when_at_bottom.Value()
            when_at_bottom <- None

    override this.Title = %"etterna_pack_browser"
    override this.OnClose() = ()