namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Sorting
open Prelude.Data
open Interlude.Content
open Interlude.UI

type EtternaPackCard(data: EtternaOnlinePack) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            ),
            Fill = (fun () -> Palette.color (120, 0.5f, 0.0f)),
            Border =
                (fun () ->
                    if this.Focused then
                        Color.White
                    else
                        Palette.color (200, 0.7f, 0.2f)
                )
        )

    let mutable progress = 0.0f

    let mutable status =
        let path = Path.Combine(get_game_folder "Songs", data.name)

        if Directory.Exists path && not (Seq.isEmpty (Directory.EnumerateDirectories path)) then
            Installed
        else
            NotDownloaded

    let download () =
        if status = NotDownloaded || status = DownloadFailed then
            let target =
                Path.Combine(get_game_folder "Downloads", System.Guid.NewGuid().ToString() + ".zip")

            WebServices.download_file.Request(
                (data.download, target, (fun p -> progress <- p)),
                fun completed ->
                    if completed then
                        Imports.convert_stepmania_pack_zip.Request(
                            (target, data.id, Content.Library),
                            function
                            | Some result ->
                                Notifications.task_feedback (
                                    Icons.DOWNLOAD, 
                                    %"notification.install_pack",
                                    [data.name; result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.install_pack.body"
                                )
                                defer charts_updated_ev.Trigger
                                status <- Installed
                                File.Delete target
                            | None ->
                                Notifications.error (%"notification.install_pack_failed", data.name)
                                status <- DownloadFailed
                                File.Delete target
                        )
                    else
                        status <- DownloadFailed
            )

            status <- Downloading

    override this.Init(parent: Widget) =
        this
        |+ Text(
            data.name,
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = Position.min
                    Right = 1.0f %- 400.0f
                    Bottom = 1.0f %- 30.0f
                }
        )
        |+ Text(
            data.size,
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = Position.min
                    Right = 1.0f %- 165.0f
                    Bottom = 1.0f %- 30.0f
                }
        )
        |+ Text(
            (fun () ->
                if status = Installed then "Downloaded!"
                elif status = DownloadFailed then "Download failed!"
                else ""
            ),
            Align = Alignment.RIGHT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = 0.0f %+ 50.0f
                    Right = 1.0f %- 165.0f
                    Bottom = Position.max
                }
        )
        |+ Text(
            (sprintf "Average difficulty (MSD): %.2f" data.overall),
            Align = Alignment.LEFT,
            Position =
                {
                    Left = 0.0f %+ 5.0f
                    Top = 0.0f %+ 50.0f
                    Right = 1.0f %- 400.0f
                    Bottom = Position.max
                }
        )
        |+ Button(
            Icons.EXTERNAL_LINK
            , fun () -> open_url (sprintf "https://beta.etternaonline.com/packs/%i" data.id)
            , Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f)
        )
        |* Button(Icons.DOWNLOAD, download, Position = Position.SliceRight(80.0f).Margin(5.0f, 10.0f))

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        match status with
        | NotDownloaded -> ()
        | Downloading ->
            Draw.rect (this.Bounds.SliceLeft(this.Bounds.Width * progress)) (Color.FromArgb(64, 255, 255, 255))
        | Installed -> Draw.rect this.Bounds (Color.FromArgb(64, 255, 255, 255))
        | DownloadFailed -> ()

    member this.Data = data

    member private this.Download() = download ()

    static member Filter(filter: Filter) =
        fun (c: EtternaPackCard) ->
            List.forall
                (function
                | Impossible -> false
                | String str -> c.Data.name.ToLower().Contains(str)
                | _ -> true)
                filter

module EtternaPacks =

    type EtternaPackSearch() as this =
        inherit Container(NodeType.Container(fun _ -> Some this.Items))

        let items = FlowContainer.Vertical<EtternaPackCard>(80.0f, Spacing = 15.0f)

        let scroll =
            ScrollContainer(items, Margin = Style.PADDING, Position = Position.TrimTop(70.0f).TrimBottom(65.0f))

        let mutable filter: Filter = []
        let query_order = Setting.simple "popularity"
        let descending_order = Setting.simple true
        let mutable when_at_bottom: (unit -> unit) option = None
        let mutable loading = false
        let mutable failed = false

        let json_downloader =
            { new Async.SwitchService<string * (unit -> unit), EtternaOnlineApiResponse option * (unit -> unit)>() with
                override this.Process((url, action_at_bottom)) =
                    async {
                        match! WebServices.download_json_async<EtternaOnlineApiResponse>(url) with
                        | Some data ->
                            return Some data, action_at_bottom
                        | None -> return None, action_at_bottom
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

        let rec search (filter: Filter) (page: int) =
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
                sprintf "https://api.beta.etternaonline.com/api/packs?page=%i&limit=36&sort=%s%s%s"
                    (page + 1)
                    (if descending_order.Value then "-" else "")
                    (query_order.Value)
                    (if search_string = "" then "" else "&filter[search]=" + System.Net.WebUtility.UrlEncode search_string)

            json_downloader.Request(url, (fun () -> search filter (page + 1)))

        let begin_search (filter: Filter) =
            search filter 0
            items.Clear()

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
            |+ Text(%"imports.disclaimer.etterna", Position = Position.SliceBottom 55.0f)
            |+ scroll
            |+ EmptyState(Icons.X, "Couldn't connect to EtternaOnline").Conditional(fun () -> failed)
            |* EmptyState(Icons.SEARCH, %"imports.etterna.no_results", Position = Position.TrimTop(120.0f))
                .Conditional(fun () -> not loading && items.Count = 0)
            
            base.Init parent

        override this.Update(elapsed_ms, moved) =
            json_downloader.Join()
            base.Update(elapsed_ms, moved)

            if when_at_bottom.IsSome && scroll.PositionPercent > 0.9f then
                when_at_bottom.Value()
                when_at_bottom <- None

        member private this.Items = items

    let tab = EtternaPackSearch()
