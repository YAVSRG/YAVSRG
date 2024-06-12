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

        let flow = FlowContainer.Vertical<EtternaPackCard>(80.0f, Spacing = 15.0f)

        let scroll =
            ScrollContainer(flow, Margin = Style.PADDING, Position = Position.TrimTop(70.0f).TrimBottom(65.0f))

        let mutable failed = false
        let mutable loading = true

        override this.Init(parent) =

            WebServices.download_json (
                "https://api.beta.etternaonline.com/api/packs?page=1&limit=36&sort=-popularity",
                fun data ->
                    match data with
                    | Some(d: EtternaOnlineApiResponse) ->
                        let cards =
                            d.data |> Seq.map (fun p -> EtternaPackCard(p))

                        defer (fun () ->
                            flow |* cards
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
                    (fun (f: Filter) -> flow.Filter <- EtternaPackCard.Filter f),
                    Position = Position.SliceTop 60.0f
                )
                |+ LoadingIndicator.Border(fun () -> loading))
            |+ EmptyState(Icons.X, "Couldn't connect to EtternaOnline").Conditional(fun () -> failed)
            |+ Text(%"imports.disclaimer.etterna", Position = Position.SliceBottom 55.0f)
            |* scroll

            base.Init parent

        member this.Items = flow

    let tab = EtternaPackSearch()
