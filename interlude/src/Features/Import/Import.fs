namespace Interlude.Features.Import

open System.IO
open System.Net
open System.Net.Security
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Web
open Interlude.UI
open Interlude.Options
open Interlude.UI.Components
open Interlude.Features.LevelSelect

module FileDropHandling =
    let tryImport(path: string) : bool =
        match Mounts.dropFunc with
        | Some f -> f path; true
        | None ->
            BackgroundTask.Create TaskFlags.NONE ("Import " + Path.GetFileName path)
                (Library.Imports.autoConvert path |> BackgroundTask.Callback(fun b -> LevelSelect.refresh <- LevelSelect.refresh || b))
            |> ignore
            true

type private SearchContainer(populate, handleFilter) as this =
    inherit StaticContainer(NodeType.None)
    let flow = FlowContainer.Vertical<Widget>(80.0f, Spacing = 15.0f)
    let scroll = ScrollContainer.Flow(flow, Margin = Style.padding, Position = Position.TrimTop 70.0f)
    let populate = populate flow
    let handleFilter = handleFilter flow
    do
        flow |* SearchContainerLoader populate
        this
        |+ (SearchBox(Setting.simple "", (fun (f: Filter) -> handleFilter f), Position = Position.SliceTop 60.0f ))
        |* scroll

type ImportScreen() as this =
    inherit Screen()
    do
        (*
            Online downloaders
        *)

        // EtternaOnline's certificate keeps expiring!! Rop get on it
        // todo: set up automated test that pings eo for certificate expiry
        ServicePointManager.ServerCertificateValidationCallback <-
            RemoteCertificateValidationCallback(
                fun _ cert _ sslPolicyErrors ->
                    if sslPolicyErrors = SslPolicyErrors.None then true
                    else cert.GetCertHashString().ToLower() = "e87a496fbc4b7914674f3bc3846368234e50fb74" )

        let eoDownloads = 
            SearchContainer(
                (fun flowContainer _ output -> downloadJson("https://api.etternaonline.com/v2/packs/", (fun (d: {| data: ResizeArray<EOPack> |}) -> sync(fun () -> for p in d.data do flowContainer.Add(new SMImportCard(p.attributes))) ))),
                (fun flowContainer filter -> flowContainer.Filter <- SMImportCard.Filter filter) )
        let osuDownloads =
            SearchContainer(
                (Beatmap.search [] 0),
                (fun flowContainer filter -> flowContainer.Clear(); flowContainer.Add(new SearchContainerLoader(Beatmap.search filter 0 flowContainer))) )
        let noteskins = 
            SearchContainer(
                (fun flowContainer _ output -> 
                    downloadJson(Noteskins.source, 
                        (fun (d: Prelude.Data.Themes.Noteskin.Repo) -> 
                            sync( fun () -> 
                                for ns in d.Noteskins do
                                    let nc = NoteskinCard ns
                                    Noteskins.image_loader.Request(ns.Preview, nc)
                                    flowContainer.Add nc
                            )
                        )
                    )
                ),
                (fun flowContainer filter -> flowContainer.Filter <- NoteskinCard.Filter filter) )

        let tabs = 
            Tabs.Container(Position = { Left = 0.0f %+ 600.0f; Top = 0.0f %+ 50.0f; Right = 1.0f %- 100.0f; Bottom = 1.0f %- 80.0f })
                .WithTab("Etterna Packs", eoDownloads)
                .WithTab("osu! Songs", osuDownloads)
                .WithTab("Noteskins", noteskins)

        this
        |+ tabs
        |+ Text("(Interlude is not affiliated with osu! or Etterna, these downloads are provided through unofficial APIs)",
            Align = Alignment.CENTER,
            Position = { Left = 0.0f %+ 600.0f; Top = 1.0f %- 90.0f; Right = 1.0f %- 100.0f; Bottom = 1.0f %- 30.0f })

        (*
            Offline importers from other games
        *)

        |+ MountControl(Mounts.Game.Osu, options.OsuMount,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 200.0f, 360.0f, 60.0f) )
        |+ MountControl(Mounts.Game.Stepmania, options.StepmaniaMount,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 270.0f, 360.0f, 60.0f) )
        |+ MountControl(Mounts.Game.Etterna, options.EtternaMount,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 340.0f, 360.0f, 60.0f) )
        |* Text("Import from game",
            Align = Alignment.CENTER,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 150.0f, 250.0f, 50.0f))

    override this.OnEnter _ = ()
    override this.OnExit _ = ()