namespace Interlude.UI.Screens.Import

open System.IO
open System.Net
open System.Net.Security
open Percyqaz.Common
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Prelude.Web
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Screens.LevelSelect

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
    inherit Widget1()
    let flowContainer = new FlowContainer(Spacing = 15.0f)
    let populate = populate flowContainer
    let handleFilter = handleFilter flowContainer
    do
        this.Add(SearchBox(Setting.simple "", fun (f: Filter) -> handleFilter f).Position( Position.SliceTop 60.0f ))
        this.Add(flowContainer.Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 70.0f; Right = 1.0f %- 0.0f; Bottom = 1.0f %+ 0.0f })
        flowContainer.Add(new SearchContainerLoader(populate))

type Screen() as this =
    inherit Screen.T()
    do
        (*
            Online downloaders
        *)

        // EtternaOnline's certificate keeps expiring!! Rop get on it
        // This hack force-trusts EO's SSL certificate even though it has expired (this was for 18th march 2021, there's a new working certificate currently)
        ServicePointManager.ServerCertificateValidationCallback <-
            RemoteCertificateValidationCallback(
                fun _ cert _ sslPolicyErrors ->
                    if sslPolicyErrors = SslPolicyErrors.None then true
                    else cert.GetCertHashString().ToLower() = "e87a496fbc4b7914674f3bc3846368234e50fb74" )

        let eoDownloads = 
            SearchContainer(
                (fun flowContainer _ output -> downloadJson("https://api.etternaonline.com/v2/packs/", (fun (d: {| data: ResizeArray<EOPack> |}) -> flowContainer.Synchronized(fun () -> for p in d.data do flowContainer.Add(new SMImportCard(p.attributes))) ))),
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
                            flowContainer.Synchronized( fun () -> 
                                for ns in d.Noteskins do
                                    let nc = NoteskinCard ns
                                    Noteskins.image_loader.Request(ns.Preview, nc)
                                    flowContainer.Add nc
                            )
                        )
                    )
                ),
                (fun flowContainer filter -> flowContainer.Filter <- NoteskinCard.Filter filter) )
        let tabs = new TabContainer("Etterna Packs", eoDownloads)
        tabs.AddTab("osu! Songs", osuDownloads)
        tabs.AddTab("Noteskins", noteskins)

        this
        |-+ tabs.Position { Left = 0.0f %+ 600.0f; Top = 0.0f %+ 50.0f; Right = 1.0f %- 100.0f; Bottom = 1.0f %- 80.0f }
        |=+ TextBox(K "(Interlude is not affiliated with osu! or Etterna, these downloads are provided through unofficial APIs)", K (Color.White, Color.Black), 0.5f)
            .Position { Left = 0.0f %+ 600.0f; Top = 1.0f %- 90.0f; Right = 1.0f %- 100.0f; Bottom = 1.0f %- 30.0f }

        (*
            Offline importers from other games
        *)

        this
        |-+ MountControl(Mounts.Types.Osu, Options.options.OsuMount)
            .Position( Position.Box(0.0f, 0.0f, 0.0f, 200.0f, 360.0f, 60.0f) )
        |-+ MountControl(Mounts.Types.Stepmania, Options.options.StepmaniaMount)
            .Position( Position.Box(0.0f, 0.0f, 0.0f, 270.0f, 360.0f, 60.0f) )
        |-+ MountControl(Mounts.Types.Etterna, Options.options.EtternaMount)
            .Position( Position.Box(0.0f, 0.0f, 0.0f, 340.0f, 360.0f, 60.0f) )
        |=+ TextBox(K "Import from game", K (Color.White, Color.Black), 0.5f )
            .Position( Position.Box(0.0f, 0.0f, 0.0f, 150.0f, 250.0f, 50.0f) )

    override this.OnEnter _ = ()
    override this.OnExit _ = ()