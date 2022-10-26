namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data
open Prelude.Data.Charts
open Interlude.UI
open Interlude.Utils
open Interlude.Features.LevelSelect

module FileDropHandling =
    let tryImport(path: string) : bool =
        match Mounts.dropFunc with
        | Some f -> f path; true
        | None -> 
            Library.Imports.auto_convert.Request(path, 
                fun success -> 
                    if success then
                        Notifications.add(L"notification.import.success", NotificationType.Task)
                        LevelSelect.refresh <- true
                    else Notifications.add(L"notification.import.failure", NotificationType.Warning)
            )
            true

type private TabButton(icon: string, name: string, container: SwapContainer, target: Widget) as this =
    inherit StaticContainer(NodeType.Switch(fun _ -> this.Button))

    let button = Button(icon + " " + name, (fun () -> container.Current <- target), Position = Position.Margin(10.0f))

    member this.Button = button

    override this.Init(parent) =
        base.Init parent
        this
        |+ Frame(NodeType.None,
            Border = fun () -> if container.Current = target then Color.White else Color.Transparent
            ,
            Fill = fun () -> if this.Focused then Style.main 100 () else Style.dark 100 ())
        |* button

type private ServiceStatus<'Request, 'Result>(name: string, service: Async.Service<'Request, 'Result>) =
    inherit StaticWidget(NodeType.None)

    let fade = Animation.Fade 0.0f
    let animation = Animation.Counter(250.0)

    let animation_frames = 
        [|
            Percyqaz.Flux.Resources.Feather.cloud_snow
            Percyqaz.Flux.Resources.Feather.cloud_drizzle
            Percyqaz.Flux.Resources.Feather.cloud_rain
            Percyqaz.Flux.Resources.Feather.cloud_drizzle
        |]

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if service.Status <> Async.ServiceStatus.Idle then
            fade.Target <- 1.0f
            animation.Update elapsedTime
        else fade.Target <- 0.0f
        fade.Update elapsedTime

    override this.Draw() =
        let icon = animation_frames.[animation.Loops % animation_frames.Length]
        let iconColor =
            match service.Status with
            | Async.ServiceStatus.Busy -> Color.FromArgb(fade.Alpha, Color.Yellow)
            | Async.ServiceStatus.Working -> Color.FromArgb(fade.Alpha, Color.Lime)
            | _ -> Color.FromArgb(fade.Alpha, Color.Gray)
        let textColor =
            let x = 127 + fade.Alpha / 2
            Color.FromArgb(x, x, x)
        Text.drawFill(Style.baseFont, icon, this.Bounds.SliceLeft this.Bounds.Height, iconColor, Alignment.CENTER)
        Text.drawFill(Style.baseFont, name, this.Bounds.TrimLeft this.Bounds.Height, textColor, Alignment.LEFT)

type ImportScreen() as this =
    inherit Screen()

    let container = SwapContainer(Position = Position.TrimLeft(400.0f).Margin(200.0f, 20.0f), Current = Mounts.tab)
    let tabs = 
        FlowContainer.Vertical<Widget>(80.0f, Spacing = 20.0f, Position = Position.SliceLeft(400.0f).Margin(20.0f))
        |+ TabButton(Icons.import_local, "Local imports", container, Mounts.tab)
        |+ TabButton(Icons.import_etterna, "Etterna packs", container, EtternaPacks.tab)
        |+ TabButton(Icons.import_osu, "osu!mania songs", container, Beatmaps.tab)
        |+ TabButton(Icons.import_noteskin, "Noteskins", container, Noteskins.tab)
        |+ ServiceStatus("Loading", WebServices.download_string)
        |+ ServiceStatus("Downloading", WebServices.download_file)
        |+ ServiceStatus("Importing", Library.Imports.convert_song_folder)
        |+ ServiceStatus("Recaching", Library.recache_service)

    do
        this
        |* (
            SwitchContainer.Row<Widget>()
            |+ tabs
            |+ container
        )

    override this.OnEnter _ = tabs.Focus()
    override this.OnExit _ = ()