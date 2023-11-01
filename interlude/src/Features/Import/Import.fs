namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data
open Prelude.Data.Charts
open Interlude.UI
open Interlude.Utils
open Interlude.Features.Online

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
            Fill = fun () -> if this.Focused then !*Palette.MAIN_100 else !*Palette.DARK_100)
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

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if service.Status <> Async.ServiceStatus.Idle then
            fade.Target <- 1.0f
            animation.Update elapsed_ms
        else fade.Target <- 0.0f
        fade.Update elapsed_ms

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
        Text.drawFill(Style.font, icon, this.Bounds.SliceLeft this.Bounds.Height, iconColor, Alignment.CENTER)
        Text.drawFill(Style.font, name, this.Bounds.TrimLeft this.Bounds.Height, textColor, Alignment.LEFT)

module ImportScreen =

    let container = SwapContainer(Position = Position.TrimLeft(400.0f).Margin(50.0f, 20.0f), Current = Mounts.tab)
    
    let switch_to_noteskins() = container.Current <- Noteskins.tab
    let switch_to_rulesets() = container.Current <- Rulesets.tab
    let switch_to_tables() = container.Current <- Tables.tab

type ImportScreen() as this =
    inherit Screen()
    let tabs = 
        FlowContainer.Vertical<Widget>(65.0f, Spacing = 20.0f, Position = Position.SliceLeft(400.0f).Margin(20.0f))
        |+ TabButton(Icons.import_local, %"imports.local.name", ImportScreen.container, Mounts.tab)
        |+ TabButton(Icons.import_etterna, %"imports.etterna_packs.name", ImportScreen.container, EtternaPacks.tab)
        |+ TabButton(Icons.import_osu, %"imports.beatmaps.name", ImportScreen.container, Beatmaps.tab)
        |+ TabButton(Icons.import_noteskin, %"imports.noteskins.name", ImportScreen.container, Noteskins.tab)
        |+ TabButton(Icons.gameplay, %"imports.rulesets.name", ImportScreen.container, Rulesets.tab)
        |+ TabButton(Icons.table, %"imports.tables.name", ImportScreen.container, Tables.tab)
        |+ ServiceStatus("Loading", WebServices.download_string)
        |+ ServiceStatus("Downloading", WebServices.download_file)
        |+ ServiceStatus("Importing", Library.Imports.convert_song_folder)
        |+ ServiceStatus("Recaching", Caching.Cache.recache_service)

    do
        this
        |* (
            NavigationContainer.Row<Widget>()
            |+ tabs
            |+ ImportScreen.container
        )

    override this.OnEnter _ = 
        tabs.Focus()
        DiscordRPC.in_menus("Importing new content")
    override this.OnExit _ = ()
    override this.OnBack() = 
        if Network.lobby.IsSome then Some Screen.Type.Lobby
        else Some Screen.Type.LevelSelect