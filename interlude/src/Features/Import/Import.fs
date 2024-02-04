namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data
open Prelude.Data.Charts
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Features.Online

module ImportScreen =

    let container =
        SwapContainer(Position = Position.TrimLeft(400.0f).Margin(50.0f, 20.0f), Current = Mounts.tab)

    let switch_to_noteskins () = container.Current <- Noteskins.tab
    let switch_to_rulesets () = container.Current <- Rulesets.tab
    let switch_to_tables () = container.Current <- Tables.tab

    let something_in_progress () =
        WebServices.download_file.Status <> Async.ServiceStatus.Idle
        || Library.Imports.convert_song_folder.Status <> Async.ServiceStatus.Idle
        || Caching.Cache.recache_service.Status <> Async.ServiceStatus.Idle
        || TableDownloader.download_service.Status <> Async.ServiceStatus.Idle

type private TabButton(icon: string, name: string, container: SwapContainer, target: Widget) as this =
    inherit StaticContainer(NodeType.Switch(fun _ -> this.Button))

    let button = Button(icon + " " + name, (fun () -> container.Current <- target), Position = Position.Margin(10.0f, 5.0f))

    member this.Button = button

    override this.Draw() =
        if container.Current = target then
            Draw.rect this.Bounds Colors.shadow_2.O2
            Draw.rect (this.Bounds.Expand(Style.PADDING, 0.0f).SliceLeft(Style.PADDING)) !*Palette.MAIN
        base.Draw()

    override this.Init(parent) =
        base.Init parent

        this |* button

type private Sidebar() as this =
    inherit StaticContainer(NodeType.Switch(fun _ -> this.Flow))

    let flow =
        FlowContainer.Vertical<Widget>(55.0f, Spacing = 5.0f, Position = Position.TrimTop(130.0f).Margin(10.0f))
        |+ TabButton(Icons.LINK, %"imports.local.name", ImportScreen.container, Mounts.tab)
        |+ TabButton(Icons.ARCHIVE, %"imports.etterna_packs.name", ImportScreen.container, EtternaPacks.tab)
        |+ TabButton(Icons.DOWNLOAD_CLOUD, %"imports.beatmaps.name", ImportScreen.container, Beatmaps.tab)
        |+ TabButton(Icons.SIDEBAR, %"imports.tables.name", ImportScreen.container, Tables.tab)
        |+ TabButton(Icons.IMAGE, %"imports.noteskins.name", ImportScreen.container, Noteskins.tab)
        |+ TabButton(Icons.SLIDERS, %"imports.rulesets.name", ImportScreen.container, Rulesets.tab)

    do
        this 
        |+ Text(%"menu.import.name", Position = Position.SliceTop(80.0f).Margin(20.0f, 10.0f))
        |+ Text(
            (fun () -> if ImportScreen.something_in_progress() then %"imports.in_progress" else %"imports.not_in_progress"), 
            Color = (fun () -> if ImportScreen.something_in_progress() then Colors.text_green else Colors.text_subheading), 
            Position = Position.TrimTop(60.0f).SliceTop(50.0f).Margin(20.0f, 5.0f))
        |+ LoadingIndicator.Strip(ImportScreen.something_in_progress, Position = Position.Row(110.0f, Style.PADDING).Margin(20.0f, 0.0f))
        |* flow

    member this.Flow = flow

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O1
        Draw.rect (this.Bounds.Expand(Style.PADDING, 0.0f).SliceRight(Style.PADDING)) !*Palette.MAIN_100
        base.Draw()
        

type ImportScreen() as this =
    inherit Screen()

    let sidebar = Sidebar(Position = Position.SliceLeft(400.0f))

    do 
        this
        |* (
            NavigationContainer.Row<Widget>()
            |+ sidebar
            |+ ImportScreen.container
        )

    override this.OnEnter _ =
        sidebar.Focus()
        DiscordRPC.in_menus ("Importing new content")

    override this.OnExit _ = ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect
