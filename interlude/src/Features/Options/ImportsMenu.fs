namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Mounts
open Interlude.Features.Import.osu
open Interlude.Features.Import.Etterna
open Interlude.Features.Tables.Browser
open Interlude.Features.Noteskins.Browser
open Interlude.Features.Rulesets

type ImportsMenuPage() =
    inherit Page()

    let mount_options =
        NavigationContainer.Column(WrapNavigation = false, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Left = 0.5f %+ 10.0f })
        |+ MountControl(MountedGameType.Osu, options.OsuMount, Position = Position.Row(100.0f, 150.0f))
        |+ MountControl(MountedGameType.Quaver, options.QuaverMount, Position = Position.Row(270.0f, 150.0f))
        |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount, Position = Position.Row(440.0f, 150.0f))
        |+ MountControl(MountedGameType.Etterna, options.EtternaMount, Position = Position.Row(610.0f, 150.0f))
        |+ Text(%"imports.mount", Align = Alignment.CENTER, Position = Position.Row(0.0f, 80.0f))

    let main_options =
        NavigationContainer.Column(WrapNavigation = false, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.5f %- 10.0f })
        |+ PageButton(%"beatmap_browser", fun () -> BeatmapBrowserPage().Show()).Pos(0, 2, PageWidth.Full)
        |+ PageButton(%"etterna_pack_browser", fun () -> EtternaPacksBrowserPage().Show()).Pos(2, 2, PageWidth.Full)
        |+ PageButton(%"rulesets.add", fun () -> AddRulesetsPage().Show()).Pos(4, 2, PageWidth.Full)
        |+ PageButton(%"tables.browser", fun () -> TableBrowserPage().Show()).Pos(6, 2, PageWidth.Full)
        |+ PageButton(%"skins.browser",fun () -> NoteskinsBrowserPage().Show()).Pos(8, 2, PageWidth.Full)

    override this.Content() =
        NavigationContainer.Row()
        |+ main_options
        |+ mount_options
        :> Widget

    override this.Title = sprintf "%s %s" Icons.DOWNLOAD (%"menu.import")
    override this.OnClose() = ()
