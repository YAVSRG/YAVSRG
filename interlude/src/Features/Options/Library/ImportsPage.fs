namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Mounts
open Interlude.Features.Import.osu
open Interlude.Features.Import.Etterna
open Interlude.Features.Tables.Browser
open Interlude.Features.Skins.Browser
open Interlude.Features.Rulesets
open Interlude.Features.Tables

type ImportsPage() =
    inherit Page()

    static member GetOsuSongs() : PageButton =
        PageButton(%"beatmap_browser", fun () -> BeatmapBrowserPage().Show())

    static member GetEtternaPacks() : PageButton =
        PageButton(%"etterna_pack_browser", fun () -> EtternaPacksBrowserPage().Show())

    static member GetSkins() : PageButton =
        PageButton(%"skins.browser", fun () -> SkinsBrowserPage().Show())

    static member GetTables() : PageButton =
        PageButton(%"tables.browser", fun () -> TableBrowserPage().Show())

    override this.Content() =

        let mount_options =
            NavigationContainer.Column()
                .WrapNavigation(false)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SlicePercentR(0.5f).ShrinkL(10.0f).TranslateY(-50.0f))
            |+ MountControl(MountedGameType.Osu, options.OsuMount)
                .Position(Position.SliceT(100.0f, 150.0f))
            |+ MountControl(MountedGameType.Quaver, options.QuaverMount)
                .Position(Position.SliceT(270.0f, 150.0f))
            |+ MountControl(MountedGameType.Etterna, options.EtternaMount)
                .Position(Position.SliceT(440.0f, 150.0f))
            |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount)
                .Position(Position.SliceT(610.0f, 150.0f))
            |+ Text(%"imports.mount")
                .Align(Alignment.CENTER)
                .Position(Position.SliceT(0.0f, 80.0f))
            |+ Text(%"imports.drag_and_drop_hint")
                .Align(Alignment.CENTER)
                .Position(Position.SliceT(770.0f, 80.0f).Translate(0.0f, -10.0f))

        let main_options =
            NavigationContainer.Column()
                .WrapNavigation(false)
                .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SlicePercentL(0.5f).ShrinkR(10.0f))
            |+ ImportsPage.GetOsuSongs().Pos(0, 2, PageWidth.Full)
            |+ ImportsPage.GetEtternaPacks().Pos(2, 2, PageWidth.Full)
            |+ ImportsPage.GetSkins().Pos(4, 2, PageWidth.Full)
            |+ ImportsPage.GetTables().Pos(6, 2, PageWidth.Full)

            |+ PageButton(%"skins.import_from_osu", fun () -> Skins.OsuSkinsListPage().Show())
                .Pos(9, 2, PageWidth.Full)

            |+ PageButton(%"rulesets", fun () -> SelectRulesetPage().Show())
                .Pos(12, 2, PageWidth.Full)
            |+ PageButton(%"library.tables", fun () -> SelectTablePage(ignore).Show())
                .Pos(14, 2, PageWidth.Full)

        NavigationContainer.Row()
        |+ main_options
        |+ mount_options
        :> Widget

    override this.Title = sprintf "%s %s" Icons.DOWNLOAD (%"menu.import")
    override this.OnClose() = ()