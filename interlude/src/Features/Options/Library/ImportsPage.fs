namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Flux.UI
open Prelude
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

    static member ImportOsuSkins() : PageButton =
         PageButton(%"skins.import_from_osu", fun () -> Skins.OsuSkinsListPage().Show())

    override this.Content() =

        let lhs_actions =
            NavigationContainer.Column()
                .WrapNavigation(false)
                .With(
                    ImportsPage.GetOsuSongs().Pos(0, 2, PageWidth.Full),
                    ImportsPage.GetEtternaPacks().Pos(2, 2, PageWidth.Full),
                    ImportsPage.GetSkins().Pos(4, 2, PageWidth.Full),
                    ImportsPage.GetTables().Pos(6, 2, PageWidth.Full),
                    ImportsPage.ImportOsuSkins().Pos(9, 2, PageWidth.Full),

                    PageButton(%"rulesets", fun () -> SelectRulesetPage().Show()).Pos(12, 2, PageWidth.Full),
                    PageButton(%"library.tables", fun () -> SelectTablePage(ignore).Show()).Pos(14, 2, PageWidth.Full)
                )

        NavigationContainer.Row()
            .With(
                lhs_actions
                    .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y).SlicePercentL(0.5f)),
                MountDisplay.CreateAll()
                    .Position(Position.Shrink(PAGE_MARGIN_X).SliceR(MountDisplay.WIDTH).SliceY(MountDisplay.ALL_HEIGHT))
            )

    override this.Footer() =
        Container.Create(base.Footer())
            .With(
                Text(Icons.INFO + " " + %"imports.drag_and_drop_hint")
                    .Align(Alignment.RIGHT)
                    .Position(Position.SliceB(80.0f).SliceY(55.0f).ShrinkR(40.0f).ShrinkL(300.0f))
            )

    override this.Title = sprintf "%s %s" Icons.DOWNLOAD (%"menu.import")