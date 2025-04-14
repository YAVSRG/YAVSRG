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
            page_container()
                .With(
                    ImportsPage.GetOsuSongs().Pos(0),
                    ImportsPage.GetEtternaPacks().Pos(2),
                    ImportsPage.GetSkins().Pos(4),
                    ImportsPage.GetTables().Pos(6),
                    ImportsPage.ImportOsuSkins().Pos(9),

                    PageButton(%"rulesets", fun () -> SelectRulesetPage().Show()).Pos(12),
                    PageButton(%"library.tables", fun () -> SelectTablePage(ignore).Show()).Pos(14)
                )

        NavigationContainer.Row()
            .With(
                lhs_actions,
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
    override this.OnClose() = ()