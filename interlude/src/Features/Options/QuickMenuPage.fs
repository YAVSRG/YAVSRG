namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.OptionsMenu.SystemSettings

type QuickMenuPage() =
    inherit Page()

    let mutable release_cooldown = 800.0

    override this.Content() =
        page_container()
            .With(
                PageButton(%"menu.options", fun () -> OptionsPage().Show())
                    .Hotkey(Bind.mk Keys.O)
                    .Icon(Icons.SETTINGS)
                    .Pos(0),
                PageButton(%"noteskin.edit", SkinActions.edit_or_extract_noteskin)
                    .Hotkey(Bind.mk Keys.N)
                    .Icon(Icons.IMAGE)
                    .Pos(3),
                PageButton(%"hud.edit", fun () -> SkinActions.edit_hud ignore)
                    .Hotkey(Bind.mk Keys.H)
                    .Icon(Icons.ZAP)
                    .Pos(5),
                PageButton(%"system.hotkeys", fun () -> HotkeysPage().Show())
                    .Hotkey(Bind.mk Keys.K)
                    .Pos(8)
            )
        :> Widget

    override this.Header() =
        Container(NodeType.None)
            .With(
                PageHeaderBase(),
                Text(sprintf "%s: %O" (%"misc.hotkeyhint") (%%"quick_menu"))
                    .Color(Colors.text_cyan)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkL(PAGE_MARGIN_X).ShrinkT(110.0f).SliceT(35.0f))
            )

    override this.Direction = PageEasing.Right
    override this.Title = %"menu.quick"
    override this.OnReturnFromNestedPage() = Menu.Back()
    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        release_cooldown <- release_cooldown - elapsed_ms
        if release_cooldown < 0.0 && (%%"quick_menu").Released() then
            Menu.Back()