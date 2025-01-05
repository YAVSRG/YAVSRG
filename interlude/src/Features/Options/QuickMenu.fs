namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Interlude.UI
open Interlude.Content
open Interlude.Features.Skins
open Interlude.Features.Skins.EditNoteskin
open Interlude.Features.Skins.EditHUD
open Interlude.Features.Gameplay
open Interlude.Features.OptionsMenu.SystemSettings

type QuickMenuPage() =
    inherit Page()
    
    let mutable release_cooldown = 800.0

    let edit_noteskin() =
        if Content.Noteskin.IsEmbedded then
            SelectSkinsPage().Show()
        else
            EditNoteskinPage().Show()

    let edit_hud() =
        if
            SelectedChart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> EditHudScreen.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, ignore))
                Screen.Type.EditHud
                Transitions.Default
        then
            Menu.Exit()

    override this.Content() =
        page_container()
        |+ PageButton(%"menu.options", (fun () -> OptionsMenuPage().Show()), Hotkey = Bind.mk Keys.O, Icon = Icons.SETTINGS).Pos(0)
        |+ PageButton(%"noteskin.edit", edit_noteskin, Hotkey = Bind.mk Keys.N, Icon = Icons.IMAGE).Pos(3)
        |+ PageButton(%"hud.edit", edit_hud, Hotkey = Bind.mk Keys.H, Icon = Icons.ZAP).Pos(5)
        |+ PageButton(%"system.hotkeys", (fun () -> HotkeysPage().Show()), Hotkey = Bind.mk Keys.K).Pos(8)
        :> Widget

    override this.Header() = 
        Container(NodeType.None)
        |+ PageHeaderBase()
        |+ Text(
            sprintf "%s: %O" (%"misc.hotkeyhint") (%%"quick_menu"), 
            Color = K Colors.text_cyan,
            Position = Position.ShrinkL(PRETTY_MARGIN_X).ShrinkT(110.0f).SliceT(35.0f),
            Align = Alignment.LEFT
        )
        :> Widget

    override this.Direction = PageEasing.Right
    override this.Title = %"menu.quick"
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = Menu.Back()
    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        release_cooldown <- release_cooldown - elapsed_ms
        if release_cooldown < 0.0 && (%%"quick_menu").Released() then
            Menu.Back()