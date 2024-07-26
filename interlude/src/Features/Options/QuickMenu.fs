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

type private QuickAction(label, action, hotkey: Bind) =
    inherit PageButton(label, action)

    override this.Init(parent) =
        this 
        |* Text(sprintf "%s: %O" (%"misc.hotkeyhint") hotkey,
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT,
            Position = Position.Margin(10.0f, 5.0f)
        )
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        if hotkey.Tapped() then action()

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
                Screen.Type.Practice
                Transitions.Default
        then
            Menu.Exit()

    let help_mode_info =
        Callout.Normal
            .Icon(Icons.INFO)
            .Title(%"options.ingame_help")
            .Body(%"options.ingame_help.hint")
            .Hotkey("tooltip")

    override this.Content() =
        page_container()
        |+ QuickAction(%"menu.options", (fun () -> OptionsMenuPage().Show()), Bind.mk Keys.O, Icon = Icons.SETTINGS).Pos(0)
        |+ QuickAction(%"noteskin.edit", edit_noteskin, Bind.mk Keys.N,  Icon = Icons.IMAGE).Pos(3)
        |+ QuickAction(%"hud.edit", edit_hud, Bind.mk Keys.H, Icon = Icons.ZAP).Pos(5)
        |+ QuickAction(%"system.hotkeys", (fun () -> HotkeysPage().Show()), Bind.mk Keys.K).Pos(8)
        |+ Callout.frame help_mode_info (fun (w, h) -> Position.SliceLeft(w).SliceBottom(h))
        :> Widget

    override this.Header() = 
        Container(NodeType.None)
        |+ PageHeaderBase()
        |+ Text(
            sprintf "%s: %O" (%"misc.hotkeyhint") (%%"quick_menu"), 
            Color = K Colors.text_cyan,
            Position = Position.TrimLeft(PRETTY_MARGIN_X).TrimTop(110.0f).SliceTop(35.0f),
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