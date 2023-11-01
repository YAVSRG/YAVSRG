namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Prelude.Common
open Prelude.Data.Scores
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.Gameplay
open Interlude.Features.Play

module Comments =
    
    let fade = Animation.Fade(0.0f)

    let private textEntry = 
        { new TextEntry( 
            Setting.make
                (fun s -> match Chart.SAVE_DATA with Some d -> d.Comment <- s | _ -> ())
                (fun () -> match Chart.SAVE_DATA with Some d -> d.Comment | _ -> ""),
            "comment",
            Position = Position.Margin(20.0f, 10.0f),
            Clickable = false) with 
            override this.OnDeselected() =
                base.OnDeselected()
                match Chart.SAVE_DATA with Some d -> d.Comment <- d.Comment.Trim() | _ -> ()
                LevelSelect.refresh_details()
        }

    let editor =
        StaticContainer(NodeType.None, Position = Position.SliceBottom(160.0f))
        |+ (
            Frame(NodeType.None, 
                Fill = K Colors.grey_2.O2,
                Border = K Colors.grey_2,
                Position = Position.Default.Margin(200.0f, 0.0f).TrimBottom(15.0f).TrimTop(60.0f))
            |+ textEntry
           )
        |+ Text((fun () -> match Chart.CACHE_DATA with Some c -> [c.Title] %> "levelselect.comments.label" | _ -> ""),
            Color = K Colors.text,
            Align = Alignment.CENTER,
            Position = Position.SliceTop 55.0f)

    let beginEdit() = editor.Select()

    let init(parent: Widget) =
        editor.Init parent

    let update(elapsed_ms, moved) =
        fade.Update elapsed_ms
        if textEntry.Selected && ((+."exit").Tapped() || (+."select").Tapped()) then Selection.clear()
        editor.Update(elapsed_ms, moved)

    let draw() = 
        if textEntry.Selected then
            Draw.rect editor.Bounds (Colors.shadow_2.O3)
            editor.Draw()

type private ActionButton(icon, action, active) =
    inherit StaticContainer(NodeType.Button (fun () -> Style.click.Play(); action()))

    member val Hotkey = "none" with get, set

    override this.Init(parent) =
         this 
         |+ Clickable.Focus this
         |* HotkeyAction(this.Hotkey, (fun () -> Style.click.Play(); action()))
         base.Init parent
        
    override this.OnFocus() = Style.hover.Play(); base.OnFocus()
    
    override this.Draw() =
         let area = this.Bounds.SliceTop(this.Bounds.Height + 5.0f)
         let isActive = active()
         Draw.rect area !*Palette.MAIN_100
         Text.drawFillB(
            Style.font,
            icon,
            area.Shrink(10.0f, 5.0f),
            ((if isActive then Colors.pink_accent elif this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2),
            Alignment.CENTER)

type ActionBar(random_chart) =
    inherit StaticContainer(NodeType.None)

    override this.Init parent =
        this
        |+ ActionButton(Icons.practice,
            (fun () ->
            // todo: wait until load if not
            if Chart.WITH_MODS.IsSome then
                Screen.changeNew 
                    (fun () -> PracticeScreen.practice_screen(0.0f<ms>))
                    Screen.Type.Practice
                    Transitions.Flags.Default),
            (K false),
            Hotkey = "practice_mode",
            Position = Position.Column(0.0f, 60.0f))
            .Tooltip(Tooltip.Info("levelselect.practice_mode").Hotkey("practice_mode"))
        |+ ActionButton(Icons.reset,
            random_chart,
            (K false),
            Hotkey = "random_chart",
            Position = Position.Column(70.0f, 60.0f))
            .Tooltip(Tooltip.Info("levelselect.random_chart").Hotkey("random_chart"))
        |* ActionButton(Icons.comment,
            (fun () -> Comments.fade.Target <- 1.0f - Comments.fade.Target),
            (fun () -> Comments.fade.Target = 1.0f),
            Hotkey = "show_comments",
            Position = Position.Column(140.0f, 60.0f))
            .Tooltip(Tooltip.Info("levelselect.show_comments").Hotkey("show_comments").Hotkey(%"levelselect.show_comments.hint", "comment"))
        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds !*Palette.DARK_100
        base.Draw()