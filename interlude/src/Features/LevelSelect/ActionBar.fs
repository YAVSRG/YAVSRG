namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Scores
open Interlude.Features.Gameplay
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components

module Comments =
    
    let fade = Animation.Fade(0.0f)

    let private textEntry = 
        { new TextEntry( 
            Setting.make
                (fun s -> match Chart.saveData with Some d -> d.Comment <- s | _ -> ())
                (fun () -> match Chart.saveData with Some d -> d.Comment | _ -> ""),
            "comment",
            Position = Position.Margin(20.0f, 10.0f),
            Clickable = false) with 
            override this.OnDeselected() =
                base.OnDeselected()
                match Chart.saveData with Some d -> d.Comment <- d.Comment.Trim() | _ -> ()
                LevelSelect.minorRefresh <- true
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
        |+ Text((fun () -> match Chart.current with Some c -> sprintf "Editing comment for %s" c.Header.Title | _ -> ""),
            Color = K Colors.text,
            Align = Alignment.CENTER,
            Position = Position.SliceTop 55.0f)

    let beginEdit() = editor.Select()

    let init(parent: Widget) =
        editor.Init parent

    let update(elapsedTime, moved) =
        fade.Update elapsedTime
        if textEntry.Selected && (!|"select").Tapped() then Selection.clear()
        editor.Update(elapsedTime, moved)

    let draw() = 
        if textEntry.Selected then
            Draw.rect editor.Bounds (Colors.shadow_2.O3)
            editor.Draw()

type private ActionButton(icon, action, active) =
    inherit StaticContainer(NodeType.Button action)

    member val Hotkey = "none" with get, set

    override this.Init(parent) =
         this 
         |+ Clickable.Focus this
         |* HotkeyAction(this.Hotkey, action)
         base.Init parent
    
    override this.Draw() =
         let area = this.Bounds.SliceTop(this.Bounds.Height + 5.0f)
         let isActive = active()
         Draw.rect area (Style.main 100 ())
         Text.drawFillB(
            Style.baseFont,
            icon,
            area.Shrink(10.0f, 5.0f),
            ((if isActive then Colors.pink_accent elif this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2),
            Alignment.CENTER)

type ActionBar(random_chart) =
    inherit StaticContainer(NodeType.None)

    override this.Init parent =
        this
        |+ ActionButton(Icons.reset,
            random_chart,
            (K false),
            Hotkey = "random_chart",
            Position = Position.Column(0.0f, 60.0f))
            .Tooltip(Tooltip.Info("levelselect.random_chart").Hotkey("random_chart"))
        |* ActionButton(Icons.comment,
            (fun () -> Comments.fade.Target <- 1.0f - Comments.fade.Target),
            (fun () -> Comments.fade.Target = 1.0f),
            Hotkey = "show_comments",
            Position = Position.Column(70.0f, 60.0f))
            .Tooltip(Tooltip.Info("levelselect.show_comments").Hotkey("show_comments").Hotkey(L"levelselect.show_comments.hint", "comment"))
        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds (Style.dark 100 ())
        base.Draw()