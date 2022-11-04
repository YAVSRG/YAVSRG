namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Data.Scores
open Interlude.Features.Gameplay

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
        StaticContainer(NodeType.None, Position = Position.SliceBottom(180.0f))
        |+ (Frame(NodeType.None, Position = Position.Default.Margin(200.0f, 50.0f)) |+ textEntry)
        |+ Text((fun () -> match Chart.current with Some c -> sprintf "Editing comment for %s" c.Header.Title | _ -> ""),
            Color = Style.text_subheading,
            Align = Alignment.CENTER,
            Position = Position.SliceTop 50.0f)

    let beginEdit() = editor.Select()

    let init(parent: Widget) =
        editor.Init parent

    let update(elapsedTime, moved) =
        fade.Target <- if (!|"tooltip").Pressed() then 1.0f else 0.0f
        fade.Update elapsedTime
        if textEntry.Selected && (!|"select").Tapped() then Selection.clear()
        editor.Update(elapsedTime, moved)

    let draw() = if textEntry.Selected then editor.Draw()