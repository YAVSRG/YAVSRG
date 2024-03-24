namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.Gameplay
open Interlude.Features.Play

module Comments =

    let fade = Animation.Fade(0.0f)

    let private text_entry =
        { new TextEntry(Setting.make
                            (fun s ->
                                match Chart.SAVE_DATA with
                                | Some d -> d.Comment <- s
                                | _ -> ()
                            )
                            (fun () ->
                                match Chart.SAVE_DATA with
                                | Some d -> d.Comment
                                | _ -> ""
                            ),
                        "comment",
                        true,
                        Position = Position.Margin(20.0f, 10.0f),
                        Clickable = false) with
            override this.OnDeselected(by_mouse: bool) =
                base.OnDeselected by_mouse

                match Chart.SAVE_DATA with
                | Some d -> d.Comment <- d.Comment.Trim()
                | _ -> ()

                LevelSelect.refresh_details ()
        }

    let editor =
        Container(NodeType.None, Position = Position.SliceBottom(160.0f))
        |+ (FrameContainer(
                NodeType.None,
                Fill = K Colors.grey_2.O2,
                Border = K Colors.grey_2,
                Position = Position.Default.Margin(200.0f, 0.0f).TrimBottom(15.0f).TrimTop(60.0f)
            )
            |+ text_entry)
        |+ Text(
            (fun () ->
                match Chart.CACHE_DATA with
                | Some c -> [ c.Title ] %> "levelselect.comments.label"
                | _ -> ""
            ),
            Color = K Colors.text,
            Align = Alignment.CENTER,
            Position = Position.SliceTop 55.0f
        )

    let begin_edit () = editor.Select false

    let init (parent: Widget) = editor.Init parent

    let update (elapsed_ms, moved) =
        fade.Update elapsed_ms

        if text_entry.Selected && ((%%"exit").Tapped() || (%%"select").Tapped()) then
            Selection.clear ()

        editor.Update(elapsed_ms, moved)

    let draw () =
        if text_entry.Selected then
            Draw.rect editor.Bounds (Colors.shadow_2.O3)
            editor.Draw()

type private ActionButton(icon, action, active) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                action ()
            )
        )

    member val Hotkey = "none" with get, set

    override this.Init(parent) =
        this |+ Clickable.Focus this
        |* HotkeyAction(
            this.Hotkey,
            (fun () ->
                Style.click.Play()
                action ()
            )
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let area = this.Bounds.SliceTop(this.Bounds.Height + 5.0f)
        let is_active = active ()
        Draw.rect area !*Palette.MAIN_100

        Text.fill_b (
            Style.font,
            icon,
            area.Shrink(10.0f, 5.0f),
            ((if is_active then Colors.pink_accent
              elif this.Focused then Colors.yellow_accent
              else Colors.grey_1),
             Colors.shadow_2),
            Alignment.CENTER
        )

type ActionBar(random_chart) =
    inherit Container(NodeType.None)

    override this.Init parent =
        this
        |+ ActionButton(
            Icons.TARGET,
            (fun () ->
                Chart.when_loaded
                <| fun info ->
                    Screen.change_new
                        (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                        Screen.Type.Practice
                        Transitions.Flags.Default
                    |> ignore
            ),
            (K false),
            Hotkey = "practice_mode",
            Position = Position.Column(0.0f, 60.0f)
        )
            .Tooltip(Tooltip.Info("levelselect.practice_mode").Hotkey("practice_mode"))
        |+ ActionButton(
            Icons.REFRESH_CCW,
            random_chart,
            (K false),
            Hotkey = "random_chart",
            Position = Position.Column(70.0f, 60.0f)
        )
            .Tooltip(Tooltip.Info("levelselect.random_chart").Hotkey("random_chart"))
        |* ActionButton(
            Icons.MESSAGE_SQUARE,
            (fun () -> Comments.fade.Target <- 1.0f - Comments.fade.Target),
            (fun () -> Comments.fade.Target = 1.0f),
            Hotkey = "show_comments",
            Position = Position.Column(140.0f, 60.0f)
        )
            .Tooltip(
                Tooltip
                    .Info("levelselect.show_comments")
                    .Hotkey("show_comments")
                    .Hotkey(%"levelselect.show_comments.hint", "comment")
            )

        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds !*Palette.DARK_100
        base.Draw()
