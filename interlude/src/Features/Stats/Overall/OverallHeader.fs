namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.User.Stats
open Interlude.Features.Online

type OverallHeader() =
    inherit Container(NodeType.Leaf)

    let xp = TOTAL_STATS.XP + CURRENT_SESSION.SessionScore
    let level = xp |> current_level
    let xp_to_next_level = xp_for_level (level + 1) - xp_for_level level
    let current_xp = xp - xp_for_level level

    override this.Draw() =

        Render.rect (this.Bounds.SliceB(120.0f)) Colors.shadow_2.O2
        let bar = this.Bounds.SliceB(40.0f).ShrinkX(20.0f).TranslateY(-20.0f)
        Render.rect (bar.Translate(10.0f, 10.0f)) Colors.black
        Render.rect bar !*Palette.DARKER
        Render.rect (bar.SlicePercentL(float32 current_xp / float32 xp_to_next_level)) !*Palette.MAIN

        base.Draw()

    override this.Init(parent) =
        this
        |+ Text(
            (if Network.credentials.Username <> "" then
                 Network.credentials.Username
             else
                 %"stats.name_placeholder"))
                 .Position(Position.SliceT(100.0f).ShrinkX(10.0f))
                 .Align(Alignment.LEFT)
        |+ Text(sprintf "%s: %i" (%"stats.sessions.notes_hit") (TOTAL_STATS.NotesHit + CURRENT_SESSION.NotesHit))
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.SliceT(50.0f).ShrinkT(15.0f).ShrinkX(10.0f))
        |+ Text(sprintf "XP: %i" xp)
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.ShrinkT(50.0f).SliceT(50.0f).ShrinkB(15.0f).ShrinkX(10.0f))
        |+ Text(sprintf "%i / %i" current_xp xp_to_next_level)
            .Color(Colors.text_subheading)
            .Align(Alignment.RIGHT)
            .Position(Position.ShrinkB(65.0f).SliceB(35.0f).ShrinkX(20.0f))
        |* Text(sprintf "Level %i" level)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkB(60.0f).SliceB(50.0f).ShrinkX(20.0f))

        base.Init parent