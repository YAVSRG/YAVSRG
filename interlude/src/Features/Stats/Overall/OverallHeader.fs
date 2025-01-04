namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.User
open Interlude.Features.Online

type OverallHeader() =
    inherit Container(NodeType.Leaf)

    let xp = Stats.TOTAL_STATS.XP + Stats.CURRENT_SESSION.SessionScore
    let level = xp |> Stats.current_level
    let xp_to_next_level = Stats.xp_for_level (level + 1) - Stats.xp_for_level level
    let current_xp = xp - Stats.xp_for_level level

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
                 %"stats.name_placeholder"),
            Position = Position.SliceT(100.0f).ShrinkX(10.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%s: %i" (%"stats.sessions.notes_hit") (Stats.TOTAL_STATS.NotesHit + Stats.CURRENT_SESSION.NotesHit),
            Color = K Colors.text_subheading,
            Position = Position.SliceT(50.0f).ShrinkT(15.0f).ShrinkX(10.0f),
            Align = Alignment.RIGHT
        )
        |+ Text(
            sprintf "XP: %i" xp,
            Color = K Colors.text_subheading,
            Position = Position.ShrinkT(50.0f).SliceT(50.0f).ShrinkB(15.0f).ShrinkX(10.0f),
            Align = Alignment.RIGHT
        )
        |+ Text(
            sprintf "%i / %i" current_xp xp_to_next_level,
            Color = K Colors.text_subheading,
            Position = Position.ShrinkB(65.0f).SliceB(35.0f).ShrinkX(20.0f),
            Align = Alignment.RIGHT
        )
        |* Text(
            sprintf "Level %i" level,
            Position = Position.ShrinkB(60.0f).SliceB(50.0f).ShrinkX(20.0f),
            Align = Alignment.LEFT
        )

        base.Init parent