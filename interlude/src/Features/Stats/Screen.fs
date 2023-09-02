namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data
open Prelude.Data.Charts
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.Features.Online

type StatsScreen() =
    inherit Screen()

    override this.Init(parent) =
        this
        |+ Text((if Network.credentials.Username <> "" then Network.credentials.Username else L"stats.name_placeholder"), Position = Position.SliceTop(140.0f).Margin(40.0f, 0.0f), Align = Alignment.LEFT)
        |+ Text(L"stats.total.title",
            Position = Position.TrimTop(140.0f).SliceTop(70.0f).Margin(40.0f, 0.0f), Align = Alignment.LEFT)
        |+ Text(sprintf "%s: %s" (L"stats.gametime") (Stats.format_long_time (Stats.total.GameTime + Stats.session.GameTime)),
            Position = Position.TrimTop(210.0f).SliceTop(50.0f).Margin(40.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |+ Text(sprintf "%s: %s" (L"stats.playtime") (Stats.format_long_time (Stats.total.PlayTime + Stats.session.PlayTime)),
            Position = Position.TrimTop(260.0f).SliceTop(50.0f).Margin(40.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |+ Text(sprintf "%s: %i" (L"stats.notes_hit") (Stats.total.NotesHit + Stats.session.NotesHit),
            Position = Position.TrimTop(310.0f).SliceTop(50.0f).Margin(40.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |+ Text(L"stats.session.title",
            Position = Position.TrimTop(360.0f).SliceTop(70.0f).Margin(40.0f, 0.0f), Align = Alignment.LEFT)
        |+ Text(sprintf "%s: %s" (L"stats.gametime") (Stats.format_long_time (Stats.session.GameTime)),
            Position = Position.TrimTop(430.0f).SliceTop(50.0f).Margin(40.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |+ Text(sprintf "%s: %s" (L"stats.playtime") (Stats.format_long_time (Stats.session.PlayTime)),
            Position = Position.TrimTop(480.0f).SliceTop(50.0f).Margin(40.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |+ Text(sprintf "%s: %i" (L"stats.notes_hit") (Stats.session.NotesHit),
            Position = Position.TrimTop(530.0f).SliceTop(50.0f).Margin(40.0f, 0.0f), Color = K Colors.text_subheading, Align = Alignment.LEFT)
        |* WIP()
        base.Init parent

    override this.OnEnter _ = ()
    override this.OnExit _ = ()
    override this.OnBack() = 
        if Network.lobby.IsSome then Some Screen.Type.Lobby
        else Some Screen.Type.LevelSelect