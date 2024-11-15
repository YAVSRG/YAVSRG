namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User

type CurrentSession() =
    inherit Container(NodeType.None)

    let current_session = Stats.CURRENT_SESSION

    override this.Init(parent: Widget) =
        this
        |+ Text("Current session", Align = Alignment.LEFT, Position = Position.SliceT 80.0f)
        |+ Text(sprintf "Notes hit: %i" current_session.NotesHit, Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Text((fun () -> sprintf "Playing for %s, since %O" (Stats.format_short_time current_session.GameTime) ((Timestamp.to_datetime current_session.Start).ToLocalTime().ToShortTimeString())), Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(105.0f).SliceT(40.0f))
        |* ScoreList(current_session.Start, Timestamp.now(), Position = Position.ShrinkT(160.0f))

        base.Init parent

type PreviousSession(session: Session) =
    inherit Container(NodeType.None)

    override this.Init(parent: Widget) =
        this
        |+ Text(sprintf "Session on %O" ((session.Start |> timestamp_to_local_day).ToShortDateString()), Align = Alignment.LEFT, Position = Position.SliceT 80.0f)
        |+ Text(sprintf "Notes hit: %i" session.NotesHit, Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Text(sprintf "Played for %s, starting at %O" (Stats.format_short_time session.GameTime) ((Timestamp.to_datetime session.Start).ToLocalTime().ToShortTimeString()), Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(105.0f).SliceT(40.0f))
        |* ScoreList(session.Start, session.End, Position = Position.ShrinkT(160.0f))

        base.Init parent