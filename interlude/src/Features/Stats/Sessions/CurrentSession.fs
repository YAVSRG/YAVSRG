namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats

type CurrentSession() =
    inherit Container(NodeType.None)

    let current_session = CURRENT_SESSION

    override this.Init(parent: Widget) =
        this
        |+ Text("Current session")
            .Align(Alignment.LEFT)
            .Position(Position.SliceT(80.0f))
        |+ Text(sprintf "%s: %i" (%"stats.sessions.notes_hit") current_session.NotesHit)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Text(
            (fun () ->
                sprintf "%s of playtime over %s, session started at %s"
                    (format_short_time current_session.PlayTime)
                    (format_short_time current_session.GameTime)
                    ((Timestamp.to_datetime current_session.Start).ToLocalTime().ToShortTimeString())
            ))
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.ShrinkT(105.0f).SliceT(40.0f))
        |* ScoreList(current_session.Start, Timestamp.now())
            .Position(Position.ShrinkT(160.0f))

        base.Init parent