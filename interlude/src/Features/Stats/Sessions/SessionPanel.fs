namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI

type SessionPanel =

    static member CreateCurrent() =

        let current_session = CURRENT_SESSION

        Container(NodeType.None)
            .With(
                Text(%"stats.current_session")
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(80.0f)),
                Text(sprintf "%s: %i" (%"stats.sessions.notes_hit") current_session.NotesHit)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkT(70.0f).SliceT(40.0f)),
                Text(fun () ->
                    [
                        format_short_time current_session.PlayTime
                        format_short_time current_session.GameTime
                        (Timestamp.to_datetime current_session.Start).ToLocalTime().ToShortTimeString()
                    ]
                    %> "stats.info_tagline"
                )
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkT(105.0f).SliceT(40.0f)),

                ScoreList(current_session.Start, Timestamp.now())
                    .Position(Position.ShrinkT(160.0f))
            )

    static member CreatePrevious(session: Session, sessions_today: Session list, close: unit -> unit, fd: unit -> unit, bk: unit -> unit) =
        Container(NodeType.None)
            .With(
                Text([(session.Start |> timestamp_to_rg_calendar_day).ToShortDateString()] %> %"stats.previous_session")
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(80.0f)),
                Text(
                    if session.NotesHit > 0 then
                        sprintf "%s: %i" (%"stats.sessions.notes_hit") session.NotesHit
                    else
                        sprintf "%s %s" Icons.ALERT_CIRCLE (%"stats.sessions.estimated_data")
                )
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkT(70.0f).SliceT(40.0f)),
                Text(fun () ->
                    [
                        format_short_time session.PlayTime
                        format_short_time session.GameTime
                        (Timestamp.to_datetime session.Start).ToLocalTime().ToShortTimeString()
                    ]
                    %> "stats.info_tagline"
                )
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkT(105.0f).SliceT(40.0f)),

                Button(Icons.X, close)
                    .Position(Position.SliceT(60.0f).SliceR(60.0f)),
                Button(sprintf "%s %s" Icons.ARROW_LEFT (%"stats.sessions.back"), bk)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkT(105.0f).SliceT(40.0f).ShrinkR(150.0f).SliceR(150.0f)),
                Button(sprintf "%s %s" (%"stats.sessions.next") Icons.ARROW_RIGHT, fd)
                    .Align(Alignment.RIGHT)
                    .Position(Position.ShrinkT(105.0f).SliceT(40.0f).SliceR(150.0f)),

                ScoreList(session.Start, session.End)
                    .Position(Position.ShrinkT(160.0f))
            )
            .WithConditional(
                sessions_today.Length > 1,
                Text(
                    [(1 + List.findIndex ((=) session) sessions_today).ToString(); sessions_today.Length.ToString()]
                    %> "stats.sessions.count_this_day"
                )
                    .Align(Alignment.CENTER)
                    .Position(Position.ShrinkT(70.0f).SliceT(40.0f).SliceR(300.0f))
            )