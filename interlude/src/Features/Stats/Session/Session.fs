namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.UI

type CurrentSession() =
    inherit Container(NodeType.None)

    let current_session = Stats.CURRENT_SESSION

    override this.Init(parent: Widget) =
        this
        |+ Text("Current session", Align = Alignment.LEFT, Position = Position.SliceT 80.0f)
        |+ Text(sprintf "Notes hit: %i" current_session.NotesHit, Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Text(
            (fun () -> 
                sprintf "%s of playtime over %s, session started at %s"
                    (Stats.format_short_time current_session.PlayTime)
                    (Stats.format_short_time current_session.GameTime)
                    ((Timestamp.to_datetime current_session.Start).ToLocalTime().ToShortTimeString())
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.ShrinkT(105.0f).SliceT(40.0f)
        )
        |* ScoreList(current_session.Start, Timestamp.now(), Position = Position.ShrinkT(160.0f))

        base.Init parent

type PreviousSession(session: Session, sessions_today: Session list, close: unit -> unit, fd: unit -> unit, bk: unit -> unit) =
    inherit Container(NodeType.None)

    override this.Init(parent: Widget) =
        this
        |+ Text(sprintf "Session on %O" ((session.Start |> timestamp_to_local_day).ToShortDateString()), Align = Alignment.LEFT, Position = Position.SliceT 80.0f)
        |+ Text(sprintf "Notes hit: %i" session.NotesHit, Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Text(
            (fun () -> 
                sprintf "%s of playtime over %s, session started at %s"
                    (Stats.format_short_time session.PlayTime)
                    (Stats.format_short_time session.GameTime)
                    ((Timestamp.to_datetime session.Start).ToLocalTime().ToShortTimeString())
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.ShrinkT(105.0f).SliceT(40.0f)
        )
        |+ Button(Icons.X, close, Position = Position.SliceT(60.0f).SliceR(60.0f))
        |* ScoreList(session.Start, session.End, Position = Position.ShrinkT(160.0f))

        if sessions_today.Length > 1 then
            this
            |+ Text(
                sprintf "Session %i of %i this day" (1 + List.findIndex ((=) session) sessions_today) sessions_today.Length,
                Align = Alignment.CENTER,
                Position = Position.ShrinkT(70.0f).SliceT(40.0f).SliceR(300.0f)
            )
            |+ Button(
                sprintf "%s Back" Icons.ARROW_LEFT,
                bk,
                Position = Position.ShrinkT(105.0f).SliceT(40.0f).ShrinkR(150.0f).SliceR(150.0f)
            )
            |* Button(
                sprintf "Next %s" Icons.ARROW_RIGHT,
                fd,
                Position = Position.ShrinkT(105.0f).SliceT(40.0f).SliceR(150.0f)
            )
        base.Init parent