namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
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

        if Stats.CURRENT_SESSION.LastPlay <= Stats.CURRENT_SESSION.Start then
            this |* EmptyState(Icons.LIST, "No scores this session", Position = Position.ShrinkT(160.0f))

        base.Init parent