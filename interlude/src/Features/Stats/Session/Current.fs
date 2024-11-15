namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data.User

type Placeholder() =
    inherit StaticWidget(NodeType.None) 

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2

type CurrentSession() =
    inherit Container(NodeType.None)

    let current_session = Stats.CURRENT_SESSION

    override this.Init(parent: Widget) =
        this
        |+ Text("Current session", Align = Alignment.LEFT, Position = Position.SliceT 80.0f)
        |+ Text((fun () -> sprintf "Notes hit: %i" current_session.NotesHit), Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Text((fun () -> sprintf "Playing for %s, since %O" (Stats.format_short_time current_session.GameTime) ((Timestamp.to_datetime current_session.Start).ToLocalTime().ToShortTimeString())), Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(105.0f).SliceT(40.0f))
        |* ScoreList(current_session.Start, Timestamp.now(), Position = Position.ShrinkT(160.0f))

        base.Init parent