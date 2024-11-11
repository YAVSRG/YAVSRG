namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data.User

type Placeholder() =
    inherit StaticWidget(NodeType.None) 

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2

type ScorePlaceholder() =
    inherit FrameContainer(NodeType.None, Fill = K Colors.shadow_2.O2, Border = K Colors.white.O2)

    override this.Init(parent: Widget) =
        this
        |* Text("Score - use your imagination", Position = Position.SliceY(40.0f))

        base.Init parent

type CurrentSession() =
    inherit Container(NodeType.None)

    let current_session = Stats.CURRENT_SESSION
    let scores = FlowContainer.Vertical<ScorePlaceholder>(60.0f, Spacing = 15.0f)

    override this.Init(parent: Widget) =
        this
        |+ Text("Current session", Align = Alignment.LEFT, Position = Position.SliceT 80.0f)
        |+ Text((fun () -> sprintf "Notes hit: %i" current_session.NotesHit), Color = K Colors.text_subheading, Align = Alignment.LEFT, Position = Position.ShrinkT(70.0f).SliceT(40.0f))
        |+ Placeholder(Position = Position.ShrinkT(160.0f))
        |* ScrollContainer(scores, Position = Position.ShrinkT(160.0f).Shrink(20.0f), Margin = 5.0f)

        for i = 0 to 10 do
            scores.Add(ScorePlaceholder())

        base.Init parent