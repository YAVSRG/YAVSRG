namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.Features.Online

type private BasicStats() =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(
            (if Network.credentials.Username <> "" then
                 Network.credentials.Username
             else
                 %"stats.name_placeholder"),
            Position = Position.SliceTop(140.0f).Margin(40.0f, 10.0f),
            Align = Alignment.LEFT
        )

        |+ Text(
            %"stats.total.title",
            Position = Position.Row(140.0f, 70.0f).Margin(40.0f, 0.0f),
            Align = Alignment.LEFT
        )

        |+ Text(
            %"stats.gametime",
            Position = Position.Row(210.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            Stats.format_long_time (Stats.total.GameTime + Stats.session.GameTime),
            Position = Position.Row(210.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.playtime",
            Position = Position.Row(250.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            Stats.format_long_time (Stats.total.PlayTime + Stats.session.PlayTime),
            Position = Position.Row(250.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.notes_hit",
            Position = Position.Row(290.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" (Stats.total.NotesHit + Stats.session.NotesHit),
            Position = Position.Row(290.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_started",
            Position = Position.Row(340.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" (Stats.total.PlaysStarted + Stats.session.PlaysStarted),
            Position = Position.Row(340.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_retried",
            Position = Position.Row(380.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" (Stats.total.PlaysRetried + Stats.session.PlaysRetried),
            Position = Position.Row(380.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_completed",
            Position = Position.Row(420.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" (Stats.total.PlaysCompleted + Stats.session.PlaysCompleted),
            Position = Position.Row(420.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_quit",
            Position = Position.Row(460.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" (Stats.total.PlaysQuit + Stats.session.PlaysQuit),
            Position = Position.Row(460.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )


        |+ Text(
            %"stats.session.title",
            Position = Position.Row(500.0f, 70.0f).Margin(40.0f, 0.0f),
            Align = Alignment.LEFT
        )

        |+ Text(
            %"stats.gametime",
            Position = Position.Row(570.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            Stats.format_long_time Stats.session.GameTime,
            Position = Position.Row(570.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.playtime",
            Position = Position.Row(610.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            Stats.format_long_time Stats.session.PlayTime,
            Position = Position.Row(610.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.notes_hit",
            Position = Position.Row(650.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" Stats.session.NotesHit,
            Position = Position.Row(650.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_started",
            Position = Position.Row(700.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" Stats.session.PlaysStarted,
            Position = Position.Row(700.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_retried",
            Position = Position.Row(740.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" Stats.session.PlaysRetried,
            Position = Position.Row(740.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_completed",
            Position = Position.Row(780.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%i" Stats.session.PlaysCompleted,
            Position = Position.Row(780.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        |+ Text(
            %"stats.plays_quit",
            Position = Position.Row(820.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT
        )
        |* Text(
            sprintf "%i" Stats.session.PlaysQuit,
            Position = Position.Row(820.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT
        )

        base.Init parent

type private Tabs() =
    inherit Container(NodeType.None)

    let table = TableStats()
    let skillsets = Skills()
    let goals = EmptyState(Icons.BAR_CHART, %"misc.nyi")

    let swap = SwapContainer(table, Position = Position.TrimTop 50.0f)

    let button (label: string, cmp: Widget) =
        FrameContainer(
            NodeType.None,
            Border = K Color.Transparent,
            Fill =
                fun () ->
                    if swap.Current = cmp then
                        !*Palette.DARK_100
                    else
                        Colors.black.O2
        )
        |+ Button(label, (fun () -> swap.Current <- cmp))

    override this.Init(parent) =
        this
        |+ (FlowContainer.LeftToRight(200.0f, Position = Position.SliceTop(50.0f))
            |+ button (%"stats.table", table)
            |+ button ("Skillsets", skillsets)
            |+ button ("Goals", goals))
        |* swap

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.TrimTop(50.0f)) !*Palette.DARK_100
        base.Draw()


type StatsScreen() =
    inherit Screen()

    override this.Init(parent) =
        this
        |+ BasicStats(
            Position =
                { Position.Default with
                    Right = 0.35f %+ 0.0f
                }
        )
        |* Tabs(
            Position =
                { Position.Default with
                    Left = 0.35f %+ 0.0f
                }
                    .Margin(40.0f)
        )

        base.Init parent

    override this.OnEnter _ = DiscordRPC.in_menus ("Admiring stats")
    override this.OnExit _ = ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect
