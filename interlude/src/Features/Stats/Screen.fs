namespace Interlude.Features.Stats

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Gameplay
open Prelude.Data.Charts
open Prelude.Data.Charts.Tables
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.Features.Online

type private BasicStats() =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text((if Network.credentials.Username <> "" then Network.credentials.Username else L"stats.name_placeholder"), Position = Position.SliceTop(140.0f).Margin(40.0f, 10.0f), Align = Alignment.LEFT)
        |+ Text(L"stats.total.title",
            Position = Position.Row(140.0f, 70.0f).Margin(40.0f, 0.0f), Align = Alignment.LEFT)

        |+ Text(L"stats.gametime", 
            Position = Position.Row(210.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(Stats.format_long_time (Stats.total.GameTime + Stats.session.GameTime), 
            Position = Position.Row(210.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.playtime", 
            Position = Position.Row(250.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(Stats.format_long_time (Stats.total.PlayTime + Stats.session.PlayTime), 
            Position = Position.Row(250.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.notes_hit", 
            Position = Position.Row(290.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" (Stats.total.NotesHit + Stats.session.NotesHit), 
            Position = Position.Row(290.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.plays_started", 
            Position = Position.Row(340.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" (Stats.total.PlaysStarted + Stats.session.PlaysStarted), 
            Position = Position.Row(340.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)
            
        |+ Text(L"stats.plays_retried", 
            Position = Position.Row(380.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" (Stats.total.PlaysRetried + Stats.session.PlaysRetried), 
            Position = Position.Row(380.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.plays_completed", 
            Position = Position.Row(420.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" (Stats.total.PlaysCompleted + Stats.session.PlaysCompleted), 
            Position = Position.Row(420.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)
        
        |+ Text(L"stats.plays_quit", 
            Position = Position.Row(460.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" (Stats.total.PlaysQuit + Stats.session.PlaysQuit), 
            Position = Position.Row(460.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)



        |+ Text(L"stats.session.title",
            Position = Position.Row(500.0f, 70.0f).Margin(40.0f, 0.0f), Align = Alignment.LEFT)

        |+ Text(L"stats.gametime", 
            Position = Position.Row(570.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(Stats.format_long_time Stats.session.GameTime, 
            Position = Position.Row(570.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.playtime", 
            Position = Position.Row(610.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(Stats.format_long_time Stats.session.PlayTime, 
            Position = Position.Row(610.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.notes_hit", 
            Position = Position.Row(650.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" Stats.session.NotesHit, 
            Position = Position.Row(650.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.plays_started", 
            Position = Position.Row(700.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" Stats.session.PlaysStarted, 
            Position = Position.Row(700.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)
            
        |+ Text(L"stats.plays_retried", 
            Position = Position.Row(740.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" Stats.session.PlaysRetried, 
            Position = Position.Row(740.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        |+ Text(L"stats.plays_completed", 
            Position = Position.Row(780.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |+ Text(sprintf "%i" Stats.session.PlaysCompleted, 
            Position = Position.Row(780.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)
        
        |+ Text(L"stats.plays_quit", 
            Position = Position.Row(820.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT)
        |* Text(sprintf "%i" Stats.session.PlaysQuit, 
            Position = Position.Row(820.0f, 40.0f).Margin(40.0f, 0.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.RIGHT)

        base.Init parent

type private TableLevelStats(level: Level, data: (int * int) array, ruleset: Ruleset, scale: float32) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        let b = this.Bounds.Shrink(20.0f, 0.0f)
        Text.drawFillB(Style.font, level.Name, b.SliceLeft(150.0f), Colors.text_subheading, Alignment.CENTER)

        let b = this.Bounds.TrimLeft(170.0f)
        let mutable x = b.Left
        let total = data |> Array.sumBy snd |> float32

        for (grade, count) in data do
            let w = b.Width * float32 count / total * scale
            Draw.rect (Rect.Create(x, b.Top, x + w, b.Bottom)) (ruleset.GradeColor grade)
            x <- x + w

type private TableStats() =
    inherit StaticContainer(NodeType.None)

    let score_data = Table.ratings() |> Array.ofSeq
    let top_scores = score_data |> Seq.choose (fun (_, c, g, s) -> if s.IsSome then Some (c, g.Value, s.Value) else None) |> Seq.sortByDescending (fun (_, _, s) -> s) |> Seq.truncate 50 |> Array.ofSeq
    let table_rating = top_scores |> Array.sumBy (fun (_, _, s) -> s) |> fun total -> total / 50.0

    let table_level_data = 
        score_data 
        |> Seq.groupBy(fun (l, _, _, _) -> l) 
        |> Seq.map (
            fun (l, data) -> (l, 
                data
                |> Seq.map (fun (_, _, g, _) -> Option.defaultValue -1 g) 
                |> Seq.countBy id
                |> Seq.sortDescending 
                |> Array.ofSeq)
            )
        |> Seq.sortBy(fun (l, _) -> l.Rank)
        |> Array.ofSeq

    override this.Init(parent) =
        
        match Table.current() with
        | Some t ->
            match Interlude.Content.Rulesets.try_get_by_hash t.RulesetId with
            | Some ruleset ->

                let flow = FlowContainer.Vertical<TableLevelStats>(30.0f)
                let scroll = ScrollContainer.Flow(flow, Position = Position.TrimTop(120.0f).Margin(40.0f))
                let biggest_level = table_level_data |> Array.map (fun (l, d) -> d |> Array.map snd |> Array.sum) |> Array.max |> float32
                for (l, d) in table_level_data do
                    flow.Add(TableLevelStats(l, d, ruleset, d |> Array.map snd |> Array.sum |> fun t -> float32 t / biggest_level))

                this
                |+ Text(t.Name, Position = Position.SliceTop(120.0f).Margin(40.0f, 10.0f), Align = Alignment.LEFT)
                |+ Text("Skill level", Position = Position.Row(10.0f, 40.0f).Margin(40.0f, 0.0f), Align = Alignment.RIGHT)
                |+ Text(sprintf "%.2f" table_rating, Position = Position.Row(50.0f, 60.0f).Margin(40.0f, 0.0f), Align = Alignment.RIGHT)
                |* scroll
            | None -> this |* EmptyState(Icons.x, "Missing ruleset")
        | None -> 
            this
            |* EmptyState(Icons.table, "No table selected")
        base.Init parent

type private Tabs() =
    inherit StaticContainer(NodeType.None)

    let table = TableStats()
    let skillsets = EmptyState(Icons.stats, "Not yet implemented")
    let goals = EmptyState(Icons.stats, "Not yet implemented")

    let swap = SwapContainer(Current = table, Position = Position.TrimTop 50.0f)

    let button(label: string, cmp) =
        Frame(NodeType.None, Border = K Color.Transparent, Fill = fun () -> if swap.Current = cmp then !*Palette.DARK_100 else Colors.black.O2)
        |+ Button(label, fun () -> swap.Current <- cmp)

    override this.Init(parent) =
        this
        |+ 
            (
                FlowContainer.LeftToRight(200.0f, Position = Position.SliceTop(50.0f))
                |+ button("Table", table)
                |+ button("Skillsets", skillsets)
                |+ button("Goals", goals)
            )
        |* swap
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.TrimTop(50.0f)) !*Palette.DARK_100
        base.Draw()


type StatsScreen() =
    inherit Screen()

    override this.Init(parent) =
        this
        |+ BasicStats(Position = { Position.Default with Right = 0.35f %+ 0.0f })
        |* Tabs(Position = { Position.Default with Left = 0.35f %+ 0.0f }.Margin(40.0f))
        base.Init parent

    override this.OnEnter _ = ()
    override this.OnExit _ = ()
    override this.OnBack() = 
        if Network.lobby.IsSome then Some Screen.Type.Lobby
        else Some Screen.Type.LevelSelect