namespace Interlude.Features.Stats

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Gameplay
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Tables
open Prelude.Data.Scores
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.Features.Online
open Interlude.Web.Shared.Requests
open Interlude.Web.Shared.API

type private WebRequestState =
    | Offline = 0
    | Loading = 1
    | ServerError = 2
    | Loaded = 3

type private Leaderboard() =
    inherit StaticContainer(NodeType.None)

    let mutable status = WebRequestState.Loading

    override this.Init(parent) =

        let contents = FlowContainer.Vertical<Widget>(70.0f)

        if Network.status = Network.Status.LoggedIn then
            Client.get<Tables.Leaderboard.Response>(
                sprintf "%s?table=%s" 
                    (snd Tables.Leaderboard.ROUTE)
                    (System.Uri.EscapeDataString <| Table.current().Value.Name.ToLower()),
                fun response -> sync <| fun () ->
                    match response with
                    | Some data ->
                        status <- WebRequestState.Loaded
                        for player in data.Players do
                            contents.Add (
                                StaticContainer(NodeType.None)
                                |+ Text(player.Username, Color = K (Color.FromArgb player.Color, Colors.shadow_2), Align = Alignment.LEFT, Position = Position.Margin(100.0f, 5.0f))
                                |+ Text("#" + player.Rank.ToString(), Align = Alignment.LEFT, Position = Position.Margin(10.0f, 5.0f))
                                |+ Text(sprintf "%.2f" player.Rating, Align = Alignment.RIGHT, Position = Position.Margin(10.0f, 5.0f))
                            )
                    | None -> status <- WebRequestState.ServerError
            )
        else status <- WebRequestState.Offline

        this
        |+ Conditional((fun () -> status = WebRequestState.Loading), EmptyState(Icons.cloud, L"misc.loading"))
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.connected, L"misc.offline"))
        |+ Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.connected, "Server error"))
        |* ScrollContainer.Flow(contents)

        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        base.Draw()

type private CompareFriend(ruleset: Ruleset, data_by_level: (Level * (TableChart * (int * float) option) array) array, name: string, on_back: unit -> unit) =
    inherit StaticContainer(NodeType.None)

    let mutable status = WebRequestState.Loading

    override this.Init(parent) =

        let contents = FlowContainer.Vertical<Widget>(50.0f)

        if Network.status = Network.Status.LoggedIn then
            Client.get<Tables.Records.Response>(
                sprintf "%s?user=%s&table=%s" 
                    (snd Tables.Records.ROUTE)
                    (System.Uri.EscapeDataString name)
                    (System.Uri.EscapeDataString <| Table.current().Value.Name.ToLower()), 
                fun response -> sync <| fun () ->
                    match response with
                    | Some data ->
                        status <- WebRequestState.Loaded
                        let their_scores = 
                            data.Scores
                            |> Seq.map (fun score -> score.Hash, (score.Grade, score.Score))
                            |> Map.ofSeq
                        for level, your_scores in data_by_level do
                            contents.Add(Text("-- " + level.Name + " --", Align = Alignment.CENTER, Color = K Colors.text_greyout))
                            for chart, your_score in your_scores do
                                let their_score = Map.tryFind chart.Hash their_scores
                                let name = match Cache.by_hash chart.Hash Library.cache with Some cc -> cc.Title | None -> sprintf "<%s>" chart.Id
                                let delta = if your_score.IsSome && their_score.IsSome then snd your_score.Value - snd their_score.Value else 0.0
                                contents.Add (
                                    StaticContainer(NodeType.None)
                                    |+ Text(name, Align = Alignment.CENTER, Position = Position.Margin(0.0f, 5.0f))
                                    |+ Text(
                                        (match your_score with None -> "--" | Some (_, acc) -> sprintf "%.2f%%" (acc * 100.0)),
                                        Color = K (ruleset.GradeColor (match your_score with None -> -1 | Some (grade, acc) -> grade), Colors.shadow_2),
                                        Align = Alignment.LEFT)
                                    |+ Text(
                                        (if delta <> 0.0 then sprintf "+%.2f%%" (abs delta * 100.0) else ""),
                                        Color = K Colors.text_green,
                                        Position = Position.Margin(150.0f, 0.0f),
                                        Align = if delta > 0 then Alignment.LEFT else Alignment.RIGHT)
                                    |+ Text(
                                        (match their_score with None -> "--" | Some (_, acc) -> sprintf "%.2f%%" (acc * 100.0)),
                                        Color = K (ruleset.GradeColor (match their_score with None -> -1 | Some (grade, acc) -> grade), Colors.shadow_2),
                                        Align = Alignment.RIGHT)
                                )
                    | None -> status <- WebRequestState.ServerError
            )
        else status <- WebRequestState.Offline

        this
        |+ Conditional((fun () -> status = WebRequestState.Loading), EmptyState(Icons.cloud, L"misc.loading"))
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.connected, L"misc.offline"))
        |+ Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.connected, "Server error"))
        |+ Text("Comparing to " + name, Align = Alignment.RIGHT, Position = Position.SliceTop(50.0f).Margin(20.0f, 0.0f))
        |+ Button(K (Icons.back + " Back"), on_back, Position = Position.Box(0.0f, 0.0f, 200.0f, 50.0f))
        |* ScrollContainer.Flow(contents, Position = Position.Margin(10.0f, 0.0f).TrimTop(55.0f))

        base.Init parent


type private FriendComparer(ruleset: Ruleset, score_data: (Level * TableChart * int option * float option) array) =
    inherit StaticContainer(NodeType.None)

    let mutable status = WebRequestState.Loading
    let mutable friends : Friends.List.Friend array option = None

    let rs_hash = Ruleset.hash ruleset
    let score_of(hash: string) =
        (Scores.getData hash).Value.PersonalBests.[rs_hash].Accuracy |> PersonalBests.get_best_above 1.0f |> Option.get
    let data_by_level = 
        score_data 
        |> Array.groupBy (fun (l, _, _, _) -> l)
        |> Array.map (fun (l, data) -> (l, data |> Array.map (fun (_, chart, grade, _) -> if grade.IsSome then chart, Some (grade.Value, score_of chart.Hash) else chart, None )))
        |> Array.sortBy(fun (l, data) -> l.Rank)

    let friend(name: string, on_compare: unit -> unit) =
        StaticContainer(NodeType.None)
        |+ Text(name, Position = Position.Margin(20.0f, 5.0f), Align = Alignment.LEFT)
        |+ Button(K "Compare >", on_compare, Position = Position.SliceRight(250.0f).Margin(5.0f))

    override this.Init(parent) =

        let friends_list = FlowContainer.Vertical(70.0f)
        let swap = SwapContainer(Current = friends_list)

        if Network.status = Network.Status.LoggedIn then
            Client.get<Friends.List.Response>(snd Friends.List.ROUTE, 
                fun response -> sync <| fun () ->
                    match response with
                    | Some data -> 
                        friends <- Some data.Friends
                        for f in data.Friends do
                            friends_list.Add(
                                friend(f.Username, 
                                    fun () -> swap.Current <- CompareFriend(ruleset, data_by_level, f.Username, fun () -> swap.Current <- friends_list)
                                )
                            )
                        status <- WebRequestState.Loaded
                    | None -> status <- WebRequestState.ServerError
            )
        else status <- WebRequestState.Offline

        this
        |+ Conditional((fun () -> status = WebRequestState.Loading), EmptyState(Icons.cloud, L"misc.loading"))
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.connected, L"misc.offline"))
        |+ Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.connected, "Server error"))
        |+ Conditional((fun () -> status = WebRequestState.Loaded && friends.Value.Length = 0), EmptyState(Icons.multiplayer, L"stats.table.friends.empty", Subtitle = L"stats.table.friends.empty.subtitle"))
        |* swap

        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        base.Draw()

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

type private TableScore(chart: TableChart, grade: int, rating: float, ruleset: Ruleset) =
    inherit StaticWidget(NodeType.None)

    let name = match Cache.by_hash chart.Hash Library.cache with Some cc -> cc.Title | None -> sprintf "<%s>" chart.Id
    let grade_name = ruleset.GradeName grade
    let grade_color = ruleset.GradeColor grade

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        Text.drawFillB(Style.font, name, this.Bounds.Shrink(10.0f, 5.0f), Colors.text, Alignment.LEFT)
        Text.drawFillB(Style.font, grade_name, this.Bounds.TrimRight(100.0f).SliceRight(100.0f).Shrink(10.0f, 5.0f), (grade_color, Colors.shadow_2), Alignment.CENTER)
        Text.drawFillB(Style.font, sprintf "%.2f" rating, this.Bounds.Shrink(10.0f, 5.0f), Colors.text, Alignment.RIGHT)

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

                let table_breakdown_items = FlowContainer.Vertical<TableLevelStats>(30.0f)
                let table_breakdown = ScrollContainer.Flow(table_breakdown_items)
                let biggest_level = table_level_data |> Array.map (fun (l, d) -> d |> Array.map snd |> Array.sum) |> Array.max |> float32
                for (l, d) in table_level_data do
                    table_breakdown_items.Add(TableLevelStats(l, d, ruleset, d |> Array.map snd |> Array.sum |> fun t -> float32 t / biggest_level))

                let table_bests_items = FlowContainer.Vertical<TableScore>(50.0f)
                let table_bests = ScrollContainer.Flow(table_bests_items)
                for (chart, grade, rating) in top_scores do
                    table_bests_items.Add(TableScore(chart, grade, rating, ruleset))

                let swap = SwapContainer(Current = table_breakdown, Position = Position.TrimTop(120.0f).Margin(40.0f))

                let button(label: string, cmp) =
                    StylishButton((fun () -> swap.Current <- cmp), K label, fun () -> if swap.Current = cmp then Colors.black.O2 else Colors.black.O1)

                this
                |+ Text(t.Name, Position = Position.SliceTop(120.0f).Margin(40.0f, 10.0f), Align = Alignment.LEFT)
                |+ Text(L"stats.table.skill_level", Position = Position.Row(10.0f, 40.0f).Margin(40.0f, 0.0f), Align = Alignment.RIGHT)
                |+ Text(sprintf "%.2f" table_rating, Position = Position.Row(50.0f, 60.0f).Margin(40.0f, 0.0f), Align = Alignment.RIGHT)
                |+
                    (
                        GridContainer(50.0f, 4, Position = Position.Row(110.0f, 50.0f).Margin(40.0f, 0.0f), Spacing = (25.0f, 0.0f))
                        |+ (button(sprintf "%s %s" Icons.stats (L"stats.table.breakdown"), table_breakdown) |> fun b -> b.TiltLeft <- false; b)
                        |+ button(sprintf "%s %s" Icons.stats_2 (L"stats.table.ratings"), table_bests)
                        |+ button(sprintf "%s %s" Icons.sparkle (L"stats.table.leaderboard"), Leaderboard())
                        |+ (button(sprintf "%s %s" Icons.multiplayer (L"stats.table.friends"), FriendComparer(ruleset, score_data)) |> fun b -> b.TiltRight <- false; b)
                    )
                |* swap
            | None -> this |* EmptyState(Icons.x, L"stats.table.missing_ruleset")
        | None -> 
            this
            |* EmptyState(Icons.table, L"stats.table.no_table")
        base.Init parent