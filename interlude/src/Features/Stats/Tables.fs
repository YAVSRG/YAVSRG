namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets
open Prelude.Backbeat
open Prelude.Data.User
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

// todo: use new the web request container component
type private WebRequestState =
    | Offline = 0
    | Loading = 1
    | ServerError = 2
    | Loaded = 3

type private Leaderboard() =
    inherit Container(NodeType.None)

    let mutable status = WebRequestState.Loading

    override this.Init(parent) =

        let contents = FlowContainer.Vertical<Widget>(70.0f)

        if Network.status = Network.Status.LoggedIn then
            Tables.Leaderboard.get (
                Content.Table.Value.Id,
                fun response ->
                    defer
                    <| fun () ->
                        match response with
                        | Some data ->
                            status <- WebRequestState.Loaded

                            for player in data.Players do
                                contents.Add(
                                    Container(NodeType.None)
                                    |+ Text(
                                        player.Username,
                                        Color = K(Color.FromArgb player.Color, Colors.shadow_2),
                                        Align = Alignment.LEFT,
                                        Position = Position.Shrink(100.0f, 5.0f)
                                    )
                                    |+ Text(
                                        "#" + player.Rank.ToString(),
                                        Align = Alignment.LEFT,
                                        Position = Position.Shrink(10.0f, 5.0f)
                                    )
                                    |+ Text(
                                        sprintf "%.2f" player.Rating,
                                        Align = Alignment.RIGHT,
                                        Position = Position.Shrink(10.0f, 5.0f)
                                    )
                                )
                        | None -> status <- WebRequestState.ServerError
            )
        else
            status <- WebRequestState.Offline

        this
        |+ LoadingState().Conditional(fun () -> status = WebRequestState.Loading)
        |+ EmptyState(Icons.GLOBE, %"misc.offline").Conditional(fun () -> status = WebRequestState.Offline)
        |+ EmptyState(Icons.GLOBE, %"misc.server_error").Conditional(fun () -> status = WebRequestState.ServerError)
        |* ScrollContainer(contents)

        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        base.Draw()

type private CompareFriend
    (
        ruleset: Ruleset,
        data_by_level: (int * (string * (int * float) option) array) array,
        name: string,
        on_back: unit -> unit
    ) =
    inherit Container(NodeType.None)

    let mutable status = WebRequestState.Loading

    override this.Init(parent) =

        let contents = FlowContainer.Vertical<Widget>(50.0f)

        if Network.status = Network.Status.LoggedIn then
            let table = Content.Table.Value

            Tables.Records.get (
                name,
                table.Id,
                fun response ->
                    defer
                    <| fun () ->
                        match response with
                        | Some data ->
                            status <- WebRequestState.Loaded

                            let their_scores =
                                data.Scores
                                |> Seq.map (fun score -> score.Hash, (score.Grade, score.Score))
                                |> Map.ofSeq

                            for level, your_scores in data_by_level do
                                contents.Add(
                                    Text(
                                        "-- " + table.Info.LevelName level + " --",
                                        Align = Alignment.CENTER,
                                        Color = K Colors.text_greyout
                                    )
                                )

                                for chart_id, your_score in your_scores do
                                    let their_score = Map.tryFind chart_id their_scores

                                    let name =
                                        match ChartDatabase.get_meta chart_id Content.Charts
                                        with
                                        | Some cc -> cc.Title
                                        | None -> sprintf "<%s>" chart_id

                                    let delta =
                                        if your_score.IsSome && their_score.IsSome then
                                            snd your_score.Value - snd their_score.Value
                                        else
                                            0.0

                                    contents.Add(
                                        Container(NodeType.None)
                                        |+ Text(name, Align = Alignment.CENTER, Position = Position.Shrink(0.0f, 5.0f))
                                        |+ Text(
                                            (match your_score with
                                             | None -> "--"
                                             | Some(_, acc) -> ruleset.FormatAccuracy acc),
                                            Color =
                                                K(
                                                    ruleset.GradeColor(
                                                        match your_score with
                                                        | None -> -1
                                                        | Some(grade, acc) -> grade
                                                    ),
                                                    Colors.shadow_2
                                                ),
                                            Align = Alignment.LEFT
                                        )
                                        |+ Text(
                                            (if delta <> 0.0 then
                                                 sprintf "+%.2f%%" (abs delta * 100.0)
                                             else
                                                 ""),
                                            Color = K Colors.text_green,
                                            Position = Position.Shrink(150.0f, 0.0f),
                                            Align = if delta > 0 then Alignment.LEFT else Alignment.RIGHT
                                        )
                                        |+ Text(
                                            (match their_score with
                                             | None -> "--"
                                             | Some(_, acc) -> ruleset.FormatAccuracy acc),
                                            Color =
                                                K(
                                                    ruleset.GradeColor(
                                                        match their_score with
                                                        | None -> -1
                                                        | Some(grade, acc) -> grade
                                                    ),
                                                    Colors.shadow_2
                                                ),
                                            Align = Alignment.RIGHT
                                        )
                                    )
                        | None -> status <- WebRequestState.ServerError
            )
        else
            status <- WebRequestState.Offline

        this
        |+ LoadingState().Conditional(fun () -> status = WebRequestState.Loading)
        |+ EmptyState(Icons.GLOBE, %"misc.offline").Conditional(fun () -> status = WebRequestState.Offline)
        |+ EmptyState(Icons.GLOBE, %"misc.server_error").Conditional(fun () -> status = WebRequestState.ServerError)
        |+ Text(
            "Comparing to " + name,
            Align = Alignment.RIGHT,
            Position = Position.SliceT(50.0f).Shrink(20.0f, 0.0f)
        )
        |+ Button(K(Icons.ARROW_LEFT_CIRCLE + " Back"), on_back, Position = Position.Box(0.0f, 0.0f, 0.0f, 0.0f, 200.0f, 50.0f))
        |* ScrollContainer(contents, Position = Position.Shrink(10.0f, 0.0f).ShrinkT(55.0f))

        base.Init parent


type private FriendComparer(ruleset: Ruleset, score_data: (int * string * int option * float option) array) =
    inherit Container(NodeType.None)

    let mutable status = WebRequestState.Loading
    let mutable friends: Friends.List.Friend array option = None

    let rs_hash = Ruleset.hash ruleset

    let score_of (hash: string) =
        (UserDatabase.get_chart_data hash Content.UserData).PersonalBests
        |> Bests.ruleset_best_above rs_hash (_.Accuracy) 1.0f<rate>
        |> Option.get
        |> fun (accuracy, _, _) -> accuracy

    let data_by_level =
        score_data
        |> Array.groupBy (fun (l, _, _, _) -> l)
        |> Array.map (fun (l, data) ->
            (l,
             data
             |> Array.map (fun (_, chart_id, grade, _) ->
                 if grade.IsSome then
                     chart_id, Some(grade.Value, score_of chart_id)
                 else
                     chart_id, None
             ))
        )
        |> Array.sortBy fst

    let friend (name: string, on_compare: unit -> unit) =
        Container(NodeType.None)
        |+ Text(name, Position = Position.Shrink(20.0f, 5.0f), Align = Alignment.LEFT)
        |+ Button(K "Compare >", on_compare, Position = Position.SliceR(250.0f).Shrink(5.0f))

    override this.Init(parent) =

        let friends_list = FlowContainer.Vertical(70.0f)
        let swap = SwapContainer(friends_list)

        if Network.status = Network.Status.LoggedIn then
            Friends.List.get (fun response ->
                defer
                <| fun () ->
                    match response with
                    | Some data ->
                        friends <- Some data.Friends

                        for f in data.Friends do
                            friends_list.Add(
                                friend (
                                    f.Username,
                                    fun () ->
                                        swap.Current <-
                                            CompareFriend(
                                                ruleset,
                                                data_by_level,
                                                f.Username,
                                                fun () -> swap.Current <- friends_list
                                            )
                                )
                            )

                        status <- WebRequestState.Loaded
                    | None -> status <- WebRequestState.ServerError
            )
        else
            status <- WebRequestState.Offline

        this
        |+ LoadingState().Conditional(fun () -> status = WebRequestState.Loading)
        |+ EmptyState(Icons.GLOBE, %"misc.offline").Conditional(fun () -> status = WebRequestState.Offline)
        |+ EmptyState(Icons.GLOBE, %"misc.server_error").Conditional(fun () -> status = WebRequestState.ServerError)
        |+ EmptyState(Icons.USERS, %"stats.table.friends.empty", Subtitle = %"stats.table.friends.empty.subtitle")
            .Conditional(fun () -> status = WebRequestState.Loaded && friends.Value.Length = 0)
        |* swap

        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        base.Draw()

type private TableLevelStats(level_name: string, data: (int * int) array, ruleset: Ruleset, scale: float32) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect this.Bounds Colors.black.O2
        let b = this.Bounds.Shrink(20.0f, 0.0f)
        Text.fill_b (Style.font, level_name, b.SliceL(150.0f), Colors.text_subheading, Alignment.CENTER)

        let b = this.Bounds.ShrinkL(170.0f)
        let mutable x = b.Left
        let total = data |> Array.sumBy snd |> float32

        for (grade, count) in data do
            let w = b.Width * float32 count / total * scale
            Draw.rect (Rect.Create(x, b.Top, x + w, b.Bottom)) (ruleset.GradeColor grade)
            x <- x + w

type private TableScore(position: int, chart_id: string, grade: int, rating: float, ruleset: Ruleset) =
    inherit StaticWidget(NodeType.None)

    let name =
        match ChartDatabase.get_meta chart_id Content.Charts with
        | Some cc -> cc.Title
        | None -> sprintf "<%s>" chart_id

    let grade_name = ruleset.GradeName grade
    let grade_color = ruleset.GradeColor grade

    override this.Draw() =
        Draw.rect
            this.Bounds
            (if position % 10 < 5 then
                 Colors.black.O2
             else
                 Colors.shadow_2.O2)

        let text_color = if position < 50 then Colors.text else Colors.text_greyout
        Text.fill_b (Style.font, name, this.Bounds.Shrink(10.0f, 5.0f), text_color, Alignment.LEFT)

        Text.fill_b (
            Style.font,
            grade_name,
            this.Bounds.ShrinkR(100.0f).SliceR(100.0f).Shrink(10.0f, 5.0f),
            (grade_color, Colors.shadow_2),
            Alignment.CENTER
        )

        Text.fill_b (Style.font, sprintf "%.2f" rating, this.Bounds.Shrink(10.0f, 5.0f), text_color, Alignment.RIGHT)

type private TableStats() =
    inherit Container(NodeType.None)

    let table = Content.Table

    let score_data =
        match table with
        | Some table ->
            Table.ratings
                (fun chart_id ->
                    (UserDatabase.get_chart_data chart_id Content.UserData).PersonalBests
                    |> Bests.ruleset_best_above table.Info.RulesetId (_.Grade) 1.0f<rate>
                    |> Option.map (fun (grade, _, _) -> grade)
                )
                table
            |> Array.ofSeq
        | None -> [||]

    let top_scores =
        score_data
        |> Seq.choose (fun (_, c, g, s) -> if s.IsSome then Some(c, g.Value, s.Value) else None)
        |> Seq.sortByDescending (fun (_, _, s) -> s)
        |> Seq.truncate 100
        |> Array.ofSeq

    let table_rating =
        top_scores
        |> Seq.truncate 50
        |> Seq.sumBy (fun (_, _, s) -> s)
        |> fun total -> total / 50.0

    let table_level_data =
        score_data
        |> Seq.groupBy (fun (l, _, _, _) -> l)
        |> Seq.map (fun (l, data) ->
            (l,
             data
             |> Seq.map (fun (_, _, g, _) -> Option.defaultValue -1 g)
             |> Seq.countBy id
             |> Seq.sortDescending
             |> Array.ofSeq)
        )
        |> Seq.sortBy fst
        |> Array.ofSeq

    override this.Init(parent) =

        match table with
        | Some table ->
            match Rulesets.by_hash table.Info.RulesetId with
            | Some ruleset ->

                let table_breakdown_items = FlowContainer.Vertical<TableLevelStats>(30.0f)
                let table_breakdown = ScrollContainer(table_breakdown_items)

                let biggest_level =
                    table_level_data
                    |> Array.map (fun (l, d) -> d |> Array.map snd |> Array.sum)
                    |> Array.max
                    |> float32

                for (l, d) in table_level_data do
                    table_breakdown_items.Add(
                        TableLevelStats(
                            table.Info.LevelName l,
                            d,
                            ruleset,
                            d |> Array.map snd |> Array.sum |> (fun t -> float32 t / biggest_level)
                        )
                    )

                let table_bests_items = FlowContainer.Vertical<TableScore>(50.0f)
                let table_bests = ScrollContainer(table_bests_items)

                for i, (chart_id, grade, rating) in top_scores |> Array.indexed do
                    table_bests_items.Add(TableScore(i, chart_id, grade, rating, ruleset))

                let swap =
                    SwapContainer(table_breakdown, Position = Position.ShrinkT(120.0f).Shrink(40.0f))

                let button (label: string, cmp) =
                    StylishButton(
                        (fun () -> swap.Current <- cmp),
                        K label,
                        fun () ->
                            if swap.Current = cmp then
                                Colors.black.O2
                            else
                                Colors.black.O1
                    )

                this
                |+ Text(
                    table.Info.Name,
                    Position = Position.SliceT(120.0f).Shrink(40.0f, 10.0f),
                    Align = Alignment.LEFT
                )
                |+ Text(
                    %"stats.table.skill_level",
                    Position = Position.Row(10.0f, 40.0f).Shrink(40.0f, 0.0f),
                    Align = Alignment.RIGHT
                )
                |+ Text(
                    sprintf "%.2f" table_rating,
                    Position = Position.Row(50.0f, 60.0f).Shrink(40.0f, 0.0f),
                    Align = Alignment.RIGHT
                )
                |+ (GridFlowContainer(
                        50.0f,
                        4,
                        Position = Position.Row(110.0f, 50.0f).Shrink(40.0f, 0.0f),
                        Spacing = (25.0f, 0.0f)
                    )
                    |+ (button (sprintf "%s %s" Icons.BAR_CHART (%"stats.table.breakdown"), table_breakdown)
                        |> fun b ->
                            b.TiltLeft <- false
                            b)
                    |+ button (sprintf "%s %s" Icons.TRENDING_UP (%"stats.table.ratings"), table_bests)
                    |+ button (sprintf "%s %s" Icons.AWARD (%"stats.table.leaderboard"), Leaderboard())
                    |+ (button (
                            sprintf "%s %s" Icons.USERS (%"stats.table.friends"),
                            FriendComparer(ruleset, score_data)
                        )
                        |> fun b ->
                            b.TiltRight <- false
                            b))
                |* swap
            | None -> this |* EmptyState(Icons.X, %"stats.table.missing_ruleset")
        | None -> this |* EmptyState(Icons.SIDEBAR, %"stats.table.no_table")

        base.Init parent
