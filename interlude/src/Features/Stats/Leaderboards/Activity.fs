namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

type PlayerXP(rank: int, username: string, color: Color, xp: int64, playtime: float) =
    inherit StaticWidget(NodeType.None)

    let is_you = username = Network.credentials.Username
    let playtime = format_long_time playtime
    let level = xp |> current_level
    let xp_to_next_level = (xp_for_level (level + 1) - xp_for_level level |> float32) / 1000.0f
    let current_xp = (xp - xp_for_level level |> float32) / 1000.0f
    let bar_percent = current_xp / xp_to_next_level

    let NUMBER_WIDTH = 50.0f * 1.5f

    static member HEIGHT = 50.0f

    override this.Draw() =
        let bounds = if is_you then this.Bounds else this.Bounds.ShrinkX(20.0f)
        Render.rect (if is_you then this.Bounds else bounds) Colors.shadow_2.O2
        if is_you then
            Render.rect (bounds.BorderT Style.PADDING) Colors.white
            Render.rect (bounds.BorderB Style.PADDING) Colors.white

        Text.fill_b(Style.font, sprintf "#%i" rank, bounds.SliceL(NUMBER_WIDTH).Shrink(10.0f, 5.0f), Colors.text_subheading, Alignment.LEFT)

        let bounds = bounds.ShrinkL(NUMBER_WIDTH)

        Text.fill_b(Style.font, username, bounds.SlicePercentL(0.33f).Shrink(10.0f, 5.0f), (color, Colors.shadow_2), Alignment.LEFT)

        Text.fill_b(Style.font, sprintf "%i" level, bounds.SlicePercentX(0.33f).Shrink(10.0f, 5.0f).SliceL(NUMBER_WIDTH), Colors.text_subheading, Alignment.CENTER)

        let bar = bounds.SlicePercentX(0.33f).ShrinkX(10.0f).ShrinkL(NUMBER_WIDTH + 10.0f).SliceY(10.0f)
        Render.rect (bar.Translate(5.0f, 5.0f)) Colors.black
        Render.rect bar Colors.cyan_shadow
        let x, y = Mouse.pos()
        if x > bar.Left && x < bar.Right && y > bounds.Top && y < bounds.Bottom then
            Render.rect (bar.SlicePercentL bar_percent) Colors.cyan_accent.O1
            Text.fill_b (Style.font, sprintf "%.1fk / %.1fk" current_xp xp_to_next_level, bar.SliceY(30.0f), Colors.text, Alignment.CENTER)
        else
            Render.rect (bar.SlicePercentL bar_percent) Colors.cyan_accent

        Text.fill_b(Style.font, playtime, bounds.SlicePercentR(0.33f).Shrink(10.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)

type ActivityHeader(by_playtime: bool, change_sort: bool -> unit) =
    inherit Container(NodeType.None)

    let playtime =
        let pos = Position.SlicePercentR(0.33f).Shrink(10.0f, 5.0f)
        if by_playtime then
            Text(Icons.CHEVRON_DOWN + " " + %"stats.leaderboards.activity.playtime")
                .Color(Colors.text_cyan_2)
                .Align(Alignment.RIGHT)
                .Position(pos)
            :> Widget
        else
            Button(
                %"stats.leaderboards.activity.playtime",
                (fun () -> change_sort true)
            )
                .Align(Alignment.RIGHT)
                .Position(pos)

    let xp =
        let pos = Position.SlicePercentX(0.33f).Shrink(10.0f, 5.0f)
        if by_playtime then
            Button(
                %"stats.leaderboards.activity.xp",
                (fun () -> change_sort false)
            )
                .Align(Alignment.CENTER)
                .Position(pos)
            :> Widget
        else
            Text(Icons.CHEVRON_DOWN + " " + %"stats.leaderboards.activity.xp")
                .Color(Colors.text_cyan_2)
                .Align(Alignment.CENTER)
                .Position(pos)

    let player =
        Text(%"stats.leaderboards.activity.player")
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.SlicePercentL(0.33f).ShrinkL(PlayerXP.HEIGHT * 1.5f).Shrink(10.0f, 5.0f))

    override this.Init(parent) =
        this
        |+ player
        |+ xp
        |* playtime
        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds Colors.black.O2
        base.Draw()

type private ActivityLeaderboard() =
    inherit Container(NodeType.None)

    let by_playtime = Setting.simple false
    let monthly = Setting.simple false

    let load (container: WebRequestContainer<_>) =
        if Network.status = Network.Status.LoggedIn then
            (if monthly.Value then Stats.Leaderboard.MonthlyXP.get else Stats.Leaderboard.XP.get)
                (
                    by_playtime.Value,
                    fun response ->
                        GameThread.defer
                        <| fun () ->
                            match response with
                            | Some result -> container.SetData result
                            | None -> container.ServerError()
                )
        else
            container.Offline()

    let rerender (container: WebRequestContainer<_>) (data: Stats.Leaderboard.XPResponse) =

        let title =
            Text(
                if monthly.Value then
                    %"stats.leaderboards.activity.monthly"
                    + "  -  "
                    + (Timestamp.now() |> Timestamp.to_datetime).ToString("MMMM yyyy")
                else
                    %"stats.leaderboards.activity.all_time"
            )
                .Position(Position.SliceT(70.0f).ShrinkX(5.0f))
        let header =
            ActivityHeader(by_playtime.Value, fun v -> by_playtime.Set v; container.Reload())
                .Position(Position.ShrinkT(80.0f).SliceT(45.0f).ShrinkX(20.0f))

        let flow = FlowContainer.Vertical<Widget>(PlayerXP.HEIGHT).Spacing(Style.PADDING)
        for i, d in Seq.indexed data.Leaderboard do
            PlayerXP(i + 1, d.Username, Color.FromArgb(d.Color), d.XP, d.Playtime)
            |> flow.Add

        match data.You with
        | Some (rank, you) ->
            Container(NodeType.None)
            |+ title
            |+ header

            |+ ScrollContainer(flow)
                .Position(Position.ShrinkT(125.0f).ShrinkB(PlayerXP.HEIGHT + Style.PADDING))

            |+ PlayerXP(int32 rank, you.Username, Color.FromArgb(you.Color), you.XP, you.Playtime)
                .Position(Position.SliceB(PlayerXP.HEIGHT))
        | None ->
            Container(NodeType.None)
            |+ title
            |+ header
            |+ ScrollContainer(flow)
                .Position(Position.ShrinkT(125.0f))
        :> Widget

    let container = WebRequestContainer<Stats.Leaderboard.XPResponse>(load, rerender)

    let by_playtime = by_playtime |> Setting.trigger (fun _ -> container.Reload())
    let monthly = monthly |> Setting.trigger (fun _ -> container.Reload())

    member this.SortByPlaytime = by_playtime
    member this.MonthlyLeaderboard = monthly

    override this.Init(parent) =
        this
        |* container
        base.Init parent