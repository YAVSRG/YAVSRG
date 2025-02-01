namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
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
    let bar_percent =
        let xp_to_next_level = xp_for_level (level + 1) - xp_for_level level
        let current_xp = xp - xp_for_level level
        float32 current_xp / float32 xp_to_next_level

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
        Render.rect (bar.SlicePercentL bar_percent) Colors.cyan_accent

        Text.fill_b(Style.font, playtime, bounds.SlicePercentR(0.33f).Shrink(10.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)

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

        let flow = FlowContainer.Vertical<Widget>(PlayerXP.HEIGHT, Spacing = Style.PADDING)
        for i, d in Seq.indexed data.Leaderboard do
            PlayerXP(i + 1, d.Username, Color.FromArgb(d.Color), d.XP, d.Playtime)
            |> flow.Add

        match data.You with
        | Some (rank, you) ->
            Container(NodeType.None)
            |+ ScrollContainer(flow, Position = Position.ShrinkB(PlayerXP.HEIGHT + Style.PADDING))
            |+ PlayerXP(int32 rank, you.Username, Color.FromArgb(you.Color), you.XP, you.Playtime, Position = Position.SliceB(PlayerXP.HEIGHT))
            :> Widget
        | None ->
            ScrollContainer(flow)

    let container = WebRequestContainer<Stats.Leaderboard.XPResponse>(load, rerender)

    let by_playtime = by_playtime |> Setting.trigger (fun _ -> container.Reload())
    let monthly = monthly |> Setting.trigger (fun _ -> container.Reload())

    member this.SortByPlaytime = by_playtime
    member this.MonthlyLeaderboard = monthly

    override this.Init(parent) =
        this
        |* container
        base.Init parent