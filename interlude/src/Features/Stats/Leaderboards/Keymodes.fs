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

type PlayerInfo(rank: int, data: Stats.Leaderboard.KeymodeLeaderboardEntry) =
    inherit StaticWidget(NodeType.None)

    let is_you = data.Username = Network.credentials.Username
    let playtime = format_long_time data.Playtime
    let color = Color.FromArgb(data.Color)

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

        Text.fill_b(Style.font, data.Username, bounds.SlicePercentL(0.25f).Shrink(10.0f, 5.0f), (color, Colors.shadow_2), Alignment.LEFT)

        Text.fill_b(Style.font, sprintf "%.0f" data.Combined, bounds.SlicePercentL(0.25f, 0.15f).Shrink(10.0f, 5.0f), Colors.text, Alignment.LEFT)
        Text.fill_b(Style.font, sprintf "%.0f" data.Jacks, bounds.SlicePercentL(0.4f, 0.15f).Shrink(10.0f, 5.0f), Colors.text_red, Alignment.LEFT)
        Text.fill_b(Style.font, sprintf "%.0f" data.Chordstream, bounds.SlicePercentL(0.55f, 0.15f).Shrink(10.0f, 5.0f), Colors.text_green, Alignment.LEFT)
        Text.fill_b(Style.font, sprintf "%.0f" data.Stream, bounds.SlicePercentL(0.7f, 0.15f).Shrink(10.0f, 5.0f), Colors.text_cyan, Alignment.LEFT)

        Text.fill_b(Style.font, playtime, bounds.SlicePercentR(0.20f).Shrink(10.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)

type KeymodeHeader(sort: Stats.Leaderboard.Sort, change_sort: Stats.Leaderboard.Sort -> unit) =
    inherit Container(NodeType.None)

    let button (for_sort: Stats.Leaderboard.Sort) (label: string) (pos: Position) =
        let align = if for_sort = Stats.Leaderboard.Sort.Playtime then Alignment.RIGHT else Alignment.CENTER
        if sort = for_sort then
            Text(Icons.CHEVRON_DOWN + " " + label)
                .Color(Colors.text_cyan_2)
                .Align(align)
                .Position(pos)
            :> Widget
        else
            Button(label, (fun () -> change_sort for_sort))
                .Align(align)
                .Position(pos)

    let player =
        Text(%"stats.leaderboards.rating.player")
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.SlicePercentL(0.25f).ShrinkL(PlayerInfo.HEIGHT * 1.5f).Shrink(10.0f, 5.0f))

    override this.Init(parent) =
        this
        |+ player
        |+ button Stats.Leaderboard.Sort.Combined %"stats.leaderboards.rating.combined" (Position.SlicePercentL(0.20f, 0.25f).ShrinkY(5.0f))
        |+ button Stats.Leaderboard.Sort.Jacks %"stats.leaderboards.rating.jacks" (Position.SlicePercentL(0.35f, 0.25f).ShrinkY(5.0f))
        |+ button Stats.Leaderboard.Sort.Chordstream %"stats.leaderboards.rating.chordstream" (Position.SlicePercentL(0.5f, 0.25f).ShrinkY(5.0f))
        |+ button Stats.Leaderboard.Sort.Stream %"stats.leaderboards.rating.stream" (Position.SlicePercentL(0.65f, 0.25f).ShrinkY(5.0f))
        |* button Stats.Leaderboard.Sort.Playtime %"stats.leaderboards.rating.playtime" (Position.SlicePercentR(0.20f).Shrink(10.0f, 5.0f))
        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds Colors.black.O2
        base.Draw()

type private KeymodesLeaderboard() =
    inherit Container(NodeType.None)

    let sort_by = Setting.simple Stats.Leaderboard.Sort.Combined
    let monthly = Setting.simple false
    let keymode = Setting.simple 4

    let load (container: WebRequestContainer<_>) =
        if Network.status = Network.Status.LoggedIn then
            (if monthly.Value then Stats.Leaderboard.MonthlyKeymode.get else Stats.Leaderboard.Keymode.get)
                (
                    keymode.Value,
                    sort_by.Value,
                    fun response ->
                        GameThread.defer
                        <| fun () ->
                            match response with
                            | Some result -> container.SetData result
                            | None -> container.ServerError()
                )
        else
            container.Offline()

    let rerender (container: WebRequestContainer<_>) (data: Stats.Leaderboard.KeymodeResponse) =

        let title =
            Text(
                if monthly.Value then
                    sprintf "%iK %s  -  %s"
                        keymode.Value
                        %"stats.leaderboards.rating.monthly"
                        ((Timestamp.now() |> Timestamp.to_datetime).ToString("MMMM yyyy"))
                else
                    sprintf "%iK %s"
                        keymode.Value
                        %"stats.leaderboards.rating.all_time"
            )
                .Position(Position.SliceT(70.0f).ShrinkX(5.0f))
        let header =
            KeymodeHeader(sort_by.Value, (fun v -> sort_by.Set v; container.Reload()))
                .Position(Position.ShrinkT(80.0f).SliceT(45.0f).ShrinkX(20.0f))
        let flow = FlowContainer.Vertical<Widget>(PlayerInfo.HEIGHT, Spacing = Style.PADDING)
        for i, d in Seq.indexed data.Leaderboard do
            PlayerInfo(i + 1, d)
            |> flow.Add

        match data.You with
        | Some (rank, you) ->
            Container(NodeType.None)
            |+ title
            |+ header
            |+ ScrollContainer(flow)
                .Position(Position.ShrinkT(125.0f).ShrinkB(PlayerInfo.HEIGHT + Style.PADDING))
            |+ PlayerInfo(int32 rank, you)
                .Position(Position.SliceB(PlayerInfo.HEIGHT))
        | None ->
            Container(NodeType.None)
            |+ title
            |+ header
            |+ ScrollContainer(flow)
                .Position(Position.ShrinkT(125.0f))
        :> Widget

    let container = WebRequestContainer<Stats.Leaderboard.KeymodeResponse>(load, rerender)

    let sort_by = sort_by |> Setting.trigger (fun _ -> container.Reload())
    let monthly = monthly |> Setting.trigger (fun _ -> container.Reload())
    let keymode = keymode |> Setting.trigger (fun _ -> container.Reload())

    member this.SortBy = sort_by
    member this.MonthlyLeaderboard = monthly
    member this.Keymode = keymode

    override this.Init(parent) =
        this
        |* container
        base.Init parent