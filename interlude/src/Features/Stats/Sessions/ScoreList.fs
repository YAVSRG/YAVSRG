namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Calculator
open Prelude.Data.User
open Prelude.Data.User.Stats
open Prelude.Data.Library
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Score
open Interlude.Features.Collections

type MissingScore() =
    inherit StaticWidget(NodeType.None)

    let fade = Animation.Fade(0.0f, Target = 1.0f)

    override this.Draw() =
        let alpha = fade.Alpha

        let fill_color =
            if this.Focused then
                Colors.yellow_accent.O1a alpha
            else
                (!*Palette.DARK).O1a alpha

        let border_color =
            if this.Focused then
                Colors.yellow_accent.O2a alpha
            else
                (!*Palette.LIGHT).O2a alpha

        Render.rect this.Bounds fill_color
        Render.rect (this.Bounds.BorderL Style.PADDING) border_color

        Text.fill_b(Style.font, %"stats.missing_score", this.Bounds.SliceY(50.0f), (Colors.grey_2.O4a alpha, Colors.shadow_2.O4a alpha), Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms

type Score(score_info: ScoreInfo) =
    inherit Container(
        NodeType.Button (fun () ->
            Style.click.Play()
            SelectedChart.change (score_info.ChartMeta, LibraryContext.None, true)
            SelectedChart.when_loaded
                true
                (fun _ ->
                    if Screen.change_new
                        (fun () -> ScoreScreen(score_info, (ImprovementFlags.None, None), false))
                        ScreenType.Score
                        Transitions.EnterGameplayNoFadeAudio
                    then Menu.Exit()
                )
        )
    )

    let mod_string = score_info.ModString()

    let fade = Animation.Fade(0.0f, Target = 1.0f)

    member this.ScoreInfo = score_info

    override this.Draw() =
        let alpha = fade.Alpha

        let fill_color =
            if this.Focused then
                Colors.yellow_accent.O1a alpha
            else
                (!*Palette.DARK).O1a alpha

        let border_color =
            if this.Focused then
                Colors.yellow_accent.O2a alpha
            else
                (!*Palette.LIGHT).O2a alpha

        Render.rect this.Bounds fill_color
        Render.rect (this.Bounds.BorderL Style.PADDING) border_color

        Text.fill_b (
            Style.font,
            score_info.ChartMeta.Title,
            this.Bounds.SliceT(35.0f).ShrinkR(420.0f).ShrinkL(10.0f),
            (Colors.white.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            score_info.ChartMeta.Artist + "  •  " + score_info.ChartMeta.Creator,
            this.Bounds.ShrinkT(30.0f).SliceT(30.0f).ShrinkR(420.0f).ShrinkL(10.0f).TranslateY(-2.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            score_info.ChartMeta.DifficultyName,
            this.Bounds.SliceB(25.0f).ShrinkR(420.0f).ShrinkL(10.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.LEFT
        )

        let box_color = Colors.shadow_2.O1a alpha
        let box = this.Bounds.SliceR(360.0f).ShrinkT(50.0f).TranslateX(-10.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            if this.Focused then
                if this.FocusedByMouse then %"scoreboard.view_hint_mouse" else [(%%"select").ToString()] %> "scoreboard.view_hint_keyboard"
            else
                mod_string
            ,
            this.Bounds.ShrinkT(50.0f).SliceR(370.0f).ShrinkX(10.0f),
            (Colors.grey_1.O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box = this.Bounds.SliceR(140.0f).SliceT(45.0f).TranslateX(-230.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            score_info.Scoring.FormattedAccuracy,
            box.ShrinkY(5.0f),
            ((score_info.Ruleset.GradeColor score_info.Grade).O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box = this.Bounds.SliceR(100.0f).SliceT(45.0f).TranslateX(-120.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            score_info.Ruleset.LampName score_info.Lamp,
            box.ShrinkY(5.0f),
            ((score_info.Ruleset.LampColor score_info.Lamp).O4a alpha, Colors.shadow_2.O4a alpha),
            Alignment.CENTER
        )

        let box = this.Bounds.SliceR(100.0f).SliceT(45.0f).TranslateX(-10.0f)
        Render.rect box box_color
        Text.fill_b (
            Style.font,
            (sprintf "%.2f" score_info.Physical),
            box.ShrinkY(5.0f),
            (Colors.white.O4a alpha, (Difficulty.color score_info.Physical).O4a alpha),
            Alignment.CENTER
        )

    override this.OnFocus (by_mouse: bool) =
        Style.hover.Play()
        base.OnFocus(by_mouse)

    override this.Init(parent: Widget) =
        this |* MouseListener().Button(this)

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        fade.Update elapsed_ms

module private ScoreList =

    let loader =
        { new Async.CancelQueueSeq<int64 * int64 * Ruleset * (Widget -> unit) * (unit -> unit), unit -> unit>() with
            member this.Process((start_time, end_time, ruleset, callback, callback_when_done)) =
                seq {
                    for chart_hash, score in UserDatabase.get_scores_between start_time end_time Content.UserData do
                        match ChartDatabase.get_meta_cached chart_hash Content.Charts with
                        | Some chart_meta ->
                            match ChartDatabase.get_chart chart_hash Content.Charts with
                            | Ok chart ->
                                let score_info = ScoreInfo.from_score chart_meta chart ruleset score
                                yield fun () -> callback(Score(score_info))
                            | _ -> ()
                        | _ -> yield fun () -> callback(MissingScore())
                    yield callback_when_done
                }

            member this.Handle(action) = action ()
        }

type ScoreList(start_time: int64, end_time: int64) =
    inherit Container(NodeType.None)

    let scores = FlowContainer.Vertical<Widget>(80.0f, Spacing = 5.0f)

    let make_playlist() =
        let date = timestamp_to_rg_calendar_day start_time
        CreatePlaylistPage(sprintf "Session on %s" (date.ToShortDateString()), fun (_, collection) ->
            match collection with
            | Playlist p ->
                scores.Iter(
                    function
                    | :? Score as s -> p.Add(s.ScoreInfo.ChartMeta, s.ScoreInfo.Rate, s.ScoreInfo.Mods) |> ignore
                    | _ -> ()
                )
            | _ -> ()
        ).Show()

    override this.Init(parent: Widget) =
        let mutable finished_loading = false
        let mutable has_scores = false

        this
        |+ ScrollContainer(scores)
            .Margin(Style.PADDING)
            .Position(Position.Shrink(20.0f))
        |+ Button(Icons.LIST + " " + %"stats.session.make_playlist", make_playlist)
            .Align(Alignment.RIGHT)
            .Floating()
            .Position(Position.BorderB(75.0f).Shrink(20.0f, 10.0f).SliceR(400.0f))
            .Conditional(fun () -> has_scores)
        |* EmptyState(Icons.WIND, "No scores this session")
            .Position(Position.ShrinkT(160.0f))
            .Conditional(fun () -> finished_loading && not has_scores)

        ScoreList.loader.Request((start_time, end_time, Rulesets.current, (fun s -> has_scores <- true; scores.Add s), fun () -> finished_loading <- true))

        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ScoreList.loader.Join()