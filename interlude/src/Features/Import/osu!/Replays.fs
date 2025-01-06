namespace Interlude.Features.Import.osu

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Data.OsuClientInterop
open Prelude.Data.User
open Interlude.UI

module Replays =

    // todo: should this be in prelude?
    let convert_replay_to_score (replay: OsuScoreDatabase_Score) (chart: Chart) (osu_chart_rate: float32<rate>) : Result<Score, string> =
        match Mods.to_interlude_rate_and_mods replay.ModsUsed with
        | None -> Error "Invalid mods used in replay"
        | Some(rate, mods) ->

        try
            let replay_data = OsuReplay.decode_replay(replay, chart.FirstNote, osu_chart_rate)

            Ok {
                Timestamp =
                    DateTime.FromFileTimeUtc(replay.Timestamp).ToLocalTime()
                    |> Timestamp.from_datetime
                Replay = Replay.compress_bytes replay_data
                Rate = MathF.Round(float32 rate, 2) * osu_chart_rate
                Mods = mods
                IsImported = true
                IsFailed = false
                Keys = chart.Keys
            }
        with err -> Error err.Message

type ImportReplayPage(replay: OsuScoreDatabase_Score, chart: Chart, show_replay: Score -> unit) =
    inherit Page()

    let detected_rate =
        Data.Library.Imports.detect_rate_mod replay.FilePath.Value
        |> Option.defaultValue 1.0f<rate>

    // todo: even better detection using the fact that replay file contains md5 of original .osu
    // strategy 1: store original .osu md5s on all osu imports, search for it
    // strategy 2: if strategy 1 fails, open up the osu!.db if available, search for chart, convert and load it temporarily
    // if both strategies fail warn user that it's gonna try its best on the selected chart but that may be wrong

    let rate = Setting.bounded (0.5f<rate>, 3.0f<rate>) detected_rate |> Setting.roundf_uom 2

    let import() =
        match Replays.convert_replay_to_score replay chart rate.Value with
        | Ok score -> show_replay score
        | Error reason -> Notifications.error ("Replay import failed", reason)

    override this.Content() =
        page_container()
        |+ PageSetting("Rate", Slider(Setting.uom rate)).Pos(0)
        |+ PageButton("Import!", import).Pos(3)
        :> Widget
    override this.Title = "Import replay"
    override this.OnClose() = ()