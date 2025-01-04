namespace Interlude.Features.Import.osu

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Data.OsuClientInterop
open Prelude.Data.User
open Interlude.UI
open System.Text.RegularExpressions


module Replays =

    let parse_replay_file (replay_path: string) =
        try
            Some(OsuScoreDatabase_Score.ReadReplay replay_path)
        with err ->
            Logging.Error "Error loading replay file %s: %O" replay_path err
            None

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
    let private RATE_REGEX =
        Regex(
            """((^|\s)([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)($|\s))|(x([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?))|(([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)[x\]])"""
        )

    let detect_rate_mod (difficulty_name: string) : float32<rate> option =
        let m = RATE_REGEX.Match difficulty_name

        if m.Success then
            let r = m.Value.Trim([| ' '; 'x'; ']' |]).Replace(',', '.')

            match Single.TryParse r with
            | true, r -> Some (r * 1.0f<rate>)
            | false, _ -> None
        else
            None
open Replays

type ImportReplayPage(replay: OsuScoreDatabase_Score, chart: Chart, show_replay: Score -> unit) =
    inherit Page()
    let extractRate (rateOpt: float32<rate> option) : float32<rate> =
        rateOpt |> Option.defaultValue 1.0f<rate>

    let detectedRate = detect_rate_mod replay.Path.Value
    let extractedRate = extractRate detectedRate    
    let rate = Setting.bounded (0.5f<rate>, 3.0f<rate>) extractedRate |> Setting.roundf_uom 2

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
