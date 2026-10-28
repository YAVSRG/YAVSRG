namespace Prelude.Data.Imports

open System.IO
open System.Text
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Data
open Prelude.Data.UserDatabase
open Prelude.Data.Library
open Prelude.Data.OsuClientInterop

[<Struct>]
type ScoreImportResult =
    {
        Scores: int
        NewScores: int
        Maps: int
    }

    member this.CombineWith(other: ScoreImportResult) : ScoreImportResult =
        {
            Scores = this.Scores + other.Scores
            NewScores = this.NewScores + other.NewScores
            Maps = this.Maps + other.Maps
        }

type ImportOsuScoresTask(root_folder: string, library: Library) =
    inherit BackgroundTask<ScoreImportResult>()

    member private this.ImportMapScores
        (
            beatmap_score_data: OsuScoreDatabase_Beatmap,
            original_osu_file_first_note: Time,
            original_osu_file_rate: Rate,
            chart: Chart,
            chart_hash: string
        ) : ScoreImportResult =

        let mutable scores = 0
        let mutable new_scores = 0

        for score in beatmap_score_data.Scores do
            let replay_file =
                Path.Combine(root_folder, "Data", "r", sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp)

            match OsuReplay.TryReadFile(replay_file) with
            | None -> ()
            | Some replay ->
                match OsuReplay.to_score replay chart original_osu_file_first_note original_osu_file_rate with
                | Error reason -> Logging.Error "Error with replay '%s': %s" replay_file reason
                | Ok score ->
                    let existing_score_replaced =
                        library.UserData.DeleteScore(chart_hash, score.Timestamp)

                    library.UserData.SaveScore(chart_hash, score)

                    scores <- scores + 1

                    if not existing_score_replaced then
                        new_scores <- new_scores + 1

        { Maps = 1; Scores = scores; NewScores = new_scores }

    override this.Run(ctx: ITaskContext) : Async<ScoreImportResult> =
        // todo: benefit from async operations
        async {
            let mutable result = { Scores = 0; NewScores = 0; Maps = 0 }

            ctx.Status("Reading osu! database")

            let scores =
                use file = Path.Combine(root_folder, "scores.db") |> File.OpenRead
                Logging.Info("Reading scores database ..")
                use reader = new BinaryReader(file, Encoding.UTF8)
                OsuScoreDatabase.Read(reader)

            Logging.Info "Read score data, containing info about %i maps" scores.Beatmaps.Length

            let main_db =
                use file = Path.Combine(root_folder, "osu!.db") |> File.OpenRead
                Logging.Info("Reading osu! database ..")
                use reader = new BinaryReader(file, Encoding.UTF8)
                OsuDatabase.Read(reader)

            Logging.Info
                "Read %s's osu! database containing %i maps, starting import .."
                main_db.PlayerName
                main_db.Beatmaps.Length

            // MD5 -> osu! beatmap data
            let osu_db_map =
                main_db.Beatmaps |> Seq.filter(fun b -> b.Mode = 3uy) |> Seq.map(fun b -> b.Hash, b) |> Map.ofSeq

            // MD5 -> Imported Interlude data
            let interlude_db_map =
                seq {
                    for entry in library.Charts.Entries do
                        for origin in entry.Origins do
                            match origin with
                            | ChartOrigin.Osu osu -> yield osu.Md5, (entry.Hash, osu.FirstNoteOffset, osu.SourceRate)
                            | _ -> ()
                }
                |> Map.ofSeq

            for i, beatmap_score_data in Seq.indexed scores.Beatmaps do

                if beatmap_score_data.Scores.Length > 0 && beatmap_score_data.Scores.[0].Mode = 3uy then

                    match Map.tryFind beatmap_score_data.Hash interlude_db_map with
                    | Some(interlude_hash, original_osu_file_first_note, original_osu_file_rate) ->

                        match library.Charts.GetChart(interlude_hash) with
                        | Error reason -> Logging.Error "Error loading chart '%s': %s" interlude_hash reason
                        | Ok chart ->

                        result <-
                            result.CombineWith(
                                this.ImportMapScores(
                                    beatmap_score_data,
                                    original_osu_file_first_note,
                                    original_osu_file_rate,
                                    chart,
                                    interlude_hash
                                )
                            )

                    | None ->
                        match Map.tryFind beatmap_score_data.Hash osu_db_map with
                        | None ->
                            Logging.Error
                                "Beatmap %s not imported into Interlude (or known in osu! database?), skipping"
                                beatmap_score_data.Hash
                        | Some beatmap_data ->
                            Logging.Error
                                "Beatmap %s (%s) not imported into Interlude, skipping"
                                beatmap_score_data.Hash
                                beatmap_data.Filename

                        result <- { result with Maps = result.Maps + 1 }

                ctx.Progress(i + 1, scores.Beatmaps.Length)

            Logging.Info
                "Finished importing osu! scores: %i scores found from %i maps, +%i new scores imported"
                result.Scores
                result.Maps
                result.NewScores

            return result
        }
