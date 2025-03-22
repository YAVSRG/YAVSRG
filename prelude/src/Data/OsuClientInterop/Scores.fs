namespace Prelude.Data.OsuClientInterop

open System.IO
open System.Text
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Data
open Prelude.Data.User
open Prelude.Data.Library

module Scores =

    type ScoreImportResult =
        {
            mutable Scores: int
            mutable NewScores: int
            mutable Maps: int
        }

    let private import_map_scores
        (result: ScoreImportResult)
        (osu_root_folder: string)
        (beatmap_score_data: OsuScoreDatabase_Beatmap, original_osu_file_first_note: Time, original_osu_file_rate: Rate)
        (chart: Chart, chart_hash: string)
        (user_db: UserDatabase) : unit =

        for score in beatmap_score_data.Scores do
            let replay_file =
                Path.Combine(
                    osu_root_folder,
                    "Data",
                    "r",
                    sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp
                )

            match OsuReplay.TryReadFile replay_file with
            | None -> ()
            | Some replay ->
                match OsuReplay.to_score replay chart original_osu_file_first_note original_osu_file_rate with
                | Error reason -> Logging.Error "Error with replay '%s': %s" replay_file reason
                | Ok score ->
                    let existing_score_replaced = UserDatabase.delete_score chart_hash score.Timestamp user_db
                    UserDatabase.save_score chart_hash score user_db

                    result.Scores <- result.Scores + 1
                    if not existing_score_replaced then
                        result.NewScores <- result.NewScores + 1

    let private import_osu_scores (osu_root_folder: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : ScoreImportResult =

        let result =
            {
                Scores = 0
                NewScores = 0
                Maps = 0
            }

        progress (Generic "Reading osu! database")

        let scores =
            use file = Path.Combine(osu_root_folder, "scores.db") |> File.OpenRead
            Logging.Info "Reading scores database .."
            use reader = new BinaryReader(file, Encoding.UTF8)
            OsuScoreDatabase.Read(reader)

        Logging.Info "Read score data, containing info about %i maps" scores.Beatmaps.Length

        let main_db =
            use file = Path.Combine(osu_root_folder, "osu!.db") |> File.OpenRead
            Logging.Info "Reading osu! database .."
            use reader = new BinaryReader(file, Encoding.UTF8)
            OsuDatabase.Read(reader)

        Logging.Info
            "Read %s's osu! database containing %i maps, starting import .."
            main_db.PlayerName
            main_db.Beatmaps.Length

        // MD5 -> osu! beatmap data
        let osu_db_map =
            main_db.Beatmaps
            |> Seq.filter (fun b -> b.Mode = 3uy)
            |> Seq.map (fun b -> b.Hash, b)
            |> Map.ofSeq

        // MD5 -> Imported Interlude data
        let interlude_db_map =
            seq {
                for entry in chart_db.Entries do
                    for origin in entry.Origins do
                        match origin with
                        | ChartOrigin.Osu osu -> yield osu.Md5, (entry.Hash, osu.FirstNoteOffset, osu.SourceRate)
                        | _ -> ()
            }
            |> Map.ofSeq

        for i, beatmap_score_data in Seq.indexed scores.Beatmaps do

            if beatmap_score_data.Scores.Length > 0 && beatmap_score_data.Scores.[0].Mode = 3uy then

                match Map.tryFind beatmap_score_data.Hash interlude_db_map with
                | Some (interlude_hash, original_osu_file_first_note, original_osu_file_rate) ->

                    match ChartDatabase.get_chart interlude_hash chart_db with
                    | Error reason -> Logging.Error "Error loading chart '%s': %s" interlude_hash reason
                    | Ok chart ->

                    import_map_scores
                        result
                        osu_root_folder
                        (beatmap_score_data, original_osu_file_first_note, original_osu_file_rate)
                        (chart, interlude_hash)
                        user_db

                | None ->
                    match Map.tryFind beatmap_score_data.Hash osu_db_map with
                    | None -> Logging.Error "Beatmap %s not imported into Interlude (or known in osu! database?), skipping" beatmap_score_data.Hash
                    | Some beatmap_data ->
                        Logging.Error "Beatmap %s (%s) not imported into Interlude, skipping" beatmap_score_data.Hash beatmap_data.Filename

                    // todo: consider importing song folder here and now

                result.Maps <- result.Maps + 1

            progress (Processing (i + 1, scores.Beatmaps.Length))

        Logging.Info "Finished importing osu! scores: %i scores found from %i maps, +%i new scores imported" result.Scores result.Maps result.NewScores
        progress Complete
        result

    let import_osu_scores_async (osu_root_folder: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<ScoreImportResult> =
        // todo: make the IO operations actually benefit from async
        async {
            try
                return import_osu_scores (osu_root_folder, chart_db, user_db, progress)
            with err ->
                Logging.Error "Unhandled exception while importing osu! scores: %O" err
                progress Faulted
                return {
                    Scores = 0
                    NewScores = 0
                    Maps = 0
                }
        }