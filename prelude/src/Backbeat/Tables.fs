namespace Prelude.Backbeat

open System
open System.IO
open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type TableRatingCalculation = | AverageTop50

[<Json.AutoCodec>]
type TableSectionInfo =
    {
        Name: string
        Description: string
        Color: int32

        LevelStart: int
        LevelEnd: int
    }

[<Json.AutoCodec>]
type TableInfo =
    {
        Name: string
        Description: string
        Keymode: int
        RulesetId: string
        RatingCalculation: TableRatingCalculation
        Sections: TableSectionInfo list
        LevelDisplayNames: Map<int, string>
    }
    member this.LevelName(level: int) =
        Map.tryFind level this.LevelDisplayNames
        |> Option.defaultValue (level.ToString())

[<Json.AutoCodec>]
type TableChart = { Hash: string; Level: int }

[<Json.AutoCodec>]
type TableClientFile =
    {
        Info: TableInfo
        Charts: TableChart list
    }

type Table =
    {
        Id: string
        LastUpdated: int64
        Info: TableInfo
        Charts: TableChart list
    }

module Table =

    let load (id: string) : Table option =
        let path = Path.Combine(get_game_folder "Data", "Tables", id + ".table")

        match JSON.FromFile path with
        | Ok(file: TableClientFile) ->
            Some
                {
                    Id = id
                    LastUpdated = DateTimeOffset.op_Implicit(File.GetLastWriteTime path).ToUnixTimeMilliseconds()
                    Info = file.Info
                    Charts = file.Charts
                }
        | Error reason -> None

    let save (table: Table) =
        let path = Path.Combine(get_game_folder "Data", "Tables", table.Id + ".table")

        JSON.ToFile
            (path, true)
            {
                Info = table.Info
                Charts = table.Charts
            }

    let points (info: TableInfo) (level: int, grade: int) =
        // currently hard-coded for SCJ4
        match grade with
        | -1
        | 0 -> 0.0
        | 1 -> max 0.0 (float level - 5.0) // C-
        | 2 -> max 0.0 (float level - 4.0) // C
        | 3 -> max 0.0 (float level - 3.0) // B-
        | 4 -> max 0.0 (float level - 2.0) // B
        | 5 -> max 0.0 (float level - 1.0) // A-
        | 6 -> float level // A
        | 7 -> float level + 0.5 // A+
        | 8 -> float level + 1.0 // S-
        | 9 -> float level + 1.5 // S
        | 10 -> float level + 2.0 // S+
        | _ -> 0.0

    let ratings (get_grade: string -> int option) (table: Table) =
        seq {
            for chart in table.Charts do
                let grade_achieved = get_grade chart.Hash

                let table_points =
                    match grade_achieved with
                    | None -> None
                    | Some i -> Some <| points table.Info (chart.Level, i)

                yield chart.Level, chart.Hash, grade_achieved, table_points
        }
