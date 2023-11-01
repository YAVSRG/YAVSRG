namespace Interlude.Features.Stats

open System.IO
open Percyqaz.Json
open Prelude

[<Json.AutoCodec(false)>]
type Stats =
    {
        mutable PlayTime: float
        mutable PracticeTime: float
        mutable GameTime: float
        mutable NotesHit: int

        mutable PlaysStarted: int
        mutable PlaysRetried: int
        mutable PlaysCompleted: int
        mutable PlaysQuit: int

        mutable MigrationVersion: int option
    }
    static member Default =
        {
            PlayTime = 0.0
            PracticeTime = 0.0
            GameTime = 0.0
            NotesHit = 0

            PlaysStarted = 0
            PlaysRetried = 0
            PlaysCompleted = 0
            PlaysQuit = 0

            MigrationVersion = None
        }

module Stats =

    // storage

    let mutable total: Stats = Stats.Default
    let mutable session: Stats = Stats.Default

    let init () =
        total <- load_important_json_file "Stats" (Path.Combine(get_game_folder "Data", "stats.json")) false

    let save () =
        total.PlayTime <- total.PlayTime + session.PlayTime
        total.PracticeTime <- total.PracticeTime + session.PracticeTime
        total.GameTime <- total.GameTime + session.GameTime
        total.NotesHit <- total.NotesHit + session.NotesHit

        total.PlaysStarted <- total.PlaysStarted + session.PlaysStarted
        total.PlaysRetried <- total.PlaysRetried + session.PlaysRetried
        total.PlaysCompleted <- total.PlaysCompleted + session.PlaysCompleted
        total.PlaysQuit <- total.PlaysQuit + session.PlaysQuit

        session <- Stats.Default
        save_important_json_file (Path.Combine(get_game_folder "Data", "stats.json")) total

    // helpers

    let format_long_time (time: float) =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0
        let days = hours / 24.0

        if days > 1 then
            sprintf "%id %02ih %02im" (floor days |> int) (floor (hours % 24.0) |> int) (floor (minutes % 60.0) |> int)
        elif hours > 1 then
            sprintf "%ih %02im" (floor hours |> int) (floor (minutes % 60.0) |> int)
        else
            sprintf "%im %02is" (floor minutes |> int) (floor (seconds % 60.0) |> int)

    let format_short_time (time: float) =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0

        if hours > 1 then
            sprintf "%i:%02i:%02i" (floor hours |> int) (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
        else
            sprintf "%02i:%02i" (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
