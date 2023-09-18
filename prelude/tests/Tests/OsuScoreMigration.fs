namespace Prelude.Test

open System
open System.IO
open Percyqaz.Common
open Prelude.Data.``osu!``

module OsuScoreMigration =

    let main() =

        Logging.Info "Reading osu database ..."
        use file = Path.Combine(Prelude.Data.Charts.Library.Imports.osuSongFolder, "..", "scores.db") |> File.OpenRead
        use reader = new BinaryReader(file, System.Text.Encoding.UTF8)
        let database = ScoreDatabase.Read(reader)

        printfn "%A" database

        Console.ReadLine()