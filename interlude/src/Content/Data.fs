namespace Interlude.Content

open System.IO
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Data
open Prelude.Data.Library

module private Data =

    let mutable database: Database = Unchecked.defaultof<_>
    let mutable score_db: ScoreDatabase = Unchecked.defaultof<_>
    let mutable library: Library = Unchecked.defaultof<_>

    let init_startup () =
        library <- Library.load ()
        database <- Database.from_file (Path.Combine(get_game_folder "Data", "scores.db"))
        score_db <- ScoreDatabase.create true database

    let deinit () =
        if not (isNull (score_db :> obj)) then
            ScoreDatabase.save_changes score_db
            Library.save library
