namespace Interlude.Content

open Percyqaz.Data.Sqlite
open Prelude.Data

module private Data =
    
    let mutable database : Database = Unchecked.defaultof<_>
    let mutable score_db : ScoreDatabase = Unchecked.defaultof<_>

    let init_startup () =
        database <- DatabaseSetup.from_file "scores.db"
        score_db <- ScoreDatabase.create database

    let deinit () =
        if not (isNull (score_db :> obj)) then ScoreDatabase.save_changes score_db