namespace Interlude.Content

open System.IO
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Data.User
open Prelude.Data.Library

module private Data =

    let mutable private database: Database = Unchecked.defaultof<_>
    let mutable user_db: UserDatabase = Unchecked.defaultof<_>
    let mutable library: Library = Unchecked.defaultof<_>

    let init () : unit =
        library <- Library.load ()
        database <- Database.from_file (Path.Combine(get_game_folder "Data", "scores.db")) // todo: rename to interlude.db
        user_db <- UserDatabase.create true database

    let deinit () : unit =
        if not (isNull (user_db :> obj)) then
            UserDatabase.save_changes user_db
            Library.save library

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish