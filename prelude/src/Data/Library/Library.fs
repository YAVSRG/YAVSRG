namespace Prelude.Data.Library

open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Data.User
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections

type Library =
    {
        Cache: Cache
        Collections: Collections
    }

module Library =

    let load () : Library =
        
        let db = Percyqaz.Data.Sqlite.Database.from_file (Path.Combine(get_game_folder "Songs", "charts.db"))
        let songs_db = ChartDatabase.create true db

        let library =
            {
                Cache = Cache.from_path (get_game_folder "Songs")
                Collections =
                    load_important_json_file
                        "Collections"
                        (Path.Combine(get_game_folder "Data", "collections.json"))
                        false
            }

        Logging.Info(
            sprintf
                "Loaded chart library of %i charts, %i folders, %i playlists"
                library.Cache.Entries.Count
                library.Collections.Folders.Count
                library.Collections.Playlists.Count
        )

        library

    let save (library: Library) =
        Cache.save library.Cache
        save_important_json_file (Path.Combine(get_game_folder "Data", "collections.json")) library.Collections

type LibraryViewContext =
    {
        Rate: float32
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        UserDatabase: UserDatabase
    }