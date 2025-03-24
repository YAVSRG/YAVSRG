namespace Prelude.Data.Library

open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library

type Library =
    {
        Charts: ChartDatabase
        Collections: Collections
    }

module Library =

    let load () : Library =

        let db = Percyqaz.Data.Sqlite.Database.from_file (Path.Combine(get_game_folder "Songs", "charts.db"))
        let charts_db = ChartDatabase.create true db

        File.WriteAllText(
            Path.Combine(get_game_folder "Songs", "HOW_TO_ADD_SONGS.txt"),
            "Dragging and dropping things into this folder won't work.\n"
            + "Instead, drag and drop things onto the *game window* while it's open and it will import."
        )

        let library =
            {
                Charts = charts_db
                Collections =
                    load_important_json_file
                        "Collections"
                        (Path.Combine(get_game_folder "Data", "collections.json"))
                        false
            }

        Logging.Info
            "Loaded chart library of %i charts, %i folders, %i playlists"
            library.Charts.Cache.Count
            library.Collections.Folders.Count
            library.Collections.Playlists.Count

        library

    let save (library: Library) =
        save_important_json_file (Path.Combine(get_game_folder "Data", "collections.json")) library.Collections

type LibraryViewContext =
    {
        Rate: Rate
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        UserDatabase: UserDatabase
    }