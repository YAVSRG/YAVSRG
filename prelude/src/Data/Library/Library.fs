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
    
    static member Load() : Library =
        
        let inline init_chart_db() : ChartDatabase =
            let db = Percyqaz.Data.Sqlite.Database.from_file (Path.Combine(get_game_folder "Songs", "charts.db"))
            
            File.WriteAllText(
                Path.Combine(get_game_folder "Songs", "HOW_TO_ADD_SONGS.txt"),
                "Dragging and dropping things into this folder won't work.\n"
                + "Instead, drag and drop things onto the *game window* while it's open and it will import."
            )
            
            ChartDatabase.CreateFullyLoaded(db)
            
        let inline init_collections() : Collections =
            load_important_json_file
                "Collections"
                (Path.Combine(get_game_folder "Data", "collections.json"))
                false
                
        let chart_db = init_chart_db()
        let collections = init_collections()
                
        Logging.Info
            "Loaded library of %i charts, %i folders, %i playlists"
            chart_db.Cache.Count
            collections.Folders.Count
            collections.Playlists.Count
        
        {
            Charts = chart_db
            Collections = collections
        }
        
    member this.Save() : unit =
        save_important_json_file (Path.Combine(get_game_folder "Data", "collections.json")) this.Collections

type LibraryViewContext =
    {
        Rate: Rate
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        UserDatabase: UserDatabase
    }