namespace Prelude.Data.Library

open System.IO
open System.Collections.Concurrent
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing.Patterns
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections

type Patterns = ConcurrentDictionary<string, PatternSummary.PatternDetailsReport>

type Library =
    {
        Cache: Cache
        Collections: Collections
        Patterns: Patterns
    }

module Library =

    let load () : Library =
        let library =
            {
                Cache = Cache.from_path (get_game_folder "Songs")
                Collections =
                    load_important_json_file
                        "Collections"
                        (Path.Combine(get_game_folder "Data", "collections.json"))
                        false
                Patterns =
                    let path = Path.Combine(get_game_folder "Data", "patterns.json")
                    match JSON.FromFile path with
                    | Ok res -> res
                    | Error reason ->
                        Logging.Info("Pattern analysis has updated, you will need to cache patterns again")
                        Patterns()
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
        Cache.save library.Cache // todo: only save if there are changes
        save_important_json_file (Path.Combine(get_game_folder "Data", "collections.json")) library.Collections
        save_important_json_file (Path.Combine(get_game_folder "Data", "patterns.json")) library.Patterns

    let cache_patterns =
        { new Async.Service<Library, unit>() with
            override this.Handle(library) =
                async {
                    for entry in library.Cache.Entries.Values do
                        //if not (library.Patterns.ContainsKey entry.Hash) then
                            match Cache.load entry library.Cache with
                            | Some c ->
                                library.Patterns.[entry.Hash] <- PatternSummary.generate_cached_pattern_data (1.0f, c)
                            | None -> ()
                }
        }
