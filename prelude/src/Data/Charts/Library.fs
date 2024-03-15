namespace Prelude.Data.Charts

open System
open System.IO
open System.IO.Compression
open System.Collections.Concurrent
open Percyqaz.Data
open Percyqaz.Common
open Prelude
open Prelude.Charts.Conversions
open Prelude.Charts.Processing.Patterns
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections

type Patterns = ConcurrentDictionary<string, PatternSummary.PatternDetailsReport>

type Library =
    {
        Cache: Cache
        Collections: Collections
        Patterns: Patterns
    }

module Library =

    // ---- Loading and saving ----

    let load () : Library =
        let library = 
            {
                Cache = Cache.from_path (get_game_folder "Songs")
                Collections = load_important_json_file "Collections" (Path.Combine(get_game_folder "Data", "collections.json")) false
                Patterns = 
                    let path = Path.Combine(get_game_folder "Data", "patterns.json")
            
                    if File.GetLastWriteTimeUtc(path) |> Timestamp.from_datetime > 1709212606000L then
                        load_important_json_file "Patterns" (Path.Combine(get_game_folder "Data", "patterns.json")) false
                    else
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
        Cache.save library.Cache
        save_important_json_file (Path.Combine(get_game_folder "Data", "collections.json")) library.Collections
        save_important_json_file (Path.Combine(get_game_folder "Data", "patterns.json")) library.Patterns

    let cache_patterns =
        { new Async.Service<Library, unit>() with
            override this.Handle(library) =
                async {
                    for entry in library.Cache.Entries.Values do
                        if not (library.Patterns.ContainsKey entry.Hash) then
                            match Cache.load entry library.Cache with
                            | Some c -> library.Patterns.[entry.Hash] <- PatternSummary.generate_cached_pattern_data (1.0f, c)
                            | None -> ()
                }
        }