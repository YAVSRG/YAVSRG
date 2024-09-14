namespace Prelude.Data.Library.OLD_CACHING

//module Cache =

    //let recache_service =
    //    { new Async.Service<Cache, unit>() with
    //        override this.Handle(cache: Cache) =
    //            async {
    //                Logging.Debug(sprintf "Rebuilding chart cache @ %s" cache.RootPath)
    //                cache.Entries.Clear()
    //                cache.Changed <- true

    //                for folder in
    //                    Directory.EnumerateDirectories cache.RootPath
    //                    |> Seq.filter (fun p -> Path.GetFileName p <> ".assets") do

    //                    for file in Directory.EnumerateFiles folder do
    //                        match Path.GetExtension(file).ToLower() with
    //                        | ".yav" ->
    //                            match Chart.from_file file with
    //                            | Ok c ->
    //                                let entry, patterns = create_entry (Path.GetFileName folder) (File.GetCreationTimeUtc file) c

    //                                match c.Header.AudioFile with
    //                                | Absolute path when not (File.Exists path) ->
    //                                    Logging.Debug(
    //                                        sprintf
    //                                            "Deleting %s because its original audio file is gone"
    //                                            file
    //                                    )

    //                                    File.Delete file
    //                                | _ -> ()
                                        

    //                                let intended_path = get_path entry cache

    //                                if intended_path <> file && File.Exists intended_path then
    //                                    Logging.Debug(
    //                                        sprintf
    //                                            "Deleting %s because it should be named %s, which already exists"
    //                                            file
    //                                            intended_path
    //                                    )

    //                                    File.Delete file
    //                                else
    //                                    if intended_path <> file then
    //                                        Logging.Debug(sprintf "Moving cache file from %s to %s" file intended_path)
    //                                        File.Move(file, intended_path)

    //                                    cache.Entries.[entry.Key] <- entry
    //                                    cache.Patterns.[entry.Hash] <- patterns
    //                                    cache.Changed <- true
    //                            | Error reason -> 
    //                                try
    //                                    File.Delete(file)
    //                                    Logging.Info(sprintf "Deleted %s because it was likely corrupt/invalid (%s)" file reason)
    //                                with err ->
    //                                    Logging.Info(sprintf "%s could be corrupt/invalid (%s), but deleting it failed" file reason, err)
    //                        | _ -> ()

    //                save cache
    //            }
    //    }

    // For copying a chart from one folder to another
    //let copy (folder: string) (entry: CachedChart) (cache: Cache) =
    //    let new_entry = { entry with Folder = folder }

    //    if cache.Entries.ContainsKey(new_entry.Key) then
    //        ()
    //    else

    //        cache.Entries.[new_entry.Key] <- new_entry
    //        cache.Changed <- true
    //        let target = get_path new_entry cache
    //        Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
    //        File.Copy(get_path entry cache, target)

    // Pattern stuff

    //let cache_patterns =
    //    { new Async.Service<Cache * bool, unit>() with
    //        override this.Handle((cache, force)) =
    //            async {
    //                if force then 
    //                    cache.Patterns.Clear()
    //                    cache.Changed <- true
    //                for entry in cache.Entries.Values do
    //                    if not (cache.Patterns.ContainsKey entry.Hash) then
    //                        match load entry cache with
    //                        | Ok chart ->
    //                            cache.Patterns.[entry.Hash] <- PatternSummary.generate_pattern_data 1.0f chart
    //                            cache.Changed <- true
    //                        | Error reason -> Logging.Warn(sprintf "Error caching patterns for %s: %s" entry.Hash reason)
    //            }
    //    }

    //let cache_patterns_if_needed (cache: Cache) (recache_complete_callback: unit -> unit) : bool =
    //    if cache.Entries.Count > 0 && cache.Patterns.Count = 0 then
    //        cache_patterns.Request((cache, true), recache_complete_callback)
    //        true
    //    else false