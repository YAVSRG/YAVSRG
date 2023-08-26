namespace Backbeat.Features.Archive

module Archive =
        
    open System.IO
    open Percyqaz.Shell
    open Backbeat.Utils
    
    let script() =
        printfn "%i in cache, %i in chart db" backbeat_cache.Entries.Count charts.Count

        //let mutable cache_hashes = Set.empty
        //for c in backbeat_cache.Entries.Values do
        //    cache_hashes <- Set.add c.Hash cache_hashes

        //let mutable osu_sources = Map.empty
        //let mutable sm_sources = Set.empty

        //for chart in charts.Keys do
        //    if Set.contains chart cache_hashes then
        //        for source in charts.[chart].Sources do
        //            match source with
        //            | Prelude.Backbeat.Archive.ChartSource.Osu d ->
        //                osu_sources <- Map.add d.BeatmapId chart osu_sources
        //            | Prelude.Backbeat.Archive.ChartSource.Stepmania id ->
        //                sm_sources <- Set.add packs.Stepmania.[id].Title sm_sources
        //            | _ -> ()

        //for chart in charts.Keys |> List.ofSeq do
        //    if not (Set.contains chart cache_hashes) then
        //        let c = charts.[chart]
        //        for source in c.Sources do
        //            match source with
        //            | Prelude.Backbeat.Archive.ChartSource.Stepmania id ->
        //                printfn "I'M FROM '%s' AND IM NOT UP TO DATE" packs.Stepmania.[id].Title
        //            | _ -> ()

        for pack in File.ReadAllLines(Path.Combine(ARCHIVE_PATH, "masterlist")) do
            Collect.slurp_sm (pack.Trim())
        Backbot.run false

    let register (ctx: ShellContext) =
        ctx
            .WithCommand("get_eo", "Fetch EO packs locally", Packs.load_stepmania_packs)
            .WithCommand("search_sm", "Search for a stepmania pack", "pack_title", Collect.search_sm)
            .WithCommand("slurp_sm", "Download, convert, add to database and upload charts from a stepmania pack", "pack_title", Collect.slurp_sm)
            .WithCommand("slurp_community", "Download, convert, add to database and upload charts from a community pack", "pack_id", Collect.slurp_community)
            .WithCommand("check_artists", "Scan artists for duplicates", Maintenance.check_all_artists)
            .WithCommand("verify_artist", "Add a verified artist name to the database", "artist_name", Maintenance.verify_artist)
            .WithCommand("backbot_auto", "Run a backbot scan of the database for metadata corrections", fun () -> Backbot.run false)
            .WithCommand("backbot_manual", "Run a backbot scan of the database for metadata corrections, including manual corrections", fun () -> Backbot.run true)
            .WithCommand("backbot_omni", "Slurp all packs in master list, upload charts to mirror, scan for metadata corrections", script)
            .WithCommand("backbot_duplicates", "Look for duplicate song entries in the backbeat database", Backbot.find_fuzzy_duplicates)
            .WithCommand("recache", "Rebuilds the backbeat chart cache", Maintenance.recache)