namespace Backbeat.Features.Archive

module Archive =
        
    open System.IO
    open Percyqaz.Shell
    open Backbeat.Utils
    
    let script() =
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