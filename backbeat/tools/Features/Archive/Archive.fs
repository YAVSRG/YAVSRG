namespace Backbeat.Features.Archive

module Archive =
        
    open Percyqaz.Shell

    let register (ctx: ShellContext) =
        ctx
            .WithCommand("get_eo", "Fetch EO packs locally", Packs.load_stepmania_packs)
            .WithCommand("script", "User script", Collect.script)
            .WithCommand("slurp", "Processes queues of chart data to add to database", Collect.slurp)
            .WithCommand("check_songs", "Scan song metadata for mistakes", Maintenance.check_all_songs)
            .WithCommand("check_artists", "Scan artists for duplicates", Maintenance.check_all_artists)
            .WithCommand("check_ids", "Fix old song ids", Maintenance.check_all_ids)
            .WithCommand("verify_artist", "Add a verified artist name to the database", "artist_name", Maintenance.verify_artist)
            .WithCommand("v2_test", "Test", Maintenance2.test)