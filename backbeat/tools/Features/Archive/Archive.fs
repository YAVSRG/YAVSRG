namespace Backbeat.Features.Archive

module Archive =
        
    open Percyqaz.Shell

    let register (ctx: Context) =
        ctx
            .WithCommand("get_eo", 
            Command.create "Fetch EO packs locally" [] <| Impl.Create(Packs.load_stepmania_packs))
            .WithCommand("script",
            Command.create "User script" [] <| Impl.Create(Collect.script))
            .WithCommand("slurp", 
            Command.create "Processes queues of chart data to add to database" [] <| Impl.Create(Collect.slurp))
            .WithCommand("check_songs", 
            Command.create "Scan song metadata for mistakes" [] <| Impl.Create(Maintenance.check_all_songs))
            .WithCommand("check_artists", 
            Command.create "Scan artists for duplicates" [] <| Impl.Create(Maintenance.check_all_artists))
