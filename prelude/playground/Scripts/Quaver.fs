namespace Prelude.Scripts

open System.IO
open Prelude.Formats.Quaver

module Quaver =

    let quaver_folder = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Quaver"
    let quaver_songs = Path.Combine(quaver_folder, "Songs")

    let run () =

        let mutable oks = 0
        let mutable errors = 0

        for song_folder in Directory.EnumerateDirectories quaver_songs do
            for chart_file in Directory.EnumerateFiles song_folder |> Seq.where (fun p -> Path.GetExtension(p).ToLower() = ".qua") do
                let outcome = (QuaverChart.from_file chart_file)
                match outcome with
                | Ok q ->
                    if not q.BPMDoesNotAffectScrollVelocity then printfn "legacy quaver sv thing"
                    oks <- oks + 1
                | Error msg ->
                    printfn "%s" chart_file
                    errors <- errors + 1

        printfn "%i OK" oks
        printfn "%i ERRORS" errors