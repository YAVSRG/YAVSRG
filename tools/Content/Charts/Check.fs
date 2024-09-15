namespace YAVSRG.CLI.Features.Backbeat

open Percyqaz.Common
open Prelude.Backbeat.Archive
open Interlude.Web.Shared

module Check =
    
    let scan_all_charts (callback: int64 * Song -> unit) =
        let rec loop p =
            Requests.Songs.Scan.get(
                p,
                function
                | None -> Logging.Error("Error scanning backbeat songs")
                | Some response ->
                    response.Results |> Seq.map (fun result -> result.SongId, result.Song) |> Seq.iter callback
                    if response.HasNextPage then loop (p + 1)
            )
        loop 0

    let test () =
        scan_all_charts (fun (id, song) ->
            let fixed_song =
                { song with 
                    Title = Metadata.prune_song_title song.Title
                    //Tags = Metadata.prune_tags song.Tags
                }
                |> Metadata.extract_better_metadata
                |> Metadata.correct_artist_typos
            if song <> fixed_song then
                printfn "SUGGESTION FOUND\n%A\n<->\n%A\n\n" song fixed_song
                System.Console.ReadLine() |> ignore
        )