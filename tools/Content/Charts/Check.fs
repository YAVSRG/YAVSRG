namespace YAVSRG.CLI.Features.Backbeat

open Percyqaz.Common
open Prelude.Data.Library.Caching
open Prelude.Backbeat.Archive
open Interlude.Web.Shared

module Check =
    
    let scan_all_charts (callback: int64 * Song -> unit) =
        let rec loop p =
            Requests.Songs.Scan.get(
                p,
                function
                | None -> Logging.Error("Error scanning all backbeat songs")
                | Some response ->
                    response.Results |> Seq.map (fun result -> result.SongId, result.Song) |> Seq.iter callback
                    if response.HasNextPage then loop (p + 1)
            )
        loop 0

    let test () =
        printfn "%s" interlude_chart_cache.RootPath
        scan_all_charts (printfn "%A")