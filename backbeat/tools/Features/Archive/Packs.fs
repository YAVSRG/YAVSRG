namespace Backbeat.Features.Archive

open System
open Percyqaz.Json
open Percyqaz.Common
open Prelude
open Prelude.Backbeat.Archive

module Packs =

    [<Json.AutoCodec>]
    type EOPackAttrs =
        {
            name: string
            average: float
            download: string
            mirror: string
            size: int64
        }
    
    [<Json.AutoCodec>]
    type EOPack =
        {
            ``type``: string
            id: int
            attributes: EOPackAttrs
        }

    let load_stepmania_packs() =
        async {
            match! Data.WebServices.download_json_async("https://api.etternaonline.com/v2/packs/") with
            | None -> printfn "Failed to get EO packs"
            | Some (d: {| data: ResizeArray<EOPack> |}) ->
                for p in d.data do
                    if packs.Stepmania.ContainsKey(p.id) then packs.Stepmania.Remove(p.id) |> ignore
                    packs.Stepmania.Add(p.id,
                            {
                                Title = p.attributes.name
                                Mirrors = [Archive.DownloadUrl.create p.attributes.download; Archive.DownloadUrl.create p.attributes.mirror] |> List.distinct
                                Size = p.attributes.size
                            })
        }
        |> Async.RunSynchronously
        save()