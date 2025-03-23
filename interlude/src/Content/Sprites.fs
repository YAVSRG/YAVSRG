namespace Interlude.Content

open System.Collections.Generic
open Percyqaz.Flux.Graphics

module Sprites =

    let private cache = new Dictionary<string, Sprite>()

    let get (id: string) : Sprite =
        if cache.ContainsKey id then
            cache.[id]
        else
            failwithf "No such loaded sprite: %s" id

    let remove (id: string) : unit =
        if cache.ContainsKey id then
            Sprite.destroy cache.[id] |> ignore
            cache.Remove id |> ignore

    let add (destroy_existing: bool) (id: string) (s: Sprite) : unit =
        if destroy_existing then
            remove id
        cache.[id] <- s