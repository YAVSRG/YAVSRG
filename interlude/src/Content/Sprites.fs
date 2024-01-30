namespace Interlude.Content

open System.Collections.Generic
open Percyqaz.Flux.Graphics

module Sprites =

    let private cache = new Dictionary<string, Sprite>()

    let get (id: string) =
        if cache.ContainsKey id then
            cache.[id]
        else
            failwithf "No such loaded sprite: %s" id

    let remove (id: string) =
        if cache.ContainsKey id then
            Sprite.destroy cache.[id]
            cache.Remove id |> ignore

    let add (id: string) (s: Sprite) =
        remove id
        cache.Add(id, s)