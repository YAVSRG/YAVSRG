namespace Interlude.Content

open System.Collections.Generic
open Percyqaz.Flux.Audio

module Sounds =

    let private cache = new Dictionary<string, SoundEffect>()

    let get (id: string) : SoundEffect =
        if cache.ContainsKey id then
            cache.[id]
        else
            failwithf "No such loaded sound: %s" id

    let add (id: string) (sound: SoundEffect) : unit =
        if cache.ContainsKey id then
            cache.[id].Free()
            cache.Remove id |> ignore

        cache.Add(id, sound)

    let init () =
        Audio.current_device_changed.Add(
            fun () ->
                for fx in cache.Keys do
                    cache.[fx].ChangeDevice()
        )