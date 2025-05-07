namespace Prelude

open System.IO
open System.Collections.Generic
open Percyqaz.Common

module Localisation =
    let private mapping = new Dictionary<string, string>()

    let load_locale (source: Stream) : unit =

        use sr = new StreamReader(source)

        try
            let text = sr.ReadToEnd()
            let lines = text.Replace("\r", "").Split("\n")

            Array.iter
                (fun (l: string) ->
                    let s: string[] = l.Split([| '=' |], 2)
                    mapping.[s.[0]] <- s.[1].Replace("\\n", "\n")
                )
                lines
        with err ->
            Logging.Critical "Failed to load localisation strings: %O" err

    let localise (key: string) : string =
        if mapping.ContainsKey key then
            mapping.[key]
        else
            key

    let localise_with (data: string list) (key: string) =
        let mutable s = localise key
        List.iteri (fun i x -> s <- s.Replace("%" + i.ToString(), x)) data
        s

[<AutoOpen>]
module LocalisationOperators =

    /// Shorthand operator to get the localised text from a locale string id
    let (~%) = Localisation.localise

    let inline (%>) (args: string list) (localisation_key: string) : string =
        Localisation.localise_with args localisation_key