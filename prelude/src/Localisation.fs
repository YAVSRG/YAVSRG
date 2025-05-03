namespace Prelude

open System.IO
open System.Diagnostics
open System.Collections.Generic
open Percyqaz.Common

module Localisation =
    let private mapping = new Dictionary<string, string>()
    let mutable private loaded_path = ""

    let load_language (language_id: string) : unit =

        let path = Path.Combine(Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName), "Locale", language_id + ".txt")

        try
            let lines = File.ReadAllLines path

            Array.iter
                (fun (l: string) ->
                    let s: string[] = l.Split([| '=' |], 2)
                    mapping.[s.[0]] <- s.[1].Replace("\\n", "\n")
                )
                lines

            loaded_path <- Path.GetFullPath path
        with err ->
            Logging.Critical "Failed to load localisation file '%s': %O" path err

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