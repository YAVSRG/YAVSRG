namespace Prelude

open System.IO
open System.Collections.Generic
open Percyqaz.Common

module Localisation =
        
    type private LocalisationContext =
        {
            CircularReferenceCheck: Set<string>
            Name: string
            Stream: Stream
            GetLocale: string -> Stream option
            Loaded: Dictionary<string, string>
        }
        
    type LocaleFile =
        {
            Inherit: string option
            Entries: IReadOnlyDictionary<string, string>
        }
        
    let private read_locale_file (name: string, stream: Stream) : LocaleFile =
        use sr = new StreamReader(stream)
        let mutable inherits_from = None
        let entries = Dictionary<string, string>()
        
        let first_line = sr.ReadLine()
        
        if first_line = null then failwithf "Locale file was empty: '%s'" name
        
        elif first_line.StartsWith("#inherit ") then
            inherits_from <- Some(first_line.Substring(9))
            
        elif first_line <> "#root" then
            failwithf "Locale file '%s' is missing #root or #inherit <file> as the first line" name
            
        while not sr.EndOfStream do
            let line = sr.ReadLine().Trim()
            if line.Length > 0 then
                let equals = line.IndexOf('=')
                if equals > 0 then
                    let key = line.Substring(0, equals).Trim()
                    let value = line.Substring(equals + 1).Replace("\\n", "\n").Trim()
                    entries.[key] <- value
                else
                    failwithf "Invalid line in locale file '%s': %s" name line
                    
        { Inherit = inherits_from; Entries = entries }
    
    let rec private load_locale_tree (ctx: LocalisationContext) : Dictionary<string, string> =
        let locale = read_locale_file(ctx.Name, ctx.Stream)
        
        match locale.Inherit with
        | Some inherits_from ->
            Logging.Debug "Locale '%s' inherits from '%s'" ctx.Name inherits_from
            
            if ctx.CircularReferenceCheck.Contains(inherits_from.ToLower()) then
                failwithf "Circular reference: '%s' inherits from '%s'" ctx.Name inherits_from
                
            match ctx.GetLocale inherits_from with
            | Some stream ->
               load_locale_tree {
                   CircularReferenceCheck = ctx.CircularReferenceCheck.Add(inherits_from.ToLower())
                   Name = inherits_from
                   Stream = stream
                   GetLocale = ctx.GetLocale
                   Loaded = ctx.Loaded
               }
               |> ignore
            | None -> failwithf "Could not inherit locale file for requested language: '%s'" inherits_from
            
        | None -> ()
        
        for e in locale.Entries do
            ctx.Loaded.[e.Key] <- e.Value

        Logging.Debug "Loaded locale '%s': %i strings" ctx.Name locale.Entries.Count
        ctx.Loaded
        
    let try_load_file(name: string, path: string) : Result<LocaleFile, string> =
        try
            use stream = File.OpenRead(path)
            Ok (read_locale_file(name, stream))
        with err ->
            Error err.Message
            
    let write_file(file: LocaleFile, path: string) =
        File.WriteAllLines(path, seq {
            yield (match file.Inherit with Some inherited_from -> "#inherit " + inherited_from | None -> "#root")
            for e in file.Entries do
                yield sprintf "%s=%s" e.Key e.Value
        })

    let load (locale: string, get_locale: string -> Stream option) : Result<Dictionary<string, string>, string> =
        
        match get_locale locale with
        | Some stream ->
            let result = Dictionary<string, string>()
            try
                load_locale_tree {
                    CircularReferenceCheck = Set.empty.Add(locale.ToLower())
                    Name = locale
                    Stream = stream
                    GetLocale = get_locale
                    Loaded = result
                }
                |> Ok
            with err -> Error err.Message
                
        | None ->
            Error (sprintf "Could not find locale file for requested language: '%s'" locale)
            
    let mutable private mapping = Dictionary<string, string>()
    let init (locale: string, get_locale: string -> Stream option) =
        match load(locale, get_locale) with
        | Ok data -> mapping <- data
        | Error reason ->
            Logging.Critical "Failed to load localisation! Text will look weird ingame"
            Logging.Critical "Reason: %s" reason

    let localise (key: string) : string =
        if mapping.ContainsKey key then
            mapping.[key]
        else
            key

    let localise_with (data: string list) (key: string) =
        let mutable result_string = localise key
        List.iteri (fun i x -> result_string <- result_string.Replace("%" + i.ToString(), x)) data
        result_string

[<AutoOpen>]
module LocalisationOperators =

    /// Shorthand operator to get the localised text from a locale string id
    let (~%) = Localisation.localise

    let inline (%>) (args: string list) (localisation_key: string) : string =
        Localisation.localise_with args localisation_key