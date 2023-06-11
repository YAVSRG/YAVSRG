namespace Backbeat.Features.Archive

open System
open System.Linq
open FParsec
open Percyqaz.Common
open Prelude.Data.Charts.Archive

module Maintenance2 =

    type ArtistFragment =
        | Verified of string
        | Artist of string

    type ArtistsAndFeatures = ArtistFragment list * ArtistFragment list option
    module ArtistsAndFeatures =
        let artists (a, f) = a |> List.map (function Verified v -> v | Artist a -> a.Trim())
        let features (a, f) = Option.defaultValue [] f |> List.map (function Verified v -> v | Artist a -> a.Trim())
        let flatten x = artists x @ features x
        let confident (a, f) =
            let mutable count1 = 0
            let mutable count2 = 0
            for a in a do match a with Artist a -> count1 <- count1 + 1 | _ -> ()
            for f in Option.defaultValue [] f do match f with Artist a -> count2 <- count2 + 1 | _ -> ()
            count1 < 2 && count2 < 2

    let collab_separator =
        pstringCI " x "
        <|> pstring " + "
        <|> pstring "/"
        <|> pstringCI " vs. "
        <|> pstringCI " vs "
        <|> pstring "&"
        <|> pstringCI ", and "
        <|> pstring ","
        <|> pstringCI " and "
        <|> pstringCI " with "
        <|> pstringCI "prod." .>> spaces
    
    let feature_separator =
        pstringCI " feat." .>> spaces
        <|> pstringCI " ft." .>> spaces
        <|> pstringCI " featuring." .>> spaces
        <|> pstringCI " feat" .>> spaces
        <|> pstringCI " ft" .>> spaces

    let parse_collab : Parser<_, unit> =
        let verified =
            let mapping = artists.CreateMappingCaseSensitive()
            match mapping.Keys |> Seq.sortByDescending (fun s -> s.Length) |> List.ofSeq with
            | [] -> pzero
            | x :: xs -> 
                let mutable parser = stringReturn x (Verified mapping.[x])
                for x in xs do parser <- parser <|> stringReturn x (Verified x)
                parser
        let unverified = many1CharsTill anyChar (followedBy collab_separator <|> followedBy feature_separator <|> eof) |>> Artist
        sepBy1 (verified <|> unverified) collab_separator

    let parse_prod_and_features : Parser<ArtistsAndFeatures, unit> =
        parse_collab .>>. opt (feature_separator >>. parse_collab)

    let extract_bracket (artist: string) =
        let bracket_regex = Text.RegularExpressions.Regex("\\((.*?)\\)$")
        
        let matches = bracket_regex.Matches artist
        if matches.Count = 1 then
            let bracket_content = matches.[0].Groups.[1].Value
            artist.Replace(matches.First().Value, "").Trim(), Some bracket_content
        else artist, None
    
    let parse_artists_and_remixers (artist: string) : string list * string list * string list * string * bool =
        let p = parse_prod_and_features .>>. restOfLine false

        let artist, bracket = extract_bracket artist

        let mutable remixers = []
        let mutable artists = [artist]
        let mutable performers = []
        let mutable confident = true
        let mutable append_to_title = ""

        match bracket with

        | Some s when s.ToLower().EndsWith(" remix") || s.ToLower().EndsWith(" mix") || s.ToLower().EndsWith(" edit") || s.ToLower().EndsWith(" bootleg") ->
            match run p s with
            | Success((res, leftover), _, _) -> 
                remixers <- ArtistsAndFeatures.flatten res
                append_to_title <- "(" + s + ")"
                if leftover <> "" then confident <- false
            | Failure(reason, _, _) -> confident <- false

        | Some s when s.ToLower().StartsWith("remix by ") || s.ToLower().StartsWith("remixed by ") || s.ToLower().StartsWith("edit by ") ->
            let remixer = s.Split(" by ", StringSplitOptions.TrimEntries).Last()
            remixers <- [remixer]
            append_to_title <- "(" + remixer + ")"
        | Some s -> 
            Logging.Info(sprintf "Weird bracket text '%s'" s)
            confident <- false
        | None -> ()

        match run p artist with
        | Success((res, leftover), _, _) ->
            artists <- ArtistsAndFeatures.artists res
            performers <- ArtistsAndFeatures.features res
            if not (ArtistsAndFeatures.confident res) then confident <- false
            if leftover <> "" then 
                confident <- false
                artists <- (List.head artists).Trim() + " " + leftover.Trim() :: List.tail artists
        | Failure(reason, _, _) -> ()

        artists, performers, remixers, append_to_title, confident

    let make_suggestion (flag: string) (id: SongId) (before: Song) (after: Song) : bool =
        let inline diff label a b = if a <> b then Logging.Info(sprintf "%s\n %A vvv\n %A" label a b)
        Logging.Info(sprintf "Backbot has a suggestion for '%s' that needs your approval" id)
        diff "Artists" before.Artists after.Artists
        diff "Performers" before.OtherArtists after.OtherArtists
        diff "Remixers" before.Remixers after.Remixers
        diff "Title" before.Title after.Title
        diff "Alt Titles" before.AlternativeTitles after.AlternativeTitles
        diff "Formatted title" before.FormattedTitle after.FormattedTitle
        diff "Tags" before.Tags after.Tags
        Logging.Info(sprintf "Reason: %s" flag)
        Logging.Info("\noptions ::\n 1 - Make this change\n 2 - Queue for manual review\n 3 - No correction needed")
        let mutable option_chosen = None
        while option_chosen.IsNone do
            match Console.ReadKey().Key with
            | ConsoleKey.D1 -> option_chosen <- Some true
            | ConsoleKey.D2 -> option_chosen <- Some false; Queue.append "song-review" id
            | ConsoleKey.D3 -> option_chosen <- Some false; Queue.append "song-ignore" id
            | _ -> ()
        option_chosen.Value

    let test() =
        for s in songs.Keys do
            let song = songs.[s]
            if song.Artists.Length = 1 && song.Remixers.Length = 0 && song.OtherArtists.Length = 0 then
                let artists, features, remixers, append_to_title, confident = parse_artists_and_remixers song.Artists.[0]
                let suggestion = 
                    { song with 
                        Artists = artists
                        OtherArtists = features
                        Remixers = remixers
                        Title = if append_to_title <> "" then song.Title.Trim() + " " + append_to_title else song.Title
                    }
                if suggestion <> song then
                    if confident || make_suggestion "artist_splitter" s song suggestion then
                        songs.[s] <- suggestion
                        if confident then Logging.Info(sprintf "Backbot making confident edit to '%s'" s) else save()
        save()
