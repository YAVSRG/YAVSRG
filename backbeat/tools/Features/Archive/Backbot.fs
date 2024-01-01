namespace Backbeat.Features.Archive

open System
open System.Linq
open FParsec
open Percyqaz.Common
open Prelude.Backbeat.Archive

module Backbot =

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

    let private collab_separator =
        pstringCI " x "
        <|> pstring " + "
        <|> pstring "/"
        <|> pstring " / "
        <|> pstringCI " vs. "
        <|> pstringCI " vs "
        <|> pstring "&"
        <|> pstring " & "
        <|> pstringCI ", and "
        <|> pstring ","
        <|> pstringCI " and "
        <|> pstringCI " with "
        <|> pstringCI "prod." .>> spaces
    
    let private feature_separator =
        pstringCI " feat." .>> spaces
        <|> pstringCI " ft." .>> spaces
        <|> pstringCI " featuring." .>> spaces
        <|> pstringCI " feat" .>> spaces
        <|> pstringCI " ft" .>> spaces

    let private parse_collab : Parser<_, unit> =
        let verified =
            let mapping = artists.ParserMapping()
            match mapping.Keys |> Seq.sortByDescending (fun s -> s.Length) |> List.ofSeq with
            | [] -> pzero
            | x :: xs -> 
                let mutable parser = stringReturn x (Verified mapping.[x])
                for x in xs do parser <- parser <|> stringReturn x (Verified x)
                parser
        let unverified = many1CharsTill anyChar (followedBy collab_separator <|> followedBy feature_separator <|> eof) |>> Artist
        sepBy1 (verified <|> unverified) collab_separator

    let private parse_prod_and_features : Parser<ArtistsAndFeatures, unit> =
        parse_collab .>>. opt (feature_separator >>. parse_collab)

    let private extract_bracket (text: string) =
        let bracket_regex = Text.RegularExpressions.Regex("\\((.*?)\\)$")
        
        let matches = bracket_regex.Matches text
        if matches.Count = 1 then
            let bracket_content = matches.[0].Groups.[1].Value
            text.Replace(matches.First().Value, "").Trim(), Some bracket_content
        else text, None
    
    let private parse_artists_and_remixers (artist: string) : string list * string list * string list * string * bool =
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
            append_to_title <- "(" + remixer + " Remix)"
        | Some s -> 
            Logging.Info(sprintf "Unusual bracket text '%s'" s)
            confident <- false
        | None -> ()

        match run p artist with
        | Success((res, leftover), _, _) ->
            artists <- ArtistsAndFeatures.artists res
            performers <- ArtistsAndFeatures.features res
            if not (ArtistsAndFeatures.confident res) then confident <- false
            if leftover <> "" then 
                confident <- false
                artists <- (List.head artists) + leftover :: List.tail artists
        | Failure(reason, _, _) -> ()

        artists, performers, remixers, append_to_title, confident

    let private parse_artists_and_remixers_title (title: string) : string * string list * string list * bool =
        let p = many1CharsTill anyChar (followedBy ((feature_separator |>> ignore) <|> eof)) .>>. opt (feature_separator >>. parse_collab)

        let title, bracket = extract_bracket title

        let mutable remixers = []
        let mutable performers = []
        let mutable confident = true
        let mutable suggested_title = title

        match bracket with
        | Some s when s.ToLower().EndsWith(" remix") || s.ToLower().EndsWith(" mix") || s.ToLower().EndsWith(" edit") || s.ToLower().EndsWith(" bootleg") ->
            if s.Contains("'s") then 
                remixers <- [s.Split("'s").[0]]
                suggested_title <- title + " (" + s + ")"
            else 
                Logging.Info(sprintf "Unknown remix name '%s'" s)
                confident <- false
                suggested_title <- title + " (" + s + ")"
        | Some s -> 
            //Logging.Info(sprintf "Unknown bracket text '%s'" s)
            confident <- false
            suggested_title <- title + " (" + s + ")"
        | None -> ()

        match run p title with
        | Success((title, features), _, _) ->
            if features.IsSome then
                suggested_title <- title
                performers <- ArtistsAndFeatures.features ([], features)
                if not (ArtistsAndFeatures.confident ([], features)) then confident <- false
        | Failure(reason, _, _) -> ()

        suggested_title, performers, remixers, confident

    let character_voice_regex = Text.RegularExpressions.Regex("[\\[\\(][cC][vV][.:\\-]?\\s?(.*)[\\]\\)]")
    let correct_artists() =
        Logging.Info "Scanning for artist name corrections"
        let map = artists.FixerMapping()
        let swap (s: string) =
            if map.ContainsKey(s.ToLower()) then 
                let replace = map.[s.ToLower()]
                replace
            else s
            
        let fix (artist: string) =
            let matches = character_voice_regex.Matches artist
            if matches.Count = 1 then
                let character_voice = matches.[0].Groups.[1].Value
                swap character_voice
            else swap artist
            
        for id in songs.Keys |> Array.ofSeq do
            let song = songs.[id]
            songs.[id] <-
                { song with 
                    Artists = List.map fix song.Artists
                    OtherArtists = List.map fix song.OtherArtists
                    Remixers = List.map fix song.Remixers
                }
        save()

    let correct_meta(user_oversight) =
        Logging.Info "Scanning for metadata corrections (artists)"
        for s in songs.Keys |> Seq.toArray do
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
                    if confident || (user_oversight && make_suggestion "artist_splitter" s song suggestion) then
                        songs.[s] <- suggestion
                        if confident then Logging.Info(sprintf "Making confident edit to '%s'" s) else save()
                        
        Logging.Info "Scanning for metadata corrections (title)"
        for s in songs.Keys |> Seq.toArray do
            let song = songs.[s]
            if song.Remixers.Length = 0 && song.OtherArtists.Length = 0 then
                let new_title, features, remixers, confident = parse_artists_and_remixers_title song.Title
                let suggestion = 
                    { song with 
                        OtherArtists = features
                        Remixers = remixers
                        Title = new_title
                    }
                if suggestion <> song then
                    if confident || (user_oversight && make_suggestion "title_splitter" s song suggestion) then
                        songs.[s] <- suggestion
                        if confident then Logging.Info(sprintf "Making confident edit to '%s'" s) else save()
        save()
    
    let private title_variations = 
        [|
            "tv size ver."; "tv size ver"; "tv ver."; "tv version"; "tv size"; "tv ver"; "tv edit"; "tv-size"; "anime ver."; "anime ver"; "op cut"; "op ver."; "op ver"
            "uncut ver."; "uncut ver"; "long ver."; "long ver"; "extended ver."; "extended ver"; "extended mix"; "radio ver."; "radio ver"; "radio edit"; "radio mix"
            "cut ver."; "cut ver";  "short ver."; "short ver"; "short edit"; "short cut"
            "album ver"; "original mix"; "bass boosted"; "full version"; "full ver"; "video mix"
        |]
    let correct_titles() =
        Logging.Info "Scanning for title variations to prune (TV Size, Extended mix, etc)"
        let remove_mixes_and_cuts (title: string) =
            let mutable title = title.Replace("[EXTRA]", "").Replace("[Extra]", "").Trim()
            for v in title_variations do
                let i = title.ToLower().IndexOf(v)
                if i >= 0 then
                    let matched_v = title.Substring(i, v.Length)
                    title <- title.Replace("("+matched_v+")", "").Replace("["+matched_v+"]", "").Replace("-"+matched_v+"-", "").Replace("- "+matched_v+" -", "").Trim()
            title
        for song_id in songs.Keys |> Array.ofSeq do
            let song = songs.[song_id]
            let suggestion = { song with Title = remove_mixes_and_cuts song.Title; AlternativeTitles = List.map remove_mixes_and_cuts song.AlternativeTitles }
            if suggestion <> song then
                Logging.Info(sprintf "%s -> %s" song.Title suggestion.Title)
                songs.[song_id] <- suggestion

    let private rehome_song_id (old_id: string, new_id: string) =
        for chart_id in charts.Keys do
            let chart = charts.[chart_id]
            if chart.SongId = old_id then
                charts.[chart_id] <- { chart with SongId = new_id }

    type Song_Deduplication = { Title: string; Artists: string list }
    let correct_duplicate_songs() =
        Logging.Info "Scanning for duplicate songs"
        let mutable seen = Map.empty
        for id in songs.Keys |> Array.ofSeq do
            let song = songs.[id]
            let ded = { Title = song.Title.ToLower(); Artists = (song.Artists @ song.OtherArtists @ song.Remixers |> List.sort) |> List.map (fun s -> s.ToLower()) }
            match Map.tryFind ded seen with
            | Some existing ->
                Logging.Info(sprintf "%s is a duplicate of %s, merging" id existing)
                let existing_song = songs.[existing]
                songs.[existing] <- 
                    { existing_song with 
                        Source = Option.orElse song.Source existing_song.Source
                        Tags = List.distinct (existing_song.Tags @ song.Tags)
                        AlternativeTitles = List.distinct (existing_song.AlternativeTitles @ song.AlternativeTitles)
                    }
                songs.Remove id |> ignore
                rehome_song_id (id, existing)
            | None -> seen <- Map.add ded id seen
        save()
    
    type Song_Fuzzy_Deduplication = { Title: string; Artists: string }
    let find_fuzzy_duplicates() =
        let mutable titles = Set.empty
        let mutable seen = Map.empty
        Logging.Info "Looking for fuzzy duplicates (to help with manual correction)"
        for id in songs.Keys |> Array.ofSeq do
            let song = songs.[id]
            let ded = { Title = song.Title.ToLower().Replace(" ", ""); Artists = (song.Artists @ song.OtherArtists @ song.Remixers) |> List.sort |> String.concat "" |> fun s -> s.ToLower().Replace(" ", "") }
            match Map.tryFind ded seen with
            | Some existing -> Logging.Info(sprintf "%s could be a duplicate of %s" id existing)
            | None -> seen <- Map.add ded id seen
            if titles.Contains ded.Title then
                Logging.Info(sprintf "%s could be a duplicate title" ded.Title)
            else titles <- Set.add ded.Title titles

    let correct_song_ids() =
        Logging.Info "Correcting song ids"
        for id in songs.Keys |> Array.ofSeq do
            let song = songs.[id]
            let new_id = (Collect.simplify_string (String.concat "" song.Artists)) + "/" + (Collect.simplify_string song.Title)
            let mutable i = 0
            let generated_id() = if i > 0 then new_id + "-" + i.ToString() else new_id
            while generated_id() <> id && songs.ContainsKey(generated_id()) do
                i <- i + 1

            let new_id = generated_id()
            if new_id <> id then 
                songs.Add(new_id, song)
                songs.Remove(id) |> ignore
                rehome_song_id (id, new_id)
                Logging.Info(sprintf "%s -> %s" id new_id)
        save()

    let run(user_oversight) =
        correct_meta(user_oversight)
        correct_artists()
        correct_titles()
        correct_duplicate_songs()
        correct_song_ids()