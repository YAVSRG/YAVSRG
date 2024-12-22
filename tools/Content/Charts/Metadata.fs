namespace YAVSRG.CLI.Features.Backbeat

open System
open System.Text.RegularExpressions
open FParsec
open Percyqaz.Common
open Prelude
open Prelude.Backbeat.Archive

// various metadata cleanup tools
module Metadata =

    let prune_tags (tags: string list) : string list =
        tags
        |> Seq.map (fun s -> s.ToLowerInvariant().Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
        |> Seq.concat
        |> Seq.filter (fun x -> x.Length > 2)
        |> Seq.distinct
        |> List.ofSeq

    let private TITLE_VARIATIONS =
        [|
            "tv size ver."
            "tv size ver"
            "tv ver."
            "tv version"
            "tv size"
            "tv ver"
            "tv edit"
            "tv-size"
            "tv.ver"
            "anime ver."
            "anime ver"
            "op cut"
            "op ver."
            "op ver"
            "uncut ver."
            "uncut ver"
            "long ver."
            "long ver"
            "extended ver."
            "extended ver"
            "extended mix"
            "extended edit"
            "radio ver."
            "radio ver"
            "radio edit"
            "radio mix"
            "cut ver."
            "cut ver"
            "cut.ver"
            "short ver."
            "short ver"
            "short edit"
            "short cut"
            "short.ver"
            "speed up ver."
            "speed up ver"
            "sped up ver."
            "sped up ver"
            "album ver"
            "original mix"
            "bass boosted"
            "full version"
            "full ver."
            "full ver"
            "video mix"
            "animever."
            "animever"
            "anime.ver"
            "tvsize"
            "bonus track"
            "extra"
        |]

    let prune_song_title (song_title: string) : string =
        let mutable title = song_title.Trim()

        for v in TITLE_VARIATIONS do
            let i = title.ToLower().IndexOf(v)

            if i >= 0 then
                let matched_v = title.Substring(i, v.Length)

                title <-
                    title
                        .Replace("(" + matched_v + ")", "")
                        .Replace("[" + matched_v + "]", "")
                        .Replace("-" + matched_v + "-", "")
                        .Replace("- " + matched_v + " -", "")
                        .Trim()

        title

    module private Extraction =

        let artists : VerifiedArtists = System.IO.Path.Combine(YAVSRG.CLI.Utils.YAVSRG_PATH, "backbeat", "archive", "artists.json") |> JSON.FromFile |> expect

        type ArtistFragment =
            | Verified of string
            | Artist of string

        type ArtistsAndFeatures = ArtistFragment list * ArtistFragment list option

        module ArtistsAndFeatures =
            let artists (a, f) =
                a
                |> List.map (
                    function
                    | Verified v -> v
                    | Artist a -> a.Trim()
                )

            let features (a, f) =
                Option.defaultValue [] f
                |> List.map (
                    function
                    | Verified v -> v
                    | Artist a -> a.Trim()
                )

            let flatten x = artists x @ features x

            let confident (a, f) =
                let mutable count1 = 0
                let mutable count2 = 0

                for a in a do
                    match a with
                    | Artist a -> count1 <- count1 + 1
                    | _ -> ()

                for f in Option.defaultValue [] f do
                    match f with
                    | Artist a -> count2 <- count2 + 1
                    | _ -> ()

                count1 < 2 && count2 < 2

        let private collab_separator : Parser<string, unit> =
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
            <|> pstringCI "prod."
            .>> spaces

        let private feature_separator : Parser<string, unit> =
            pstringCI " feat." .>> spaces <|> pstringCI " ft." .>> spaces
            <|> pstringCI " featuring."
            .>> spaces
            <|> pstringCI " feat"
            .>> spaces
            <|> pstringCI " ft"
            .>> spaces

        let private parse_collab: Parser<_, unit> =
            let verified =
                let mapping = artists.ParserMapping()

                match mapping.Keys |> Seq.sortByDescending (fun s -> s.Length) |> List.ofSeq with
                | [] -> pzero
                | x :: xs ->
                    let mutable parser = stringReturn x (Verified mapping.[x])

                    for x in xs do
                        parser <- parser <|> stringReturn x (Verified x)

                    parser

            let unverified =
                many1CharsTill anyChar (followedBy collab_separator <|> followedBy feature_separator <|> eof)
                |>> Artist

            sepBy1 (verified <|> unverified) collab_separator

        let private parse_prod_and_features: Parser<ArtistsAndFeatures, unit> =
            parse_collab .>>. opt (feature_separator >>. parse_collab)

        let private extract_bracket (text: string) =
            let bracket_regex = Regex("\\((.*?)\\)$")

            let matches = bracket_regex.Matches text

            if matches.Count = 1 then
                let bracket_content = matches.[0].Groups.[1].Value
                text.Replace(matches.[0].Value, "").Trim(), Some bracket_content
            else
                text, None

        let extract_artists_remixers_from_artist_string (artists: string) : string list * string list * string list * string * bool =
            let p = parse_prod_and_features .>>. restOfLine false

            let artist, bracket = extract_bracket artists

            let mutable remixers = []
            let mutable artists = [ artist ]
            let mutable performers = []
            let mutable confident = true
            let mutable append_to_title = ""

            match bracket with
            | Some s when
                s.ToLower().EndsWith(" remix")
                || s.ToLower().EndsWith(" mix")
                || s.ToLower().EndsWith(" edit")
                || s.ToLower().EndsWith(" bootleg")
                ->
                match run p s with
                | Success((res, leftover), _, _) ->
                    remixers <- ArtistsAndFeatures.flatten res
                    append_to_title <- "(" + s + ")"

                    if leftover <> "" then
                        confident <- false
                | Failure(reason, _, _) -> confident <- false

            | Some s when
                s.ToLower().StartsWith("remix by ")
                || s.ToLower().StartsWith("remixed by ")
                || s.ToLower().StartsWith("edit by ")
                ->
                let remixer = let split = s.Split(" by ", StringSplitOptions.TrimEntries) in split.[split.Length - 1]
                remixers <- [ remixer ]
                append_to_title <- "(" + remixer + " Remix)"
            | Some s ->
                Logging.Info "Unusual bracket text '%s'" s
                confident <- false
            | None -> ()

            match run p artist with
            | Success((res, leftover), _, _) ->
                artists <- ArtistsAndFeatures.artists res
                performers <- ArtistsAndFeatures.features res

                if not (ArtistsAndFeatures.confident res) then
                    confident <- false

                if leftover <> "" then
                    confident <- false
                    artists <- (List.head artists) + leftover :: List.tail artists
            | Failure(reason, _, _) -> ()

            artists, performers, remixers, append_to_title, confident

        let extract_artists_remixers_from_title_string (title: string) : string * string list * string list * bool =
            let p =
                many1CharsTill anyChar (followedBy ((feature_separator |>> ignore) <|> eof))
                .>>. opt (feature_separator >>. parse_collab)

            let title, bracket = extract_bracket title

            let mutable remixers = []
            let mutable performers = []
            let mutable confident = true
            let mutable suggested_title = title

            match bracket with
            | Some s when
                s.ToLower().EndsWith(" remix")
                || s.ToLower().EndsWith(" mix")
                || s.ToLower().EndsWith(" edit")
                || s.ToLower().EndsWith(" bootleg")
                ->
                if s.Contains("'s") then
                    remixers <- [ s.Split("'s").[0] ]
                    suggested_title <- title + " (" + s + ")"
                else
                    Logging.Info "Unknown remix name '%s'" s
                    confident <- false
                    suggested_title <- title + " (" + s + ")"
            | Some s ->
                Logging.Info "Unusual bracket text '%s'" s
                confident <- false
                suggested_title <- title + " (" + s + ")"
            | None -> ()

            match run p title with
            | Success((title, features), _, _) ->
                if features.IsSome then
                    suggested_title <- title
                    performers <- ArtistsAndFeatures.features ([], features)

                    if not (ArtistsAndFeatures.confident ([], features)) then
                        confident <- false
            | Failure(reason, _, _) -> ()

            suggested_title, performers, remixers, confident

    let extract_better_metadata (song: Song) : Song =
        let suggestion_1, suggestion_1_confident =

            if song.Artists.Length = 1 && song.OtherArtists = [] && song.Remixers = [] then
                let artists, features, remixers, append_to_title, confident =
                    Extraction.extract_artists_remixers_from_artist_string song.Artists.[0]

                { song with
                    Artists = artists
                    OtherArtists = features
                    Remixers = remixers
                    Title =
                        if append_to_title <> "" then
                            song.Title.Trim() + " " + append_to_title
                        else
                            song.Title
                }, confident
            else song, true

        if suggestion_1_confident && suggestion_1 <> song then suggestion_1 else

        let suggestion_2, suggestion_2_confident =
            if song.Artists.Length = 1 && song.OtherArtists = [] && song.Remixers = [] then
                let new_title, features, remixers, confident =
                    Extraction.extract_artists_remixers_from_title_string song.Title

                { song with
                    OtherArtists = features
                    Remixers = remixers
                    Title = new_title
                }, confident
            else song, true

        if suggestion_2_confident && suggestion_2 <> song then suggestion_2 else

        song

    let private character_voice_regex = Regex("[\\[\\(][cC][vV][.:\\-]?\\s?(.*)[\\]\\)]")
    let private fixed_artists = Extraction.artists.FixerMapping()
    let correct_artist_typos (song: Song) : Song =

        let swap (s: string) =
            if fixed_artists.ContainsKey(s.ToLower()) then
                let replace = fixed_artists.[s.ToLower()]
                replace
            else
                s

        let fix (artist: string) =
            let matches = character_voice_regex.Matches artist

            if matches.Count = 1 then
                let character_voice = matches.[0].Groups.[1].Value
                swap character_voice
            else
                swap artist

        { song with
            Artists = List.map fix song.Artists
            OtherArtists = List.map fix song.OtherArtists
            Remixers = List.map fix song.Remixers
        }