namespace Backbeat.Features.Archive

open System
open System.Linq
open Percyqaz.Common
open Prelude.Data.Charts.Archive

module Maintenance =

    type Approval =
        | APPLY_NOW
        | MANUAL_DATA of string
        | IGNORE
    let make_suggestion (flag: string) (before: Song) (after: Song) : Approval =
        let inline diff label a b = if a <> b then Logging.Info(sprintf "%s\n %A vvv\n %A" label a b)
        Logging.Info(sprintf "Suggested metadata changes for %s" before.FormattedTitle)
        diff "Artists" before.Artists after.Artists
        diff "Performers" before.OtherArtists after.OtherArtists
        diff "Remixers" before.Remixers after.Remixers
        diff "Title" before.Title after.Title
        diff "Alt Titles" before.AlternativeTitles after.AlternativeTitles
        diff "Formatted title" before.FormattedTitle after.FormattedTitle
        diff "Tags" before.Tags after.Tags
        Logging.Info(sprintf "Reason: %s" flag)
        Logging.Info("\noptions ::\n 1 - Accept changes\n 2 - Enter manual fix\n 3 - No correction needed, don't suggest for this song")
        let mutable option_chosen = None
        while option_chosen.IsNone do
            match Console.ReadKey().Key with
            | ConsoleKey.D1 -> option_chosen <- Some APPLY_NOW
            | ConsoleKey.D2 -> option_chosen <- Some (MANUAL_DATA (Console.ReadLine()))
            | ConsoleKey.D3 -> option_chosen <- Some IGNORE
            | _ -> ()
        option_chosen.Value

    let variations = [|"tv size ver."; "tv ver."; "tv size"; "anime ver."; "op ver."; "uncut ver."; "cut ver."; "long ver."; "short ver."; "extended ver."; "extended mix"; "original mix"|]
    let song_version (song_id: SongId) (song: Song) =
        let remove_mixes_and_cuts (title: string) =
            let mutable title = title
            for v in variations do
                let i = title.ToLower().IndexOf(v)
                if i >= 0 then
                    let matched_v = title.Substring(i, v.Length)
                    title <- title.Replace("("+matched_v+")", "").Replace("["+matched_v+"]", "").Replace("-"+matched_v+"-", "").Replace("- "+matched_v+" -", "").Trim()
            title
        let suggestion = { song with Title = remove_mixes_and_cuts song.Title; AlternativeTitles = List.map remove_mixes_and_cuts song.AlternativeTitles }
        if suggestion <> song then
            match make_suggestion "SONGMIXES" song suggestion with
            | APPLY_NOW -> songs.[song_id] <- suggestion; save()
            | MANUAL_DATA s -> Logging.Debug "Ignoring manual data for now"
            | IGNORE -> ()

    let remix_regex = Text.RegularExpressions.Regex("\\((.*?) [rR]emix\\)$")
    let remixers_from_title (song_id: SongId) (song: Song) =
        let title_matches = remix_regex.Matches song.Title

        let suggestion = 
            if title_matches.Count = 1 then
                let remix_name = title_matches.[0].Groups.[1].Value
                let original_artist = song.Title.Replace(title_matches.First().Value, "").Trim()
                let remixer = if remix_name.Contains("'s") then remix_name.Split("'s").[0] else remix_name
                if song.Remixers = [] then
                    { song with Remixers = remixer :: song.Remixers }
                else song
            else song
        if suggestion <> song then
            match make_suggestion "REMIXINTITLE" song suggestion with
            | APPLY_NOW -> songs.[song_id] <- suggestion; save()
            | MANUAL_DATA s -> songs.[song_id] <- { song with Remixers = s.Split(",", StringSplitOptions.TrimEntries) |> List.ofArray }; save()
            | IGNORE -> ()

    let remixers_to_title (song_id: SongId) (song: Song) =
        let add_remix_to_title (title: string) =
            if song.Remixers <> [] && not (title.Contains("remix", StringComparison.InvariantCultureIgnoreCase)) then
                title + " (" + String.concat " & " song.Remixers + " Remix)"
            else title
        let suggestion = { song with Title = add_remix_to_title song.Title; AlternativeTitles = List.map add_remix_to_title song.AlternativeTitles }
        if suggestion <> song then
            match make_suggestion "REMIXTOTITLE" song suggestion with
            | APPLY_NOW -> songs.[song_id] <- suggestion; save()
            | MANUAL_DATA s -> Logging.Debug "Ignoring manual data for now"
            | IGNORE -> ()

    let song_meta_checks_v2 (song_id: SongId) (song: Song) =
        song_version song_id song
        remixers_from_title song_id song
        remixers_to_title song_id song

    let feature_separators = [|"FEAT."; "FT."; "Feat."; "Ft."; "featuring."; "feat."; "ft."|]
    let collab_separators = [|" x "; " X "; " / "; " VS "; " Vs "; " vs "; " vs. "; " Vs. "; " VS. "; "&"; ", and "; ","; " and "|]
    let artist_separators = [|"&"; ", and "; ","; " and "|]
    let song_meta_checks (song_id: SongId) (song: Song) =
        let fmt = song.FormattedTitle
        let suggestion =
            if song.Artists.Length = 1 && song.OtherArtists = [] && song.Remixers = [] then

                let artist, title, remixers =
                    let artist_matches = remix_regex.Matches(song.Artists.Head)
                    let title_matches = remix_regex.Matches(song.Title)

                    if artist_matches.Count = 1 then
                        let r: string = artist_matches.[0].Groups.[1].Value
                        let original_artist = song.Artists.Head.Replace(artist_matches.First().Value, "").Trim()
                        original_artist, song.Title, r.Split(collab_separators, StringSplitOptions.TrimEntries) |> List.ofArray

                    elif title_matches.Count = 1 then
                        let r: string = title_matches.[0].Groups.[1].Value
                        let original_title = song.Title.Replace(title_matches.First().Value, "").Trim()
                        song.Artists.Head, original_title, r.Split(collab_separators, StringSplitOptions.TrimEntries) |> List.ofArray

                    else song.Artists.Head, song.Title, []

                let artists, features =
                    let ftSplit : string array = artist.Split(feature_separators, StringSplitOptions.TrimEntries)
                    ftSplit.[0].TrimEnd([|' '; '('|]).Split(collab_separators, StringSplitOptions.TrimEntries) |> List.ofArray,
                    if ftSplit.Length > 1 then
                        ftSplit.[1].TrimEnd([|' '; ')'|]).Split(artist_separators, StringSplitOptions.TrimEntries) |> List.ofArray
                    else []

                if artists <> song.Artists || features <> song.OtherArtists || remixers <> song.Remixers || title <> song.Title then
                    Some { song with Artists = artists; OtherArtists = features; Remixers = remixers; Title = title }
                else None
            else None
        match suggestion with
        | Some suggestion ->
            Logging.Info(sprintf "Suggested metadata change\n%A\n vvv \n%A" song suggestion)
            Logging.Info(sprintf "New: %s" suggestion.FormattedTitle)
            Logging.Info(sprintf "Old: %s" fmt)
            Logging.Info("\noptions ::\n 1 - Accept changes\n 2 - Accept changes but mark for manual edit\n 3 - Ignore changes but mark for manual edit\n 4 - No correction needed, don't suggest for this song")
            let mutable option_chosen = false
            while not option_chosen do
                match Console.ReadKey().Key with
                | ConsoleKey.D1 -> songs.[song_id] <- suggestion; save(); option_chosen <- true
                | ConsoleKey.D2 -> songs.[song_id] <- suggestion; Queue.append "songs-review" song_id; save(); option_chosen <- true
                | ConsoleKey.D3 -> Queue.append "songs-review" song_id; option_chosen <- true
                | ConsoleKey.D4 -> Queue.append "songs-ignore" song_id; option_chosen <- true
                | _ -> ()
        | _ -> ()

    let rehome_song_id (old_id: string, new_id: string) =
        for chart_id in charts.Keys do
            let chart = charts.[chart_id]
            if chart.SongId = old_id then
                charts.[chart_id] <- { chart with SongId = new_id }
        save()

    let clean_duplicate_songs() =
        let mutable seen = Map.empty
        for id in songs.Keys |> Array.ofSeq do
            match Map.tryFind songs.[id] seen with
            | Some existing ->
                Logging.Info(sprintf "%s is an exact duplicate of %s, removing" id existing)
                songs.Remove id |> ignore
                rehome_song_id (id, existing)
            | None -> seen <- Map.add songs.[id] id seen

    let check_all_songs() =
        clean_duplicate_songs()
        let ignores = Queue.get "songs-ignore"
        let reviews = Queue.get "songs-review"
        for id in songs.Keys |> Seq.except ignores |> Seq.except reviews do
            song_meta_checks_v2 id songs.[id]

    let rename_artist (old_artist: string, new_artist: string) =
        let swap = (fun a -> if a = old_artist then new_artist else a)
        for id in songs.Keys do
            let song = songs.[id]
            songs.[id] <-
                { song with
                    Artists = List.map swap song.Artists
                    OtherArtists = List.map swap song.OtherArtists
                    Remixers = List.map swap song.Remixers
                }
        save()

    let rec levenshtein (a: char list) (b: char list) =
        if abs (a.Length - b.Length) > 5 then 100 else
        match a, b with
        | [], ys -> ys.Length
        | xs, [] -> xs.Length
        | x :: xs, y :: ys when x = y -> levenshtein xs ys
        | x :: xs, y :: ys ->
            let a = levenshtein (x :: xs) ys
            if a >= 100 then 100 else
            let b = levenshtein xs (y :: ys)
            if b >= 100 then 100 else
            let c = levenshtein xs ys
            if c >= 100 then 100 else
            let res = 1 + min (min a b) c
            if res > 5 then 100 else res

    let check_all_artists() =
        let artists = ResizeArray<string>()

        let distinct_artists = Queue.get "artists-distinct" |> Array.ofList

        let filter (name: string) =
            name.Length > 3 && String.forall Char.IsAscii name

        let check_artist (context: Song) (artist: string) =
            if artists.Contains artist then () else

            let b = List.ofSeq (artist.ToLower())
            let mutable closest_match = ""
            let mutable closest_match_v = artist.Length / 2
            for a in artists |> Array.ofSeq do
                if filter a && filter artist && not (distinct_artists.Contains (artist + "," + a)) then
                    let dist = levenshtein (List.ofSeq (a.ToLower())) b
                    if dist < closest_match_v then closest_match <- a; closest_match_v <- dist
                    
            let mutable artist = artist
            if closest_match <> "" then
                Logging.Info(sprintf "Possible artist match")
                Logging.Info(sprintf "Existing: %A" closest_match)
                Logging.Info(sprintf "Incoming: %A" artist)
                Logging.Info(sprintf " Context: %s" context.FormattedTitle)
                Logging.Info("\noptions ::\n 1 - Existing is correct\n 2 - Incoming is correct\n 3 - These are not the same artist")
                let mutable option_chosen = false
                while not option_chosen do
                    match Console.ReadKey().Key with
                    | ConsoleKey.D1 -> rename_artist (artist, closest_match); artists.Remove closest_match |> ignore; artist <- closest_match; option_chosen <- true
                    | ConsoleKey.D2 -> rename_artist (closest_match, artist); artists.Remove closest_match |> ignore; option_chosen <- true
                    | ConsoleKey.D3 -> Queue.append "artists-distinct" (artist + "," + closest_match); option_chosen <- true
                    | _ -> ()
            artists.Add artist
                    
        for id in songs.Keys |> Array.ofSeq do
            let song = songs.[id]
            List.iter (check_artist song) song.Artists
            List.iter (check_artist song) song.OtherArtists
            List.iter (check_artist song) song.Remixers