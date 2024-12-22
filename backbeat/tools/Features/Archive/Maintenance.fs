namespace Backbeat.Features.Archive

open System
open System.Linq

module Maintenance =

    let rec levenshtein (a: char list) (b: char list) =
        if abs (a.Length - b.Length) > 5 then
            100
        else
            match a, b with
            | [], ys -> ys.Length
            | xs, [] -> xs.Length
            | x :: xs, y :: ys when x = y -> levenshtein xs ys
            | x :: xs, y :: ys ->
                let a = levenshtein (x :: xs) ys

                if a >= 100 then
                    100
                else
                    let b = levenshtein xs (y :: ys)

                    if b >= 100 then
                        100
                    else
                        let c = levenshtein xs ys

                        if c >= 100 then
                            100
                        else
                            let res = 1 + min (min a b) c
                            if res > 5 then 100 else res

    //let rename_artist (old_artist: string, new_artist: string) =
    //    let swap = (fun a -> if a = old_artist then new_artist else a)

    //    for id in songs.Keys do
    //        let song = songs.[id]

    //        songs.[id] <-
    //            { song with
    //                Artists = List.map swap song.Artists
    //                OtherArtists = List.map swap song.OtherArtists
    //                Remixers = List.map swap song.Remixers
    //            }

    //    save ()

    //let check_all_artists () =
    //    let mutable checked_artists = Map.empty

    //    let distinct_artists = Queue.get "artists-distinct" |> Array.ofList

    //    let filter (name: string) =
    //        name.Length > 3 && String.forall Char.IsAscii name

    //    let check_artist (context: Song) (artist: string) =
    //        if checked_artists.ContainsKey artist then
    //            if checked_artists.[artist] = 2 && not (artists.Artists.ContainsKey artist) then
    //                Console.WriteLine(
    //                    sprintf
    //                        "'%s' looks like a common artist, want to make a note to verify them? [1 for yes, 2 for no]"
    //                        artist
    //                )

    //                let mutable option_chosen = None

    //                while option_chosen.IsNone do
    //                    match Console.ReadKey().Key with
    //                    | ConsoleKey.D1 -> option_chosen <- Some true
    //                    | ConsoleKey.D2 -> option_chosen <- Some false
    //                    | _ -> ()

    //                if option_chosen.Value then
    //                    Queue.append "artists-verify" artist

    //            checked_artists <- checked_artists.Add(artist, checked_artists.[artist] + 1)
    //        else

    //        let b = List.ofSeq (artist.ToLower())
    //        let mutable closest_match = ""
    //        let mutable closest_match_v = artist.Length / 2

    //        for a in checked_artists.Keys |> Array.ofSeq do
    //            if filter a && filter artist && not (distinct_artists.Contains(artist + "," + a)) then
    //                let dist = levenshtein (List.ofSeq (a.ToLower())) b

    //                if dist < closest_match_v then
    //                    closest_match <- a
    //                    closest_match_v <- dist

    //        let mutable artist = artist

    //        if closest_match <> "" then
    //            Logging.Info "Possible artist match"
    //            Logging.Info "Existing: %A" closest_match
    //            Logging.Info "Incoming: %A" artist
    //            Logging.Info " Context: %s" context.FormattedTitle

    //            Logging.Info(
    //                "\noptions ::\n 1 - Existing is correct\n 2 - Incoming is correct\n 3 - These are not the same artist"
    //            )

    //            let mutable option_chosen = false

    //            while not option_chosen do
    //                match Console.ReadKey().Key with
    //                | ConsoleKey.D1 ->
    //                    rename_artist (artist, closest_match)
    //                    checked_artists <- checked_artists.Remove closest_match
    //                    artist <- closest_match
    //                    option_chosen <- true
    //                | ConsoleKey.D2 ->
    //                    rename_artist (closest_match, artist)
    //                    checked_artists <- checked_artists.Remove closest_match
    //                    option_chosen <- true
    //                | ConsoleKey.D3 ->
    //                    Queue.append "artists-distinct" (artist + "," + closest_match)
    //                    option_chosen <- true
    //                | _ -> ()

    //        checked_artists <- Map.add artist 1 checked_artists

    //    for id in songs.Keys |> Array.ofSeq do
    //        let song = songs.[id]
    //        List.iter (check_artist song) song.Artists
    //        List.iter (check_artist song) song.OtherArtists
    //        List.iter (check_artist song) song.Remixers

    //let verify_artist (name: string) =
    //    if artists.Artists.ContainsKey name then
    //        Logging.Warn("Already exists")
    //    else
    //        let is_japanese = name.Contains ' ' && Collect.romaji_regex.IsMatch(name.ToLower())

    //        artists.Artists.Add(
    //            name,
    //            {
    //                Alternatives = []
    //                IsJapaneseFullName = is_japanese
    //            }
    //        )

    //        Logging.Info "Added %s, Is Japanese: %b" name is_japanese
    //        save ()