namespace Backbeat.Features.Archive

module Backbot = ()

    //let make_suggestion (flag: string) (id: int64) (before: Song) (after: Song) : bool =
    //    let inline diff label a b =
    //        if a <> b then
    //            Logging.Info "%s\n %A vvv\n %A" label a b

    //    Logging.Info "Backbot has a suggestion for '%i' that needs your approval" id
    //    diff "Artists" before.Artists after.Artists
    //    diff "Performers" before.OtherArtists after.OtherArtists
    //    diff "Remixers" before.Remixers after.Remixers
    //    diff "Title" before.Title after.Title
    //    diff "Alt Titles" before.AlternativeTitles after.AlternativeTitles
    //    diff "Formatted title" before.FormattedTitle after.FormattedTitle
    //    diff "Tags" before.Tags after.Tags
    //    Logging.Info "Reason: %s" flag
    //    Logging.Info("\noptions ::\n 1 - Make this change\n 2 - Queue for manual review\n 3 - No correction needed")
    //    let mutable option_chosen = None

    //    //while option_chosen.IsNone do
    //    //    match Console.ReadKey().Key with
    //    //    | ConsoleKey.D1 -> option_chosen <- Some true
    //    //    | ConsoleKey.D2 ->
    //    //        option_chosen <- Some false
    //    //        Queue.append "song-review" id
    //    //    | ConsoleKey.D3 ->
    //    //        option_chosen <- Some false
    //    //        Queue.append "song-ignore" id
    //    //    | _ -> ()

    //    option_chosen.Value

    //type Song_Deduplication = { Title: string; Artists: string list }

    //let correct_duplicate_songs () =
    //    Logging.Info "Scanning for duplicate songs"
    //    let mutable seen = Map.empty

    //    for id in songs.Keys |> Array.ofSeq do
    //        let song = songs.[id]

    //        let ded =
    //            {
    //                Title = song.Title.ToLower()
    //                Artists =
    //                    (song.Artists @ song.OtherArtists @ song.Remixers |> List.sort)
    //                    |> List.map (fun s -> s.ToLower())
    //            }

    //        match Map.tryFind ded seen with
    //        | Some existing ->
    //            Logging.Info "%s is a duplicate of %s, merging" id existing
    //            let existing_song = songs.[existing]

    //            songs.[existing] <-
    //                { existing_song with
    //                    Source = Option.orElse song.Source existing_song.Source
    //                    Tags = List.distinct (existing_song.Tags @ song.Tags)
    //                    AlternativeTitles = List.distinct (existing_song.AlternativeTitles @ song.AlternativeTitles)
    //                }

    //            songs.Remove id |> ignore
    //            rehome_song_id (id, existing)
    //        | None -> seen <- Map.add ded id seen

    //    save ()

    //type Song_Fuzzy_Deduplication = { Title: string; Artists: string }

    //let find_fuzzy_duplicates () =
    //    let mutable titles = Set.empty
    //    let mutable seen = Map.empty
    //    Logging.Info "Looking for fuzzy duplicates (to help with manual correction)"

    //    for id in songs.Keys |> Array.ofSeq do
    //        let song = songs.[id]

    //        let ded =
    //            {
    //                Title = song.Title.ToLower().Replace(" ", "")
    //                Artists =
    //                    (song.Artists @ song.OtherArtists @ song.Remixers)
    //                    |> List.sort
    //                    |> String.concat ""
    //                    |> fun s -> s.ToLower().Replace(" ", "")
    //            }

    //        match Map.tryFind ded seen with
    //        | Some existing -> Logging.Info "%s could be a duplicate of %s" id existing
    //        | None -> seen <- Map.add ded id seen

    //        if titles.Contains ded.Title then
    //            Logging.Info "%s could be a duplicate title" ded.Title
    //        else
    //            titles <- Set.add ded.Title titles