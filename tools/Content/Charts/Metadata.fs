namespace YAVSRG.CLI.Features.Backbeat

// various metadata cleanup tools
module Metadata =

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
            "full ver"
            "video mix"
            "animever."
            "animever"
            "tvsize"
        |]

    let prune_song_title (song_title: string) : string =
        let mutable title = song_title.Replace("[EXTRA]", "").Replace("[Extra]", "").Trim()

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

    let prune_tags (tags: string list) : string list =
        tags |> List.filter (fun x -> x.Length > 2)