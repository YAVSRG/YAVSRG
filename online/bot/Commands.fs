namespace Interlude.Web.Bot

open Discord
open Discord.WebSocket
open Prelude.Data.Charts.Archive

module Commands =

    let dispatch (context: SocketMessage) (command: string) (args: string option) =
        task {
            let reply msg =
                task { 
                    let! _ = context.Channel.SendMessageAsync(msg)
                    return ()
                }
            let reply_embed embed = 
                task {
                    let! _ = context.Channel.SendMessageAsync(embed = embed)
                    return ()
                }
            match command with
            | "search" ->
                match args with

                | None -> 
                    do! reply "Enter a search term, for example: $search PLANET//SHAPER"

                | Some args -> 
                    let matches = Charts.search_for_charts args |> List.ofSeq
                    match matches with
                    | [] -> 
                        do! reply "No matches found."
                    | (song, charts) :: [] ->
                        let embed = 
                            EmbedBuilder(Title = song.Title)
                                .AddField((if song.Artists.Length > 1 then "Artists" else "Artist"), String.concat ", " song.Artists, true)
                        if song.OtherArtists <> [] then
                            embed.AddField("Featuring", String.concat ", " song.OtherArtists, true) |> ignore
                        if song.Remixers <> [] then
                            embed.AddField("Remixed by", String.concat ", " song.Remixers, true) |> ignore
                        if song.Tags <> [] then
                            embed.AddField("Tags", String.concat ", " song.Tags) |> ignore
                        if song.Source <> None then
                            embed.AddField("Source", song.Source.Value) |> ignore
                        embed.WithColor(Color.Blue) |> ignore
                        for chart in charts do
                            embed.AddField(chart.DifficultyName + " by " + String.concat ", " chart.Creators, String.concat "  |  " (chart.Sources |> List.map Charts.format_source)) |> ignore
                        do! reply_embed (embed.Build())
                    | _ ->
                        do! reply (String.concat "\n" (List.map (fun (song: Song, charts) -> song.FormattedTitle) matches))
            | _ -> ()
        }