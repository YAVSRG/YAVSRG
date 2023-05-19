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
            | "song" ->
                match args with

                | None -> 
                    do! reply "Enter a search term, for example: $song PLANET//SHAPER"

                | Some args -> 
                    let songs = Charts.search_for_songs args |> List.ofSeq
                    match songs with
                    | [] -> 
                        do! reply "No matches found."
                    | (id, song) :: [] ->
                        let embed = 
                            EmbedBuilder(Title = song.Title)
                                .AddField((if song.Artists.Length > 1 then "Artists" else "Artist"), String.concat ", " song.Artists)
                        if song.OtherArtists <> [] then
                            embed.AddField("Featuring", String.concat ", " song.OtherArtists) |> ignore
                        if song.Remixers <> [] then
                            embed.AddField("Remixed by", String.concat ", " song.Remixers) |> ignore
                        if song.Tags <> [] then
                            embed.AddField("Tags", String.concat ", " song.Tags) |> ignore
                        if song.Source <> None then
                            embed.AddField("Source", song.Source.Value) |> ignore
                        embed.WithColor(Color.Blue) |> ignore
                        do! reply_embed (embed.Build())
                    | _ ->
                        do! reply (String.concat "\n" (List.map (fun (id, song: Song) -> song.FormattedTitle) songs))
            | _ -> ()
        }