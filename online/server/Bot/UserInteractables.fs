namespace Interlude.Web.Server.Bot

open Discord
open Discord.WebSocket
open Prelude.Backbeat.Archive
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat

module UserInteractables =

    let song_search (query: string) (page: int) (details: bool) : Embed * MessageComponent =
        let matches = Songs.search_songs (if details then 1L else 20L) page query

        if matches.Length = 0 && page = 0 then
            EmbedBuilder(Title = "No results found").Build(),
            ComponentBuilder().Build()
        elif matches.Length = 1 then
            let song = snd matches.[0]

            let embed =
                EmbedBuilder(Title = song.Title)
                    .AddField(
                        (if song.Artists.Length > 1 then "Artists" else "Artist"),
                        String.concat ", " song.Artists,
                        true
                    )

            if song.OtherArtists <> [] then
                embed.AddField("Featuring", String.concat ", " song.OtherArtists, true)
                |> ignore

            if song.Remixers <> [] then
                embed.AddField("Remixed by", String.concat ", " song.Remixers, true) |> ignore

            if song.Tags <> [] then
                embed.AddField("Tags", String.concat ", " song.Tags) |> ignore

            if song.Source <> None then
                embed.AddField("Source", song.Source.Value) |> ignore

            embed.WithFooter(EmbedFooterBuilder().WithText(sprintf "%i" (fst matches.[0]))) |> ignore

            embed.WithColor(Color.Blue) |> ignore
            
            let components = 
                ComponentBuilder()
                    .AddRow(
                        ActionRowBuilder()
                            .WithButton(
                                ButtonBuilder(Emote = Emoji.Parse(":arrow_left:"), CustomId = sprintf "search %s$%i%s" query (page - 1) (if details then "$d" else ""))
                                    .WithStyle(ButtonStyle.Secondary)
                                    .WithDisabled(page <= 0)
                            )
                            .WithButton(
                                ButtonBuilder(
                                    Emote = Emoji.Parse(":arrow_right:"),
                                    CustomId = sprintf "search %s$%i%s" query (page + 1) (if details then "$d" else "")
                                )
                                    .WithStyle(ButtonStyle.Secondary)
                                    .WithDisabled(not details)
                            )
                    )

            embed.Build(), components.Build()
        else
            let embed =
                EmbedBuilder(
                    Title = sprintf "Search results - Page %i" (page + 1)
                )
                    .WithDescription(
                        String.concat
                            "\n"
                            (Array.map
                                (fun (_, song: Song) -> song.FormattedTitle.Replace("*", "\\*"))
                                matches)
                    )
                    .WithColor(Color.Blue)

            let components = 
                ComponentBuilder()
                    .AddRow(
                        ActionRowBuilder()
                            .WithButton(
                                ButtonBuilder(Emote = Emoji.Parse(":arrow_left:"), CustomId = sprintf "search %s$%i" query (page - 1))
                                    .WithStyle(ButtonStyle.Secondary)
                                    .WithDisabled(page <= 0)
                            )
                            .WithButton(
                                ButtonBuilder(
                                    Emote = Emoji.Parse(":arrow_right:"),
                                    CustomId = sprintf "search %s$%i" query (page + 1)
                                )
                                    .WithStyle(ButtonStyle.Secondary)
                            )
                    )
            embed.Build(), components.Build()

    let handle_interaction
        (client: DiscordSocketClient)
        (user_id: int64, user_info: User)
        (context: SocketMessageComponent)
        (command: string)
        (args: string list)
        =
        task {
            match command with
            | "search" ->
                match args with
                | query :: page :: [] ->
                    let embed, components = song_search query (int page) false

                    do!
                        context.UpdateAsync(fun msg ->
                            msg.Embed <- embed
                            msg.Components <- components
                        )
                | query :: page :: _ :: [] ->
                    let embed, components = song_search query (int page) true

                    do!
                        context.UpdateAsync(fun msg ->
                            msg.Embed <- embed
                            msg.Components <- components
                        )
                | _ -> failwith "impossible"
            | _ -> ()
        }