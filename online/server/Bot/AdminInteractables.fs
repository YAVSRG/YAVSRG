namespace Interlude.Web.Server.Bot

open Discord
open Discord.WebSocket
open Prelude.Backbeat.Archive
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat

module AdminInteractables =

    let user_list (page: int) : Embed * MessageComponent =
        let user_list = User.list (page)
        let user_count = User.count()

        let embed =
            EmbedBuilder(Title = sprintf "All registered users (%i)" user_count)
                .WithDescription(
                    if user_list.Length = 0 then
                        "Empty page :("
                    else
                        user_list
                        |> Seq.map snd
                        |> Seq.map (fun user -> user.Username)
                        |> String.concat "\n"
                )
                .WithColor(Color.Blue)
                .Build()

        let components =
            ComponentBuilder()
                .AddRow(
                    ActionRowBuilder()
                        .WithButton(
                            ButtonBuilder(Emote = Emoji.Parse(":arrow_left:"), CustomId = sprintf "users %i" (page - 1))
                                .WithStyle(ButtonStyle.Secondary)
                                .WithDisabled(page <= 0)
                        )
                        .WithButton(
                            ButtonBuilder(
                                Emote = Emoji.Parse(":arrow_right:"),
                                CustomId = sprintf "users %i" (page + 1)
                            )
                                .WithStyle(ButtonStyle.Secondary)
                                .WithDisabled(user_list.Length < 15)
                        )
                )
                .Build()

        embed, components

    let do_songs_match (song_a_id: int64, song_a: Song, song_b_id: int64, song_b: Song) : Embed * MessageComponent =
        let embed =
            EmbedBuilder(Title = "Are these the same song?")
                .WithDescription(
                    sprintf "%s\n <-> \n%s" song_a.FormattedTitle song_b.FormattedTitle
                )
                .Build()

        let components =
            ComponentBuilder()
                .AddRow(
                    ActionRowBuilder()
                        .WithButton(
                            ButtonBuilder(Emote = Emoji.Parse(":white_check_mark:"), CustomId = sprintf "merge_songs %i$%i" song_a_id song_b_id)
                                .WithStyle(ButtonStyle.Secondary)
                        )
                        .WithButton(
                            ButtonBuilder(
                                Emote = Emoji.Parse(":x:"),
                                CustomId = sprintf "dismiss"
                            )
                                .WithStyle(ButtonStyle.Secondary)
                        )
                )
                .Build()

        embed, components

    let handle_interaction
        (client: DiscordSocketClient)
        (user_id: int64, user_info: User)
        (context: SocketMessageComponent)
        (command: string)
        (args: string list)
        =
        task {
            match command with
            | "users" ->
                match args with
                | page :: [] ->
                    let embed, components = user_list (int page)

                    do!
                        context.UpdateAsync(fun msg ->
                            msg.Embed <- embed
                            msg.Components <- components
                        )
                | _ -> failwith "impossible"
            | "search" ->
                match args with
                | query :: page :: [] ->
                    let embed, components = UserInteractables.song_search query (int page) false

                    do!
                        context.UpdateAsync(fun msg ->
                            msg.Embed <- embed
                            msg.Components <- components
                        )
                | query :: page :: _ :: [] ->
                    let embed, components = UserInteractables.song_search query (int page) true

                    do!
                        context.UpdateAsync(fun msg ->
                            msg.Embed <- embed
                            msg.Components <- components
                        )
                | _ -> failwith "impossible"
            | "merge_songs" ->
                match args with
                | id_a :: id_b :: [] ->
                    if Songs.merge_songs (int64 id_b) (int64 id_a) then
                        do! context.Message.DeleteAsync()
                | _ -> failwith "impossible"
            | "dismiss" -> do! context.Message.DeleteAsync()
            | _ -> ()
        }