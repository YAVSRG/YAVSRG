namespace Interlude.Features.Online

open System
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Interlude.Utils
open Interlude.UI.Components
open Interlude.Web.Shared.Requests

module Players =

    let mutable current_player = None
    let mutable player_changed = ignore

    let switch(player: string option) =
        current_player <- player
        player_changed()

type RecentScores(scores: Players.Profile.RecentScore array) =
    inherit StaticWidget(NodeType.None)

    let scores = scores |> Array.map (fun score -> score, (DateTimeOffset.Now - DateTimeOffset.FromUnixTimeMilliseconds(score.Timestamp) |> formatTimeOffset) + " ago")

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        let h = this.Bounds.Height / 10.0f
        let mutable y = 0.0f
        for score, ago in scores do

            let b = this.Bounds.TrimTop(y).SliceTop(h)

            Text.drawFillB(Style.font, score.Title, b.SliceTop(50.0f).Shrink(10.0f, 0.0f), Colors.text, Alignment.LEFT)
            Text.drawFillB(Style.font, score.Artist + "  •  " + score.Difficulty + "  •  " + ago, b.TrimTop(50.0f).Shrink(10.0f, 0.0f).Translate(0.0f, -5.0f), Colors.text_subheading, Alignment.LEFT)

            Text.drawFillB(Style.font, sprintf "%.2f%%" (score.Score * 100.0), b.TrimRight(h * 3.5f).SliceRight(h * 2.0f).Shrink(0.0f, h * 0.2f), Colors.text, Alignment.RIGHT)
            Text.drawFillB(Style.font, score.Lamp, b.TrimRight(h * 2.0f).SliceRight(h).Shrink(0.0f, h * 0.2f), Colors.text, Alignment.CENTER)
            Text.drawFillB(Style.font, score.Mods, b.TrimRight(h * 0.5f).SliceRight(h).Shrink(0.0f, h * 0.2f), Colors.text, Alignment.CENTER)

            y <- y + h

type PlayerButton(username, color) =
    inherit StaticContainer(NodeType.Button(fun () -> Players.switch (Some username)))

    override this.Init(parent) =
        this 
        |+ Text(username, Color = K (Color.FromArgb color, Colors.shadow_2), Align = Alignment.LEFT, Position = Position.Margin(20.0f, 5.0f))
        |* Clickable.Focus this
        base.Init parent

    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
        base.Draw()

type private OnlineList() =
    inherit WebRequestContainer<Players.Online.Response>(
        fun this ->
            if Network.status = Network.Status.LoggedIn then
                Players.Online.get(
                    fun response -> sync <| fun () ->
                        match response with
                        | Some result -> this.SetData result
                        | None -> this.ServerError()
                )
            else this.Offline()
        , 
        fun data ->
            let contents = FlowContainer.Vertical<Widget>(70.0f)
            for player in data.Players do contents.Add (PlayerButton(player.Username, player.Color))
            ScrollContainer.Flow(contents) :> Widget
        )
    
    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

type private FriendList() =
    inherit WebRequestContainer<Friends.List.Response>(
        fun this ->
            if Network.status = Network.Status.LoggedIn then
                Friends.List.get(
                    fun response -> sync <| fun () ->
                        match response with
                        | Some result -> this.SetData result
                        | None -> this.ServerError()
                )
            else this.Offline()
        , 
        fun data ->
            let contents = FlowContainer.Vertical<Widget>(70.0f)
            for player in data.Friends do 
                contents.Add (
                    StaticContainer(NodeType.None)
                    |+ PlayerButton(player.Username, player.Color)
                    |+ Text((if player.Online then "Online" else "Offline"), Color = K (if player.Online then Colors.text_green_2 else Colors.text_greyout), Align = Alignment.RIGHT, Position = Position.Margin(20.0f, 15.0f))
                )          
            ScrollContainer.Flow(contents) :> Widget
        )
    
    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()

type PlayerList() =
    inherit StaticContainer(NodeType.None)

    let online = OnlineList()
    let friends = FriendList()

    let swap = SwapContainer(Current = online, Position = Position.TrimTop(50.0f).Margin(40.0f))

    let button(label: string, cmp) =
        Frame(NodeType.None, Border = K Color.Transparent, Fill = fun () -> if swap.Current = cmp then !*Palette.DARK_100 else Colors.black.O2)
        |+ Button(label, fun () -> swap.Current <- cmp)

    override this.Init(parent) =
        this
        |+ 
            (
                FlowContainer.LeftToRight(200.0f, Position = Position.SliceTop(50.0f))
                |+ button(L"online.players.online", online)
                |+ button(L"online.players.friends", friends)
            )
        |* swap
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.TrimTop(50.0f)) !*Palette.DARK_100
        base.Draw()

type Profile() =
    inherit WebRequestContainer<Players.Profile.Response>(
        fun this ->
            if Network.status = Network.Status.LoggedIn then
                match Players.current_player with
                | Some p ->
                    Players.Profile.get(p,
                        fun response -> sync <| fun () ->
                            match response with
                            | Some result -> this.SetData result
                            | None -> this.ServerError()
                    )
                | None ->
                    Players.Profile.get_me(
                        fun response -> sync <| fun () ->
                            match response with
                            | Some result -> this.SetData result
                            | None -> this.ServerError()
                    )
            else this.Offline()
        , 
        fun data ->
            StaticContainer(NodeType.None)
            |+ Text(data.Username, 
                Color = K (Color.FromArgb data.Color, Colors.shadow_2),
                Align = Alignment.LEFT,
                Position = Position.SliceTop(80.0f).Margin(45.0f, 5.0f))
            |+ Text(String.concat ", " (data.Badges |> Seq.map (fun b -> b.Name)), 
                Color = K Colors.text_subheading,
                Align = Alignment.LEFT,
                Position = Position.TrimTop(70.0f).SliceTop(40.0f).Margin(45.0f, 0.0f))
            |+ Text(sprintf "Player since %O" (DateTimeOffset.FromUnixTimeMilliseconds(data.DateSignedUp).ToLocalTime().DateTime.ToShortDateString()),
                Color = K Colors.text_subheading,
                Align = Alignment.RIGHT,
                Position = Position.SliceTop(80.0f).Margin(40.0f, 20.0f))
            |+ Text("Recent scores",
                Color = K Colors.text,
                Align = Alignment.LEFT,
                Position = Position.TrimTop(125.0f).SliceTop(45.0f).Margin(45.0f, 0.0f))
            |+ RecentScores(data.RecentScores, 
                Position = Position.TrimTop(130.0f).Margin(40.0f))
            :> Widget
        )

    override this.Init(parent) =
        Players.player_changed <- this.Reload
        base.Init parent
    
    override this.Draw() =
        Draw.rect this.Bounds !*Palette.DARK_100
        base.Draw()

type PlayersPage() as this =
    inherit Dialog()

    let contents =
        StaticContainer(NodeType.None)
        |+ PlayerList(Position = { Position.Default with Right = 0.35f %+ 40.0f }.Margin(40.0f))
        |+ Profile(Position = { Position.Default with Left = 0.35f %- 0.0f }.Margin(40.0f))
        |+ HotkeyAction("exit", this.Close)

    override this.Init(parent) =
        base.Init parent
        contents.Init this

    override this.Draw() =
        contents.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        contents.Update(elapsedTime, moved)