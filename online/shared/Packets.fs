namespace Interlude.Web.Shared

open System
open System.IO
open Prelude

[<AutoOpen>]
module Packets =

    let PROTOCOL_VERSION = 17uy

    let MULTIPLAYER_REPLAY_DELAY_SECONDS = 1
    let MULTIPLAYER_REPLAY_DELAY_MS = float32 MULTIPLAYER_REPLAY_DELAY_SECONDS * 1000.0f
    let REPLAY_FRAME_SIZE_BYTES = 6

    let PLAY_PACKET_THRESHOLD_PER_SECOND = 600

    type LobbyPlayerStatus =
        | NotReady = 0uy
        | Ready = 1uy
        | ReadyToSpectate = 2uy
        | Playing = 3uy
        | AbandonedPlay = 4uy
        | Spectating = 5uy
        | MissingChart = 6uy

    type LobbyChart =
        {
            Hash: string
            Artist: string
            Title: string
            Creator: string
            Rate: Rate
            Mods: (string * int) array
        }
        member this.Write(bw: BinaryWriter) =
            bw.Write this.Hash
            bw.Write this.Artist
            bw.Write this.Title
            bw.Write this.Creator
            bw.Write (float32 this.Rate)
            bw.Write(byte this.Mods.Length)

            for (id, state) in this.Mods do
                bw.Write id
                bw.Write state

        static member Read(br: BinaryReader) =
            {
                Hash = br.ReadString()
                Artist = br.ReadString()
                Title = br.ReadString()
                Creator = br.ReadString()
                Rate = br.ReadSingle() * 1.0f<rate>
                Mods = Array.init (br.ReadByte() |> int) (fun _ -> br.ReadString(), br.ReadInt32())
            }

    type LobbySettings =
        {
            Name: string
            HostRotation: bool
            AutomaticRoundCountdown: bool
        }
        member this.Write(bw: BinaryWriter) =
            bw.Write this.Name
            bw.Write this.HostRotation
            bw.Write this.AutomaticRoundCountdown
        static member Read(br: BinaryReader) =
            {
                Name = br.ReadString()
                HostRotation = br.ReadBoolean()
                AutomaticRoundCountdown = br.ReadBoolean()
            }
        static member Default =
            {
                Name = "Loading..."
                HostRotation = false
                AutomaticRoundCountdown = true
            }

    type LobbyInfo =
        {
            Id: Guid
            Name: string
            Players: byte
            CurrentlyPlaying: string option
        }
        member this.Write(bw: BinaryWriter) =
            bw.Write(this.Id.ToByteArray())
            bw.Write this.Name
            bw.Write this.Players
            bw.Write(Option.defaultValue "" this.CurrentlyPlaying)

        static member Read(br: BinaryReader) =
            {
                Id = new Guid(br.ReadBytes 16)
                Name = br.ReadString()
                Players = br.ReadByte()
                CurrentlyPlaying =
                    br.ReadString()
                    |> function
                        | "" -> None
                        | s -> Some s
            }

    type LobbyEvent =
        | Join = 0uy
        | Leave = 1uy
        | Host = 2uy
        | Ready = 3uy
        | Ready_Spectate = 4uy
        | NotReady = 5uy
        | Invite = 6uy
        | Generic = 7uy

    type ReadyFlag =
        | NotReady = 0uy
        | Play = 1uy
        | Spectate = 2uy

    [<RequireQualifiedAccess>]
    type Upstream =
        | VERSION of byte
        | LOGIN_WITH_DISCORD
        | COMPLETE_REGISTRATION_WITH_DISCORD of username: string
        | LOGIN of token: string
        | LOGOUT

        | GET_LOBBIES
        | JOIN_LOBBY of id: Guid
        | CREATE_LOBBY of name: string

        | INVITE_TO_LOBBY of username: string
        | LEAVE_LOBBY
        | CHAT of message: string
        | READY_STATUS of ReadyFlag
        | MISSING_CHART

        | BEGIN_PLAYING
        | PLAY_DATA of timestamp: float32 * byte array
        | BEGIN_SPECTATING
        | FINISH_PLAYING of abandoned: bool

        | TRANSFER_HOST of username: string
        | SELECT_CHART of LobbyChart
        | LOBBY_SETTINGS of LobbySettings
        | START_GAME
        | CANCEL_GAME

        | KICK_PLAYER of username: string // nyi

        static member Read(kind: byte, data: byte array) : Upstream =
            use ms = new MemoryStream(data)
            use br = new BinaryReader(ms)

            let packet =
                match kind with
                | 0x00uy -> VERSION(br.ReadByte())
                | 0x01uy -> LOGIN_WITH_DISCORD
                | 0x02uy -> COMPLETE_REGISTRATION_WITH_DISCORD(br.ReadString())
                | 0x04uy -> LOGIN(br.ReadString())
                | 0x05uy -> LOGOUT

                | 0x10uy -> GET_LOBBIES
                | 0x11uy -> JOIN_LOBBY(new Guid(br.ReadBytes 16))
                | 0x12uy -> CREATE_LOBBY(br.ReadString())

                | 0x20uy -> INVITE_TO_LOBBY(br.ReadString())
                | 0x21uy -> LEAVE_LOBBY
                | 0x22uy -> CHAT(br.ReadString())
                | 0x23uy -> READY_STATUS(br.ReadByte() |> LanguagePrimitives.EnumOfValue)
                | 0x24uy -> MISSING_CHART

                | 0x30uy -> BEGIN_PLAYING
                | 0x31uy -> BEGIN_SPECTATING
                | 0x32uy ->
                    let length = int (br.BaseStream.Length - br.BaseStream.Position)

                    if length > PLAY_PACKET_THRESHOLD_PER_SECOND * MULTIPLAYER_REPLAY_DELAY_SECONDS then
                        failwithf "Excessive replay data being sent to server"

                    PLAY_DATA(br.ReadSingle(), br.ReadBytes(length))
                | 0x33uy -> FINISH_PLAYING(br.ReadBoolean())

                | 0x40uy -> TRANSFER_HOST(br.ReadString())
                | 0x41uy -> SELECT_CHART(LobbyChart.Read br)
                | 0x42uy -> LOBBY_SETTINGS(LobbySettings.Read br)
                | 0x43uy -> START_GAME
                | 0x44uy -> CANCEL_GAME

                | 0x50uy -> KICK_PLAYER(br.ReadString())

                | _ -> failwithf "Unknown packet type: %i" kind

            if ms.Position <> ms.Length then
                failwithf "Expected end-of-packet (%x) but there are %i extra bytes" kind (ms.Length - ms.Position)

            packet

        member this.Write() : byte * byte array =
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)

            let kind =
                match this with
                | VERSION v ->
                    bw.Write v
                    0x00uy
                | LOGIN_WITH_DISCORD -> 0x01uy
                | COMPLETE_REGISTRATION_WITH_DISCORD username ->
                    bw.Write username
                    0x02uy
                | LOGIN name ->
                    bw.Write name
                    0x04uy
                | LOGOUT -> 0x05uy

                | GET_LOBBIES -> 0x10uy
                | JOIN_LOBBY id ->
                    bw.Write(id.ToByteArray())
                    0x11uy
                | CREATE_LOBBY name ->
                    bw.Write name
                    0x12uy

                | INVITE_TO_LOBBY username ->
                    bw.Write username
                    0x20uy
                | LEAVE_LOBBY -> 0x21uy
                | CHAT msg ->
                    bw.Write msg
                    0x22uy
                | READY_STATUS flag ->
                    bw.Write(byte flag)
                    0x23uy
                | MISSING_CHART -> 0x24uy

                | BEGIN_PLAYING -> 0x30uy
                | BEGIN_SPECTATING -> 0x31uy
                | PLAY_DATA (timestamp, data) ->
                    bw.Write timestamp
                    bw.Write data
                    0x32uy
                | FINISH_PLAYING abandon ->
                    bw.Write abandon
                    0x33uy

                | TRANSFER_HOST username ->
                    bw.Write username
                    0x40uy
                | SELECT_CHART chart ->
                    chart.Write bw
                    0x41uy
                | LOBBY_SETTINGS settings ->
                    settings.Write bw
                    0x42uy
                | START_GAME -> 0x43uy
                | CANCEL_GAME -> 0x44uy

                | KICK_PLAYER username ->
                    bw.Write username
                    0x50uy

            kind, ms.ToArray()

    [<RequireQualifiedAccess>]
    type Downstream =
        | DISCONNECT of reason: string
        | HANDSHAKE_SUCCESS
        | DISCORD_AUTH_URL of url: string
        | COMPLETE_REGISTRATION_WITH_DISCORD of identifier: string
        | REGISTRATION_FAILED of reason: string
        | AUTH_TOKEN of string
        | LOGIN_SUCCESS of username: string
        | LOGIN_FAILED of reason: string

        | LOBBY_LIST of lobbies: LobbyInfo array
        | YOU_JOINED_LOBBY of players: (string * int32) array
        | INVITED_TO_LOBBY of by_who: string * id: Guid

        | YOU_LEFT_LOBBY
        | YOU_ARE_HOST of bool
        | PLAYER_JOINED_LOBBY of username: string * color: int32
        | PLAYER_LEFT_LOBBY of username: string
        | SELECT_CHART of LobbyChart
        | LOBBY_SETTINGS of LobbySettings
        | LOBBY_EVENT of LobbyEvent * data: string
        | SYSTEM_MESSAGE of string
        | CHAT of sender: string * message: string
        | PLAYER_STATUS of username: string * status: LobbyPlayerStatus
        | COUNTDOWN of reason: string * seconds: int // todo: merge countdown and game_countdown -> countdown_started

        | GAME_COUNTDOWN of bool // todo: become countdown_stopped
        | GAME_START
        | PLAY_DATA of username: string * timestamp: float32 * data: byte array
        | GAME_END

        static member Read(kind: byte, data: byte array) : Downstream =
            use ms = new MemoryStream(data)
            use br = new BinaryReader(ms)

            let packet =
                match kind with
                | 0x00uy -> DISCONNECT(br.ReadString())
                | 0x01uy -> HANDSHAKE_SUCCESS
                | 0x02uy -> DISCORD_AUTH_URL(br.ReadString())
                | 0x03uy -> COMPLETE_REGISTRATION_WITH_DISCORD(br.ReadString())
                | 0x04uy -> REGISTRATION_FAILED(br.ReadString())
                | 0x05uy -> AUTH_TOKEN(br.ReadString())
                | 0x06uy -> LOGIN_SUCCESS(br.ReadString())
                | 0x07uy -> LOGIN_FAILED(br.ReadString())

                | 0x10uy -> LOBBY_LIST(Array.init (br.ReadByte() |> int) (fun _ -> LobbyInfo.Read br))
                | 0x11uy ->
                    YOU_JOINED_LOBBY(Array.init (br.ReadByte() |> int) (fun _ -> br.ReadString(), br.ReadInt32()))
                | 0x12uy -> INVITED_TO_LOBBY(br.ReadString(), new Guid(br.ReadBytes 16))
                | 0x13uy -> SYSTEM_MESSAGE(br.ReadString())

                | 0x20uy -> YOU_LEFT_LOBBY
                | 0x21uy -> YOU_ARE_HOST(br.ReadBoolean())
                | 0x22uy -> PLAYER_JOINED_LOBBY(br.ReadString(), br.ReadInt32())
                | 0x23uy -> PLAYER_LEFT_LOBBY(br.ReadString())
                | 0x24uy -> SELECT_CHART(LobbyChart.Read br)
                | 0x25uy -> LOBBY_SETTINGS(LobbySettings.Read br)
                | 0x26uy -> LOBBY_EVENT(br.ReadByte() |> LanguagePrimitives.EnumOfValue, br.ReadString())
                | 0x27uy -> CHAT(br.ReadString(), br.ReadString())
                | 0x28uy -> PLAYER_STATUS(br.ReadString(), br.ReadByte() |> LanguagePrimitives.EnumOfValue)
                | 0x29uy -> COUNTDOWN(br.ReadString(), br.ReadInt32())

                | 0x30uy -> GAME_COUNTDOWN(br.ReadBoolean())
                | 0x31uy -> GAME_START
                | 0x32uy ->
                    PLAY_DATA(br.ReadString(), br.ReadSingle(), br.ReadBytes(int (br.BaseStream.Length - br.BaseStream.Position)))
                | 0x33uy -> GAME_END

                | _ -> failwithf "Unknown packet type: %i" kind

            if ms.Position <> ms.Length then
                failwithf "Expected end-of-packet (%x) but there are %i extra bytes" kind (ms.Length - ms.Position)

            packet

        member this.Write() : byte * byte array =
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)

            let kind =
                match this with
                | DISCONNECT reason ->
                    bw.Write reason
                    0x00uy
                | HANDSHAKE_SUCCESS -> 0x01uy
                | DISCORD_AUTH_URL url ->
                    bw.Write url
                    0x02uy
                | COMPLETE_REGISTRATION_WITH_DISCORD identifier ->
                    bw.Write identifier
                    0x03uy
                | REGISTRATION_FAILED reason ->
                    bw.Write reason
                    0x04uy
                | AUTH_TOKEN token ->
                    bw.Write token
                    0x05uy
                | LOGIN_SUCCESS name ->
                    bw.Write name
                    0x06uy
                | LOGIN_FAILED reason ->
                    bw.Write reason
                    0x07uy

                | LOBBY_LIST lobbies ->
                    bw.Write(byte lobbies.Length)

                    for lobby in lobbies do
                        lobby.Write bw

                    0x10uy
                | YOU_JOINED_LOBBY players ->
                    bw.Write(byte players.Length)

                    for (player, color) in players do
                        bw.Write player
                        bw.Write color

                    0x11uy
                | INVITED_TO_LOBBY(by_who, id) ->
                    bw.Write by_who
                    bw.Write(id.ToByteArray())
                    0x12uy
                | SYSTEM_MESSAGE message ->
                    bw.Write message
                    0x13uy

                | YOU_LEFT_LOBBY -> 0x20uy
                | YOU_ARE_HOST you_are_host ->
                    bw.Write you_are_host
                    0x21uy
                | PLAYER_JOINED_LOBBY(username, color) ->
                    bw.Write username
                    bw.Write color
                    0x22uy
                | PLAYER_LEFT_LOBBY username ->
                    bw.Write username
                    0x23uy
                | SELECT_CHART chart ->
                    chart.Write bw
                    0x24uy
                | LOBBY_SETTINGS settings ->
                    settings.Write bw
                    0x25uy
                | LOBBY_EVENT(kind, data) ->
                    bw.Write(byte kind)
                    bw.Write data
                    0x26uy
                | CHAT(sender, msg) ->
                    bw.Write sender
                    bw.Write msg
                    0x27uy
                | PLAYER_STATUS(username, status) ->
                    bw.Write username
                    bw.Write(byte status)
                    0x28uy
                | COUNTDOWN(reason, seconds) ->
                    bw.Write reason
                    bw.Write seconds
                    0x29uy

                | GAME_COUNTDOWN b ->
                    bw.Write b
                    0x30uy
                | GAME_START -> 0x31uy
                | PLAY_DATA(username, timestamp, data) ->
                    bw.Write username
                    bw.Write timestamp
                    bw.Write data
                    0x32uy
                | GAME_END -> 0x33uy

            kind, ms.ToArray()