namespace Percyqaz.Flux.Audio

open System
open ManagedBass
open Percyqaz.Common

type SoundEffect =
    private {
        ID: int
        mutable ChannelID: int
    }

    static member Default = { ID = 0; ChannelID = 0 }
    
    static member FromStream(source: string, stream: IO.Stream) : SoundEffect =
        use ms = new IO.MemoryStream()
        stream.CopyTo(ms)
        let data = ms.ToArray()

        let id =
            Bass.SampleLoad(data, 0L, data.Length, 1, BassFlags.SampleOverrideLongestPlaying ||| BassFlags.Prescan)

        if id = 0 then
            Logging.Error "Couldn't load sound effect '%s': %O" source Bass.LastError

        let channel = Bass.SampleGetChannel(id)
        { ID = id; ChannelID = channel }

    member this.Free() : unit =
        if this.ID <> 0 then
            Bass.SampleFree(this.ID) |> display_bass_error

    member this.ChangeDevice() : unit =
        Bass.ChannelSetDevice(this.ID, Bass.CurrentDevice) |> display_bass_error
        this.ChannelID <- Bass.SampleGetChannel(this.ID)

    member this.Play(volume: float) : unit =
        if this.ID <> 0 then
            Bass.ChannelSetAttribute(this.ChannelID, ChannelAttribute.Volume, volume * 2.0) |> display_bass_error
            Bass.ChannelPlay(this.ChannelID, true) |> display_bass_error
        
    member this.Play() : unit = this.Play(1.0)