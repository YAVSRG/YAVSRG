namespace Prelude.Tests.Replays

open NUnit.Framework
open System.IO
open Prelude
open Prelude.Gameplay.Replays

module Replays =

    let SAMPLE_REPLAY_BYTES = File.ReadAllBytes("./Data/replay.bin")
    let SAMPLE_REPLAY_DATA = Replay.decompress_bytes SAMPLE_REPLAY_BYTES
    
    [<Test>]
    let RoundTrip_Bytes() =
        
        let compressed_bytes = Replay.compress_bytes SAMPLE_REPLAY_DATA
        let decompressed_bytes = Replay.decompress_bytes compressed_bytes
        Assert.AreEqual(SAMPLE_REPLAY_DATA, decompressed_bytes)

    [<Test>]
    let RoundTrip_String() =
        
        let compressed_string = Replay.compress_string SAMPLE_REPLAY_DATA
        let decompressed_data = Replay.decompress_string compressed_string

        Assert.AreEqual(SAMPLE_REPLAY_DATA, decompressed_data)

    [<Test>]
    let DecompressUntrusted_ValidData() =
        
        let replay_string = Replay.compressed_bytes_to_string SAMPLE_REPLAY_BYTES
        
        match Replay.decompress_string_untrusted 60000.0f<ms> replay_string with
        | Ok decoded -> Assert.AreEqual(SAMPLE_REPLAY_DATA, decoded)
        | Error reason -> Assert.Fail(reason)

    [<Test>]
    let DecompressUntrusted_TooMuchData_ForChartLength() =
        
        let replay_string = Replay.compressed_bytes_to_string SAMPLE_REPLAY_BYTES
        
        match Replay.decompress_string_untrusted 1000.0f<ms> replay_string with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)
    
    [<Test>]
    let DecompressUntrusted_BadTimestamps() =

        let bad_replay_string = 
            [|
                struct (0.0f<ms>, 0us)
                struct (10.0f<ms>, 1us)
                struct (20.0f<ms>, 2us)
                struct (19.0f<ms>, 1us)
            |]
            |> Replay.compress_string
        
        match Replay.decompress_string_untrusted 1000.0f<ms> bad_replay_string with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let DecompressUntrusted_InvalidTimestamps_NaN() =

        let bad_replay_string = 
            [|
                struct (0.0f<ms>, 0us)
                struct (10.0f<ms>, 1us)
                struct (20.0f<ms>, 2us)
                struct (System.Single.NaN * 1.0f<ms>, 1us)
            |]
            |> Replay.compress_string
        
        match Replay.decompress_string_untrusted 1000.0f<ms> bad_replay_string with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let DecompressUntrusted_InvalidTimestamps_Infinity() =

        let bad_replay_string = 
            [|
                struct (0.0f<ms>, 0us)
                struct (10.0f<ms>, 1us)
                struct (20.0f<ms>, 2us)
                struct (System.Single.NegativeInfinity * 1.0f<ms>, 1us)
            |]
            |> Replay.compress_string
        
        match Replay.decompress_string_untrusted 1000.0f<ms> bad_replay_string with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)