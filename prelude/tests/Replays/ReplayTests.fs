namespace Prelude.Tests.Replays

open NUnit.Framework
open System.IO
open Prelude
open Prelude.Gameplay.Replays

module ReplayTests =

    let SAMPLE_REPLAY_BYTES = File.ReadAllBytes("./Data/replay.bin")
    let SAMPLE_REPLAY_DATA = Replay.FromByteArray(SAMPLE_REPLAY_BYTES)

    [<Test>]
    let RoundTrip_Bytes () =

        let compressed_bytes = SAMPLE_REPLAY_DATA.ToByteArray()
        let decompressed_bytes = Replay.FromByteArray(compressed_bytes)
        Assert.AreEqual(SAMPLE_REPLAY_DATA, decompressed_bytes)

    [<Test>]
    let RoundTrip_String () =

        let compressed_string = SAMPLE_REPLAY_DATA.ToBase64String()
        let decompressed_data = Replay.FromBase64String(compressed_string)

        Assert.AreEqual(SAMPLE_REPLAY_DATA, decompressed_data)

    [<Test>]
    let DecompressUntrusted_ValidData () =

        let replay_string = Replay.FromByteArray(SAMPLE_REPLAY_BYTES).ToBase64String()

        match Replay.FromUntrustedBase64String(60000.0f<ms>, replay_string) with
        | Ok decoded -> Assert.AreEqual(SAMPLE_REPLAY_DATA, decoded)
        | Error reason -> Assert.Fail(reason)

    [<Test>]
    let DecompressUntrusted_TooMuchData_ForChartLength () =

        let replay_string = Replay.FromByteArray(SAMPLE_REPLAY_BYTES).ToBase64String()

        match Replay.FromUntrustedBase64String(1000.0f<ms>, replay_string) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let DecompressUntrusted_BadTimestamps () =

        let bad_replay_string =
            Replay
                .FromArray(
                    ReplayFrame.Create(0.0f<ms>, 0us),
                    ReplayFrame.Create(10.0f<ms>, 1us),
                    ReplayFrame.Create(20.0f<ms>, 2us),
                    ReplayFrame.Create(19.0f<ms>, 1us)
                )
                .ToBase64String()

        match Replay.FromUntrustedBase64String(1000.0f<ms>, bad_replay_string) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let DecompressUntrusted_InvalidTimestamps_NaN () =

        let bad_replay_string =
            Replay
                .FromArray(
                    ReplayFrame.Create(0.0f<ms>, 0us),
                    ReplayFrame.Create(10.0f<ms>, 1us),
                    ReplayFrame.Create(20.0f<ms>, 2us),
                    ReplayFrame.Create(System.Single.NaN * 1.0f<ms>, 1us)
                )
                .ToBase64String()

        match Replay.FromUntrustedBase64String(1000.0f<ms>, bad_replay_string) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

    [<Test>]
    let DecompressUntrusted_InvalidTimestamps_Infinity () =

        let bad_replay_string =
            Replay
                .FromArray(
                    ReplayFrame.Create(0.0f<ms>, 0us),
                    ReplayFrame.Create(10.0f<ms>, 1us),
                    ReplayFrame.Create(20.0f<ms>, 2us),
                    ReplayFrame.Create(System.Single.NegativeInfinity * 1.0f<ms>, 1us)
                )
                .ToBase64String()

        match Replay.FromUntrustedBase64String(1000.0f<ms>, bad_replay_string) with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)
