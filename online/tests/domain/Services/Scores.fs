namespace Interlude.Web.Tests.Domain.Services

open NUnit.Framework

open Percyqaz.Common
open Prelude.Gameplay.Replays
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Scores =

    module Scores = Interlude.Web.Tests.Domain.Core.Scores

    [<Test>]
    let DecompressUntrustedString () =
        match Replay.decompress_string_untrusted 200000.0f<ms> Scores.CRESCENT_MOON_REPLAY_STRING with
        | Ok _ -> Assert.Pass()
        | Error e -> Assert.Fail(e)

    [<Test>]
    let ScoreSubmission_HappyPath () =
        let user = User.create ("ScoreSubmissionHappyPath", 999999uL)
        let user_id = User.save_new user

        let result =
            Scores.submit (
                user_id,
                Scores.CRESCENT_MOON,
                Scores.CRESCENT_MOON_REPLAY_STRING,
                1.0f,
                Map.empty,
                Scores.TIMEPLAYED
            )
            |> Async.RunSynchronously

        printfn "%A" result

    [<Test>]
    let ScoreSubmission_FakeData () =
        let user = User.create ("ScoreSubmissionFakeData", 999999uL)
        let user_id = User.save_new user

        let result =
            Scores.submit (
                user_id,
                Scores.CRESCENT_MOON,
                "FAKEREPLAYDATAFAKEREPLAYDATA",
                1.0f,
                Map.empty,
                Scores.TIMEPLAYED
            )
            |> Async.RunSynchronously

        printfn "%A" result

    [<Test>]
    let ScoreSubmission_BadModCombo () =
        let user = User.create ("ScoreSubmissionBadModCombo", 999999uL)
        let user_id = User.save_new user

        let result =
            Scores.submit (
                user_id,
                Scores.CRESCENT_MOON,
                Scores.CRESCENT_MOON_REPLAY_STRING,
                1.0f,
                Map.ofList [ "invalid", 999 ],
                Scores.TIMEPLAYED
            )
            |> Async.RunSynchronously

        printfn "%A" result
