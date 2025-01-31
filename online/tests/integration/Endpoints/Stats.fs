namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open System.Threading

module Stats =

    [<Test>]
    let Fetch () =

        use done_signal = new AutoResetEvent(false)

        Stats.Fetch.get (
            Option.get
            >> fun (res: Stats.Fetch.Response) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_XP () =

        use done_signal = new AutoResetEvent(false)

        Stats.Leaderboard.XP.get (
            false,
            Option.get
            >> fun (res: Stats.Leaderboard.XP.Response) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Playtime () =

        use done_signal = new AutoResetEvent(false)

        Stats.Leaderboard.XP.get (
            true,
            Option.get
            >> fun (res: Stats.Leaderboard.XP.Response) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Monthly_XP () =

        use done_signal = new AutoResetEvent(false)

        Stats.Leaderboard.MonthlyXP.get (
            false,
            Option.get
            >> fun (res: Stats.Leaderboard.MonthlyXP.Response) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Monthly_Playtime () =

        use done_signal = new AutoResetEvent(false)

        Stats.Leaderboard.MonthlyXP.get (
            true,
            Option.get
            >> fun (res: Stats.Leaderboard.MonthlyXP.Response) ->
                printfn "%A" res
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Keymode_4K () =

        let sort_modes =
            [
                Stats.Leaderboard.Keymode.Playtime
                Stats.Leaderboard.Keymode.Combined
                Stats.Leaderboard.Keymode.Jacks
                Stats.Leaderboard.Keymode.Chordstream
                Stats.Leaderboard.Keymode.Stream
        ]

        for sort in sort_modes do
            use done_signal = new AutoResetEvent(false)

            Stats.Leaderboard.Keymode.get (
                4,
                sort,
                Option.get
                >> fun (res: Stats.Leaderboard.Keymode.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Keymode_7K () =

        let sort_modes =
            [
                Stats.Leaderboard.Keymode.Playtime
                Stats.Leaderboard.Keymode.Combined
                Stats.Leaderboard.Keymode.Jacks
                Stats.Leaderboard.Keymode.Chordstream
                Stats.Leaderboard.Keymode.Stream
        ]

        for sort in sort_modes do
            use done_signal = new AutoResetEvent(false)

            Stats.Leaderboard.Keymode.get (
                7,
                sort,
                Option.get
                >> fun (res: Stats.Leaderboard.Keymode.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Keymode_Monthly_4K () =

        let sort_modes =
            [
                Stats.Leaderboard.MonthlyKeymode.Playtime
                Stats.Leaderboard.MonthlyKeymode.Combined
                Stats.Leaderboard.MonthlyKeymode.Jacks
                Stats.Leaderboard.MonthlyKeymode.Chordstream
                Stats.Leaderboard.MonthlyKeymode.Stream
        ]

        for sort in sort_modes do
            use done_signal = new AutoResetEvent(false)

            Stats.Leaderboard.MonthlyKeymode.get (
                4,
                sort,
                Option.get
                >> fun (res: Stats.Leaderboard.MonthlyKeymode.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_Keymode_Monthly_7K () =

        let sort_modes =
            [
                Stats.Leaderboard.MonthlyKeymode.Playtime
                Stats.Leaderboard.MonthlyKeymode.Combined
                Stats.Leaderboard.MonthlyKeymode.Jacks
                Stats.Leaderboard.MonthlyKeymode.Chordstream
                Stats.Leaderboard.MonthlyKeymode.Stream
        ]

        for sort in sort_modes do
            use done_signal = new AutoResetEvent(false)

            Stats.Leaderboard.MonthlyKeymode.get (
                7,
                sort,
                Option.get
                >> fun (res: Stats.Leaderboard.MonthlyKeymode.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))