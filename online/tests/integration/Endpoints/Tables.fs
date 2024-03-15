namespace Interlude.Web.Tests.Integration

open NUnit.Framework
open Interlude.Web.Shared.Requests
open System.Threading

module Tables =

    [<Test>]
    let Records () =
        use done_signal = new AutoResetEvent(false)

        Tables.Records.get (
            USERNAME,
            "crescent",
            Option.get
            >> fun (res: Tables.Records.Response) ->
                printfn "%A" res.Scores
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Records_TableNotFound () =
        use done_signal = new AutoResetEvent(false)

        Tables.Records.get (
            USERNAME,
            "doesntexist",
            function
            | Some _ -> Assert.Fail()
            | None -> done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Records_UserNotFound () =
        use done_signal = new AutoResetEvent(false)

        Tables.Records.get (
            "ThisUserDoesNotExist",
            "crescent",
            function
            | Some _ -> Assert.Fail()
            | None -> done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard () =
        use done_signal = new AutoResetEvent(false)

        Tables.Leaderboard.get (
            "crescent",
            Option.get
            >> fun (res: Tables.Leaderboard.Response) ->
                printfn "%A" res.Players
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Leaderboard_NotFound () =
        use done_signal = new AutoResetEvent(false)

        Tables.Leaderboard.get (
            "doesntexist",
            function
            | Some _ -> Assert.Fail()
            | None -> done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let List () =
        use done_signal = new AutoResetEvent(false)

        Tables.List.get (
            Option.get
            >> fun (res: Tables.List.Response) ->
                printfn "%A" res.Tables
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Charts_All () =
        use done_signal = new AutoResetEvent(false)

        Tables.Charts.get (
            "crescent",
            Option.get
            >> fun (res: Tables.Charts.Response) ->
                printfn "%A" res.Charts
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Charts_Section () =
        use done_signal = new AutoResetEvent(false)

        Tables.Charts.get_section (
            "crescent",
            "advanced",
            Option.get
            >> fun (res: Tables.Charts.Response) ->
                printfn "%A" res.Charts
                done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Charts_TableNotFound () =
        use done_signal = new AutoResetEvent(false)

        Tables.Charts.get_section (
            "doesntexist",
            "advanced",
            function
            | Some _ -> Assert.Fail()
            | None -> done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    [<Test>]
    let Charts_SectionNotFound () =
        use done_signal = new AutoResetEvent(false)

        Tables.Charts.get_section (
            "crescent",
            "doesntexist",
            function
            | Some _ -> Assert.Fail()
            | None -> done_signal.Set() |> ignore
        )

        Assert.IsTrue(done_signal.WaitOne(500))

    module Suggestions =

        [<Test>]
        let List () =
            use done_signal = new AutoResetEvent(false)

            Tables.Suggestions.List.get (
                "crescent",
                Option.get
                >> fun (res: Tables.Suggestions.List.Response) ->
                    printfn "%A" res.Suggestions
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Missing () =
            use done_signal = new AutoResetEvent(false)

            Tables.Suggestions.Missing.get (
                "crescent",
                Option.get
                >> fun (res: Tables.Suggestions.Missing.Response) ->
                    printfn "%A" res.Charts
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Vote () =
            use done_signal = new AutoResetEvent(false)

            Tables.Suggestions.Vote.post (
                {
                    TableId = "crescent"
                    ChartId = "chart_id"
                    Level = 5
                },
                Option.get
                >> fun (res: Tables.Suggestions.Vote.Response) ->
                    printfn "%A" res
                    done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Vote_TableNotFound () =
            use done_signal = new AutoResetEvent(false)

            Tables.Suggestions.Vote.post (
                {
                    TableId = "doesntexist"
                    ChartId = "chart_id"
                    Level = 5
                },
                function
                | Some _ -> Assert.Fail()
                | None -> done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Accept_PermissionDenied () =
            use done_signal = new AutoResetEvent(false)

            Tables.Suggestions.Accept.post (
                {
                    TableId = "crescent"
                    ChartId = "chart_id"
                    Level = 5
                },
                function
                | Some _ -> Assert.Fail()
                | None -> done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))

        [<Test>]
        let Reject_PermissionDenied () =
            use done_signal = new AutoResetEvent(false)

            Tables.Suggestions.Reject.post (
                {
                    TableId = "crescent"
                    ChartId = "chart_id"
                    Reason = "I don't like it"
                },
                function
                | Some _ -> Assert.Fail()
                | None -> done_signal.Set() |> ignore
            )

            Assert.IsTrue(done_signal.WaitOne(500))
