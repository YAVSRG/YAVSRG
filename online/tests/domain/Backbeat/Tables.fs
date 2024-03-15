namespace Interlude.Web.Tests.Domain.Backbeat

open NUnit.Framework

open Interlude.Web.Server.Domain.Backbeat

module Tables =

    [<Test>]
    let TableLevel_RoundTrip () =
        let user_id = 5L
        TableLevel.add_or_move user_id "round_trip" "chart_id" 3

        let changelog = TableLevel.get_changes_since "round_trip" 0L

        Assert.AreEqual(Some 3, TableLevel.get "round_trip" "chart_id")
        Assert.AreEqual(1, changelog.Length)
        Assert.AreEqual(user_id, changelog.[0].UserId)
        Assert.AreEqual("chart_id", changelog.[0].ChartId)
        Assert.AreEqual(TableChangeEventDetails.Add 3, changelog.[0].Details)
        Assert.AreNotEqual(None, TableLevel.get_time_last_changed "round_trip")

    [<Test>]
    let TableLevel_DoesntExist () =
        let user_id = 5L
        TableLevel.add_or_move user_id "exists" "chart_id" 3

        Assert.AreEqual(None, TableLevel.get "doesnt_exist" "chart_id")
        Assert.AreEqual(None, TableLevel.get "doesnt_exist" "doesnt_exist")
        Assert.AreEqual(None, TableLevel.get "exists" "doesnt_exist")
        Assert.AreEqual(None, TableLevel.get_time_last_changed "doesnt_exist")
        Assert.AreEqual([||], TableLevel.get_changes_since "doesnt_exist" 0L)
        Assert.AreEqual([||], TableLevel.get_changes_since "doesnt_exist" System.Int64.MaxValue)

    [<Test>]
    let TableLevel_Idempotence () =
        let user_id = 5L
        TableLevel.add_or_move user_id "idempotence" "chart_id" 3
        TableLevel.add_or_move user_id "idempotence" "chart_id" 3
        TableLevel.add_or_move user_id "idempotence" "chart_id" 3
        TableLevel.add_or_move user_id "idempotence" "chart_id" 4
        TableLevel.add_or_move user_id "idempotence" "chart_id" 4
        TableLevel.add_or_move user_id "idempotence" "chart_id" 4
        TableLevel.remove user_id "idempotence" "chart_id"
        TableLevel.remove user_id "idempotence" "chart_id"
        TableLevel.remove user_id "idempotence" "chart_id"

        let changelog = TableLevel.get_changes_since "idempotence" 0L

        printfn "%A" changelog

        Assert.AreEqual(None, TableLevel.get "idempotence" "chart_id")

        Assert.AreEqual(3, changelog.Length)

        Assert.AreEqual(user_id, changelog.[0].UserId)
        Assert.AreEqual("chart_id", changelog.[0].ChartId)
        Assert.AreEqual(TableChangeEventDetails.Add 3, changelog.[0].Details)

        Assert.AreEqual(user_id, changelog.[1].UserId)
        Assert.AreEqual("chart_id", changelog.[1].ChartId)
        Assert.AreEqual(TableChangeEventDetails.Move(3, 4), changelog.[1].Details)

        Assert.AreEqual(user_id, changelog.[2].UserId)
        Assert.AreEqual("chart_id", changelog.[2].ChartId)
        Assert.AreEqual(TableChangeEventDetails.Remove 4, changelog.[2].Details)

    [<Test>]
    let TableLevel_BatchGet () =
        let user_id = 5L
        TableLevel.add_or_move user_id "batchget" "chart_A" 1
        TableLevel.add_or_move user_id "batchget" "chart_B" 1
        TableLevel.add_or_move user_id "batchget" "chart_B" 2
        TableLevel.remove user_id "batchget" "chart_B"
        TableLevel.add_or_move user_id "batchget" "chart_B" 2
        TableLevel.add_or_move user_id "batchget" "chart_C" 3
        TableLevel.add_or_move user_id "batchget" "chart_D" 4
        TableLevel.remove user_id "batchget" "chart_D"

        let all = TableLevel.get_all "batchget"
        let between = TableLevel.get_range "batchget" 2 10

        Assert.AreEqual(3, all.Length)
        Assert.AreEqual(2, between.Length)

        let all_map = Map.ofArray all
        Assert.AreEqual(1, all_map.["chart_A"])
        Assert.AreEqual(2, all_map.["chart_B"])
        Assert.AreEqual(3, all_map.["chart_C"])

        let between_map = Map.ofArray between
        Assert.AreEqual(2, between_map.["chart_B"])
        Assert.AreEqual(3, between_map.["chart_C"])

    [<Test>]
    let TableSuggestion_VotesRoundTrip () =
        Assert.True(TableSuggestion.suggest "votesroundtrip" "chart_id" 10L 5)
        Assert.True(TableSuggestion.suggest "votesroundtrip" "chart_id" 20L 5)
        Assert.True(TableSuggestion.suggest "votesroundtrip" "chart_id" 30L 7)

        let expected_votes = Map.ofList [ 10L, 5; 20L, 5; 30L, 7 ]

        match TableSuggestion.pending_by_chart "votesroundtrip" "chart_id" with
        | None -> Assert.Fail()
        | Some votes ->
            Assert.AreEqual(3, Map.count votes)
            Assert.AreEqual(expected_votes, votes)

        let all_by_chart = TableSuggestion.all_by_chart "votesroundtrip" "chart_id"

        Assert.AreEqual(1, all_by_chart.Length)
        Assert.AreEqual(TableSuggestionStatus.Pending, all_by_chart.[0].Status)
        Assert.AreEqual(expected_votes, all_by_chart.[0].Votes)
        Assert.AreEqual(10L, all_by_chart.[0].UserId)

        let pending_by_table = TableSuggestion.pending_by_table "votesroundtrip"

        Assert.AreEqual(1, pending_by_table.Length)
        Assert.AreEqual("chart_id", pending_by_table.[0].ChartId)
        Assert.AreEqual(expected_votes, pending_by_table.[0].Votes)
        Assert.AreEqual(10L, pending_by_table.[0].UserId)

    [<Test>]
    let TableSuggestion_SuggestIdempotent () =
        Assert.True(TableSuggestion.suggest "suggestidempotent" "chart_id" 10L 5)
        Assert.True(TableSuggestion.suggest "suggestidempotent" "chart_id" 10L 5)
        Assert.True(TableSuggestion.suggest "suggestidempotent" "chart_id" 10L 5)

        let expected_votes = Map.ofList [ 10L, 5 ]

        match TableSuggestion.pending_by_chart "suggestidempotent" "chart_id" with
        | None -> Assert.Fail()
        | Some votes -> Assert.AreEqual(expected_votes, votes)

    [<Test>]
    let TableSuggestion_ApprovalProcess () =
        Assert.True(TableSuggestion.suggest "approvalprocess" "chart_id" 10L 5)
        Assert.True(TableSuggestion.suggest "approvalprocess" "chart_id" 20L 6)
        Assert.True(TableSuggestion.accept "approvalprocess" "chart_id" 10L 5)

        match TableSuggestion.pending_by_chart "rejectionprocess" "chart_id" with
        | None -> ()
        | Some _ -> Assert.Fail()

        Assert.True(TableSuggestion.suggest "approvalprocess" "chart_id" 20L 7)

        match TableSuggestion.pending_by_chart "approvalprocess" "chart_id" with
        | None -> Assert.Fail()
        | Some votes ->
            Assert.AreEqual(1, votes.Count)
            Assert.AreEqual(7, votes.[20L])

        Assert.AreEqual(2, (TableSuggestion.all_by_chart "approvalprocess" "chart_id").Length)
        Assert.AreEqual(Some 5, TableLevel.get "approvalprocess" "chart_id")

    [<Test>]
    let TableSuggestion_RejectionProcess () =
        Assert.True(TableSuggestion.suggest "rejectionprocess" "chart_id" 10L 5)
        Assert.True(TableSuggestion.suggest "rejectionprocess" "chart_id" 20L 6)
        Assert.True(TableSuggestion.reject "rejectionprocess" "chart_id" 10L "This chart has vibro in it or something!")
        Assert.False(TableSuggestion.suggest "rejectionprocess" "chart_id" 20L 7)

        match TableSuggestion.pending_by_chart "rejectionprocess" "chart_id" with
        | None -> ()
        | Some _ -> Assert.Fail()

        TableSuggestion.suggest_allow_reopening_rejected "rejectionprocess" "chart_id" 20L 7

        match TableSuggestion.pending_by_chart "rejectionprocess" "chart_id" with
        | None -> Assert.Fail()
        | Some votes ->
            Assert.AreEqual(1, votes.Count)
            Assert.AreEqual(7, votes.[20L])

        Assert.AreEqual(2, (TableSuggestion.all_by_chart "approvalprocess" "chart_id").Length)

    [<Test>]
    let TableSuggestion_DoesntExist () =
        Assert.True(TableSuggestion.suggest "exists" "chart_id" 10L 5)

        Assert.False(TableSuggestion.accept "doesntexist" "doesntexist" 10L 5)
        Assert.False(TableSuggestion.accept "exists" "doesntexist" 10L 5)
        Assert.False(TableSuggestion.accept "doesntexist" "chart_id" 10L 5)

        Assert.False(TableSuggestion.reject "doesntexist" "doesntexist" 10L "Vibro")
        Assert.False(TableSuggestion.reject "exists" "doesntexist" 10L "Vibro")
        Assert.False(TableSuggestion.reject "doesntexist" "chart_id" 10L "Vibro")

        Assert.AreEqual(0, (TableSuggestion.all_by_chart "doesntexist" "doesntexist").Length)
        Assert.AreEqual(0, (TableSuggestion.all_by_chart "doesntexist" "chart_id").Length)
        Assert.AreEqual(0, (TableSuggestion.all_by_chart "exists" "doesntexist").Length)

        Assert.AreEqual(None, TableSuggestion.pending_by_chart "doesntexist" "doesntexist")
        Assert.AreEqual(None, TableSuggestion.pending_by_chart "exists" "doesntexist")
        Assert.AreEqual(None, TableSuggestion.pending_by_chart "doesntexist" "chart_id")

        Assert.AreEqual(0, (TableSuggestion.pending_by_table "doesntexist").Length)
