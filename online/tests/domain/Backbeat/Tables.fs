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
        Assert.AreEqual(TableChangeEventDetails.Add("chart_id", 3), changelog.[0].Details)
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
        Assert.AreEqual(TableChangeEventDetails.Add("chart_id", 3), changelog.[0].Details)

        Assert.AreEqual(user_id, changelog.[1].UserId)
        Assert.AreEqual(TableChangeEventDetails.Move("chart_id", 3, 4), changelog.[1].Details)

        Assert.AreEqual(user_id, changelog.[2].UserId)
        Assert.AreEqual(TableChangeEventDetails.Remove("chart_id", 4), changelog.[2].Details)

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

