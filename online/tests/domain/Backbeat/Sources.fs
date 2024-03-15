namespace Interlude.Web.Tests.Domain.Backbeat

open NUnit.Framework

open Interlude.Web.Server.Domain.Backbeat

module Sources =

    let test_source: Source =
        {
            Id = "ultra 7k megapack vol.1"
            Mirrors =
                [
                    "https://yavsrg.net/packs/ultra-7k-megapack-1"
                    "https://chart-pack-mirrors.io/ultra-7k-megapack-1"
                ]
            Namespace = "ultra-7k-megapack"
        }

    [<Test>]
    let Source_RoundTrip () =
        Source.add_or_update test_source

        let result = Source.by_id test_source.Id
        Assert.AreEqual(Some test_source, result)

    [<Test>]
    let Source_Idempotent () =
        Source.add_or_update test_source
        Source.add_or_update test_source
        Source.add_or_update test_source

        let result = Source.by_id test_source.Id
        Assert.AreEqual(Some test_source, result)

    [<Test>]
    let Source_DoesntExist () =
        let result = Source.by_id test_source.Id
        Assert.AreEqual(None, result)

        Source.add_or_update test_source

        let result = Source.by_id "doesntexist"
        Assert.AreEqual(None, result)
