namespace Prelude.Tests.Data.Options

open System.IO
open NUnit.Framework
open Prelude.Data.Options

module RulesetLibraryTests =

    [<Test>]
    let Constructor_CreatesEmptyLibrary () : unit =
        let library = RulesetLibrary(Directory.CreateTempSubdirectory("rulesets").FullName)
        Assert.IsEmpty(library.ToList())
