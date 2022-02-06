open System
open Prelude.Common
open Prelude.Test

(*
    Writing tests for Prelude is HARD because checking if a thing "works" is difficult
      - Example: To check my algorithm for mirroring charts works, I have to write the "correct algorithm" a second time to compare it to.
        I'm testing that my code = my code! So might as well perform this test with my eyes, (and this logic applies to a lot of features)
      - Example: To check scoring works correctly, I have to "test" user input cases.
        Of course I can collect replay data, run it in like it's live input and check for regressions but then I'm just checking my code stays the same as before (if I fix a bug the scoring results will change and I'll think I've broken my code)

    So my test todo-list looks like this
    - A setup to en-masse convert stepmania/osu files on my PC, and when anything goes wrong output those cases to a special folder I can look through
    - A sanity checker algorithm I can feed the results of mod charts and conversions into to make sure no obvious DTIs have been broken
    - A collection of replays/replay data to feed into score algorithms - Tests my code is equivalent to what it was before a change
    - A checklist of manual things to look at with my eyes and see if they seem correct (mainly for things where correctness is not critical/can be fixed retroactively without problems for users)
*)

[<EntryPoint>]
let main argv =
    Console.BufferHeight <- 32766
    Logging.Info "Welcome to the Prelude test track"

    //Generic.main()
    //Imports.main()
    //Metrics.main()
    Trendy.main()

    Logging.Info "Test track complete, showing reports ..."

    Reports.display true
    0