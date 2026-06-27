namespace YAVSRG.CLI

open System.Diagnostics

module Shell =

    let exec (cmd: string) (args: string) =
        Process
            .Start(ProcessStartInfo(cmd, args, WorkingDirectory = YAVSRG_PATH))
            .WaitForExit()

    let eval (cmd: string) (args: string) : string =
        let p =
            Process.Start(ProcessStartInfo(cmd, args, WorkingDirectory = YAVSRG_PATH, RedirectStandardOutput = true))

        let output = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        output.Trim()

    let exec_at (path: string) (cmd: string) (args: string) =
        Process
            .Start(ProcessStartInfo(cmd, args, WorkingDirectory = path))
            .WaitForExit()