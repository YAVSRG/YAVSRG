namespace YAVSRG.CLI

open System.Diagnostics

type Shell =
    { WorkingDirectory: string }
    static member DefaultLocation = { WorkingDirectory = YAVSRG_PATH }

    member this.Exec(cmd: string, args: string) : unit =
        Process
            .Start(ProcessStartInfo(cmd, args, WorkingDirectory = this.WorkingDirectory))
            .WaitForExit()

    static member Exec(cmd: string, args: string) : unit =
        Shell.DefaultLocation.Exec(cmd, args)

    member this.Eval(cmd: string, args: string) : string =
        let start_info =
            ProcessStartInfo(
                cmd, args,
                WorkingDirectory = this.WorkingDirectory,
                RedirectStandardOutput = true
            )
        let proc = Process.Start(start_info)
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        output.Trim()

    static member Eval(cmd: string, args: string) : string =
        Shell.DefaultLocation.Eval(cmd, args)

    static member At(path: string) = { WorkingDirectory = path }