namespace YAVSRG.CLI

open System.Diagnostics

type Shell =
    {
        WorkingDirectory: string
    }

    static member DefaultLocation = { WorkingDirectory = YAVSRG_PATH }

    member this.Exec(cmd: string, args: string) : int =
        let proc =
            Process.Start(ProcessStartInfo(cmd, args, WorkingDirectory = this.WorkingDirectory))

        proc.WaitForExit()
        proc.ExitCode

    static member Exec(cmd: string, args: string) : int = Shell.DefaultLocation.Exec(cmd, args)

    member this.Eval(cmd: string, args: string) : int * string =
        let start_info =
            ProcessStartInfo(cmd, args, WorkingDirectory = this.WorkingDirectory, RedirectStandardOutput = true)

        let proc = Process.Start(start_info)
        let output = proc.StandardOutput.ReadToEnd()
        proc.WaitForExit()
        proc.ExitCode, output.Trim()

    static member Eval(cmd: string, args: string) : int * string = Shell.DefaultLocation.Eval(cmd, args)

    static member At(path: string) : Shell = { WorkingDirectory = path }
