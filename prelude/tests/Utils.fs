namespace Prelude.Test

open Percyqaz.Common

module Reports =

    type Severity =
        | Minor
        | Major

    type Report =
        {
            Severity: Severity
            Title: string
            Content: string
        }

    let private reports = ResizeArray<Report>()
    let private agent = new MailboxProcessor<Report>(fun box -> async { while (true) do let! e = box.Receive() in reports.Add e })
    do agent.Start()

    let minor title content = agent.Post { Severity = Minor; Title = title; Content = content }
    let major title content = agent.Post { Severity = Major; Title = title; Content = content }

    let display (step: bool) =
        let mutable minorCount = 0
        let mutable majorCount = 0
        for report in reports do
            match report.Severity with
            | Minor -> printfn "Warning> %s" report.Title; minorCount <- minorCount + 1
            | Major -> printfn "Error> %s" report.Title; majorCount <- majorCount + 1
            printfn "%s" report.Content
            if step then System.Console.ReadLine() |> ignore
        printfn "%i Warnings, %i Errors." minorCount majorCount

module Reporter =
    
    let mutable func = Reports.minor
    do Logging.Subscribe (fun (level, message, details) -> if level = LoggingLevel.ERROR then func message details)