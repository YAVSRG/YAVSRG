module Prelude.Web

open System
open System.Net
open System.Net.Http
open System.ComponentModel
open Prelude.Common

let client = new HttpClient()
let wClient = new WebClient()
do
    client.DefaultRequestHeaders.Add("User-Agent", "Interlude")
    wClient.Headers.Add("User-Agent", "Interlude")
    ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12

let private downloadString(url: string, callback) =
    async {
        try
            let! result = client.GetStringAsync(url) |> Async.AwaitTask
            callback(result)
        with
        | err -> Logging.Error("Failed to get web data from " + url)(err.ToString())
    }
        
let downloadJson<'T>(url, callback) =
    downloadString(url,
        fun s ->
            try
                callback(Json.JsonHelper.load(s) : 'T)
            with
            | err -> Logging.Error("Failed to parse json data from "+ url)(err.ToString()))

let downloadFile(url, target): LoggableTask =
    fun output ->
        let mutable wait = true
        let mutable err = null
        let prog = new DownloadProgressChangedEventHandler(fun (_: obj) (e: DownloadProgressChangedEventArgs) -> output(url + " (" + e.ProgressPercentage.ToString() + "%)"))
        let finish = new AsyncCompletedEventHandler(fun (_: obj) (e: AsyncCompletedEventArgs) -> wait <- false; err <- e.Error)
        output("Waiting for download...")
        lock (wClient)
            (fun () ->
                wClient.DownloadProgressChanged.AddHandler(prog)
                wClient.DownloadFileCompleted.AddHandler(finish)
                wClient.DownloadFileAsync(new Uri(url), target)
                while wait do ()
                wClient.DownloadProgressChanged.RemoveHandler(prog)
                wClient.DownloadFileCompleted.RemoveHandler(finish))
        if isNull err then
            true
        else
            Logging.Error("Failed to download file from " + url)(err.ToString())
            false