using System;
using System.Net;
using System.Net.Http;
using Prelude.Utilities;

namespace Interlude.Net.Web
{
    public class WebUtils
    {
        public async static void DownloadString(string url, Action<string> callback) //fetches text from a url
        {
            using (var client = new HttpClient())
            {
                client.DefaultRequestHeaders.Add("User-Agent", "Interlude");
                //impersonates a web browser so interlude isnt blocked as a bot
                ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12; //ensures TLS 1.2 connection can be made to github (no support for TSL 1.1 or 1.0 i dont think)
                try
                {
                    callback(await client.GetStringAsync(url)); //tries to fetch the data and will run the callback with it
                }
                catch (Exception e)
                {
                    Logging.Log("Failed to get web data from " + url, e.ToString(), Logging.LogType.Error);
                }
            }
        }

        public static Utilities.TaskManager.UserTask DownloadFile(string url, string target) //downloads a file as a user task
        {
            return (Output) =>
            {
                using (var client = new WebClient())
                {
                    client.Headers.Add("User-Agent", "Interlude");
                    bool wait = true;
                    Exception error = null;
                    client.DownloadProgressChanged += (o, e) => { Output(url + " (" + e.ProgressPercentage.ToString() + "%)"); };
                    client.DownloadFileCompleted += (o, e) => { wait = false; error = e.Error; };
                    client.DownloadFileAsync(new Uri(url), target);
                    while (wait) { }
                    if (error == null)
                    {
                        return true;
                    }
                    else
                    {
                        Logging.Log("Failed to download file from " + url, error.ToString(), Logging.LogType.Error);
                        return false;
                    }
                }
            };
        }

        public static void DownloadJsonObject<T>(string url, Action<T> callback) //fetches a json object from a url
        {
            DownloadString(url, (s) => {
                try { callback(Newtonsoft.Json.JsonConvert.DeserializeObject<T>(s)); } //just does the string downloader and converts it
                catch (Exception e)
                { Logging.Log("Failed to get json data from " + url, e.ToString(), Logging.LogType.Error); } }); //error if json decoding failed, all web errors happen in DownloadString
        }
    }
}
