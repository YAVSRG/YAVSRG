using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Net;
using System.Net.Http;

namespace YAVSRG.Net.Web
{
    public class WebUtils
    {
        public async static void DownloadString(string url, Action<string> callback) //fetches text from a url
        {
            using (var client = new HttpClient())
            {
                client.DefaultRequestHeaders.Add("User-Agent", "Interlude"); //sites require this to prevent against random web clients downloading
                ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12; //ensures TLS 1.2 connection can be made to github (no support for TSL 1.1 or 1.0 i dont think)
                try
                {
                    callback(await client.GetStringAsync(url)); //tries to fetch the data and will run the callback with it
                }
                catch (Exception e)
                {
                    Utilities.Logging.Log("Failed to get web data from " + url + ": " + e.ToString(), Utilities.Logging.LogType.Error);
                }
            }
        }

        public static bool DownloadFile(string url, string target, Action<int> callback) //downloads a file, not async (cause i couldn't make it work)
        {
            using (var client = new WebClient())
            {
                client.Headers.Add("User-Agent", "Interlude"); //sites require this to prevent against random web clients downloading
                try
                {
                    client.DownloadProgressChanged += (o, e) => { callback(e.ProgressPercentage); };
                    client.DownloadFile(new Uri(url), target);
                    return true;
                }
                catch (Exception e)
                {
                    Utilities.Logging.Log("Failed to download file from " + url + ": " + e.ToString(), Utilities.Logging.LogType.Error);
                    return false;
                }
            }
        }

        public static void DownloadJsonObject<T>(string url, Action<T> callback) //fetches a json object from a url
        {
            DownloadString(url, (s) => {
                try { callback(Newtonsoft.Json.JsonConvert.DeserializeObject<T>(s)); } //just does the string downloader and converts it
                catch (Exception e)
                { Utilities.Logging.Log("Failed to get json data from " + url + ": " + e.ToString(), Utilities.Logging.LogType.Error); } }); //error if json decoding failed, all web errors happen in DownloadString
        }
    }
}
