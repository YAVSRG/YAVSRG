using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
                client.DefaultRequestHeaders.Add("User-Agent", "Interlude"); //github requires this and i dunno why
                ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12; //ensures TLS 1.2 connection can be made to github (no support for TSL 1.1 or 1.0 i dont think)
                try
                {
                    callback(await client.GetStringAsync(url)); //tries to fetch the data and will run the callback with it
                }
                catch (Exception e)
                {
                    Utilities.Logging.Log("Couldn't get web data from " + url + ": " + e.ToString(), Utilities.Logging.LogType.Error);
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
