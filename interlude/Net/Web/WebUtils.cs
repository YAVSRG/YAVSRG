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
        public async static void DownloadString(string url, Action<string> callback)
        {
            using (var client = new HttpClient())
            {
                client.DefaultRequestHeaders.Add("User-Agent", "Interlude");
                ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls12;
                /*DownloadStringCompletedEventHandler handler = (o, e) =>
                {
                    if (e.Error == null)
                    {
                        callback(e.Result);
                    }
                    else
                    {
                        Utilities.Logging.Log("Couldn't get web data: " + e.Error.ToString(), Utilities.Logging.LogType.Error);
                    }
                };*/
                //client.DownloadStringCompleted += handler; //client is destroyed after so i dont need to remove it
                callback(await client.GetStringAsync(url));
            }
        }

        public static void DownloadJsonObject<T>(string url, Action<T> callback)
        {
            DownloadString(url, (s) => {
                try { callback(Newtonsoft.Json.JsonConvert.DeserializeObject<T>(s)); }
                catch (Exception e)
                { Utilities.Logging.Log("Failed to get json data from " + url + ": " + e.ToString(), Utilities.Logging.LogType.Error); } });
        }
    }
}
