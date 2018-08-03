using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Threading.Tasks;
using System.Runtime.Serialization.Formatters.Binary;
using Newtonsoft.Json;

namespace YAVSRG.Net.P2P.Protocol
{
    public class Protocol
    {
        public static void SendPacket(Stream s, object Packet)
        {
            using (StreamWriter w = new StreamWriter(s))
            {
                w.Write(Packet.GetType().ToString() + "\r");
                w.Write(JsonConvert.SerializeObject(Packet, Formatting.None) + "\n");
            }
        }

        public static void ReceivePacket(Stream s)
        {
            var bf = new BinaryFormatter();
            using (StreamReader r = new StreamReader(s))
            {
                Type t = Type.GetType(r.ReadLine());
                t.GetMethod("HandlePacket").Invoke(null, new[] { Convert.ChangeType(typeof(JsonConvert).GetMethod("DeserializeObject", new[] { typeof(string), typeof(Type) }).Invoke(null, new object[] { r.ReadLine(), t }), t) });
            }
        }
    }
}
