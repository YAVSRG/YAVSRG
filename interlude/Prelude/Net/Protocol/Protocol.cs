using System;
using Newtonsoft.Json;
using Prelude.Utilities;

namespace Prelude.Net.Protocol
{
    public class Protocol
    {
        public static readonly int PROTOCOLVERSION = 3;

        public static string WritePacket(object Packet)
        {
            return Packet.GetType().ToString() + "\r" + JsonConvert.SerializeObject(Packet, Formatting.None) + "\n";
        }

        public static void HandlePacket(string s, int id)
        {
            string[] split = s.Split(new[] { '\r' }, 2);
            try
            {
                Type t = Type.GetType(split[0]);
                var m = typeof(Protocol).GetMethod("DeserializePacket").MakeGenericMethod(t);
                var o = Convert.ChangeType(m.Invoke(null, new object[] { split[1] }), t);
                typeof(Packets.Packet<>).MakeGenericType(t).GetMethod("HandlePacket").Invoke(null, new[] { o, id });
            }
            catch (Exception e)
            {
                Logging.Log("Error parsing packet for id " + id.ToString(), e.ToString() + "\n" + split[0] + "\n" + split[1], Logging.LogType.Error);
            }
        }

        public static T DeserializePacket<T>(string s) //unambiguous method for reflection
        {
            return JsonConvert.DeserializeObject<T>(s);
        }
    }
}
