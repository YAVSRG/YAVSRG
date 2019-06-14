namespace Prelude.Net.Protocol.Packets
{
    public class PacketAuth : Packet<PacketAuth>
    {
        public string username;
        public string passkey;
        public int protocolversion;
    }
}
