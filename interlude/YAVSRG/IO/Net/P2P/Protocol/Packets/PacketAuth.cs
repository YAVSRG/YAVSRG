namespace Interlude.Net.P2P.Protocol.Packets
{
    public class PacketAuth : Packet<PacketAuth>
    {
        public string username;
        public string passkey;
        public int protocolversion;
    }
}
