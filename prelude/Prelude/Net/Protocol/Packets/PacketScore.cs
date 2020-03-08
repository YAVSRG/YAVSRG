using Prelude.Gameplay;

namespace Prelude.Net.Protocol.Packets
{
    public class PacketScore : Packet<PacketScore>
    {
        public Score score = null;
        public string chartHash;
    }
}
