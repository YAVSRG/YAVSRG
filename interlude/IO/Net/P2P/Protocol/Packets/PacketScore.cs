using Prelude.Gameplay;

namespace Interlude.Net.P2P.Protocol.Packets
{
    public class PacketScore : Packet<PacketScore>
    {
        public Score score = null;
    }
}
