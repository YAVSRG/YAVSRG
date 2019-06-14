using System;
using System.Collections.Generic;
using Prelude.Utilities;

namespace Prelude.Net.Protocol.Packets
{
    public class PacketPlay : Packet<PacketPlay> //sent to the server to tell it what chart you're playing
        //if you're the chart picker this is forwarded to others so they can synchronise play
    {
        public string hash;
        public string name;
        public string pack;
        public string diff;
        public Dictionary<string, DataGroup> mods;
        public float rate;
    }
}
