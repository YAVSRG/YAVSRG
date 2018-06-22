using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ManagedBass;

namespace YAVSRG.Audio
{
    public class Track
    {
        public int ID;
        public int Frequency;
        public double Duration;

        public Track(string file)
        {
            ID = Bass.CreateStream(file);
            if (ID == 0)
            {
                Utilities.Logging.Log(Bass.LastError.ToString(), Utilities.Logging.LogType.Error);
            }
            else
            {
                var d = Bass.ChannelGetInfo(ID);
                Duration = Bass.ChannelBytes2Seconds(ID,Bass.ChannelGetLength(ID))*1000;
                Frequency = d.Frequency;
            }
        }

        public void Dispose()
        {
            Bass.StreamFree(ID);
            Bass.StreamFree(ID);
        }
        
        public static implicit operator int(Track t)
        {
            if (t == null) { return 0; }
            return t.ID;
        }
    }
}
