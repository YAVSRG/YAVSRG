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
                Console.Out.WriteLine(Bass.LastError);
            }
            else
            {
                
                //Bass.ChannelSetSync(ID, SyncFlags.Position, 0, Test);
                var d = Bass.ChannelGetInfo(ID);
                /*
                long b = Bass.ChannelSeconds2Bytes(ID, 2);
                long l = Bass.ChannelGetLength(ID);
                byte[] data = new byte[l];
                Bass.SampleGetData(ID,data);
                Bass.StreamFree(ID);
                byte[] newdata = new byte[l + b * 2];
                Array.Copy(data, 0, newdata, b, l);
                ID = Bass.CreateStream(newdata, 0, 0, BassFlags.Default);*/
                Duration = Bass.ChannelBytes2Seconds(ID,Bass.ChannelGetLength(ID))*1000;
                Frequency = d.Frequency;
            }
            if (ID == 0)
            {
                Console.Out.WriteLine(Bass.LastError);
            }
        }

        private void Test(int handle, int channel, int bytes, IntPtr user)
        {

        }

        public void Dispose()
        {
            Bass.StreamFree(ID);
        }
        
        public static implicit operator int(Track t)
        {
            if (t == null) { return 0; }
            return t.ID;
        }
    }
}
