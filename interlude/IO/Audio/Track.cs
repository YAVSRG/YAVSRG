using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ManagedBass;
using ManagedBass.Fx;

namespace Interlude.IO.Audio
{
    public class Track
    {
        public int ID; //id used by Bass
        public int Frequency; //frequency the file is encoded in, so it knows how to up and downrate the audio correctly
        public double Duration; //duration of the song so it doesn't have to be tediously recalculated in case of mp3 and stuff

        public Track(string file)
        {
            ID = Bass.CreateStream(file, 0, 0, BassFlags.Decode); //loads file
            if (ID == 0) //this means it didn't work
            {
                Utilities.Logging.Log("Couldn't load audio track from "+file, Bass.LastError.ToString(), Utilities.Logging.LogType.Error);
            }
            else
            {
                var d = Bass.ChannelGetInfo(ID); //asks bass for info about the track
                Duration = Bass.ChannelBytes2Seconds(ID,Bass.ChannelGetLength(ID))*1000;
                Frequency = d.Frequency;
                ID = BassFx.TempoCreate(ID, BassFlags.FxFreeSource);
            }
        }

        public void Dispose() //cleans up after a track doesn't need to be used any more
        {
            Bass.StreamFree(ID);
        }
        
        public static implicit operator int(Track t) //you can throw around this track object as if it is the ID used in bass
        //track can be used instead of track.ID in bass commands
        {
            if (t == null) { return 0; }
            return t.ID;
        }
    }
}
