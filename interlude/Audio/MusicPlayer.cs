using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;

namespace YAVSRG.Audio
{
    public class MusicPlayer
    {
        private static readonly int BUFFER = 2000;

        private Track nowplaying;
        private Stopwatch timer;
        private double startTime;
        private bool leadIn;
        public bool Loop = true;
        private double Rate;

        public MusicPlayer()
        {
            timer = new Stopwatch();
            //ManagedBass.Bass.Volume = 0.3f;
            ManagedBass.Bass.GlobalStreamVolume = 2000;
        }

        public void SetRate(double rate)
        {
            ManagedBass.Bass.ChannelSetAttribute(nowplaying, ManagedBass.ChannelAttribute.Frequency, nowplaying.Frequency * rate);
            Rate = rate;
        }

        public bool Playing
        {
            get
            {
                return Now() - BUFFER < nowplaying?.Duration;
            }
        }

        public void PlayLeadIn()
        {
            startTime = -BUFFER;
            leadIn = true;
            timer.Start();
        }

        public void Play(long start)
        {
            Stop();
            Seek(start);
            Play();
            startTime = start;
        }

        public void Play()
        {
            ManagedBass.Bass.ChannelPlay(nowplaying);
            timer.Start();
        }

        public void Stop()
        {
            ManagedBass.Bass.ChannelStop(nowplaying);
            timer.Stop();
            timer.Reset();
        }

        public void Pause()
        {
            ManagedBass.Bass.ChannelPause(nowplaying);
            timer.Stop();
        }

        public double Now()
        {
            if (nowplaying == null) return 0;
            if (leadIn) return (long)(timer.ElapsedMilliseconds * Rate + startTime);
            return ManagedBass.Bass.ChannelBytes2Seconds(nowplaying,ManagedBass.Bass.ChannelGetPosition(nowplaying))*1000;
        }

        public float NowPercentage()
        {
            return (float)(Now() / nowplaying.Duration);
        }

        public void Seek(double position)
        {
            ManagedBass.Bass.ChannelSetPosition(nowplaying, ManagedBass.Bass.ChannelSeconds2Bytes(nowplaying,position/1000));
        }

        public void Update()
        {
            if (leadIn && Now() > 0)
            {
                Seek(Now());
                ManagedBass.Bass.ChannelPlay(nowplaying);
                leadIn = false;
            }
            if (!Playing && Loop)
            {
                Play(0);
            }
        }

        public void ChangeTrack(string path)
        {
            var t = new Track(path);
            //if (t.ID == 0) return;
            nowplaying?.Dispose();
            nowplaying = t;
        }
    }
}
