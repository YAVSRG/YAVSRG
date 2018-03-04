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
        public float[] WaveForm;
        public bool Paused;

        public MusicPlayer()
        {
            WaveForm = new float[256];
            timer = new Stopwatch();
        }

        public void SetVolume(float volume)
        {
            ManagedBass.Bass.GlobalStreamVolume = (int)(volume * 10000);
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
            Paused = false;
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
            Paused = false;
        }

        public void Stop()
        {
            ManagedBass.Bass.ChannelStop(nowplaying);
            timer.Stop();
            timer.Reset();
            Seek(0);
            Paused = true;
        }

        public void Pause()
        {
            ManagedBass.Bass.ChannelPause(nowplaying);
            timer.Stop();
            Paused = true;
        }

        public double Now()
        {
            if (nowplaying == null) return 0;
            if (leadIn) return (long)(timer.ElapsedMilliseconds * Rate + startTime) - Game.Options.General.UniversalAudioOffset;
            return ManagedBass.Bass.ChannelBytes2Seconds(nowplaying,ManagedBass.Bass.ChannelGetPosition(nowplaying))*1000 - Game.Options.General.UniversalAudioOffset;
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
            float[] temp = new float[256];
            if (!Paused && Playing)
            {
                //thanks peppy lad i stole this off you
                ManagedBass.Bass.ChannelGetData(nowplaying, temp, (int)ManagedBass.DataFlags.FFT256);
            }
            for (int i = 0; i < 256; i++)
            {
                WaveForm[i] = WaveForm[i] * 0.8f + temp[i] * 0.2f;
            }
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
