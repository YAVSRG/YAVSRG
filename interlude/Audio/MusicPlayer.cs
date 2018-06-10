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
        private double Rate;

        private float[] fft = new float[1024];

        public float[] WaveForm;
        public float Level;
        public bool Paused;
        public bool LeadingIn;
        public float LocalOffset = 0;
        public Action OnPlaybackFinish;

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

        protected double AudioOffset { get { return Game.Options.General.UniversalAudioOffset * Rate + LocalOffset; } }

        public double Duration
        {
            get
            {
                return nowplaying.Duration;
            }
        }

        public bool Playing
        {
            get
            {
                return Now()+AudioOffset < nowplaying?.Duration;
            }
        }

        public void PlayLeadIn()
        {
            startTime = -BUFFER;
            LeadingIn = true;
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
            if (LeadingIn) return (long)(timer.ElapsedMilliseconds * Rate + startTime) - AudioOffset;
            return ManagedBass.Bass.ChannelBytes2Seconds(nowplaying,ManagedBass.Bass.ChannelGetPosition(nowplaying))*1000 - AudioOffset;
        }

        public float NowPercentage()
        {
            if (nowplaying == null) return 0;
            return (float)(Now() / Duration);
        }

        public void Seek(double position)
        {
            ManagedBass.Bass.ChannelSetPosition(nowplaying, ManagedBass.Bass.ChannelSeconds2Bytes(nowplaying,position/1000));
            if (LeadingIn)
            {
                LeadingIn = false;
            }
        }

        public void UpdateWaveform()
        {
            //https://www.codeproject.com/Articles/797537/Making-an-Audio-Spectrum-analyzer-with-Bass-dll-Cs
            ManagedBass.Bass.ChannelGetData(nowplaying, fft, (int)ManagedBass.DataFlags.FFT2048);
            int b0 = 0;
            int y;
            for (int x = 0; x < 256; x++)
            {
                float peak = 0;
                int b1 = (int)Math.Pow(2, x * 10.0 / 255);
                if (b1 > 1023) b1 = 1023;
                if (b1 <= b0) b1 = b0 + 1;
                for (; b0 < b1; b0++)
                {
                    if (peak < fft[1 + b0]) peak = fft[1 + b0];
                }
                y = (int)(Math.Sqrt(peak) * 3 * 255 - 4);
                if (y > 255) y = 255;
                if (y < 0) y = 0;
                WaveForm[x] = WaveForm[x] * 0.9f + y * 0.1f;
            }
        }

        public void Update()
        {
            float[] temp = new float[256];
            if (!Paused)
            {
                UpdateWaveform();
            }
            Level = Level * 0.9f + (ManagedBass.Bass.ChannelGetLevelRight(nowplaying) + ManagedBass.Bass.ChannelGetLevelLeft(nowplaying)) * 0.0000002f;
            if (LeadingIn && Playing && Now() + AudioOffset > 0)
            {
                Seek(Now() + AudioOffset);
                ManagedBass.Bass.ChannelPlay(nowplaying);
                LeadingIn = false;
            }
            if (!Playing)
            {
                if (OnPlaybackFinish != null)
                {
                    OnPlaybackFinish();
                }
                else if (!LeadingIn) { LeadingIn = true; }
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
