using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ManagedBass;
using System.Diagnostics;

namespace YAVSRG
{
    public class DELETETHISAudio
    {
        static MediaPlayer mp;
        private static float seekTo;
        static Stopwatch timer;
        static double relativeTime;
        static float rate;

        public static void Init()
        {
            timer = new Stopwatch();
            mp = new MediaPlayer();
            mp.MediaLoaded += OnLoadSong;
        }

        public static void LoadSong(string path, float time)
        {
            mp.Stop();
            mp.LoadAsync(path);
            seekTo = time;
        }

        public static void OnLoadSong(int a)
        {
            mp.Stop();
            Play();
            Seek(seekTo);
        }

        public static void SetRate(float r)
        {
            rate = r;
            mp.Frequency = 44100 * r;
        }

        public static void Pause()
        {
            mp.Pause();
        }

        public static void Play()
        {
            timer.Reset();
            timer.Start();
            mp.Play();
            relativeTime = mp.Position.TotalMilliseconds;
        }

        public static void Stop()
        {
            timer.Stop();
            mp.Stop();
        }

        public static bool EndOfSong()
        {
            return mp.Position.TotalMilliseconds == mp.Duration.TotalMilliseconds;
        }

        public static double Length()
        {
            return mp.Duration.TotalMilliseconds;
        }

        public static float PositionPercentage()
        {
            return (float)(Position() / Length());
        }

        public static void SeekPercentage(float p)
        {
            Seek(Length() * p);
        }

        public static double Position()
        {
            return relativeTime + mp.Position.TotalMilliseconds;
        }

        public static void Seek(double ms)
        {
            Stop();
            mp.Position = new TimeSpan((long)ms*10000);
            Play();
        }

        public static void PlayEffect(int id)
        {
            Bass.ChannelPlay(id,true);
        }
    }
}
