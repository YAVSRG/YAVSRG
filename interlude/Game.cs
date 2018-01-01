using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Drawing;
using YAVSRG.Beatmap;
using YAVSRG.Interface;
using YAVSRG.Audio;

namespace YAVSRG
{
    class Game : GameWindow
    {
        public static readonly string Version = "v0.0.1_hotfix2";

        public static Game Instance;
        public static Chart CurrentChart;
        public static MusicPlayer Audio;
        public static Options.Options Options;
        public static Toolbar Toolbar;

        public Game() : base(1800, 960)
        {
            Title = "YAVSRG";
            Instance = this;
            CursorVisible = false;
            VSync = VSyncMode.Off;
            FullScreen();
        }

        public void FullScreen()
        {
            Width = 1920;
            Height = 1030;
            WindowBorder = WindowBorder.Hidden;
            WindowState = WindowState.Maximized;
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            ScreenUtils.UpdateBounds(Width, Height);
            ClientRectangle = new Rectangle(0, 0, Width, Height);
            GL.Viewport(ClientRectangle);
        }

        protected override void OnRenderFrame(FrameEventArgs e)
        {
            base.OnRenderFrame(e);
            GL.Clear(ClearBufferMask.ColorBufferBit);
            GL.ClearColor(0, 0, 0, 0);
            SpriteBatch.Begin(Width,Height);
            if (!ChartLoader.Loaded)
            {
                SpriteBatch.DrawCentredText("LOADING...", 100f, 0, 0, Color.White);
                ChartLoader.Init();
                ChartLoader.RandomPack();
            }
            else
            {
                Screen.DrawScreens();
                Toolbar.Draw();
            }
            SpriteBatch.End();
            SwapBuffers();
        }

        protected override void OnUpdateFrame(FrameEventArgs e)
        {
            base.OnUpdateFrame(e);
            if (Screen.Current == null) { Exit(); return; }
            Toolbar.Update();
            Audio.Update();
            Screen.Current.Update();
            Screen.UpdateAnimation();
            Input.Update();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            //might need to move all this
            ScreenUtils.UpdateBounds(Width, Height);
            Options = new Options.Options();
            YAVSRG.Options.Options.Init();
            ManagedBass.Bass.Init();
            Audio = new MusicPlayer();
            Input.Init();
            SpriteBatch.Init();
            Toolbar = new Toolbar();
        }

        public void ChangeChart(Chart c)
        {
            if (CurrentChart != null)
            {
                CurrentChart.UnloadBackground();
            }
            CurrentChart = c;
            Audio.ChangeTrack(c.AudioPath());
            Audio.Play((long)c.PreviewTime);
        }

        protected override void OnUnload(EventArgs e)
        {
            base.OnUnload(e);
            Options.SaveProfile();
        }
    }
}
