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
        public static readonly string Version = "v0.1.2";
        
        public static Game Instance; //keep track of instance of the game (should only be one).

        protected Chart currentChart; //the current selected chart ingame 
        protected MusicPlayer audio; //audio engine instance
        protected Options.Options options; //options handler instance
        public Toolbar Toolbar; //toolbar instance ??

        public static Options.Options Options
        {
            get { return Instance.options; }
        }

        public static Chart CurrentChart
        {
            get { return Instance.currentChart; }
        }

        public static MusicPlayer Audio
        {
            get { return Instance.audio; }
        }

        Sprite test;

        public Game() : base(800, 600)
        {
            Title = "YAVSRG";
            Instance = this;
            Cursor = null; //hack to hide cursor but not confine it. at time of writing this code, opentk doesn't seperate cursor confine from cursor hiding
            VSync = VSyncMode.Off; //probably keeping this permanently as opentk has issues with vsync on
            ManagedBass.Bass.Init();
            audio = new MusicPlayer();
            YAVSRG.Options.Options.Init();
            options = new Options.Options();
            ApplyWindowSettings(options.General);
            ScreenUtils.UpdateBounds(Width, Height); //why is this here? todo: find out
            test = Content.UploadTexture(Utils.CaptureScreen(), 1, 1);
            SpriteBatch.Init();
        }

        public void ApplyWindowSettings(Options.General settings) //apply video settings
        {
            TargetRenderFrequency = settings.FrameLimiter; //set frame limit
            if (settings.WindowMode == YAVSRG.Options.General.WindowType.Borderless)
            { //settings for borderless
                WindowState = WindowState.Maximized;
                WindowBorder = WindowBorder.Hidden;
            }
            else if (settings.WindowMode == YAVSRG.Options.General.WindowType.Fullscreen)
            {//settings for fullscreen
                WindowState = WindowState.Fullscreen;
            }
            else
            {//settings for windowed
                WindowState = WindowState.Normal;
                WindowBorder = WindowBorder.Resizable;
            }
            Audio.SetVolume(settings.AudioVolume);
        }

        protected override void OnResize(EventArgs e) //handles resizing of the window. tells OpenGL the new resolution etc
        {
            base.OnResize(e);
            ScreenUtils.UpdateBounds(Width, Height);
            ClientRectangle = new Rectangle(0, 0, Width, Height);
            GL.Viewport(ClientRectangle);
        }

        protected override void OnRenderFrame(FrameEventArgs e) //frame rendering code. runs every frame.
        {
            base.OnRenderFrame(e);
            GL.Clear(ClearBufferMask.ColorBufferBit); //clear screen
            SpriteBatch.Begin(Width,Height); //start my render code
            if (!ChartLoader.Loaded) //some temp hack to show "LOADING..."
            {
                SpriteBatch.Draw(test, -ScreenUtils.Width, -ScreenUtils.Height, ScreenUtils.Width, ScreenUtils.Height, Color.White);
                SpriteBatch.DrawCentredText("Loading files...", 100f, 0, 0, Color.White);
                SpriteBatch.End();
                SwapBuffers();
                ChartLoader.Init();
                ChartLoader.RandomPack();
            }//it's supposed to show "LOADING" when the game first opens and stay that way until packs are loaded
            else
            {
                Screen.DrawScreens(); //the whole UI
                Toolbar.Draw(); //the toolbar which is separate atm. I'll move it into screens when i redesign the whole screen thing
            }
            SpriteBatch.End();
            SwapBuffers(); //send rendered pixels to screen
        }

        protected override void OnUpdateFrame(FrameEventArgs e) //this is update loop code (tries to hit 120 times a second)
        {
            base.OnUpdateFrame(e);
            if (Screen.Current == null) { Exit(); return; } //close game when you close all the screens (main menu goes last)
            Toolbar.Update();
            audio.Update(); //audio needs updating to handle pauses before song starts and automatic looping
            Screen.UpdateScreens(); //this is the fade to black transition between screens. needs removing for a fancy transition.
            Input.Update(); //input engine is polling based. let's hope noone exceeds some 40kps with one button
        }

        protected override void OnLoad(EventArgs e) //called when game loads up
        {
            base.OnLoad(e);
            Input.Init();
            Toolbar = new Toolbar();
            //i'll clean this up later
        }

        public void ChangeChart(Chart c) //tells the game to change selected chart. handles changing loaded audio file and background
        {
            if (currentChart != null)
            {
                currentChart.UnloadBackground(); //delete old background image from ram. (otherwise it's a GL ram leak)
            }
            currentChart = c;
            audio.ChangeTrack(c.AudioPath());
            audio.Play((long)c.PreviewTime); //play from the preview point given in the chart data
        }

        protected override void OnUnload(EventArgs e)
        {
            base.OnUnload(e);
            options.Save(); //remember to dump any updated profile settings to file
        }
    }
}
