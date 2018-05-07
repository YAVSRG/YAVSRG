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
using YAVSRG.Gameplay;

namespace YAVSRG
{
    class Game : GameWindow
    {
        public static readonly string Version = "Interlude v0.2.5";
        
        public static Game Instance; //keep track of instance of the game (should only be one).

        protected GameplayManager gameplay; //the current selected chart ingame 
        protected MusicPlayer audio; //audio engine instance
        protected Options.Options options; //options handler instance
        protected ScreenManager screens;
        //protected System.Windows.Forms.NotifyIcon test;

        public static Options.Options Options
        {
            get { return Instance.options; }
        }

        public static Chart CurrentChart
        {
            get { return Instance.gameplay.CurrentChart; }
        }

        public static GameplayManager Gameplay
        {
            get { return Instance.gameplay; }
        }

        public static ScreenManager Screens
        {
            get { return Instance.screens; }
        }

        public static MusicPlayer Audio
        {
            get { return Instance.audio; }
        }

        public Game() : base(800,600, new OpenTK.Graphics.GraphicsMode(32,24,8,0))
        {
            Sprite s = Content.UploadTexture(Utils.CaptureScreen(new Rectangle(0, 0, DisplayDevice.Default.Width, DisplayDevice.Default.Height)), 1, 1);
            Title = "Interlude";
            Instance = this;
            Cursor = null; //hack to hide cursor but not confine it. at time of writing this code, opentk doesn't seperate cursor confine from cursor hiding
            VSync = VSyncMode.Off; //probably keeping this permanently as opentk has issues with vsync on. best performance is no frame cap and no vsync otherwise you get stutters
            ManagedBass.Bass.Init(); //init bass
            audio = new MusicPlayer(); //init my music player

            YAVSRG.Options.Options.Init(); //init options i.e load profiles
            options = new Options.Options(); //create options data from profile
            ApplyWindowSettings(options.General); //apply window settings from options
            ScreenUtils.UpdateBounds(Width, Height);

            gameplay = new GameplayManager();
            screens = new ScreenManager();
            screens.toolbar = new Toolbar();
            screens.AddScreen(new Interface.Screens.ScreenLoading(s));
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
            SpriteBatch.Begin(ScreenUtils.ScreenWidth*2, ScreenUtils.ScreenHeight*2); //start my render code
            screens.Draw();
            SpriteBatch.End();
            SwapBuffers(); //send rendered pixels to screen
        }

        protected override void OnUpdateFrame(FrameEventArgs e) //this is update loop code (tries to hit 120 times a second)
        {
            base.OnUpdateFrame(e);
            audio.Update(); //audio needs updating to handle pauses before song starts and automatic looping
            screens.Update(); //updates the current screen as well as animations and stuff to transition between them
            Input.Update(); //input engine is polling based. let's hope noone exceeds some 40kps with one button
        }

        protected override void OnLoad(EventArgs e) //called when game loads up
        {
            base.OnLoad(e);
            Input.Init();
            var test = new Discord.EventHandlers();
            Discord.Initialize("420320424199716864", ref test, true, "");
            Utils.SetDiscordData("Just started playing", "Pick a song already!");
            SpriteBatch.Init();
            /*
            test = new System.Windows.Forms.NotifyIcon();
            test.Icon = new Icon(@"C:\Users\percy\Documents\thishadmynamehere\bird.ico");
            test.Visible = true;
            test.ShowBalloonTip(2000, "YAVSRG", "I'm down here!", System.Windows.Forms.ToolTipIcon.Info);
            test.Text = "YAVSRG";*/
        }

        protected override void OnUnload(EventArgs e)
        {
            base.OnUnload(e);
            gameplay.Unload();
            //test.Dispose();
            Discord.Shutdown();
            options.Save(); //remember to dump any updated profile settings to file
        }
    }
}
