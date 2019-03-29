using System;
using System.Drawing;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using Interlude.Graphics;
using Interlude.Interface;
using Interlude.Utilities;
using Interlude.Net.P2P;
using Interlude.IO;
using Interlude.IO.Audio;
using Interlude.Gameplay;
using Interlude.Gameplay.Charts.YAVSRG;

namespace Interlude
{
    class Game : GameWindow
    {
        public static readonly string Version = "Interlude v0.3.7";
        
        public static Game Instance; //keep track of instance of the game (should only be one).

        protected GameplayManager gameplay; //the current selected chart ingame 
        protected MusicPlayer audio; //audio engine instance
        protected Options.Options options; //options handler instance
        protected ScreenManager screens;
        protected TrayIcon trayIcon;
        protected TaskManager taskManager;
        protected P2PManager netManager;

        public float FPS;

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

        public static TaskManager Tasks
        {
            get { return Instance.taskManager; }
        }

        public static P2PManager Multiplayer
        {
            get { return Instance.netManager; }
        }

        public static string WorkingDirectory
        {
            get { return Interlude.Options.Options.general.WorkingDirectory; }
        }

        public Game() : base(500, 200, new OpenTK.Graphics.GraphicsMode(32,24,8,0,0))
        {
            options = new Options.Options(); //create options data from profile
            Sprite s = Content.UploadTexture(Utils.CaptureDesktop(new Rectangle(0, 0, DisplayDevice.Default.Width, DisplayDevice.Default.Height)), 1, 1);
            Title = "Interlude";
            Instance = this;
            Cursor = null; //hack to hide cursor but not confine it. at time of writing this code, opentk doesn't seperate cursor confine from cursor hiding
            VSync = VSyncMode.Off; //probably keeping this permanently as opentk has issues with vsync on. best performance is no frame cap and no vsync otherwise you get stutters

            Input.Init();
            SpriteBatch.Init();
            ManagedBass.Bass.Init(); //init bass
            audio = new MusicPlayer(); //init my music player

            gameplay = new GameplayManager();
            screens = new ScreenManager();
            screens.Toolbar = new Toolbar();
            screens.AddScreen(new Interface.Screens.ScreenLoading(s));
            taskManager = new TaskManager();
            netManager = new P2PManager();
            trayIcon = new TrayIcon();

            ApplyWindowSettings(Options.General); //apply window settings from options
            
            Discord.Init();
        }

        public void ApplyWindowSettings(Options.General settings) //apply video settings
        {
            TargetRenderFrequency = settings.FrameLimiter; //set frame limit
            if (settings.WindowMode == Interlude.Options.General.WindowType.Window)
            { //settings for windows
                WindowState = WindowState.Normal;
                WindowBorder = WindowBorder.Resizable;
                Size = new Size(Interlude.Options.General.RESOLUTIONS[settings.Resolution].Item1, Interlude.Options.General.RESOLUTIONS[settings.Resolution].Item2);
                Location = new Point((DisplayDevice.Default.Width - Size.Width) / 2, (DisplayDevice.Default.Height - Size.Height) / 2);
            }
            else if (settings.WindowMode == Interlude.Options.General.WindowType.Fullscreen)
            {//settings for fullscreen
                WindowState = WindowState.Fullscreen;
            }
            else
            {//settings for borderless
                WindowBorder = WindowBorder.Hidden;
                WindowState = WindowState.Maximized;
            }
            Audio.SetVolume(settings.AudioVolume);
        }

        public void CollapseToIcon()
        {
            WindowState = WindowState.Minimized;
            Visible = false;
            Audio.SetVolume(0f);
            trayIcon.Show();
            trayIcon.Text("I'm down here!");
        }

        public void ExpandFromIcon()
        {
            trayIcon.Hide();
            Visible = true;
            ApplyWindowSettings(Options.General);
        }

        protected override void OnResize(EventArgs e) //handles resizing of the window. tells OpenGL the new resolution etc
        {
            base.OnResize(e);
            ScreenUtils.UpdateBounds(Width, Height);
            screens.Resize();
            ClientRectangle = new Rectangle(0, 0, Width, Height);
            GL.Viewport(ClientRectangle);
        }

        protected override void OnRenderFrame(FrameEventArgs e) //frame rendering code. runs every frame.
        {
            base.OnRenderFrame(e);
            if (!Visible) return;
            SpriteBatch.Begin(ScreenUtils.ScreenWidth*2, ScreenUtils.ScreenHeight*2); //start my render code
            screens.Draw();
            SpriteBatch.End();
            SwapBuffers(); //send rendered pixels to screen
            FPS = FPS * 0.999f + (float)(0.001f / e.Time);
        }

        protected override void OnUpdateFrame(FrameEventArgs e) //this is update loop code (tries to hit 120 times a second)
        {
            base.OnUpdateFrame(e);
            if (!Visible) return;
            audio.Update(); //audio needs updating to handle pauses before song starts and automatic looping
            screens.Update(); //updates the current screen as well as animations and stuff to transition between them
            Input.Update(); //input engine is polling based. let's hope noone exceeds some 40kps with one button
            netManager.Update();
            Discord.Update();
        }

        protected override void OnLoad(EventArgs e) //called when game loads up
        {
            base.OnLoad(e);
            Icon = new Icon("icon.ico");
        }

        protected override void OnUnload(EventArgs e)
        {
            base.OnUnload(e);
            gameplay.Unload();
            gameplay.SaveScores();
            gameplay.Collections.Save();
            trayIcon.Destroy();
            taskManager.StopAll();
            netManager.Disconnect();
            Discord.Shutdown();
            options.Save(); //remember to dump any updated profile settings to file
        }
    }
}
