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
        public static readonly string Version = "v0.0.1SOON";

        //why the fuck is half of this static and half of it not?
        //todo: clean this up a bit
        public static Game Instance; //keep track of instance of the game (should only be one).
        public static Chart CurrentChart; //the current selected chart ingame 
        public static MusicPlayer Audio; //audio engine instance
        public static Options.Options Options; //options handler instance
        public static Toolbar Toolbar; //toolbar instance ??

        public Game() : base(1800, 960)
        {
            Title = "YAVSRG";
            Instance = this;
            CursorVisible = false;
            VSync = VSyncMode.Off; //probably keeping this permanently as opentk has issues with vsync on
            FullScreen(); //this needs fiddling with for user options
        }

        public void FullScreen() //temporary code to put the game as a borderless window
        {
            Width = 1920;
            Height = 1030;
            WindowBorder = WindowBorder.Hidden;
            WindowState = WindowState.Maximized;
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
            GL.ClearColor(0, 0, 0, 0);
            SpriteBatch.Begin(Width,Height); //start my render code
            if (!ChartLoader.Loaded) //some temp hack that dont even work delete this
            { //it just flashes for one frame
                SpriteBatch.DrawCentredText("LOADING...", 100f, 0, 0, Color.White); 
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
            Audio.Update(); //audio needs updating to handle pauses before song starts and automatic looping
            Screen.Current.Update();
            Screen.UpdateAnimation(); //this is the fade to black transition between screens. needs removing for a fancy transition.
            Input.Update(); //input engine is polling based. let's hope noone exceeds some 40kps with one button
        }

        protected override void OnLoad(EventArgs e) //called when game loads up
        {
            base.OnLoad(e);
            //might need to move all this
            ManagedBass.Bass.Init();
            ScreenUtils.UpdateBounds(Width, Height); //why is this here? todo: find out
            Options = new Options.Options();
            YAVSRG.Options.Options.Init();
            Audio = new MusicPlayer();
            Input.Init();
            SpriteBatch.Init();
            Toolbar = new Toolbar();
            //i'll clean this up later
        }

        public void ChangeChart(Chart c) //tells the game to change selected chart. handles changing loaded audio file and background
        {
            if (CurrentChart != null)
            {
                CurrentChart.UnloadBackground(); //delete old background image from ram. (otherwise it's a GL ram leak)
            }
            CurrentChart = c;
            Audio.ChangeTrack(c.AudioPath());
            Audio.Play((long)c.PreviewTime); //play from the preview point given in the chart data
        }

        protected override void OnUnload(EventArgs e)
        {
            base.OnUnload(e);
            Options.SaveProfile(); //remember to dump any updated profile settings to file
        }
    }
}
