using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Widgets;
using YAVSRG.Interface.Animations;
using YAVSRG.Interface.Screens;
using YAVSRG.Interface.Widgets.Toolbar;

namespace YAVSRG.Interface
{
    public class Toolbar : Widget
    {
        AnimationSlider _Height, _NotifFade;
        AnimationSeries _NotifAnimation;
        string Notification = "";
        //AnimationColorMixer NotificationColor;
        public ChatBox Chat;
        WidgetState CursorMode = WidgetState.NORMAL;

        public Toolbar()
        {
            AddChild(
                new Button("buttonback", "Back", Back)
                .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(240, 80, AnchorType.MIN, AnchorType.MIN));
            AddChild(
                new Button("buttoninfo", "Notifications", () => { Chat.Expand(); })
                .PositionTopLeft(80, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MIN));
            AddChild(
                new Button("buttonmusic", "Visualiser", () => { if (!(Game.Screens.Current is ScreenVisualiser) && !(Game.Screens.Current is ScreenScore) && Game.Gameplay.CurrentCachedChart != null) Game.Screens.AddScreen(new ScreenVisualiser()); })
                .PositionTopLeft(160, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(80, 80, AnchorType.MAX, AnchorType.MIN));
            AddChild(
                new Button("buttonoptions", "Options", () => { if (!(Game.Screens.Current is ScreenOptions) && !(Game.Screens.Current is ScreenScore)) Game.Screens.AddScreen(new ScreenOptions()); })
                .PositionTopLeft(240, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(160, 80, AnchorType.MAX, AnchorType.MIN));
            AddChild(
                new Button("buttonimport", "Import", () => { if (!(Game.Screens.Current is ScreenImport) && !(Game.Screens.Current is ScreenScore)) Game.Screens.AddScreen(new ScreenImport()); })
                .PositionTopLeft(320, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(240, 80, AnchorType.MAX, AnchorType.MIN));
            AddChild(
                new Button("buttononline", "Multiplayer", () => { if (!(Game.Screens.Current is ScreenLobby) && !(Game.Screens.Current is ScreenScore)) Game.Screens.AddScreen(new ScreenLobby()); })
                .PositionTopLeft(400, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(320, 80, AnchorType.MAX, AnchorType.MIN));
            AddChild(new ProfileInfoPanel());
            AddChild(Chat = new ChatBox());
            AddChild(new MusicControls());
            Animation.Add(_Height = new AnimationSlider(-5));
            Animation.Add(_NotifAnimation = new AnimationSeries(true)); Animation.Add(_NotifFade = new AnimationSlider(0));
            Utilities.Logging.OnLog += (s, d, t) => { if (t != Utilities.Logging.LogType.Debug) AddNotification(s, Color.White); };
        }

        public void AddNotification(string notif, Color color) //todo: use color
        {
            Notification = notif;
            _NotifAnimation.Clear();
            _NotifFade.Target = 1;
            _NotifAnimation.Add(new AnimationCounter(240, false));
            _NotifAnimation.Add(new AnimationAction(() => { _NotifFade.Target = 0; }));
        }

        private void Back()
        {
            if (Game.Screens.Current is ScreenMenu && Game.Tasks.HasTasksRunning())
            {
                Game.Screens.AddDialog(new Dialogs.ConfirmDialog("You have background tasks running. Are you sure you want to cancel them and quit?", (r) => { if (r == "Y") Game.Screens.PopScreen(); }));
            }
            else
            {
                Game.Screens.PopScreen();
            }
        }

        private void Collapse()
        {
            _Height.Target = -5;
            lock (Children)
            {
                foreach (Widget w in Children)
                {
                    if (w is ToolbarWidget)
                    {
                        ((ToolbarWidget)w).OnToolbarCollapse();
                    }
                }
            }
        }

        private void Expand()
        {
            _Height.Target = 80;
        }

        public override void SetState(WidgetState s)
        {
            base.SetState(s);
            if (State < WidgetState.ACTIVE)
            {
                Collapse();
            }
            else
            {
                Expand();
            }
        }

        public void SetCursorState(bool s)
        {
            CursorMode = s ? WidgetState.NORMAL : WidgetState.DISABLED;
        }

        public float Height
        {
            get { return Math.Max(_Height.Val, 0); }
        }

        public override void Draw(Rect bounds)
        {
            if (_Height > 1)
            {
                Game.Screens.DrawChartBackground(bounds.SliceTop(_Height), Game.Screens.DarkColor, 2f);
                Game.Screens.DrawChartBackground(bounds.SliceBottom(_Height), Game.Screens.DarkColor, 2f);

                float s = ScreenWidth / 24f;
                for (int i = 0; i < 48; i++) //draws the waveform
                {
                    float level = Game.Audio.WaveForm[i];
                    level += 0.01f;
                    level *= _Height * 0.005f;
                    level = Math.Min(_Height, level);
                    SpriteBatch.DrawRect(new Rect(-ScreenWidth + i * s + 2, -ScreenHeight, -ScreenWidth + (i + 1) * s - 2, -ScreenHeight + level), Color.FromArgb((int)level, Game.Screens.HighlightColor));

                    SpriteBatch.DrawRect(new Rect(ScreenWidth - i * s - 2, ScreenHeight - level, ScreenWidth - (i + 1) * s + 2, ScreenHeight), Color.FromArgb((int)level, Game.Screens.HighlightColor));
                }

                //SpriteBatch.Font1.DrawText(Game.Options.Profile.Name, 30f, -ScreenWidth, ScreenHeight - _Height + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.Font2.DrawCentredText("Plays: " + Game.Options.Profile.Stats.TimesPlayed.ToString(), 18f, 0, ScreenHeight - _Height + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.Font2.DrawCentredText("Playtime: " + Utils.FormatTime(Game.Options.Profile.Stats.SecondsPlayed * 1000), 18f, 0, ScreenHeight - _Height + 28, Game.Options.Theme.MenuFont);
                SpriteBatch.Font2.DrawCentredText("S Ranks: " + Game.Options.Profile.Stats.SRanks, 18f, 0, ScreenHeight - _Height + 51, Game.Options.Theme.MenuFont);
                SpriteBatch.Font1.DrawJustifiedText(Game.Version, 25f, ScreenWidth, ScreenHeight - _Height + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.Font1.DrawJustifiedText(DateTime.Now.ToShortDateString() + " " + DateTime.Now.ToLongTimeString(), 25f, ScreenWidth, ScreenHeight - _Height + 45, Game.Options.Theme.MenuFont);

                DrawFrame(new Rect(-ScreenWidth - 30, -ScreenHeight - 30, ScreenWidth + 30, -ScreenHeight + _Height + 3), 30f, Game.Screens.BaseColor);
                DrawFrame(new Rect(-ScreenWidth - 30, ScreenHeight - _Height - 3, ScreenWidth + 30, ScreenHeight + 30), 30f, Game.Screens.BaseColor);

                base.Draw(bounds.ExpandY(80 - _Height));
                if (Chat.Collapsed)
                {
                    Game.Screens.DrawChartBackground(bounds.SliceBottom(_Height), Color.FromArgb((int)(255 * _NotifFade), Game.Screens.DarkColor), 2f);
                    SpriteBatch.Font1.DrawCentredTextToFill(Notification, bounds.SliceBottom(_Height), Color.FromArgb((int)(255 * _NotifFade), Game.Options.Theme.MenuFont), true);
                }
            }

            if (CursorMode > WidgetState.DISABLED) SpriteBatch.Draw("cursor", new Rect(Input.MouseX, Input.MouseY, Input.MouseX + Game.Options.Theme.CursorSize, Input.MouseY + Game.Options.Theme.CursorSize), Game.Screens.HighlightColor);
        }

        public override void Update(Rect bounds)
        {
            if (State != WidgetState.DISABLED)
            {
                if (Input.KeyTap(Game.Options.General.Binds.Exit))
                {
                    Back();
                }
                if (Input.KeyTap(OpenTK.Input.Key.T) && Input.KeyPress(OpenTK.Input.Key.ControlLeft))
                {
                    if (State == WidgetState.NORMAL)
                    {
                        SetState(WidgetState.ACTIVE);
                    }
                    else
                    {
                        SetState(WidgetState.NORMAL);
                    }
                }
                base.Update(bounds.ExpandY(80 - _Height));
            }
            else
            {
                Animation.Update();
            }
        }
    }
}
