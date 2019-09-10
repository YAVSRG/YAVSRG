using System;
using System.Drawing;
using Prelude.Utilities;
using Interlude.IO;
using Interlude.Graphics;
using Interlude.Interface.Widgets;
using Interlude.Interface.Animations;
using Interlude.Interface.Screens;
using Interlude.Interface.Widgets.Toolbar;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface
{
    class Toolbar : Widget
    {
        AnimationSlider _Height, _NotifFade, _TooltipFade, _TooltipFade2;
        AnimationSeries _NotifAnimation;
        public ToolbarIcons Icons = new ToolbarIcons();
        string Notification;
        string[] Tooltip, Tooltip2;
        public ChatBox Chat;
        WidgetState CursorMode = WidgetState.NORMAL;

        public Toolbar()
        {
            AddChild(Icons.Reposition(0, 0, 0, 0, 0, 1, 80, 0));
            AddChild(
                new SpriteButton("buttonback", Back, null) { Tooltip = "Back to previous screen" }
                .Reposition(0, 0, 0, 0, 240, 0, 80, 0));
            AddChild(new ProfileInfoPanel());
            AddChild(Chat = new ChatBox());
            AddChild(new MusicControls());
            Animation.Add(_Height = new AnimationSlider(-5));
            Animation.Add(_NotifAnimation = new AnimationSeries(true)); Animation.Add(_NotifFade = new AnimationSlider(0));
            Animation.Add(_TooltipFade = new AnimationSlider(0)); Animation.Add(_TooltipFade2 = new AnimationSlider(0));
            Logging.OnLog += (s, d, t) => { if (t != Logging.LogType.Debug) AddNotification(s); };
        }

        public void AddNotification(string notif)
        {
            Notification = notif;
            _NotifAnimation.Clear();
            _NotifFade.Target = 1;
            _NotifAnimation.Add(new AnimationCounter(240, false));
            _NotifAnimation.Add(new AnimationAction(() => { _NotifFade.Target = 0; }));
        }

        public void SetTooltip(string text, string extra)
        {
            if (text != "")
            {
                Tooltip = text.Split('\n');
                Tooltip2 = extra.Split('\n');
                _TooltipFade.Target = 1;
            }
        }

        private void Back()
        {
            if (Game.Screens.Current is ScreenMenu && Game.Tasks.HasTasksRunning())
            {
                Game.Screens.AddDialog(new Dialogs.ConfirmDialog("You have background tasks running. Cancel them and quit?", (r) => { if (r == "Y") Game.Screens.PopScreen(); }));
            }
            else
            {
                Game.Screens.PopScreen();
            }
        }

        private void Collapse()
        {
            _Height.Target = -5;
            Chat.Collapse();
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

                SpriteBatch.Font1.DrawJustifiedText(Game.Version, 18f, ScreenWidth, ScreenHeight - _Height + 5, Game.Options.Theme.MenuFont);
                SpriteBatch.Font1.DrawJustifiedText(((int)Game.Instance.FPS).ToString() + "fps", 18f, ScreenWidth, ScreenHeight - _Height + 28, Game.Options.Theme.MenuFont);
                SpriteBatch.Font1.DrawJustifiedText(DateTime.Now.ToShortDateString() + " " + DateTime.Now.ToLongTimeString(), 18f, ScreenWidth, ScreenHeight - _Height + 51, Game.Options.Theme.MenuFont);

                DrawFrame(new Rect(-ScreenWidth - 30, -ScreenHeight - 30, ScreenWidth + 30, -ScreenHeight + _Height + 3), Game.Screens.BaseColor);
                DrawFrame(new Rect(-ScreenWidth - 30, ScreenHeight - _Height - 3, ScreenWidth + 30, ScreenHeight + 30), Game.Screens.BaseColor);

                base.Draw(bounds.ExpandY(80 - _Height));
                if (Chat.Collapsed && _NotifFade > 0.01f)
                {
                    Game.Screens.DrawChartBackground(bounds.SliceBottom(_Height), Color.FromArgb((int)(180 * _NotifFade), Game.Screens.DarkColor), 2f);
                    SpriteBatch.Font1.DrawCentredTextToFill(Notification, new Rect(bounds.Left, bounds.Bottom - _Height, bounds.Right, bounds.Bottom + 80 - _Height), Color.FromArgb((int)(255 * _NotifFade), Game.Options.Theme.MenuFont), true);
                }
            }

            if (CursorMode > WidgetState.DISABLED)
            {
                SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("cursor"), new Rect(Input.MouseX, Input.MouseY, Input.MouseX + Game.Options.Theme.CursorSize, Input.MouseY + Game.Options.Theme.CursorSize), Game.Screens.HighlightColor));
                float f = _TooltipFade * _TooltipFade2;
                if (f >= 0.001f)
                {
                    float x = Math.Min(bounds.Right - 450, Input.MouseX);
                    float y = Math.Min(bounds.Bottom - 100 - 45 * Tooltip.Length, Input.MouseY);
                    var b = new Rect(x + 50, y + 50, x + 400, y + 50 + 45 * Tooltip.Length);
                    SpriteBatch.DrawRect(b, Color.FromArgb((int)(f * 127), 0, 0, 0));
                    for (int i = 0; i < Tooltip.Length; i++)
                    {
                        SpriteBatch.Font1.DrawText(Tooltip[i], 30f, b.Left, b.Top + i * 45, Color.FromArgb((int)(f * 255), Game.Options.Theme.MenuFont));
                    }
                }
            }
        }

        public override void Update(Rect bounds)
        {
            if (State != WidgetState.DISABLED)
            {
                //here and not attached to the button to not double fire when closing chat box
                if (Game.Options.General.Hotkeys.Exit.Tapped())
                {
                    Back();
                }
                if (Game.Options.General.Hotkeys.CollapseToolbar.Tapped())
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
                _TooltipFade2.Target = Game.Options.General.Hotkeys.Help.Held() ? 1 : 0;
                base.Update(bounds.ExpandY(80 - _Height));
                _TooltipFade.Target = 0;
            }
            else
            {
                Animation.Update();
            }
        }
    }
}
