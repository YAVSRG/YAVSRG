using System;
using System.Drawing;
using Interlude.Graphics;
using Interlude.Interface.Widgets;

namespace Interlude.Interface.Screens
{
    public class ScreenOptions : Screen
    {
        Widget container, selected;
        public ScreenOptions()
        {
            FlowContainer list;
            AddChild(container = new Widget().Reposition(300, 0, 0, 0, 0, 1, 0, 1));
            AddChild((list = new FlowContainer() { RowSpacing = 20 }).Reposition(0, 0, 0, 0, 300, 0, 0, 1));
            list.AddChild(Button("General", "Settings such as audio, screen resolution, frame limiter", () => new GeneralPanel()));
            list.AddChild(Button("Hotkeys", "Bind hotkeys for all aspects of the UI", () => new HotkeysPanel()));
            list.AddChild(Button("Gameplay", "Settings such as noteskins, scroll speed, accuracy", () => new GameplayPanel()));
            list.AddChild(Button("Noteskin & Layout", "Rebind gameplay keys", () => new LayoutPanel()));
            list.AddChild(Button("Themes", "Select, create and edit themes to customise Interlude", () => new ThemePanel()));
            list.AddChild(Button("Debug", "Debug tools", () => new DebugPanel()));
            list.AddChild(Button("Credits", "Credits & special thanks to everyone who has made Interlude possible so far", () => new CreditsPanel()));
        }

        public override void Draw(Rect bounds)
        {
            SpriteBatch.DrawTilingTexture("levelselectbase", GetBounds(bounds), 400, 0, 0, Color.FromArgb(30,Game.Screens.HighlightColor));
            base.Draw(bounds);
            SpriteBatch.Font1.DrawCentredText("Hold " + Game.Options.General.Hotkeys.Help.ToString().ToUpper() + " to see more info when hovering over settings", 30f, 0, bounds.Bottom - 50, Color.White, true, Color.Black);
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Toolbar.Icons.Filter(0b00000001);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Gameplay.UpdateChart();
            //Gameplay.ChartLoader.Refresh();
            //refresh sorting, chart, etc
        }

        Widget Button(string name, string tooltip, Func<Widget> obj)
        {
            if (name == Game.Options.General.LastSelectedOptionsTab)
            {
                container.AddChild(selected = obj());
            }
            return new FramedButton(name, () =>
            {
                Game.Options.General.LastSelectedOptionsTab = name;
                if (selected != null)
                {
                    selected.Dispose();
                    container.RemoveChild(selected);
                }
                container.AddChild(selected = obj());
            }, null)
            {
                Highlight = () =>
         Game.Options.General.LastSelectedOptionsTab == name,
                Frame = 170,
                HorizontalFade = 50,
                Tooltip = tooltip
            }.Reposition(0, 0, 0, 0, 0, 1, 50, 0);
        }
    }
}
