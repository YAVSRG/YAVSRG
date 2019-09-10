using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class ChartInfoControls : Widget
    {
        Scoreboard sb;
        ChartInfoPanel ip;
        FlowContainer scroll;
        object lastChart = null;

        class LevelSelectButtons : FrameContainer
        {
            public LevelSelectButtons(ModMenu modMenu)
            {
                HorizontalFade = 50; Frame = 170;
                AddChild(new SimpleButton("Collections", () => { Game.Screens.AddDialog(new Dialogs.ConfirmDialog("NYI lol", (s) => { })); }, () => false, () => Game.Options.General.Hotkeys.Collections) { FontSize = 40, Tooltip = "nyi lol" }
                    .Reposition(0, 0.02f, 10, 0, 0, 0.18f, -10, 1));
                AddChild(new SimpleButton("Goals", () => { Game.Screens.AddDialog(new Dialogs.ConfirmDialog("NYI lol", (s) => { })); }, () => false, () => Game.Options.General.Hotkeys.Goals) { FontSize = 40, Tooltip = "nyi lol" }
                    .Reposition(0, 0.22f, 10, 0, 0, 0.38f, -10, 1));
                AddChild(new SimpleButton("Editor", () => { Game.Screens.AddScreen(new Screens.ScreenEditor()); }, () => false, () => Game.Options.General.Hotkeys.Editor) { FontSize = 40, Tooltip = "Edit the selected chart" }
                    .Reposition(0, 0.42f, 10, 0, 0, 0.58f, -10, 1));
                AddChild(new SimpleButton("Mods", () => { modMenu.Toggle(); }, () => false, () => Game.Options.General.Hotkeys.Mods) { FontSize = 40, Tooltip = "Choose gameplay modifiers" }
                    .Reposition(0, 0.62f, 10, 0, 0, 0.78f, -10, 1));
                AddChild(new SimpleButton("Play", () => { Game.Gameplay.PlaySelectedChart(); }, () => false, () => Game.Options.General.Hotkeys.Select) { FontSize = 40, Tooltip = "Play the selected chart" }
                    .Reposition(0, 0.82f, 10, 0, 0, 0.98f, -10, 1));
            }
        }

        public ChartInfoControls() : base()
        {
            sb = new Scoreboard();
            ip = new ChartInfoPanel();

            scroll = new FlowContainer() { Frame = 0, RowSpacing = 10f, BackColor = () => System.Drawing.Color.Transparent };

            AddChild(scroll.Reposition(10, 0, 260, 0, ScreenUtils.ScreenWidth * 2 - 750, 0, -10, 1));
            scroll.AddChild(sb.Reposition(0, 0, 0, 0, -15, 0.5f, 0, 1));
            scroll.AddChild(ip.Reposition(0, 0, 0, 0, -15, 0.5f, 0, 1));
            ChangeChart(true);

            ModMenu modMenu = new ModMenu();
            AddChild(modMenu.Reposition(0, 0, 250, 0, 0, 1, 0, 1));

            AddChild(new LevelSelectButtons(modMenu).Reposition(0, 0, 150, 0, 0, 1, 250, 0));
        }

        public override void OnResize()
        {
            base.OnResize();
            scroll.Reposition(new Rect(10, 260, ScreenUtils.ScreenWidth * 2 - 750, -10));
        }

        public void ChangeChart(bool force)
        {
            if (Game.Gameplay.CurrentChart != lastChart || force)
            {
                sb.UseScoreList(Game.Gameplay.ChartSaveData.Scores);
                lastChart = Game.Gameplay.CurrentChart;
            }
            scroll.Reposition(new Rect(10, 260, ScreenUtils.ScreenWidth * 2 - 750, -10)); //just to make sure
            ip.ChangeChart(); //ip needs to change length/bpm. difficulty is already recalculated
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            ScreenUtils.DrawParallelogramWithBG(bounds.SliceTop(150), 0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, bounds.SliceTop(100), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + Game.CurrentChart.Data.Creator + "         From " + Game.CurrentChart.Data.SourcePack, new Rect(bounds.Left + 50, bounds.Top + 80, bounds.Right - 50, bounds.Top+150), Game.Options.Theme.MenuFont, true);

            DrawWidgets(bounds);
        }
    }
}
