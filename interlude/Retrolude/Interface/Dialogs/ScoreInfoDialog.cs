using System;
using System.Drawing;
using Prelude.Net.Protocol.Packets;
using Interlude.Gameplay;
using Interlude.Interface.Widgets;
using Interlude.Graphics;

namespace Interlude.Interface.Dialogs
{
    public class ScoreInfoDialog : FadeDialog
    {
        ScoreInfoProvider Data;
        ScoreGraph Graph;

        public ScoreInfoDialog(ScoreInfoProvider data, Action<string> a) : base(a)
        {
            Reposition(100, 0, ScreenUtils.ScreenHeight * 2 + 100, 0, -100, 1, ScreenUtils.ScreenHeight * 2 - 100, 1);
            Move(new Rect(100, 100, -100, -100));
            Data = data;
            AddChild(new TextBox(Data.FormattedAccuracy, TextAnchor.LEFT, 0, true, Color.White, Color.Black).Reposition(0, 0, 0, 0, 200, 0, 100, 0));
            AddChild((Graph = new ScoreGraph(data)).Reposition(20, 0, -200, 1, -20, 1, -20, 1));
            Game.Online.SendPacket(new PacketScore() { score = data.Score, chartHash = Game.Gameplay.CurrentChart.GetHash() });
        }

        public override void Draw(Rect bounds)
        {
            PreDraw(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(127, 0, 0, 0));
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1f);
            ScreenUtils.DrawFrame(bounds, Color.White);
            DrawWidgets(bounds);
            PostDraw(bounds);
        }

        protected override void OnClosing()
        {
            base.OnClosing();
            Move(new Rect(100, ScreenUtils.ScreenHeight * 2 + 100, -100, ScreenUtils.ScreenHeight * 2 - 100));
        }

        public override void Dispose()
        {
            Graph.RequestRedraw(); //frees fbo
        }
    }
}
