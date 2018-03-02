using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface
{
    public class Toolbar : Widget
    {
        Sprite texture, cursor, frame;
        
        public bool hide;

        public Toolbar()
        {
            hide = false;
            texture = Content.LoadTextureFromAssets("toolbar");
            frame = Content.LoadTextureFromAssets("frame");
            cursor = Content.LoadTextureFromAssets("cursor");
            AddChild(
                new Button("buttonback", "", () => { Screen.Pop(); })
                .PositionTopLeft(0,0,AnchorType.MIN,AnchorType.MIN)
                .PositionBottomRight(160,80,AnchorType.MIN,AnchorType.MIN)
                );
            AddChild(
                new Button("buttonplay", "", () => { Game.Audio.Play(); })
                .PositionTopLeft(240,0,AnchorType.MAX,AnchorType.MIN)
                .PositionBottomRight(160,80,AnchorType.MAX,AnchorType.MIN)
                );
            AddChild(
                new Button("buttonpause", "", () => { Game.Audio.Pause(); })
                .PositionTopLeft(160, 0, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(80, 80, AnchorType.MAX, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonstop", "", () => { Game.Audio.Stop(); })
                .PositionTopLeft(80, 0, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MIN)
                );
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            if (hide) { return; }

            float s = (Height * 2 - 80 * 2)/24f;
            for (int i = 0; i < 24; i++)
            {
                float level = Game.Audio.WaveForm[i * 4] + Game.Audio.WaveForm[i * 4 + 1] + Game.Audio.WaveForm[i * 4 + 2] + Game.Audio.WaveForm[i * 4 + 3];
                SpriteBatch.DrawRect(-Width, -Height + 80 + i * s, -Width + 5 + 400 *level, -Height + 78 + s + i * s, Color.FromArgb(100,Game.Options.Theme.Highlight));
                SpriteBatch.DrawRect(Width - 5 - 400 * level, -Height + 80 + i * s, Width, -Height + 78 + s + i * s, Color.FromArgb(100, Game.Options.Theme.Highlight));
            }

            //SpriteBatch.Draw(texture,-Width, -Height, Width, -Height + 80, Game.Options.Theme.Dark);
            DrawStaticChartBackground(-Width, -Height, Width, -Height + 80, Game.Options.Theme.Dark);
            SpriteBatch.DrawFrame(frame, -Width-30, -Height - 30, Width+30, -Height + 85, 30f, Game.Options.Theme.Base);
            SpriteBatch.DrawRect(-Width, -Height + 80, Width, -Height + 85, Game.Options.Theme.Base);
            SpriteBatch.DrawRect(Width-725, -Height, Width-720, -Height + 80, Game.Options.Theme.Base);

            SpriteBatch.DrawRect(Width - 710, -Height + 55,Width - 710 + 460 * Game.Audio.NowPercentage(), -Height + 65, Game.Options.Theme.Base);
            SpriteBatch.DrawCentredTextToFill(ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title, Width - 710, -Height+20, Width - 250, -Height+60, Game.Options.Theme.MenuFont);

            //SpriteBatch.Draw(texture, -Width, Height-80, Width, Height, Game.Options.Theme.Dark);
            DrawStaticChartBackground(-Width, Height-80, Width, Height, Game.Options.Theme.Dark);
            SpriteBatch.DrawRect(-Width, Height - 85, Width, Height - 80, Game.Options.Theme.Base);

            base.Draw(left,top,right,bottom);

            SpriteBatch.DrawText(Game.Options.Profile.Name, 30f, -Width, Height - 75, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(Game.Version, 25f, Width, Height - 75, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(DateTime.Now.ToShortDateString() + " " + DateTime.Now.ToLongTimeString(), 25f, Width, Height - 35, Game.Options.Theme.MenuFont);

            SpriteBatch.Draw(cursor, Input.MouseX, Input.MouseY, Input.MouseX + 48, Input.MouseY + 48, Game.Options.Theme.Base);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            if (hide) { return; }
            base.Update(left, top, right, bottom);
        }
    }
}
