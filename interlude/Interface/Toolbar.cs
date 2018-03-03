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
        SlidingEffect slide;

        public Toolbar()
        {
            texture = Content.LoadTextureFromAssets("toolbar");
            frame = Content.LoadTextureFromAssets("frame");
            cursor = Content.LoadTextureFromAssets("cursor");
            AddChild(
                new Button("buttonback", "", () => { Game.Screens.PopScreen(); })
                .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(160, 80, AnchorType.MIN, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonplay", "", () => { Game.Audio.Play(); })
                .PositionTopLeft(240, 0, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(160, 80, AnchorType.MAX, AnchorType.MIN)
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
            slide = new SlidingEffect(80);
            Animation.Add(slide);
        }

        public void Collapse()
        {
            slide.Target = -10;
        }

        public void Expand()
        {
            slide.Target = 80;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            if (slide < 0) { return; }

            float s = (Height * 2 - slide * 2) / 24f;
            for (int i = 0; i < 24; i++)
            {
                float level = Game.Audio.WaveForm[i * 4] + Game.Audio.WaveForm[i * 4 + 1] + Game.Audio.WaveForm[i * 4 + 2] + Game.Audio.WaveForm[i * 4 + 3];
                level += 0.01f;
                level *= slide * 5;
                SpriteBatch.DrawRect(-Width, -Height + slide + i * s, -Width + level, -Height + slide - 2 + (i + 1) * s, Color.FromArgb(100, Game.Options.Theme.Highlight));
                SpriteBatch.DrawRect(Width - level, -Height + slide + i * s, Width, -Height + slide - 2 + (i + 1) * s, Color.FromArgb(100, Game.Options.Theme.Highlight));
            }

            //SpriteBatch.Draw(texture,-Width, -Height, Width, -Height + 80, Game.Options.Theme.Dark);
            DrawStaticChartBackground(-Width, -Height, Width, -Height + slide, Game.Options.Theme.Dark);
            SpriteBatch.DrawFrame(frame, -Width - 30, -Height - 30, Width + 30, -Height + slide + 5, 30f, Game.Options.Theme.Base);
            //SpriteBatch.DrawRect(-Width, -Height + 80, Width, -Height + 85, Game.Options.Theme.Base);
            //SpriteBatch.DrawRect(Width-725, -Height, Width-720, -Height + 80, Game.Options.Theme.Base);

            SpriteBatch.DrawRect(Width - 710, -Height + slide - 25, Width - 710 + 460 * Game.Audio.NowPercentage(), -Height + slide - 15, Game.Options.Theme.Base);
            SpriteBatch.DrawCentredTextToFill(ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title, Width - 710, -Height + slide - 60, Width - 250, -Height + slide - 20, Game.Options.Theme.MenuFont);

            //SpriteBatch.Draw(texture, -Width, Height-80, Width, Height, Game.Options.Theme.Dark);
            DrawStaticChartBackground(-Width, Height - slide, Width, Height, Game.Options.Theme.Dark);
            //SpriteBatch.DrawRect(-Width, Height - slide - 5, Width, Height - slide, Game.Options.Theme.Base);
            SpriteBatch.DrawFrame(frame, -Width - 30, Height - slide - 5, Width + 30, Height + 30, 30f, Game.Options.Theme.Base);

            base.Draw(left, top + slide - 80, right, bottom);

            SpriteBatch.DrawText(Game.Options.Profile.Name, 30f, -Width, Height - slide + 5, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(Game.Version, 25f, Width, Height - slide + 5, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(DateTime.Now.ToShortDateString() + " " + DateTime.Now.ToLongTimeString(), 25f, Width, Height - slide + 45, Game.Options.Theme.MenuFont);

            SpriteBatch.Draw(cursor, Input.MouseX, Input.MouseY, Input.MouseX + 48, Input.MouseY + 48, Game.Options.Theme.Base);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
        }
    }
}
