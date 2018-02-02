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
    class Toolbar : Screen
    {
        Sprite texture, cursor;
        
        public bool hide;
        OptionsMenu options;
        bool collapseOptions;

        public Toolbar()
        {
            hide = false;
            texture = Content.LoadTextureFromAssets("toolbar");
            Widgets.Add(
                new Button("buttonback", "", () => { Pop(); })
                .PositionTopLeft(0,0,AnchorType.MIN,AnchorType.MIN)
                .PositionBottomRight(160,80,AnchorType.MIN,AnchorType.MIN)
                );
            Widgets.Add(
                new Button("buttonplay", "", () => { Game.Audio.Play(); })
                .PositionTopLeft(240,0,AnchorType.MAX,AnchorType.MIN)
                .PositionBottomRight(160,80,AnchorType.MAX,AnchorType.MIN)
                );
            Widgets.Add(
                new Button("buttonpause", "", () => { Game.Audio.Pause(); })
                .PositionTopLeft(160, 0, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(80, 80, AnchorType.MAX, AnchorType.MIN)
                );
            Widgets.Add(
                new Button("buttonstop", "", () => { Game.Audio.Stop(); })
                .PositionTopLeft(80, 0, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MIN)
                );
            Widgets.Add(
                new Button("buttonbase", "Options", () => { collapseOptions = !collapseOptions; var m = collapseOptions ? -1000 : 1000; options.A.Move(m, 0);options.B.Move(m, 0); })
                .PositionTopLeft(160, 0, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(320, 80, AnchorType.MIN, AnchorType.MIN)
                );
            options = new OptionsMenu();
            options.PositionTopLeft(20,105,AnchorType.MIN,AnchorType.MIN).PositionBottomRight(820,105,AnchorType.MIN,AnchorType.MAX);
            Widgets.Add(options);
            cursor = Content.LoadTextureFromAssets("cursor");
        }

        public override void Draw()
        {
            if (hide) { return; }

            SpriteBatch.Draw(texture,-Width, -Height, Width, -Height + 80, Game.Options.Theme.Dark);
            SpriteBatch.DrawRect(-Width, -Height + 80, Width, -Height + 85, Game.Options.Theme.Base);
            SpriteBatch.DrawRect(Width-725, -Height, Width-720, -Height + 80, Game.Options.Theme.Base);

            SpriteBatch.DrawRect(Width - 710, -Height + 55,Width - 710 + 460 * Game.Audio.NowPercentage(), -Height + 65, Game.Options.Theme.Base);
            SpriteBatch.DrawCentredTextToFill(ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title, Width - 710, -Height+20, Width - 250, -Height+60, Game.Options.Theme.MenuFont);

            SpriteBatch.Draw(texture, -Width, Height-80, Width, Height, Game.Options.Theme.Dark);
            SpriteBatch.DrawRect(-Width, Height - 85, Width, Height - 80, Game.Options.Theme.Base);

            base.Draw();

            SpriteBatch.DrawText(Game.Options.Profile.Name, 30f, -Width, Height - 75, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(Game.Version, 25f, Width, Height - 75, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(DateTime.Now.ToShortDateString() + " " + DateTime.Now.ToLongTimeString(), 25f, Width, Height - 35, Game.Options.Theme.MenuFont);

            SpriteBatch.Draw(cursor, Input.MouseX, Input.MouseY, Input.MouseX + 48, Input.MouseY + 48, Game.Options.Theme.Base);
        }

        public override void Update()
        {
            if (hide) { return; }

            base.Update();
        }
    }
}
