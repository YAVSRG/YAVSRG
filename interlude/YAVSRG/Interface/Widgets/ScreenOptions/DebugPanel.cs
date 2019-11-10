using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Graphics;
using Interlude.Gameplay;

namespace Interlude.Interface.Widgets
{
    class DebugPanel : Widget
    {
        public DebugPanel()
        {
            AddChild(new SimpleButton("Recache Charts", () => Game.Tasks.AddTask(ChartLoader.Recache(), ChartLoader.RefreshCallback, "Recaching charts", true), () => false, null).Reposition(50, 0, 50, 0, 350, 0, 100, 0));
            //Prelude.Utilities.Logging.Log(Game.Gameplay.CurrentCachedChart.title);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.TextureAtlas.GetTexture(Game.Options.Themes.AssetsList.Keys.First()), bounds, System.Drawing.Color.White));
        }
    }
}
