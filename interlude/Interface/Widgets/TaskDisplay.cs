using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets
{
    class TaskDisplay : Widget
    {
        AnimationSlider slide;

        public TaskDisplay()
        {
            Animation.Add(slide = new AnimationSlider(0));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, System.Drawing.Color.Black);
            float y = top + 10f;
            foreach (Utilities.TaskManager.NamedTask t in Game.Tasks.tasks)
            {
                SpriteBatch.Font1.DrawText(t.name, 30f, left, y, Game.Options.Theme.MenuFont);
                SpriteBatch.Font2.DrawText(t.Status.ToString(), 20f, left, y + 25f, Game.Options.Theme.MenuFont);
                y += 50f;
            }
        }
    }
}
