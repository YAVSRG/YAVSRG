using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    class TaskDisplay : Widget
    {
        class NotifButton : Button
        {
            public NotifButton(string a, string b, Action c) : base(a, b, c) { }

            public void Color(Color c)
            {
                color.Target(c);
            }
        }
        AnimationSlider slide;
        NotifButton b;

        public TaskDisplay()
        {
            Animation.Add(slide = new AnimationSlider(0));
            b = new NotifButton("buttoninfo", "Notifications", () => { slide.Target = 1; });
            AddChild(b.PositionTopLeft(80, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MIN));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            if (slide > 0)
            {
                int a = (int)(255 * slide);
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                SpriteBatch.DrawRect(left, top, right, bottom, Color.FromArgb((int)(a*0.75f), Color.Black));
                float y = top + 160f;
                foreach (Utilities.TaskManager.NamedTask t in Game.Tasks.tasks)
                {
                    SpriteBatch.Font1.DrawJustifiedText(t.name, 30f, right, y, Color.FromArgb(a, Game.Options.Theme.MenuFont));
                    SpriteBatch.Font2.DrawJustifiedText(t.Status.ToString(), 20f, right, y + 30f, Color.FromArgb(a, Game.Options.Theme.MenuFont));
                    y += 50f;
                }
                SpriteBatch.Font1.DrawCentredText("Notifications", 40f, 0, top + 50, Color.FromArgb(a, Game.Options.Theme.MenuFont));
                SpriteBatch.Font2.DrawCentredText("Right click on stuff >> to remove / cancel", 20f, 0, top + 100, Color.FromArgb(a, Game.Options.Theme.MenuFont));
                y = bottom - Utilities.Logging.LogBuffer.Count * 30;
                for (int i = 0; i < Utilities.Logging.LogBuffer.Count; i++)
                {
                    SpriteBatch.Font2.DrawText(Utilities.Logging.LogBuffer[i], 20f, left, y, Color.FromArgb(a, Game.Options.Theme.MenuFont));
                    y += 30;
                }
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (slide.Target == 1)
            {
                if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                {
                    slide.Target = 0;
                }
                else if (Input.KeyTap(OpenTK.Input.Key.BackSlash))
                {
                    Game.Tasks.AddTask(new Utilities.TaskManager.NamedTask(() => { System.Threading.Thread.Sleep(1000); }, "Dummy task", () => { }));
                }
                float y = top + 160f;
                for (int i = 0; i < Game.Tasks.tasks.Count; i++)
                {
                    if (ScreenUtils.MouseOver(right - 250, y, right, y + 40) && Input.MouseClick(OpenTK.Input.MouseButton.Right))
                    {
                        if (Game.Tasks.tasks[i].Status != TaskStatus.Running)
                        {
                            Game.Tasks.tasks[i].Cancel();
                            Game.Tasks.tasks.RemoveAt(i);
                        }
                    }
                    y += 50f;
                }
            }
            if (Game.Tasks.tasks.Count > 0)
            {
                b.Color(Game.Tasks.tasks[Game.Tasks.tasks.Count - 1].Status == TaskStatus.RanToCompletion ? Color.Green : Color.Orange);
            }
        }

        public void Show()
        {
            slide.Target = 1;
        }
    }
}
