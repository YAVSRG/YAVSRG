using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface
{
    class ScreenManager
    {
        readonly Color bgdim = Color.FromArgb(80, 80, 80);
        AnimationFade fade1;
        AnimationFade fade2;
        SlidingEffect parallax = new SlidingEffect(15);
        Toolbar toolbar = new Toolbar();
        List<Screen> stack = new List<Screen>() { null };
        List<Dialog> dialogs = new List<Dialog>();
        AnimationSeries animation = new AnimationSeries(true);
        public Screen Current = null;
        Screen Previous = null;

        public ScreenManager()
        {
            AddScreen(new Screens.ScreenMenu());
        }
        
        public void AddDialog(Dialog d)
        {
            dialogs.Insert(0, d);
        }

        public void AddScreen(Screen s)
        {
            Console.WriteLine("+ " + s.ToString());
            stack.Insert(0, s);
            SetNextScreen(stack[0]);
            animation.Clear();
            fade1 = new AnimationFade(0, 2, 0.996f);
            fade2 = new AnimationFade(0, 1, 0.996f);
            animation.Add(fade1);
            animation.Add(fade2);
        }

        public void PopScreen()
        {
            Console.WriteLine("- " + stack[0].ToString());
            stack.RemoveAt(0);
            SetNextScreen(stack[0]);
            animation.Clear();
            fade1 = new AnimationFade(0, 2, 0.996f);
            fade2 = new AnimationFade(0, 1, 0.996f);
            animation.Add(fade1);
            animation.Add(fade2);
        }

        private void SetNextScreen(Screen s)
        {
            s?.OnEnter(Current);
            Current?.OnExit(s);
            Previous = Current;
            Current = s;
        }

        public void Toolbar(bool x)
        {
            if (x) { toolbar.Expand(); } else { toolbar.Collapse(); };
        }

        public void Draw()
        {
            float parallaxX = Input.MouseX * parallax / Width;
            float parallaxY = Input.MouseY * parallax / Height;
            DrawChartBackground(-Width - parallaxX - parallax, -Height - parallaxY - parallax, Width - parallaxX + parallax, Height - parallaxY + parallax, bgdim);
            if (animation.Running)
            {
                if (fade1.Running)
                {
                    Previous?.Draw(-Width, -Height, Width, Height);
                    DrawChartBackground(-Width - parallaxX - parallax, -Height - parallaxY - parallax, Width - parallaxX + parallax, Height - parallaxY + parallax, Color.FromArgb((int)(255 * fade1), bgdim));
                }
                else
                {
                    Current?.Draw(-Width, -Height, Width, Height);
                    DrawChartBackground(-Width - parallaxX - parallax, -Height - parallaxY - parallax, Width - parallaxX + parallax, Height - parallaxY + parallax, Color.FromArgb((int)(255 * (1-fade2)), bgdim));
                }
            }
            else
            {
                Current?.Draw(-Width, -Height, Width, Height);
            }
            if (dialogs.Count > 0)
            {
                dialogs[0].Draw(-Width, -Height, Width, Height);
            }
            toolbar.Draw(-Width, -Height, Width, Height);
        }

        public void Update()
        {
            if (dialogs.Count > 0)
            {
                dialogs[0].Update(-Width, -Height, Width, Height);
                if (dialogs[0].Closed)
                {
                    dialogs.RemoveAt(0);
                }
            }
            else
            {
                Current?.Update(-Width, -Height, Width, Height);
            }
            if (Previous != null && !animation.Running)
            {
                Previous = null; //lets the old screen get cleaned up
            }
            toolbar.Update(-Width, -Height, Width, Height);
            animation.Update();
            parallax.Update();
        }
    }
}
