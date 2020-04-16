using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    class AnimationSeries : Animation
    {
        private List<Animation> items = new List<Animation>();
        private bool temporary;

        public AnimationSeries(bool permanent)
        {
            temporary = !permanent;
        }

        public void Clear()
        {
            items.Clear();
        }

        public void Add(Animation a)
        {
            items.Add(a);
        }

        public override bool DisposeMe
        {
            get
            {
                return temporary && !Running;
            }
        }

        public override bool Running
        {
            get
            {
                return items.Count > 0;
            }
        }

        public override void Update()
        {
            if (!Running) { return; }
            if (items[0].DisposeMe)
            {
                items.RemoveAt(0);
            }
            else
            {
                items[0].Update();
            }
        }
    }
}
