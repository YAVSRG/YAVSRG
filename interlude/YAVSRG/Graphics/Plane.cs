using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;

namespace Interlude.Graphics
{
    public struct Plane
    {
        public Vector3 P1, P2, P3, P4;

        public Plane(Vector3 p1, Vector3 p2, Vector3 p3, Vector3 p4)
        {
            P1 = p1;
            P2 = p2;
            P3 = p3;
            P4 = p4;
        }

        public Plane Interpolate(float a, Plane other)
        {
            return new Plane(other.P1 * a + (1 - a) * P1,
                other.P2 * a + (1 - a) * P2,
                other.P3 * a + (1 - a) * P3,
                other.P4 * a + (1 - a) * P4);
        }

        public Plane Translate(Vector3 vec)
        {
            return new Plane(P1 + vec, P2 + vec, P3 + vec, P4 + vec);
        }

        public Plane Rotate(int r)
        {
            switch (r)
            {
                case 3:
                    return new Plane(P2, P3, P4, P1);
                case 2:
                    return new Plane(P3, P4, P1, P2);
                case 1:
                    return new Plane(P4, P1, P2, P3);
                case 0:
                default:
                    return this;
            }
        }
    }
}
