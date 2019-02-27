using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;

namespace YAVSRG.Graphics
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

        /*
        public Plane Transform(Matrix4 mat)
        {
            return new Plane(mat * P1, mat * P2, mat * P3, mat * P4);
        }*/

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
