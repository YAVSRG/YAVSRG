using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Graphics.OpenGL;

namespace YAVSRG
{
    struct Shader
    {
        public int VertexShader;
        public int FragmentShader;
        public int Program;

        public Shader(string vs, string fs)
        {
            FragmentShader = GL.CreateShader(ShaderType.FragmentShader);
            VertexShader = GL.CreateShader(ShaderType.VertexShader);

            Program = GL.CreateProgram();
            GL.AttachShader(Program, FragmentShader);
            GL.AttachShader(Program, VertexShader);
        }
    }
}
