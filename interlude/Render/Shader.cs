using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Graphics.OpenGL;

namespace YAVSRG
{
    public class Shader
    {
        public int VertexShader;
        public int FragmentShader;
        public int Program;

        public Shader(string vs, string fs)
        {
            FragmentShader = GL.CreateShader(ShaderType.FragmentShader);
            VertexShader = GL.CreateShader(ShaderType.VertexShader);
            GL.ShaderSource(FragmentShader, fs);
            GL.ShaderSource(VertexShader, vs);
            GL.CompileShader(FragmentShader);
            if (GL.GetError() != ErrorCode.NoError)
            {
                Utilities.Logging.Log("Couldn't compile fragment shader", GL.GetError().ToString(), Utilities.Logging.LogType.Error);
            }
            GL.CompileShader(VertexShader);
            if (GL.GetError() != ErrorCode.NoError)
            {
                Utilities.Logging.Log("Couldn't compile vertex shader", GL.GetError().ToString(), Utilities.Logging.LogType.Error);
            }

            Program = GL.CreateProgram();
            GL.AttachShader(Program, FragmentShader);
            GL.AttachShader(Program, VertexShader);

            GL.LinkProgram(Program);
        }
    }
}
