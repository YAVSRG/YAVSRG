namespace Percyqaz.Flux.Graphics

open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Utils

type private Shader = int

module private Shader =

    let compile (stype: ShaderType) (src: string) : Shader =
        let handle = GL.CreateShader stype
        GL.ShaderSource(handle, src)
        GL.CompileShader handle

        let mutable success = 0
        GL.GetShader(handle, ShaderParameter.CompileStatus, &success)

        if success = 0 then
            let output = GL.GetShaderInfoLog handle

            if output <> null && output <> "" then
                Logging.Critical(sprintf "Error compiling shader type %O: %s" stype output)

        handle

    let set_uniform_mat4 (id: string, value: Matrix4) (shader: Shader) =
        let loc = GL.GetUniformLocation(shader, id)

        if loc < -1 then
            Logging.Error(sprintf "Uniform %s not found in this shader" id)

        GL.UniformMatrix4(loc, false, ref value)

    let set_uniform_f32 (id: string, value: float32) (shader: Shader) =
        let loc = GL.GetUniformLocation(shader, id)

        if loc < -1 then
            Logging.Error(sprintf "Uniform %s not found in this shader" id)

        GL.Uniform1(loc, value)

    let set_uniform_i32 (id: string, value: int) (shader: Shader) =
        let loc = GL.GetUniformLocation(shader, id)

        if loc < -1 then
            Logging.Error(sprintf "Uniform %s not found in this shader" id)

        GL.Uniform1(loc, value)

    let destroy (shader: Shader) = GL.DeleteProgram shader

    let create (vsh: string) (fsh: string) =
        let vert = compile ShaderType.VertexShader vsh
        let frag = compile ShaderType.FragmentShader fsh
        let program = GL.CreateProgram()
        GL.AttachShader(program, vert)
        GL.AttachShader(program, frag)
        GL.LinkProgram program
        let status = GL.GetProgram(program, GetProgramParameterName.LinkStatus)

        if status = 0 then
            Logging.Critical(sprintf "Program failed to link: %s" (GL.GetProgramInfoLog program))

        GL.DetachShader(program, vert)
        GL.DetachShader(program, frag)
        GL.DeleteShader vert
        GL.DeleteShader frag

        program

    let on (shader: Shader) = GL.UseProgram shader

    let main =
        create (get_resource_text "shader.vert") (get_resource_text "shader.frag")
