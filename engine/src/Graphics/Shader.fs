namespace Percyqaz.Flux.Graphics

open OpenTK.Graphics.OpenGL
open OpenTK.Mathematics

module private Shader =

    let private compile_shader (stype: ShaderType, src: string) : int =
        let handle = GL.CreateShader stype
        GL.ShaderSource(handle, src)
        GL.CompileShader handle

        let mutable success = 0
        GL.GetShader(handle, ShaderParameter.CompileStatus, &success)

        if success = 0 then
            let output = GL.GetShaderInfoLog handle

            if output <> null && output <> "" then
                failwithf "Error compiling shader type %O: %s" stype output

        handle

    let private get_uniform_location(uniform: string, shader: int) : int =
        GL.GetUniformLocation(shader, uniform)

    let private compile_and_use (vsh: string, fsh: string) : int =
        let vert = compile_shader (ShaderType.VertexShader, vsh)
        let frag = compile_shader (ShaderType.FragmentShader, fsh)
        let program = GL.CreateProgram()
        GL.AttachShader(program, vert)
        GL.AttachShader(program, frag)
        GL.LinkProgram program
        let status = GL.GetProgram(program, GetProgramParameterName.LinkStatus)

        if status = 0 then
            failwithf "Program failed to link: %s" (GL.GetProgramInfoLog program)

        GL.DetachShader(program, vert)
        GL.DetachShader(program, frag)
        GL.DeleteShader vert
        GL.DeleteShader frag

        GL.UseProgram program

        program

    let mutable projection_loc = 0
    let mutable alpha_mult_loc = 0
    let mutable alpha_masking_loc = 0
    let mutable sampler_loc = 0

    let init() : unit =
        let shader_src (name: string) : string =
            use s =
                System.Reflection.Assembly
                    .GetCallingAssembly()
                    .GetManifestResourceStream("Percyqaz.Flux.Resources." + name)
            use tr = new System.IO.StreamReader(s)
            tr.ReadToEnd()

        let program = compile_and_use (shader_src "shader.vert", shader_src "shader.frag")
        projection_loc <- get_uniform_location("uProjection", program)
        alpha_mult_loc <- get_uniform_location("alphaMult", program)
        alpha_masking_loc <- get_uniform_location("alphaMasking", program)
        sampler_loc <- get_uniform_location("sampler", program)

    let set_uniform_mat4 (location: int, value: Matrix4) : unit =
        GL.UniformMatrix4(location, false, ref value)

    let set_uniform_f32 (location: int, value: float32) : unit =
        GL.Uniform1(location, value)

    let set_uniform_i32 (location: int, value: int) : unit =
        GL.Uniform1(location, value)