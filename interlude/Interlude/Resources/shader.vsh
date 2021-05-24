#version 410

layout (location = 0) in vec4 vert;

layout (location = 0) out vec4 out_color;

uniform mat4 transform;

void main()
{
	out_color = vec4(1.0, 0.5, 0.5, 0.05);
	gl_Position = transform * vert;
}
