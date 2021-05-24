#version 330

layout (location = 0) in vec4 vert;

uniform mat4 transform;

void main()
{
	gl_Position = transform * vert;
}
