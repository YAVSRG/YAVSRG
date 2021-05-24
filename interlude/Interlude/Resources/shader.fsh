#version 410

layout (location = 0) in vec4 in_color;

out vec4 fragColor;

void main()
{
    fragColor = in_color;
}