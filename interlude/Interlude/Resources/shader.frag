#version 330 core
in vec2 fUv;
in vec4 fColor;
//flat in int fTexUnit;

uniform bool alphaMasking;
uniform sampler2D sampler;
//uniform sampler2D samplers[16];

out vec4 FragColor;

void main()
{
    FragColor = fColor * texture(sampler, fUv);

    if (alphaMasking && FragColor.a < 0.01f) discard;
}