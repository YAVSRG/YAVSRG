#version 330 core
in vec2 fUv;
in vec4 fColor;

uniform sampler2D uTexture0;
uniform bool alphaMasking;

out vec4 FragColor;

void main()
{
    FragColor = fColor * texture(uTexture0, fUv);

    if (alphaMasking && FragColor.a < 0.01f) discard;
}