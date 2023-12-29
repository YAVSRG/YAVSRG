#version 330 core
in vec2 fUv;
in vec4 fColor;
flat in int fTexUnit;

uniform bool alphaMasking;
uniform sampler2DArray sampler;

out vec4 FragColor;

void main()
{
    FragColor = vec4(0, 0, 0, 0);
    FragColor += texture(sampler, vec3(fUv, 0));
    FragColor *= fColor;

    if (alphaMasking && FragColor.a < 0.01f) discard;
}