#version 330 core
in vec2 fUv;
in vec4 fColor;
in float fTexLayer;

uniform bool alphaMasking;
uniform sampler2DArray sampler;
uniform float alphaMult;

out vec4 FragColor;

void main()
{
    FragColor = vec4(0, 0, 0, 0);
    FragColor += texture(sampler, vec3(fUv, fTexLayer));
    FragColor *= fColor;
    FragColor.a *= alphaMult;

    if (alphaMasking && FragColor.a < 0.01f) discard;
}