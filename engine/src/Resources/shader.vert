#version 130
in vec2 vPos;
in vec2 vUv;
in vec4 vColor;
in float vTexLayer;

uniform mat4 uProjection;

out vec2 fUv;
out vec4 fColor;
out float fTexLayer;

void main()
{
    gl_Position = uProjection * vec4(vPos, 0.0, 1.0);
    fUv = vUv;
    fColor = vColor;
    fTexLayer = vTexLayer;
}