#version 330 core
layout (location = 0) in vec2 vPos;
layout (location = 1) in vec2 vUv;
layout (location = 2) in vec4 vColor;
layout (location = 3) in int vTexUnit;

uniform mat4 uProjection;

out vec2 fUv;
out vec4 fColor;
flat out int fTexUnit;

void main()
{
    gl_Position = uProjection * vec4(vPos, 0.0, 1.0);
    //gl_Position.x += float(vTexUnit);
    fUv = vUv;
    fColor = vColor;
    fTexUnit = vTexUnit;
}