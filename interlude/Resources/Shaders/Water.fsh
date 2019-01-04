#version 330 core

out vec4 gl_FragColor;

in vec4 gl_FragCoord;

uniform sampler2D tex;
uniform float time;

void main()
{
	vec2 pos = vec2(gl_FragCoord.x/1920, gl_FragCoord.y/1080);
	vec2 refract = vec2(sin(time/15.0 + pos.x*100.0 + pos.y*50.0),cos(time/15.0 + pos.y*100.0 + pos.x*50.0)) * 0.005;
	gl_FragColor = texture2D(tex, pos + refract);
}