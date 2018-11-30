uniform int time;
uniform sampler2D texture;
//in vec4 gl_FragCoord;

void main()
{
	vec2 refract = vec2(sin(time/15.0 + gl_FragCoord.x*100.0 + gl_FragCoord.y*50.0),cos(time/15.0 + gl_FragCoord.y*100.0 + gl_FragCoord.x*50.0));
	vec3 color = texture2D(texture, vec2(gl_FragCoord.x, gl_FragCoord.y) + refract * 5).rgb;
	gl_FragColor = vec4(color, 1.0);
}