#version 330 core

uniform mat4 uMVP;

in vec3  aPosition;
in vec4  aColor;

out vec4 vColor;

void main( void ) { 

  gl_Position = uMVP * vec4(aPosition, 1.0);

  vColor = aColor;
}
