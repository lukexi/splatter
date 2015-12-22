#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aColor;

out vec3 vColor;

void main( void ) { 

  gl_Position = uMVP * vec4(aPosition, 1.0);

  vColor = aColor;
}
