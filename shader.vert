#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 uv;
layout(location = 4) in uvec4 color;

layout(location = 0) out vec3 outColor;
layout(location = 1) out vec2 outUV;

layout(binding = 0) uniform Object {
    mat4 mvp;
} obj;


void main() {
    gl_Position = obj.mvp * vec4(position.xyz, 1);
    outColor = color.xyz / 255.0;
    outUV = uv;
}
