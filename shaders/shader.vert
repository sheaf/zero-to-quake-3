#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec2 in_UV;
layout(location = 4) in uvec4 in_Color;

layout(location = 0) out vec4 out_Color;
layout(location = 1) out vec2 out_UV;

layout(binding = 0) uniform Object {
    mat4 mvp;
} obj;


void main() {
    gl_Position = obj.mvp * vec4(in_Position.xyz, 1);
    out_Color = in_Color.rgba / 255.0;
    out_UV = in_UV;
}
