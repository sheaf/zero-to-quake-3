#version 450
#extension GL_ARB_separate_shader_objects : enable

out gl_PerVertex {
    vec4 gl_Position;
};

layout(location = 0) out vec3 fragColor;

layout(location = 0) in vec3 position;

layout(location = 1) in vec3 color;

layout(push_constant) uniform Object {
    mat4 mvp;
} obj;

void main() {
    gl_Position = obj.mvp * vec4(position.xyz, 1);
    fragColor = color;
}
