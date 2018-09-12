#version 450
#extension GL_ARB_separate_shader_objects : enable

out gl_PerVertex {
    vec4 gl_Position;
};

layout(location = 0) out vec3 fragColor;

layout(location = 0) in vec3 position;

layout(location = 1) in uvec4 color;

layout(binding = 0) uniform Object {
    mat4 mvp;
} obj;

void main() {
    gl_Position = obj.mvp * vec4(vec3(1,-1,1)*position.xzy, 1);
    fragColor = color.xyz / 255.0;
}
