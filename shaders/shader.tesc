#version 450
#extension GL_ARB_separate_shader_objects : enable


layout(location = 0) in vec4 in_Color[];
layout(location = 1) in vec2 in_UV[];

layout(vertices = 9) out;
layout(location = 0) out vec4 out_Color[];
layout(location = 1) out vec2 out_UV[];

void main(void) {

    if (gl_InvocationID == 0) {
        gl_TessLevelInner[0] = 4.0;
        gl_TessLevelInner[1] = 4.0;
        gl_TessLevelOuter[0] = 4.0;
        gl_TessLevelOuter[1] = 4.0;
        gl_TessLevelOuter[2] = 4.0;
        gl_TessLevelOuter[3] = 4.0;
    }

    gl_out[gl_InvocationID].gl_Position =
        gl_in[gl_InvocationID].gl_Position;

    out_Color[gl_InvocationID] = in_Color[gl_InvocationID];
    out_UV[gl_InvocationID] = in_UV[gl_InvocationID];
  
}
