#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(quads, equal_spacing, ccw) in;

layout(location = 0) in vec4 in_Color[];
layout(location = 1) in vec2 in_UV[];

layout(location = 0) out vec4 out_Color;
layout(location = 1) out vec2 out_UV;

float choose(int n, int k) {
    float c = 1;
    for ( int i = 0; i < k; i++) {
        c *= float(n - i) / float(k - i);
    }
    return c;
}

int n = 3;
int m = 3;

void main() {
    float u = gl_TessCoord.x;
    float v = gl_TessCoord.y;
    gl_Position = vec4(0.0, 0.0, 0.0, 0.0);
    out_Color = vec4(0.0, 0.0, 0.0, 0.0);
    out_UV = vec2(0.0, 0.0);
    float c = 1.0; 
    for ( int i = 0; i < n; i ++) {
        for ( int j = 0; j < m; j++) {
            c = choose(n,i) * pow(u,i) * pow(1-u,n-i) 
              * choose(m,j) * pow(v,j) * pow(1-v,m-j);
            gl_Position += c * gl_in[i+n*j].gl_Position;
            out_Color += c * in_Color[i+n*j];
            // TODO: the following line is wrong
            out_UV += c * in_UV[i+n*j];
        }
    }
}
