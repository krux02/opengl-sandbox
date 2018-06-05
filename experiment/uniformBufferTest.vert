#version 440 core


layout(std140, binding=0) uniform b00 {
  int b0a;
  long b0c;
};

layout(std140, binding=1) uniform b01 {
  int b1a;
  int b1b;
  long b1c;
};

layout(std140, binding=2) uniform b02 {
  int b2a[2];
  long b2c;
};

layout(std140, binding=3) uniform b03 {
  bool b3a;
  bool b3b;
  bool b3c;
  float b3d;
};

layout(std140, binding=4) uniform b04 {
  mat2 b4a;
  mat2 b4b;
  mat4 b4c;
};

layout(std140, binding=5) uniform b05 {
  mat2 b5a[2];
  mat4 b5c;
};

struct MyStruct {
  float f1;
  float f2;
};

layout(std140, binding=6) uniform b06 {
  MyStruct b6a;
  MyStruct b6b;
};

layout(std140, binding=7) uniform b07 {
  vec3 b7a;
  float b7b;
};

layout(std140, binding=8) uniform b08 {
  vec3 b8a;
  double b8b;
};

layout(std140, binding=9) uniform b09 {
  dvec3 b9a;
  vec2 b9b;
};

layout(std140, binding=10) uniform b10 {
  bool b10a;
  bool b10b;
};

struct StructA {
  vec4 m0;
  vec4 m1;
  float f1;
};

struct StructB {
  dvec4 m0;
  float f1;
};


layout(std140, binding=11) uniform b11 {
  StructA b11a;
  float   b11b;
};

layout(std140, binding=12) uniform b12 {
  StructB b12a;
  float   b12b;
};


layout(std140, binding=13) uniform b13 {
  float   b13a;
  StructB b13b;
};

/*
layout(std140, binding=14) uniform b14 {
  float   b14a;
  StructB b14b;
};
*/


void main() {
  gl_Position = vec4(0.5, 0.5, 0.5, 1);
}
