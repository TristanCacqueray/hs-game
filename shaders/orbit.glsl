#version 120
uniform vec2 iResolution;
uniform vec2 center;
const float range = 1626.0;
uniform float trap1_offset;
uniform float trap2_offset;
const float color_power = 32.0;
uniform vec2 seed;

float hypot(vec2 c) {
  float x = abs(c.x);
  float y = abs(c.y);
  float t = min(x, y);
  x = max(x, y);
  t = (t / x);
  if (c.x == 0.0 && c.y == 0.0) {
    return 0.0;
  } else {
    return (x * sqrt((1.0 + (t * t))));
  }
}

vec2 cLog(vec2 c) {
  return vec2(log(hypot(c)), atan(c.x, c.y));
}

vec3 color(vec2 coord) {
  float idx = 0.0;
  vec2 z = coord;
  vec2 c = seed;
  float ci = 0.0;
  vec4 trap = vec4(1e+20);
  while (idx < 64.0) {
    z.y = abs(z.y);
    z = cLog((z + c));
    trap = min(trap, vec4(abs((z.y + (0.5 * sin((trap1_offset + z.x))))), abs((z.x + (0.5 * sin((trap2_offset + z.y))))), dot(z, z), length((fract(z) - 0.5))));
    ci = (ci + length(z));
    idx = (idx + 1.0);
  }
  ci = (1.0 - log2((0.5 * log2((ci / color_power)))));
  vec3 col = vec3(ci);
  col = mix(col, vec3(0.373, 0.694, 0.961), min(1.0, pow((0.5 * trap.x), 0.5)));
  col = mix(col, vec3(0.918, 0.961, 0.98), min(1.0, pow((10.5 * trap.y), 0.5)));
  col = mix(col, vec3(0.824, 0.235, 0.306), (1.0 - min(1.0, pow((1.0 * trap.z), 0.15))));
  return pow((col * col), vec3(1.3));
}

void main(void) {
  vec3 col = vec3(0.0);
  vec2 uv = (((gl_FragCoord.xy / iResolution.xy) * 2.0) - 1.0);
  uv.y = (uv.y * -(iResolution.y / iResolution.x));
  vec2 pos = (center + (uv * range));
  pos = vec2(pos.y, pos.x);
  col = color(pos);
  gl_FragColor = vec4(col, 1.0);
}
