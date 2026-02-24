# Based on a renderman shader by Michael Rivero this demo isn't to show the
# amazing fractal visualizer, but about how easy it is to integrate a shader
# from shadertoy in here
import ../fancygl

declareFramebuffer(RenderTarget(renderTargetSize: Vec2i)):  
  depth  = newDepthRenderBuffer(renderTargetSize)
  color  = newTexture2D(renderTargetSize, GL_RGBA8)

const sharedCode = """
// Remnant X
// by David Hoskins.
// Thanks to boxplorer and the folks at 'Fractalforums.com'
// HD Video:- https://www.youtube.com/watch?v=BjkK9fLXXo0

// #define STEREO

vec3 sunDir  = normalize( vec3(  0.35, 0.1,  0.3 ) );
const vec3 sunColour = vec3(1.0, .95, .8);


#define SCALE 2.8
#define MINRAD2 .25
float minRad2 = clamp(MINRAD2, 1.0e-9, 1.0);
#define scale (vec4(SCALE, SCALE, SCALE, abs(SCALE)) / minRad2)
float absScalem1 = abs(SCALE - 1.0);
float AbsScaleRaisedTo1mIters = pow(abs(SCALE), float(1-10));
vec3 surfaceColour1 = vec3(.8, .0, 0.);
vec3 surfaceColour2 = vec3(.4, .4, 0.5);
vec3 surfaceColour3 = vec3(.5, 0.3, 0.00);
vec3 fogCol = vec3(0.4, 0.4, 0.4);
float gTime;


//----------------------------------------------------------------------------------------
float Noise( in vec3 x )
{
  vec3 p = floor(x);
  vec3 f = fract(x);
	f = f*f*(3.0-2.0*f);

	//vec2 uv = (p.xy + vec2(37.0,17.0)*p.z) + f.xy;
	//vec2 rg = texture( iChannel0, (uv+ 0.5)/256.0, -99.0 ).yx;
	//return mix( rg.x, rg.y, f.z );
  return texture(iChannel0, (p + f + 0.5) / 256.0, -99.0).x;
}

//----------------------------------------------------------------------------------------
float Map(vec3 pos)
{

	vec4 p = vec4(pos,1);
	vec4 p0 = p;  // p.w is the distance estimate

	for (int i = 0; i < 9; i++)
	{
		p.xyz = clamp(p.xyz, -1.0, 1.0) * 2.0 - p.xyz;

		float r2 = dot(p.xyz, p.xyz);
		p *= clamp(max(minRad2/r2, minRad2), 0.0, 1.0);

		// scale, translate
		p = p*scale + p0;
	}
	return ((length(p.xyz) - absScalem1) / p.w - AbsScaleRaisedTo1mIters);
}

//----------------------------------------------------------------------------------------
vec3 Colour(vec3 pos, float sphereR)
{
	vec3 p = pos;
	vec3 p0 = p;
	float trap = 1.0;

	for (int i = 0; i < 6; i++)
	{

		p.xyz = clamp(p.xyz, -1.0, 1.0) * 2.0 - p.xyz;
		float r2 = dot(p.xyz, p.xyz);
		p *= clamp(max(minRad2/r2, minRad2), 0.0, 1.0);

		p = p*scale.xyz + p0.xyz;
		trap = min(trap, r2);
	}
	// |c.x|: log final distance (fractional iteration count)
	// |c.y|: spherical orbit trap at (0,0,0)
	vec2 c = clamp(vec2( 0.3333*log(dot(p,p))-1.0, sqrt(trap) ), 0.0, 1.0);

    float t = mod(length(pos) - gTime*150., 16.0);
    surfaceColour1 = mix( surfaceColour1, vec3(.4, 3.0, 5.), pow(smoothstep(0.0, .3, t) * smoothstep(0.6, .3, t), 10.0));
	return mix(mix(surfaceColour1, surfaceColour2, c.y), surfaceColour3, c.x);
}


//----------------------------------------------------------------------------------------
vec3 GetNormal(vec3 pos, float distance)
{
    distance *= 0.001+.0001;
	vec2 eps = vec2(distance, 0.0);
	vec3 nor = vec3(
	    Map(pos+eps.xyy) - Map(pos-eps.xyy),
	    Map(pos+eps.yxy) - Map(pos-eps.yxy),
	    Map(pos+eps.yyx) - Map(pos-eps.yyx));
	return normalize(nor);
}

//----------------------------------------------------------------------------------------
float GetSky(vec3 pos)
{
    pos *= 2.3;
	float t = Noise(pos);
    t += Noise(pos * 2.1) * .5;
    t += Noise(pos * 4.3) * .25;
    t += Noise(pos * 7.9) * .125;
	return t;
}

//----------------------------------------------------------------------------------------
float BinarySubdivision(in vec3 rO, in vec3 rD, vec2 t)
{
    float halfwayT;

    for (int i = 0; i < 6; i++)
    {

        halfwayT = dot(t, vec2(.5));
        float d = Map(rO + halfwayT*rD);
        //if (abs(d) < 0.001) break;
        t = mix(vec2(t.x, halfwayT), vec2(halfwayT, t.y), step(0.0005, d));

    }

	return halfwayT;
}

//----------------------------------------------------------------------------------------
vec2 Scene(in vec3 rO, in vec3 rD, in vec2 fragCoord)
{
	// float t = .05 + 0.05 * texelFetch(iChannel0, ivec3(ivec2(fragCoord.xy), 0), 0).y;
  float t = .05 + 0.05 * texture(iChannel0, vec3(fragCoord.xy / iChannelResolution0.xy, 0.5 / 256.0)).y;
	vec3 p = vec3(0.0);
    float oldT = 0.0;
    bool hit = false;
    float glow = 0.0;
    vec2 dist;
	for( int j=0; j < 100; j++ )
	{
		if (t > 12.0) break;
        p = rO + t*rD;

		float h = Map(p);

		if(h  <0.0005)
		{
            dist = vec2(oldT, t);
            hit = true;
            break;
        }
       	glow += clamp(.05-h, 0.0, .4);
        oldT = t;
      	t +=  h + t*0.001;
 	}
    if (!hit)
        t = 1000.0;
    else       t = BinarySubdivision(rO, rD, dist);
    return vec2(t, clamp(glow*.25, 0.0, 1.0));

}

//----------------------------------------------------------------------------------------
float Hash(vec2 p)
{
	return fract(sin(dot(p, vec2(12.9898, 78.233))) * 33758.5453)-.5;
}

//----------------------------------------------------------------------------------------
vec3 PostEffects(vec3 rgb, vec2 xy)
{
	// Gamma first...


	// Then...
	#define CONTRAST 1.08
	#define SATURATION 1.5
	#define BRIGHTNESS 1.5
	rgb = mix(vec3(.5), mix(vec3(dot(vec3(.2125, .7154, .0721), rgb*BRIGHTNESS)), rgb*BRIGHTNESS, SATURATION), CONTRAST);
	// Noise...
	//rgb = clamp(rgb+Hash(xy*iTime)*.1, 0.0, 1.0);
	// Vignette...
	rgb *= .5 + 0.5*pow(20.0*xy.x*xy.y*(1.0-xy.x)*(1.0-xy.y), 0.2);

    rgb = pow(rgb, vec3(0.47 ));
	return rgb;
}

//----------------------------------------------------------------------------------------
float Shadow( in vec3 ro, in vec3 rd)
{
	float res = 1.0;
    float t = 0.05;
	float h;

    for (int i = 0; i < 8; i++)
	{
		h = Map( ro + rd*t );
		res = min(6.0*h / t, res);
		t += h;
	}
    return max(res, 0.0);
}

//----------------------------------------------------------------------------------------
mat3 RotationMatrix(vec3 axis, float angle)
{
    axis = normalize(axis);
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;

    return mat3(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,
                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,
                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c);
}

//----------------------------------------------------------------------------------------
vec3 LightSource(vec3 spotLight, vec3 dir, float dis)
{
    float g = 0.0;
    if (length(spotLight) < dis)
    {
		g = pow(max(dot(normalize(spotLight), dir), 0.0), 500.0);
    }

    return vec3(.6) * g;
}

//----------------------------------------------------------------------------------------
vec3 CameraPath( float t )
{
    vec3 p = vec3(-.78 + 3. * sin(2.14*t),.05+2.5 * sin(.942*t+1.3),.05 + 3.5 * cos(3.594*t) );
	return p;
}

//----------------------------------------------------------------------------------------
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
  float m = (iMouse.x/iResolution.x)*300.0;
  gTime = (iTime+m)*.01 + 15.00;
    vec2 xy = fragCoord.xy / iResolution.xy;
  vec2 uv = (-1.0 + 2.0 * xy) * vec2(iResolution.x/iResolution.y, 1.0);


  #ifdef STEREO
  float isRed = mod(fragCoord.x + mod(fragCoord.y, 2.0),2.0);
  #endif

  vec3 cameraPos  = CameraPath(gTime);
    vec3 camTar    = CameraPath(gTime + .01);

  float roll = 13.0*sin(gTime*.5+.4);
  vec3 cw = normalize(camTar-cameraPos);

  vec3 cp = vec3(sin(roll), cos(roll),0.0);
  vec3 cu = normalize(cross(cw,cp));

  vec3 cv = normalize(cross(cu,cw));
    cw = RotationMatrix(cv, sin(-gTime*20.0)*.7) * cw;
  vec3 dir = normalize(uv.x*cu + uv.y*cv + 1.3*cw);

  #ifdef STEREO
  cameraPos += .008*cu*isRed; // move camera to the right
  #endif

    vec3 spotLight = CameraPath(gTime + .03) + vec3(sin(gTime*18.4), cos(gTime*17.98), sin(gTime * 22.53))*.2;
  vec3 col = vec3(0.0);
  vec3 sky = vec3(0.03, .04, .05) * GetSky(dir);
  vec2 ret = Scene(cameraPos, dir,fragCoord);

    if (ret.x < 900.0)
    {
    vec3 p = cameraPos + ret.x*dir;
    vec3 nor = GetNormal(p, ret.x);

         vec3 spot = spotLight - p;
    float atten = length(spot);

        spot /= atten;

        float shaSpot = Shadow(p, spot);
        float shaSun = Shadow(p, sunDir);

         float bri = max(dot(spot, nor), 0.0) / pow(atten, 1.5) * .15;
        float briSun = max(dot(sunDir, nor), 0.0) * .3;

       col = Colour(p, ret.x);
       col = (col * bri * shaSpot) + (col * briSun* shaSun);

       vec3 ref = reflect(dir, nor);
       col += pow(max(dot(spot,  ref), 0.0), 10.0) * 2.0 * shaSpot * bri;
       col += pow(max(dot(sunDir, ref), 0.0), 10.0) * 2.0 * shaSun  * bri;
    }

    col = mix(sky, col, min(exp(-ret.x+1.5), 1.0));
    col += vec3(pow(abs(ret.y), 2.)) * vec3(.02, .04, .1);

    col += LightSource(spotLight-cameraPos, dir, ret.x);
  col = PostEffects(col, xy);


  #ifdef STEREO
    col = vec3((col.x + col.y + col.z) / 3.0);
  col *= vec3( isRed, 1.0-isRed, 1.0-isRed );
  #endif

  fragColor=vec4(col,1.0);
}

//--------------------------------------------------------------------------

"""

proc noiseTexture3D*(size: Vec3i): Texture3D =
  var data : seq[uint32] = newSeq[uint32](size.x * size.y * size.z)
  for it in data.mitems:
    it = rand_u32()

  result = newTexture3D(size)
  result.setData(data)

  result.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
  result.parameter(GL_TEXTURE_WRAP_T, GL_REPEAT)
  result.parameter(GL_TEXTURE_WRAP_R, GL_REPEAT)

  result.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  result.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)

const
  iChannelResolution0 = vec2i(256)

proc main*(window: Window): void = 

  let iChannel0 = noiseTexture3D(vec3i(256))
  iChannel0.parameter(GL_TEXTURE_WRAP_S, GL_REPEAT)
  iChannel0.parameter(GL_TEXTURE_WRAP_T, GL_REPEAT)
  
  glDisable(GL_DEPTH_TEST)
  let aspectRatio = window.aspectRatio.float32

  var runGame: bool = true

  let timer = newStopWatch(true)

  var centerScale = vec3f(0,0,1)
  var mousePos: Vec2i

  let windowSize = window.size
  # rendering at full resolution costs a lot of performance. This renders at
  # half the fullscreen resolution to upscale later. This is worth it on laptop
  # hardware.
  let framebufferSize = window.size / 2
  
  let fb1 = newRenderTarget(framebufferSize)

  while runGame:
    #var buttonStates:uint32 =
    discard getMouseState(mousePos.x.addr, mousePos.y.addr)
    
    var texCoord: Vec2f = mousePos.vec2f / window.size.vec2f
    texCoord.y = 1 - texCoord.y
    let pos: Vec2f = centerScale.xy + (texCoord - 0.5) * vec2(aspectRatio, 1) * centerScale.z;
    centerScale.xy = pos

    for evt in events():
      if evt.kind == QUIT:
        runGame = false
        break
      if evt.kind == KEY_DOWN:
        if evt.key.keysym.scancode == SCANCODE_ESCAPE:
          runGame = false
        if evt.key.keysym.scancode == SCANCODE_F10:
          window.screenshot

      if evt.kind == MOUSEWHEEL:
        centerScale.z *= pow(1.05f, -evt.wheel.y.float32)

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)

    blockBindFramebuffer(fb1): 
      glViewport(0, 0, framebufferSize.x, framebufferSize.y)
      shadingDsl:
        uniforms:
          iChannel0
          iChannelResolution0
          iMouse = mousePos
          iResolution = vec2f(framebufferSize)
          iTime = float32(timer.time)
        includes:
          sharedCode
        fragmentMain:
          """
          mainImage(color, gl_FragCoord.xy);
          """

    glViewport(0,0,windowsize.x, windowsize.y)
    shadingDsl:
      uniforms:
        tex = fb1.color
      fragmentMain:
        """
        color = texture(tex, texCoord);
        """

    glSwapWindow(window)

when isMainModule:
  let (window, _) = defaultSetup()
  main(window)

# Local Variables:
# compile-command: "cd examples; nim c -r -d:release fractalworld.nim"
# End:
