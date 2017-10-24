

render myVA: (vs, gl) ->

    # face normal test
    let normal = normalize cross(v[0].pos - v[1].pos, v[0].pos - v[2].pos)
    for 1..5:
        for 1..3:
            emit proj * view * model * v.position
        endPrimitive

    # per vertex -> line in vertex normal dir
    var color: Vec3
    var normal: Vec3
    for v in vs:
        for i in 0..1:
            normal = v.normal
            color = if i == 0: vec3(1,0,0) else: vec3(0,0,1)
            emit proj * view * model * (v.position + vec4(v.normal, 0) * i)
        endPrimitive

    result.color = dot(color, normal)


    # per vertex -> line in vertex normal dir
    for v in vs:
        for i in 0..1:
            gl.Position = proj * view * model * (v.position + vec4(v.normal, 0) * i)
            let normal = v.normal
            let color = if i == 0: vec3(1,0,0) else: vec3(0,0,1)
            result.color = dot(color,normal)
            emitVertex()
        endPrimitive(GL_LINE_STRIP)


    # per vertex -> line in vertex normal dir
    for v in vs:

        gl.Position = proj * view * model * (v.position + vec4(v.normal, 0) * i)
        let normal = v.normal
        let color = vec3(1,0,0)
        result.color = dot(color,normal)
        emitVertex()

        gl.Position = proj * view * model * (v.position + vec4(v.normal, 0) * i)
        let normal = v.normal
        let color = vec3(0,0,1)
        result.color = dot(color,normal)
        emitVertex()


        endPrimitive(GL_LINE_STRIP)


anmerkunger:

invertiere amhängigkeit: Framebuffer-Typ -> Fragment-Typ   =>   Fragment-Typ -> Framebuffer-Typ

render myVA: (v, gl) ->


cities.sort(proc (x,y: string): int =
    cmp(x.len, y.len))

sort(cities) do (x,y: string) -> int:
  cmp(x.len, y.len)

performWithUndo do:
  # multiple-line block of code
  # to perform the task
do:
  # code to undo it
