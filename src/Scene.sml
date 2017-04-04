signature SCENE = sig
	type object
	type lighting
	type scene
	val getHit : scene * Line.line -> (object * Vector3.vector3) option
	val traceRay : scene * Line.line * int -> Vector3.vector3
end

structure Scene : SCENE = struct
	type object = Shape.shape * (Vector3.vector3 -> Vector3.vector3 * real * real * real * real * real) (*shape, at a point: colour, ambient, diffuse, specular, specular shininess, reflectivity*)
	type light = Vector3.vector3 * Vector3.vector3 * real (*position, colour, intensity*)
	type lighting = Vector3.vector3 * light list (*ambient RGB, lights*)
	type scene = Vector3.vector3 * lighting * object list (*background colour, refractive index, lighting, objects*)

	fun getHit((_, _, objects), l as (p, d)) =
		let
			fun closestHit [] = NONE
			|	closestHit (v::vs) =
					if Vector3.dot(Vector3.subtract(v, p), d) >= 0.0 then
						case closestHit vs of
								NONE =>
									SOME(v)
							|	SOME(min_v) =>
									if Vector3.distance(v, p) < Vector3.distance(min_v, p) then
										SOME(v)
									else
										SOME(min_v)
					else
						closestHit vs


			fun closestHitObj [] = NONE
			|	closestHitObj ((_, NONE)::xs) = closestHitObj xs
			|	closestHitObj ((obj, SOME(hit))::xs) =
					case closestHitObj xs of
							NONE => SOME(obj, hit)
						|	SOME(minobj, minhit) =>
								if Vector3.distance(p, hit) < Vector3.distance(p, minhit) then
									SOME(obj, hit)
								else
									SOME(minobj, minhit)

		in
			closestHitObj
				(map
					(fn obj as (shp, _) => (obj, closestHit(Shape.getHit (shp, l))))
					objects)
		end

	fun traceRay(s as (bg, (ambient, lights), objects), l as (p, d), maxRef) = 
		let
			fun getPhong (obj as (shp, func), pos) =
				let
					val (colour, k_a, k_d, k_s, alpha, reflectivity) = func pos

					val N =
						let
							val tmp = Vector3.normalised(Option.valOf(Shape.getNormal(shp, pos)))
								handle Option => (0.0, 0.0, 0.0)
						in
							Vector3.mult(tmp, (if Vector3.dot(tmp, d) < 0.0 then 1.0 else ~1.0))
						end

					fun forLights [] = (0.0, 0.0, 0.0)
					|	forLights ((light_pos, light_colour, light_intensity)::ls) =
						let
							val L = Vector3.normalised(Vector3.subtract(light_pos, pos))
							val R = Vector3.normalised(Vector3.mult(Vector3.reflect(L, N), ~1.0))
							val V = Vector3.normalised(Vector3.subtract(p, pos))
							val distance = Vector3.distance(pos, light_pos)
							val illumination = Vector3.mult(light_colour, light_intensity / (4.0 * Math.pi * distance * distance))
							val NdotL = Vector3.dot(N, L)
							val diffuse =
								Vector3.mult(
									Vector3.hadamard(colour, illumination),
									k_d * NdotL
								)
							val canSeeLight = NdotL > 0.0
							val RdotV = Vector3.dot(R, V)
							val specular =
								Vector3.mult(
									illumination,
									if RdotV > 0.0 then k_s * Math.pow(RdotV, alpha) else 0.0
								)
							val inShadow =
								let
									val dir = Vector3.normalised(Vector3.subtract(light_pos, pos))
									val new_pos = Vector3.add(pos, Vector3.mult(dir, Vector3.EPSILON))
								in
									case getHit(s, Line.fromPosAndDir(new_pos, dir)) of
											NONE => false
										|	SOME(_) => true
								end
									handle InvalidLine => false
						in
							if canSeeLight andalso not inShadow then
								Vector3.add(Vector3.add(diffuse, specular), forLights ls)
							else
								forLights(ls)
						end

					val directIllumination = Vector3.add(Vector3.mult(Vector3.hadamard(ambient, colour), k_a), forLights lights)
				in
					if maxRef = 0 orelse Vector3.realEquals(reflectivity, 0.0) then
						directIllumination
					else
						let
							val reflectedIllumination =
								let
									val dir = Vector3.normalised(Vector3.reflect(d, N))
									val new_pos = Vector3.add(pos, Vector3.mult(dir, Vector3.EPSILON))
								in
									traceRay(s, Line.fromPosAndDir(new_pos, dir), maxRef - 1)
										(*handle InvalidLine => bg*)
								end
						in
							Vector3.add(
								Vector3.mult(directIllumination, 1.0 - reflectivity),
								Vector3.mult(reflectedIllumination, reflectivity)
							)
						end
							handle InvalidLine => directIllumination
				end
		in
			case getHit(s, l) of
					NONE => bg
				|	SOME(x) => getPhong(x)
		end	
end