signature VECTOR3 = sig
	type vector3
	val EPSILON : real
	val realEquals : real * real -> bool
	val add : vector3 * vector3 -> vector3
	val mult : vector3 * real -> vector3
	val dot : vector3 * vector3 -> real
	val cross : vector3 * vector3 -> vector3
	val hadamard : vector3 * vector3 -> vector3
	val subtract : vector3 * vector3 -> vector3
	val length : vector3 -> real
	val normalised : vector3 -> vector3
	val reflect : vector3 * vector3 -> vector3
	val rotate : vector3 * vector3 * real -> vector3
	val orthogonal : vector3 * vector3 -> bool
	val equals : vector3 * vector3 -> bool
	val parallel : vector3 * vector3 -> bool
	val coplanar : vector3 * vector3 * vector3 -> bool
	val angleBetween : vector3 * vector3 -> real
	val acuteAngle : vector3 * vector3 -> real
	val projection : vector3 * vector3 -> vector3
	val distance : vector3 * vector3 -> real
	val toString : vector3 -> string
end

structure Vector3 : VECTOR3 = struct
	type vector3 = real * real * real
	val EPSILON = 0.00000001
	fun realEquals(d1, d2) = abs(d1 - d2) < EPSILON
	fun add((x1, y1, z1), (x2, y2, z2)) = (x1 + x2, y1 + y2, z1 + z2) : vector3
	fun mult((x, y, z), r) = (x * r, y * r, z * r) : vector3
	fun dot((x1, y1, z1), (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2 : real
	fun cross((x1, y1, z1), (x2, y2, z2)) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2) : vector3
	fun hadamard((x1, y1, z1), (x2, y2, z2)) = (x1 * x2, y1 * y2, z1 * z2) : vector3
	fun subtract(v1, v2) = add(v1, mult(v2, ~1.0))
	fun length(x, y, z) = Math.sqrt(x * x + y * y + z * z)
	fun normalised(v as (x, y, z)) = let val len = length(v) in (x / len, y / len, z / len) : vector3 end
	fun reflect(v, axis) = let val norm = normalised(axis) in subtract(v, mult(norm, 2.0 * dot(v, norm))) end
	fun rotate ((x, y, z), axis, angle) =
		let
			val (ax, ay, az) = normalised(axis)
			val cos = Math.cos(angle)
			val sin = Math.sin(angle)
		in
			(
				  x * (cos + ax * ax * (1.0 - cos))
				+ y * (ax * ay * (1.0 - cos) - az * sin)
				+ z * (ax * az * (1.0 - cos) + ay * sin),

				  x * (ay * ax * (1.0 - cos) + az * sin)
				+ y * (cos + ay * ay * (1.0 - cos))
				+ z * (ay * az * (1.0 - cos) - ax * sin),

				  x * (az * ax * (1.0 - cos) - ay * sin)
				+ y * (az * ay * (1.0 - cos) + ax * sin)
				+ z * (cos + az * az * (1.0 - cos))
			) : vector3
		end
	fun orthogonal(v1, v2) = abs(dot(v1, v2)) < EPSILON
	fun coplanar(v1, v2, v3) = realEquals(dot(v1, cross(v2, v3)), 0.0)
	fun angleBetween(v1, v2) = Math.acos(dot(v1, v2) / (length(v1) * length(v2)))
	fun acuteAngle(v1, v2) = let val tmp = angleBetween(v1, v2) in if tmp <= Math.pi / 2.0 then tmp else Math.pi - tmp end
	fun projection(v, axis) = let val n = normalised(axis) in mult(n, dot(v, n)) end
	fun distance(v1, v2) = length(subtract(v2, v1))
	fun equals(v1, v2) = distance(v1, v2) < EPSILON
	fun parallel(v1, v2) = equals(cross(v1, v2), (0.0, 0.0, 0.0))
	fun toString(x, y, z) = "(" ^ Real.toString x ^ ", " ^ Real.toString y ^ ", " ^ Real.toString z ^ ")"
end