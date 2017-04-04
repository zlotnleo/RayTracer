structure LineLineIntersection = struct
	datatype linelineintersection = SKEW | PARALLEL | COINCIDE | POINT of Vector3.vector3
end

signature LINE = sig
	type line
	exception InvalidLine
	val fromTwoPoints : Vector3.vector3 * Vector3.vector3 -> line
	val fromPosAndDir : Vector3.vector3 * Vector3.vector3 -> line
	val direction : line -> Vector3.vector3
	val isPointOn : line * Vector3.vector3 -> bool
	val parallel : line * line -> bool
	val perpendicular : line * line -> bool
	val coincide : line * line -> bool
	val skew : line * line -> bool
	val intersectAtPoint : line * line -> bool
	val intersection : line * line -> LineLineIntersection.linelineintersection
	val angleBetween : line * line -> real
	val lineDistance : line * line -> real
	val pointDistance : line * Vector3.vector3 -> real
	val parallelThroughPoint : line * Vector3.vector3 -> line
end

structure Line : LINE = struct
	type line = Vector3.vector3 * Vector3.vector3 (*starting point and direction*)

	exception InvalidLine
	fun fromTwoPoints(v1, v2) = if not (Vector3.equals(v1, v2)) then (v1, Vector3.subtract(v2, v1)) else raise InvalidLine
	fun fromPosAndDir (p as (_, d)) = if not (Vector3.equals(d, (0.0, 0.0, 0.0))) then p else raise InvalidLine
	fun direction(_, d) = d
	fun isPointOn((p as (px, py, pz), d as (dx, dy, dz)), r as (rx, ry, rz)) =
		if Vector3.realEquals(dx, 0.0) then
			if Vector3.realEquals(dy, 0.0) then
				if Vector3.realEquals(dz, 0.0) then
					raise InvalidLine
				else
					Vector3.realEquals(rx, px) andalso Vector3.realEquals(ry, py)
			else
				if Vector3.realEquals(dz, 0.0) then
					Vector3.realEquals(rx, px) andalso Vector3.realEquals(rz, pz)
				else
					Vector3.realEquals(rx, px) andalso Vector3.realEquals((ry - py) / dy, (rz - pz) / dz)
		else
			if Vector3.realEquals(dy, 0.0) then
				if Vector3.realEquals(dz, 0.0) then
					Vector3.realEquals(ry, py) andalso Vector3.realEquals(rz, pz)
				else
					Vector3.realEquals(ry, py) andalso Vector3.realEquals((rx - px) / dx, (rz - pz) / dz)
			else
				if Vector3.realEquals(dz, 0.0) then
					Vector3.realEquals(rz, pz) andalso Vector3.realEquals((rx - px) / dx, (ry - py) / dy)
				else
					Vector3.realEquals((rx - px) / dx, (ry - py) / dy) andalso Vector3.realEquals((rx - px) / dx, (rz - pz) / dz)
	fun parallel((_, d1), (_, d2)) = Vector3.parallel(d1, d2)
	fun perpendicular((_, d1), (_, d2)) = Vector3.orthogonal(d1, d2)
	fun coincide(l1 as (p1, _), l2) = parallel(l1, l2) andalso isPointOn(l2, p1)
	fun skew(l1 as (p1, d1), l2 as (p2, d2)) = not (parallel(l1, l2)) andalso not(Vector3.orthogonal(Vector3.subtract(p1, p2), Vector3.cross(d1, d2)))
	fun intersection(l1 as (p1 as (p1x, p1y, p1z), d1 as (d1x, d1y, d1z)), l2 as (p2 as (p2x, p2y, p2z), d2 as (d2x, d2y, d2z))) =
		if parallel(l1, l2) then
			if coincide(l1, l2) then
				LineLineIntersection.COINCIDE
			else
				LineLineIntersection.PARALLEL
		else
			if skew(l1, l2) then
				LineLineIntersection.SKEW
			else
				LineLineIntersection.POINT(Vector3.add(p1, Vector3.mult(d1, 
					if not (Vector3.realEquals(d2x, 0.0)) then
						if Vector3.realEquals(d1x * d2y / d2x - d1y, 0.0) then
							0.0
						else
							(p1y - p2y - (p1x - p2x) * d2y / d2x) / (d1x * d2y / d2x - d1y)
					else
						if not (Vector3.realEquals(d2y, 0.0)) then
							if Vector3.realEquals(d1y * d2x / d2y - d1x, 0.0) then
								0.0
							else
								(p1x - p2x - (p1y - p2y) * d2z / d2y) / (d1y * d2x / d2y - d1x)
						else
							if not (Vector3.realEquals(d2z, 0.0)) then
								if Vector3.realEquals(d1z * d2y / d2z - d1y, 0.0) then
									0.0
								else
									(p1y - p2y - (p1z - p2z) * d2y / d2z) / (d1z * d2y / d2z - d1y)
							else
								raise InvalidLine
					)))
	fun intersectAtPoint(l1, l2) =
		case intersection(l1, l2) of
			LineLineIntersection.POINT(_) => true
			| _ => false

	fun angleBetween(l1 as (p1, d1), l2 as (_, d2)) =
		case intersection(l1, l2) of
				LineLineIntersection.POINT(_) => Vector3.acuteAngle(d1, d2)
			|	LineLineIntersection.COINCIDE => 0.0
			|	LineLineIntersection.PARALLEL => 0.0
			|	LineLineIntersection.SKEW => angleBetween(l1, (p1, d2))

		

	fun lineDistance(l1 as (p1, d1), l2 as (p2, d2)) =
		case intersection(l1, l2) of
			LineLineIntersection.POINT(_) => 0.0
		|	LineLineIntersection.COINCIDE => 0.0
		|	LineLineIntersection.PARALLEL =>
				let
					val dp = Vector3.subtract(p2, p1)
				in
					Vector3.length(Vector3.subtract(dp, Vector3.projection(dp, d1)))
				end
		|	LineLineIntersection.SKEW => Vector3.length(Vector3.projection(Vector3.subtract(p2, p1), Vector3.cross(d1, d2)))

	fun pointDistance(l as (p, d), r) = let
			val w = Vector3.subtract(r, p)
		in
			Vector3.length(Vector3.subtract(w, Vector3.projection(w, d)))
		end

	fun parallelThroughPoint((p, d), v) = (v, d)
end