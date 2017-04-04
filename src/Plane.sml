structure LinePlaneIntersection = struct
	datatype lineplaneintersection = PARALLEL | CONTAINED | POINT of Vector3.vector3
end

structure PlanePlaneIntersection = struct
	datatype planeplaneintersection = PARALLEL | COINCIDE | LINE of Line.line
end

signature PLANE = sig
	type plane
	exception InvalidPlane
	val fromNormalAndDistance : Vector3.vector3 * real -> plane
	val fromNormalAndPoint : Vector3.vector3 * Vector3.vector3 -> plane
	val fromTwoLines : Line.line * Line.line -> plane
	val fromPointAndTwoDirections : Vector3.vector3 * Vector3.vector3 * Vector3.vector3 -> plane
	val fromThreePoints : Vector3.vector3 * Vector3.vector3 * Vector3.vector3 -> plane
	val getNormal : plane -> Vector3.vector3
	val anyPoint : plane -> Vector3.vector3
	val pointOn : plane * Vector3.vector3 -> bool
	val normal : plane * Vector3.vector3 -> bool
	val containsLine : plane * Line.line -> bool
	val lineNormal : plane * Line.line -> bool
	val orthogonal : plane * plane -> bool
	val lineParallel : plane * Line.line -> bool
	val planeParallel : plane * plane -> bool
	val coincide : plane * plane -> bool
	val planeIntersectAtLine : plane * plane -> bool
	val lineIntersectAtPoint : plane * Line.line -> bool
	val lineIntersection : plane * Line.line -> LinePlaneIntersection.lineplaneintersection
	val planeIntersection : plane * plane -> PlanePlaneIntersection.planeplaneintersection
	val vectorAngle : plane * Vector3.vector3 -> real
	val lineAngle : plane * Line.line -> real
	val planeAngle : plane * plane -> real
	val pointDistance : plane * Vector3.vector3 -> real
	val lineDistance : plane * Line.line -> real
	val planeDistance : plane * plane -> real
	val parallelThroughPoint : plane * Vector3.vector3 -> plane
	val parallelThroughLine : plane * Line.line -> plane
end

structure Plane : PLANE = struct
	type plane = Vector3.vector3 * real (*normal and dot product of any point with it*)
	exception InvalidPlane
	fun fromNormalAndDistance(n, d) = (n, d)
	fun fromNormalAndPoint(n, p) = (n, Vector3.dot(n, p))
	fun fromPointAndTwoDirections(r, d1, d2) = let val n = Vector3.normalised(Vector3.cross(d1, d2)) in (n, Vector3.dot(n, r)) end
	fun fromTwoLines(l1 as (p1, d1), l2 as (p2, d2)) =
		case Line.intersection(l1, l2) of
				LineLineIntersection.POINT(r) => let val n = Vector3.normalised(Vector3.cross(d1, d2)) in (n, Vector3.dot(n, r)) end
			|	LineLineIntersection.PARALLEL => fromPointAndTwoDirections(p1, d1, Vector3.subtract(p1, p2))
			|	_ => raise InvalidPlane

	fun fromThreePoints(a, b, c) = fromPointAndTwoDirections(a, Vector3.subtract(b, a), Vector3.subtract(c, a))
	fun getNormal(n, _) = Vector3.normalised(n)
	fun anyPoint ((nx, ny, nz), d) =
		if not(Vector3.realEquals(nx, 0.0)) then
			(d / nx, 0.0, 0.0)
		else
			if not(Vector3.realEquals(ny, 0.0)) then
				(0.0, d / ny, 0.0)
			else
				if not(Vector3.realEquals(nz, 0.0)) then			
					(0.0, 0.0, d / nz)
				else
					raise InvalidPlane

	fun pointOn((n, d), r) = Vector3.realEquals(Vector3.dot(r, n), d)
	fun normal((n, _), a) = Vector3.parallel(n, a)
	fun containsLine(p, (r, d)) = pointOn(p, r) andalso pointOn(p, Vector3.add(r, d))
	fun lineNormal(p, (r, d)) = normal(p, d)
	fun orthogonal((n1, _), (n2, _)) = Vector3.orthogonal(n1, n2)
	fun planeParallel((n1, _), (n2, _)) = Vector3.parallel(n1, n2)
	fun lineParallel ((n, _), (_, d)) = Vector3.orthogonal(n, d)
	fun coincide(p1 as (n1, d1), p2 as (n2, d2)) =
		planeParallel(p1, p2) andalso Vector3.realEquals(d1 / Vector3.length(n1), d2 / Vector3.length(n2))
	fun planeIntersectAtLine(p1, p2) = not (planeParallel(p1, p2))
	fun lineIntersectAtPoint(p, l) = not (lineParallel(p, l))

	fun lineIntersection (pl as (n, D), l as (p, d)) =
		if containsLine(pl, l) then
			LinePlaneIntersection.CONTAINED
		else
			if lineParallel(pl, l) then
				LinePlaneIntersection.PARALLEL
			else
				LinePlaneIntersection.POINT(Vector3.add(p, Vector3.mult(d, (D - Vector3.dot(p, n)) / Vector3.dot(d, n))))

	fun planeIntersection(p1 as (n1 as (n1x, n1y, n1z), d1), p2 as (n2 as (n2x, n2y, n2z), d2)) =
		if coincide(p1, p2) then
			PlanePlaneIntersection.COINCIDE
		else
			if planeParallel(p1, p2) then
				PlanePlaneIntersection.PARALLEL
			else
				let
					val (dx, dy, dz) = Vector3.cross(n1, n2)

					val p =
						if not(Vector3.realEquals(dx, 0.0)) then
							if not(Vector3.realEquals(n1y, 0.0)) then
								let
									val z = (d2 - d1 * n2y / n1y) / (n2z - n1z * n2y / n1y)
									val y = (d1 - z * n1z) / n1y
								in
									(0.0, y, z)
								end
							else
								if not(Vector3.realEquals(n2y, 0.0)) then
									let
										val z =
											if Vector3.realEquals(n1z, 0.0) then
												if Vector3.realEquals(d1, 0.0) then
													0.0
												else
													raise InvalidPlane
											else
												d1 / n1z
										val y = (d2 - z * n2z) / n2y
									in
										(0.0, y, z)
									end
								else
									if Vector3.realEquals(n1z, 0.0) then
										if Vector3.realEquals(d1, 0.0) then
											if Vector3.realEquals(n2z, 0.0) then
												if Vector3.realEquals(d2, 0.0) then
													(0.0, 0.0, 0.0)
												else
													raise InvalidPlane
											else
												(0.0, 0.0, d2 / n2z)
										else
											raise InvalidPlane
									else
										if Vector3.realEquals(n2z, 0.0) then
											if Vector3.realEquals(d2, 0.0) then
												(0.0, 0.0, d1 / n1z)
											else
												raise InvalidPlane
										else
											let
												val z1 = d1 / n1z
												val z2 = d2 / n2z
											in
												if Vector3.realEquals(z1, z2) then
													(0.0, 0.0, z1)
												else
													raise InvalidPlane
											end
						else
							if not (Vector3.realEquals(dy, 0.0)) then
								if not (Vector3.realEquals(n1x, 0.0)) then
									let
										val z = (d2 - d1 * n2x / n1x) / (n2z - n1z * n2x / n1x)
										val x = (d1 - z * n1z) / n1x
									in
										(x, 0.0, z)
									end
								else
									if not (Vector3.realEquals(n2x, 0.0)) then
										let
											val z =
												if Vector3.realEquals(n1z, 0.0) then
													if Vector3.realEquals(d1, 0.0) then
														0.0
													else
														raise InvalidPlane
												else
													d1 / n1z
											val x = (d2 - z * n2z) / n2x
										in
											(x, 0.0, z)
										end
									else
										if Vector3.realEquals(n1z, 0.0) then
											if Vector3.realEquals(d1, 0.0) then
												if Vector3.realEquals(n2z, 0.0) then
													if Vector3.realEquals(d2, 0.0) then
														(0.0, 0.0, 0.0)
													else
														raise InvalidPlane
												else
													(0.0, 0.0, d2 / n2z)
											else
												raise InvalidPlane
										else
											if Vector3.realEquals(n2z, 0.0) then
												if Vector3.realEquals(d2, 0.0) then
													(0.0, 0.0, d1 / n1z)
												else
													raise InvalidPlane
											else
												let
													val z1 = d1 / n1z
													val z2 = d2 / n2z
												in
													if Vector3.realEquals(z1, z2) then
														(0.0, 0.0, z1)
													else
														raise InvalidPlane
												end
							else
								if not (Vector3.realEquals(dz, 0.0)) then
									if not(Vector3.realEquals(n1y, 0.0)) then
										let
											val x = (d2 - d1 * n2y / n1y) / (n2x - n1x * n2y / n1y)
											val y = (d1 - x * n1x) / n1y
										in
											(x, y, 0.0)
										end
									else
										if not(Vector3.realEquals(n2y, 0.0)) then
											let
												val x =
													if Vector3.realEquals(n1x, 0.0) then
														if Vector3.realEquals(d1, 0.0) then
															0.0
														else
															raise InvalidPlane
													else
														d1 / n1x
												val y = (d2 - x * n2x) / n2y
											in
												(x, y, 0.0)
											end
										else
											if Vector3.realEquals(n1x, 0.0) then
												if Vector3.realEquals(d1, 0.0) then
													if Vector3.realEquals(n2x, 0.0) then
														if Vector3.realEquals(d2, 0.0) then
															(0.0, 0.0, 0.0)
														else
															raise InvalidPlane
													else
														(d2 / n2x, 0.0, 0.0)
												else
													raise InvalidPlane
											else
												if Vector3.realEquals(n2x, 0.0) then
													if Vector3.realEquals(d2, 0.0) then
														(d1 / n1x, 0.0, 0.0)
													else
														raise InvalidPlane
												else
													let
														val x1 = d1 / n1x
														val x2 = d2 / n2x
													in
														if Vector3.realEquals(x1, x2) then
															(x1, 0.0, 0.0)
														else
															raise InvalidPlane
													end
								else
									raise InvalidPlane
				in
					PlanePlaneIntersection.LINE(Line.fromPosAndDir(p, (dx, dy, dz)))
				end

	fun vectorAngle((n, _), v) = abs(Math.pi / 2.0 - Vector3.angleBetween(n, v))
	fun lineAngle(p, (_, d)) = vectorAngle(p, d)
	fun planeAngle((n1, _), (n2, _)) = Vector3.acuteAngle(n1, n2)

	fun pointDistance(p as (n, _), v) = Vector3.length(Vector3.projection(Vector3.subtract(v, anyPoint(p)), Vector3.normalised(n)))

	fun lineDistance(pl, l as (r, _)) =
		if lineParallel(pl, l) then
			pointDistance(pl, r)
		else
			0.0

	fun planeDistance(p1, p2) =
		if planeParallel(p1, p2) then 
			pointDistance(p1, anyPoint(p2))
		else
			0.0

	fun parallelThroughPoint((n, _), v) = (n, Vector3.dot(n, v))

	fun parallelThroughLine(pl, l as (p, _)) =
		if lineParallel(pl, l) then
			parallelThroughPoint(pl, p)
		else
			raise InvalidPlane
end