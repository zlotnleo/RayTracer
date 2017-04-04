signature SINGLE_SHAPE = sig
	type shape
	val getNormal : shape * Vector3.vector3 -> Vector3.vector3 option
	val getHit : shape * Line.line -> Vector3.vector3 list
end

structure ShapeSphere : SINGLE_SHAPE = struct
	type shape = Vector3.vector3 * real (*centre point and radius*)

	fun getNormal((c, r), p) =
		if Vector3.realEquals(Vector3.distance(c, p), r) then
			SOME(Vector3.normalised(Vector3.subtract(p, c)))
		else
			NONE

	fun getHit((c, r), (p, d)) =
		let
			val cp = Vector3.subtract(p, c)
			val A = Vector3.dot(d, d)
			val B = 2.0 * Vector3.dot(d, cp)
			val C = Vector3.dot(cp, cp) - r * r
		in
			map
			(fn x => Vector3.add(p, Vector3.mult(d, x)))
			(Equations.realQuadratic (A, B, C))
				handle WholeDomain => []
		end
end

structure ShapePlane : SINGLE_SHAPE = struct
	type shape = Plane.plane (*infinite plane*)
	
	fun getNormal(pl, v) =
		if Plane.pointOn(pl, v) then
			SOME(Plane.getNormal(pl))
		else
			NONE

	fun getHit(pl, l as (p, d)) =
		case Plane.lineIntersection(pl, l) of
				LinePlaneIntersection.POINT(v) => [v]
			|	_ => []
end

structure ShapeInfCylinder : SINGLE_SHAPE = struct
	type shape = Line.line * real (*axis, radius*)

	fun getNormal((ln as (p, d), r), v) =
		if Vector3.realEquals(Line.pointDistance(ln, v), r) then
			let
				val pv = Vector3.subtract(v, p)
			in
				SOME(Vector3.normalised(Vector3.subtract(pv, Vector3.projection(pv, d))))
			end
		else
			NONE

	fun getHit((l1 as (p1, d1), r), l2 as (p2, d2)) =
		case Line.intersection(l1, l2) of
				LineLineIntersection.PARALLEL => []
			|	LineLineIntersection.COINCIDE => []
			|	_=>
					let
						val d_ = Vector3.subtract(d2, Vector3.projection(d2, d1))
						val p1p2 = Vector3.subtract(p2, p1)
						val p_ = Vector3.add(p1, Vector3.projection(p1p2, d1))
						val p_p2 = Vector3.subtract(p2, p_)
						val A = Vector3.dot(d_, d_)
						val B = 2.0 * Vector3.dot(p_p2, d_)
						val C = Vector3.dot(p_p2, p_p2) - r * r
					in
						map
						(fn x => Vector3.add(p2, Vector3.mult(d2, x)))
						(Equations.realQuadratic (A, B, C))
							handle WholeDomain => []
					end
end

structure ShapeInfCone : SINGLE_SHAPE = struct
	type shape = Line.line * real (*line : tip and axis, angle*)

	fun getNormal((ln as (t, a), theta), v) =
		if not (Vector3.equals(t, v)) andalso Vector3.realEquals(Vector3.distance(t, v) * Math.sin(theta), Line.pointDistance(ln, v)) then
			SOME(Vector3.normalised(Vector3.reflect(Vector3.subtract(v, t), a)))
		else
			NONE

	fun getHit(((t, a), theta), (p, d)) =
		let
			val a2cosT2 = Vector3.dot(a, a) * Math.cos(theta) * Math.cos(theta)
			val tp = Vector3.subtract(p, t)
			val adotd = Vector3.dot(a, d)
			val adottp = Vector3.dot(a, tp)

			val A = adotd * adotd - Vector3.dot(d, d) * a2cosT2
			val B = 2.0 * (adotd * adottp - Vector3.dot(d, tp) * a2cosT2)
			val C = adottp * adottp - Vector3.dot(tp, tp) * a2cosT2
		in
			map
			(fn x => Vector3.add(p, Vector3.mult(d, x)))
			(Equations.realQuadratic (A, B, C))
				handle WholeDomain => []
		end

end

structure ShapeTorus : SINGLE_SHAPE = struct
	type shape = Vector3.vector3 * Vector3.vector3 * real * real (*centre, normal, radius from centre, radius around a ring*)

	fun getNormal ((c, n, R, r), v) =
		let
			val cv = Vector3.subtract(v, c)
			val v_ = Vector3.add(c, Vector3.mult(Vector3.normalised(Vector3.subtract(cv, Vector3.projection(cv, n))), R))
		in
			if Vector3.realEquals(Vector3.distance(v, v_), r) then
				SOME(Vector3.subtract(v, v_))
			else
				NONE
		end

	fun getHit ((c, n_, R, r), l as (p, d)) =
		let
			val n = Vector3.normalised(n_)
			val cp = Vector3.subtract(p, c)
			val dd = Vector3.dot(d, d)
			val cpd = Vector3.dot(cp, d)
			val dn = Vector3.dot(d, n)
			val cpn = Vector3.dot(cp, n)
			val cpcp = Vector3.dot(cp, cp)
			val RR4 = 4.0 * R * R
			val cpcpRRrr = cpcp + R * R - r * r

			val A = dd * dd
			val B = 4.0 * dd * cpd
			val C = 4.0 * cpd * cpd + 2.0 * dd * cpcpRRrr + RR4 * (dn * dn - dd)
			val D = 4.0 * cpd * cpcpRRrr + 2.0 * RR4 * (dn * cpn - cpd)
			val E = cpcpRRrr * cpcpRRrr + RR4 * (cpn * cpn - cpcp)
		in
			map
			(fn x => Vector3.add(p, Vector3.mult(d, x)))
			(Equations.realQuartic (A, B, C, D, E))
				handle WholeDomain => []
		end
end

signature SHAPE = sig
	type shape
	val Sphere : ShapeSphere.shape -> shape (*centre, radius*)
	val Plane : ShapePlane.shape -> shape (*plane*)
	val InfCylinder : ShapeInfCylinder.shape -> shape (*axis, radius*)
	val InfCone : ShapeInfCone.shape -> shape (*line: tip and axis, angle*)
	val Torus : ShapeTorus.shape -> shape (*centre, normal, radius from centre, radius around a ring*)
	val Constrained : shape * (Vector3.vector3 -> bool) -> shape
	val Composite : shape list -> shape

	val Circle : Vector3.vector3 * Vector3.vector3 * real -> shape (*centre, normal, radius*)
	val Triangle : Vector3.vector3 * Vector3.vector3 * Vector3.vector3 -> shape (*three vertices*)
	val Cone : Line.line * real * real * real -> shape (*line: tip and axis, angle, min height, max height*)
	val Cylinder : Vector3.vector3 * Vector3.vector3 * real -> shape (*endpoint, endpoint, radius*)

	val getNormal : shape * Vector3.vector3 -> Vector3.vector3 option 
	val getHit : shape * Line.line -> Vector3.vector3 list 
end

structure Shape : SHAPE = struct
	datatype shape =
			Sphere of ShapeSphere.shape
		|	Plane of ShapePlane.shape
		|	InfCylinder of ShapeInfCylinder.shape
		|	InfCone of ShapeInfCone.shape
		|	Torus of ShapeTorus.shape
		|	Constrained of shape * (Vector3.vector3 -> bool)
		|	Composite of shape list

	fun getNormal (Sphere(s), p) = ShapeSphere.getNormal(s, p)
	|	getNormal (Plane(s), p) = ShapePlane.getNormal(s, p)
	|	getNormal (InfCylinder(s), p) = ShapeInfCylinder.getNormal(s, p)
	|	getNormal (InfCone(s), p) = ShapeInfCone.getNormal(s, p)
	|	getNormal (Torus(s), p) = ShapeTorus.getNormal(s, p)
	|	getNormal (Constrained(s, func), p) = if func p then getNormal(s, p) else NONE
	|	getNormal (Composite(ss), p) = (findAvg o (map Option.valOf) o (List.filter Option.isSome) o (map getNormal) o (map (fn s => (s, p)))) ss
	and findAvg [] = NONE
	|	findAvg vs = SOME((Vector3.normalised o (List.foldl Vector3.add (0.0, 0.0, 0.0))) vs)

	fun getHit (Sphere(s), l) = ShapeSphere.getHit(s, l)
	|	getHit (Plane(s), l) = ShapePlane.getHit(s, l)
	|	getHit (InfCylinder(s), l) = ShapeInfCylinder.getHit(s, l)
	|	getHit (InfCone(s), l) = ShapeInfCone.getHit(s, l)
	|	getHit (Torus(s), l) = ShapeTorus.getHit(s, l)
	|	getHit (Constrained(s, func), l) = List.filter func (getHit (s, l))
	|	getHit (Composite(ss), l) = (List.concat o (map getHit) o (map (fn s => (s, l)))) ss

	fun Circle (c, n, r) = Constrained(
		Plane(Plane.fromNormalAndPoint(n, c)),
		fn p => Vector3.distance(p, c) <= r
	)

	fun Triangle (a, b, c) =
	let
		fun sameSide(l1, l2, v1, v2) =
			let
				val dl = Vector3.subtract(l2, l1)
				val d1 = Vector3.subtract(v1, l1)
				val d2 = Vector3.subtract(v2, l1)
				val cp1 = Vector3.cross(d1, dl)
				val cp2 = Vector3.cross(d2, dl)
			in
				Vector3.dot(cp1, cp2) >= 0.0
			end

		val pl = Plane.fromThreePoints(a, b, c)
	in
		Constrained(
			Plane(pl),
			fn v =>
				if Plane.pointOn(pl, v) then
					sameSide(a, b, c, v) andalso sameSide(a, c, b, v) andalso sameSide(b, c, a, v)
				else
					false
		)
	end

	fun Cone (l as (t, a), theta, min, max) = Constrained(
		InfCone(l, theta),
		fn v =>
			let
				val x = Vector3.projection(Vector3.subtract(v, t), a)
				val xlen = Vector3.length(x) * (if Vector3.dot(x, a) >= 0.0 then 1.0 else ~1.0)
			in
				xlen >= min andalso xlen <= max
			end
	)

	fun Cylinder (v1, v2, r) =
	let
		val l as (_, d) = Line.fromTwoPoints(v1, v2)
	in
		Composite(
			[
				Circle(v1, d, r),
				Circle(v2, d, r),
				Constrained(
					InfCylinder(l, r),
					fn v => Vector3.dot(d, Vector3.subtract(v, v1)) * Vector3.dot(d, Vector3.subtract(v, v2)) <= 0.0
				)
			]
		)
	end
end