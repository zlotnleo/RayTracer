signature EQUATIONS = sig
	exception WholeDomain
	val linear : Complex.complex * Complex.complex -> Complex.complex list
	val quadratic : Complex.complex * Complex.complex * Complex.complex -> Complex.complex list
	val cubic : Complex.complex * Complex.complex * Complex.complex * Complex.complex -> Complex.complex list
	val quartic : Complex.complex * Complex.complex * Complex.complex * Complex.complex * Complex.complex -> Complex.complex list

	val reals : Complex.complex list -> real list
	val realLinear : real * real -> real list
	val realQuadratic : real * real * real -> real list
	val realCubic : real * real * real * real -> real list
	val realQuartic : real * real * real * real * real -> real list
end

structure Equations : EQUATIONS = struct
	exception WholeDomain
	exception NoSolution

	fun linear (a, b) =
		if Complex.equals(a, Complex.ZERO) then
			if Complex.equals(b, Complex.ZERO) then
				raise WholeDomain
			else
				[]
		else
			[
				Complex.divide(Complex.mult(b, Complex.fromReal(~1.0)), a)
			]

	fun quadratic (a, b, c) =
		if Complex.equals(a, Complex.ZERO) then
			linear(b, c)
		else	
			map
			(
				fn x =>
					Complex.divide(
						Complex.add(
							Complex.mult(b, Complex.fromReal(~1.0)), x
						),
						Complex.mult(a, Complex.fromReal(2.0)))
			)
			(
				Complex.intRoot(
					Complex.subtract(
						Complex.mult(b, b),
						Complex.mult(Complex.fromReal(4.0), Complex.mult(a, c))
					),
					2
				)
			)

	fun cubic (a, b, c, d) =
		if Complex.equals(a, Complex.ZERO) then
			quadratic(b, c, d)
		else
			let
				val p = (*(3.0 * a * c - b * b) / (3.0 * a * a)*)
					Complex.divide(
						Complex.subtract(
							Complex.mult(Complex.fromReal(3.0), Complex.mult(a, c)),
							Complex.mult(b, b)
						),
						Complex.mult(Complex.fromReal(3.0), Complex.mult(a, a))
					)

				val q = (*(2.0 * b * b * b - 9.0 * a * b * c + 27.0 * a * a * d) / (27.0 * a * a * a)*)
					Complex.divide(
						Complex.add(
							Complex.subtract(
								Complex.mult(Complex.fromReal(2.0), Complex.realpow(b, 3.0)),
								Complex.mult(Complex.fromReal(9.0), Complex.mult(a, Complex.mult(b, c)))
							),
							Complex.mult(Complex.fromReal(27.0), Complex.mult(d, Complex.mult(a, a)))
						),
						Complex.mult(Complex.fromReal(27.0), Complex.realpow(a, 3.0))
					)
						
				val (u3, v3) =
					case
						(quadratic(Complex.ONE, q,
							Complex.divide(
								Complex.realpow(p, 3.0),
								Complex.fromReal(~27.0)
							)
						)) of
							[x, y] => (x, y)
							|_ => raise NoSolution	(*Removes "Declaration is not exhaustive" warning*)
					

				val u = Complex.intRoot(u3, 3)
				val v = Complex.intRoot(v3, 3)
				val uv = List.concat (map (fn l => (map (fn m => (l, m))) v) u)
				val y =
					map
					(fn (l, m) => Complex.add(l, m))
					(
						List.filter
						(
							fn (l, m) =>
								Complex.equals(
									Complex.add(
										Complex.mult(Complex.mult(l, m), Complex.fromReal(3.0)),
										p
									),
									Complex.ZERO
								)
						)
						uv
					)
				val x =
					map
					(
						fn t =>
							Complex.subtract(
								t, 
								Complex.divide(
									b,
									Complex.mult(Complex.fromReal(3.0), a)
								)
								
							)
					)
					y
			in
				x
			end
				(*Remove "pattern not exhaustive" warning when matching [u3, v3]*)

	fun quartic (a, b, c, d, e) =
		if Complex.equals(a, Complex.ZERO) then
			cubic(b, c, d, e)
		else
			let
				val p = (*(8.0 * a * c - 3.0 * b * b) / (8.0 * a * a)*)
					Complex.divide(
						Complex.subtract(
							Complex.mult(Complex.fromReal(8.0), Complex.mult(a, c)),
							Complex.mult(Complex.fromReal(3.0), Complex.mult(b, b))
						),
						Complex.mult(Complex.fromReal(8.0), Complex.mult(a, a))
					)

				val q = (*(b * b * b - 4.0 * a * b * c + 8.0 * a * a * d) / (8.0 * a * a * a)*)
					Complex.divide(
						Complex.add(
							Complex.subtract(
								Complex.realpow(b, 3.0),
								Complex.mult(Complex.fromReal(4.0), Complex.mult(a, Complex.mult(b, c)))
							),
							Complex.mult(Complex.fromReal(8.0), Complex.mult(d, Complex.mult(a, a)))
						),
						Complex.mult(Complex.fromReal(8.0), Complex.realpow(a, 3.0))
					)

				val r = (*(16.0 * a * b * b * c - 3.0 * b * b * b * b - 64.0 * a * a * b * d + 256.0 * a * a * a * e) / (256.0 * a * a * a * a)*)
					Complex.divide(
						Complex.add(
							Complex.subtract(
								Complex.subtract(
									Complex.mult(Complex.fromReal(16.0), Complex.mult(a, Complex.mult(c, Complex.mult(b, b)))),
									Complex.mult(Complex.fromReal(3.0), Complex.realpow(b, 4.0))
								),
								Complex.mult(Complex.fromReal(64.0), Complex.mult(b, Complex.mult(d, Complex.mult(a, a))))
							),
							Complex.mult(Complex.fromReal(256.0), Complex.mult(e, Complex.realpow(a, 3.0)))
						),
						Complex.mult(Complex.fromReal(256.0), Complex.realpow(a, 4.0))
					)

				val k = (hd o cubic)
						(
							Complex.fromReal(2.0),
							Complex.mult(p, Complex.fromReal(~1.0)),
							Complex.mult(r, Complex.fromReal(~2.0)),
							Complex.subtract(
								Complex.mult(r, p),
								Complex.mult(Complex.mult(q, q), Complex.fromReal(0.25))
							)
						)

				val sqrt2kp = Complex.realpow(Complex.subtract(Complex.mult(Complex.fromReal(2.0), k), p), 0.5)
				val y =
					quadratic(
						Complex.fromReal(1.0),
						Complex.mult(sqrt2kp, Complex.fromReal(~1.0)),
						Complex.add(k, Complex.divide(q, Complex.mult(sqrt2kp, Complex.fromReal(2.0))))
					)
					@
					quadratic(
						Complex.fromReal(1.0),
						sqrt2kp,
						Complex.subtract(k, Complex.divide(q, Complex.mult(sqrt2kp, Complex.fromReal(2.0))))
					)
				val x =
					map
					(
						fn t =>
							Complex.subtract(
								t, 
								Complex.divide(
									b,
									Complex.mult(Complex.fromReal(4.0), a)
								)
								
							)
					)
					y
			in
				x
			end		

	local
		fun checkRoots [] = []
		|	checkRoots (r::rs) = List.filter Real.isFinite (r :: (((List.filter (fn x => not (Vector3.realEquals(x, r)))) o checkRoots) rs))
	in
		val reals = checkRoots o (map Complex.getReal) o (List.filter Complex.isReal)
	end

	fun realLinear (a, b) = reals (linear (Complex.fromReal(a), Complex.fromReal(b)))
	fun realQuadratic (a, b, c) = reals (quadratic (Complex.fromReal(a), Complex.fromReal(b), Complex.fromReal(c)))
	fun realCubic (a, b, c, d) = reals (cubic (Complex.fromReal(a), Complex.fromReal(b), Complex.fromReal(c), Complex.fromReal(d)))
	fun realQuartic (a, b, c, d, e) = reals (quartic (Complex.fromReal(a), Complex.fromReal(b), Complex.fromReal(c), Complex.fromReal(d), Complex.fromReal(e)))
end