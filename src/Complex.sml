signature COMPLEX = sig
	type complex
	val add : complex * complex -> complex
	val subtract : complex * complex -> complex
	val mult : complex * complex -> complex
	val divide : complex * complex -> complex
	val conjugate : complex -> complex
	val realpow : complex * real -> complex
	val rootsUnity : int -> complex list
	val intRoot : complex * int -> complex list

	val fromReal : real -> complex
	val fromInt : int -> complex
	val modulus : complex -> real

	val isReal : complex -> bool
	val getReal : complex -> real
	val getImaginary : complex -> real
	val equals : complex * complex -> bool

	val toString : complex -> string

	val ZERO : complex
	val ONE : complex
	val i : complex
end

structure Complex : COMPLEX = struct
	type complex = real * real (*real, imaginary*)

	fun add((r1, i1), (r2, i2)) = (r1 + r2, i1 + i2) : complex
	fun subtract((r1, i1), (r2, i2)) = (r1 - r2, i1 - i2) : complex
	fun mult((r1, i1), (r2, i2)) = (r1 * r2 - i1 * i2, r1 * i2 + r2 * i1) : complex
	fun divide((r1, i1), (r2, i2)) = ((r1 * r2 + i1 * i2) / (r2 * r2 + i2 * i2), (i1 * r2 - r1 * i2) / (r2 * r2 + i2 * i2)) : complex
	fun conjugate(r, i) = (r, ~i) : complex
	fun modulus (r, i) = Math.sqrt(r * r + i * i)
	fun realpow((r, i), p) =
		let
			val theta = Math.atan2(i, r)
			val r = modulus (r, i)
			val newr = Math.pow(r, p)
		in
				(newr * Math.cos(p * theta), newr * Math.sin(p * theta)) : complex
		end

	fun rootsUnity n =
		let
			fun helper 0 = [(1.0, 0.0)]
			|	helper k =
				let
					val theta = 2.0 * Math.pi * (Real.fromInt k) / (Real.fromInt n)
				in
					(Math.cos(theta), Math.sin(theta)) :: (helper (k - 1))
				end
		in
			helper (n - 1) : complex list
		end

	fun intRoot(c as (r, i), n) =
		let
			val principal = realpow(c, 1.0 / (Real.fromInt n))
		in
			map (fn x => mult(principal, x)) (rootsUnity n) : complex list
		end

	fun fromReal r = (r, 0.0) : complex
	fun fromInt i = (Real.fromInt i, 0.0) : complex

	fun isReal (_, i) = Vector3.realEquals(i, 0.0)
	fun getReal (r : real, _) = r
	fun getImaginary (_, i : real) = i
	fun equals((r1, i1) : complex, (r2, i2) : complex)= Vector3.realEquals(r1, r2) andalso Vector3.realEquals(i1, i2)

	fun toString(r, i) = Real.toString(r) ^ " + i" ^ Real.toString(i)

	val ZERO = (0.0, 0.0)
	val ONE = (1.0, 0.0)
	val i = (0.0, 1.0)
end