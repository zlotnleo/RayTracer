signature IMAGE = sig
	type colour
	type xy
	type image
	val createImage : xy -> colour -> image
	val size : image -> xy
	val drawPixel : image -> colour -> xy -> unit
	val drawLine : image -> colour -> xy -> xy -> unit
	val drawRect : image -> colour -> xy -> xy -> unit
	val drawAll : image -> (xy -> colour) -> unit
	val toTextPPM : image -> string -> unit
	val toBinPPM : image -> string -> unit
end

structure Image : IMAGE = struct
	type colour = int*int*int
	type xy = int*int
	type image = xy * colour array array

	fun createImage (dim as (w, h)) clr = (dim, Array.tabulate(h, fn i => Array.tabulate(w, fn j => clr)))

	fun size (dim, _) = dim

	fun drawPixel (dim, data) clr (pos as (x, y)) =
		Array.update(Array.sub(data, y), x, clr)
			handle Subscript => ()

	fun drawLine img clr pos0 pos1 =
		let
			val (x0, y0) = pos0
			val (x1, y1) = pos1
			val dx = Int.abs(x1 - x0)
			val dy = Int.abs(y1 - y0)
			val sx = if x0 < x1 then 1 else ~1;
			val sy = if y0 < y1 then 1 else ~1;

			fun helper x y err = (
				drawPixel img clr (x, y);
				if (x = x1) andalso (y = y1) then ()
				else
					if 2 * err > ~dy then
						if 2 * err < dx then
							helper (x + sx) (y + sy) (err - dy + dx)
						else
							helper (x + sx) y (err - dy)
					else
						if 2 * err < dx then
							helper x (y + sy) (err + dx)
						else
							()
				)
		in
			helper x0 y0 (dx - dy)
		end

	fun drawRect _ _ _ (_, 0) = ()
	|	drawRect img clr (x, y) (w, h) = (
			drawLine img clr (x, y) (x + w, y);
			drawRect img clr (x, y + 1) (w, h - 1)
		)

	fun toTextPPM ((w,h), data) filename =
		let
			val oc = TextIO.openOut filename

			fun componentToStr i = StringCvt.padLeft #" " 4 (Int.toString i)

			fun colourToStr (R, G, B) = (componentToStr R) ^ (componentToStr G) ^ (componentToStr B)

			fun printRow row = 
				let
					fun helper i =
						if i = w then
							()
						else (
							TextIO.output(oc, colourToStr (Array.sub(row, i)));
							helper (i + 1)
						)
				in
					helper 0
				end

			fun printData () = 
				let
					fun helper i = 
						if i = h then
							()
						else (
							printRow (Array.sub (data, i));
							TextIO.output(oc, "\n");
							helper (i + 1)
						)
				in
					helper 0
				end
		in
			TextIO.output(oc,"P3\n" ^ Int.toString w ^ " " ^ Int.toString h ^ "\n255\n");
			printData ();
			TextIO.closeOut oc
		end

	fun toBinPPM ((w, h), data) filename =
		let
			val oc = BinIO.openOut filename

			fun colourToBytes (R, G, B) = Word8Vector.fromList (map (Word8.fromInt) [R, G, B])

			fun printRow row = 
				let
					fun helper i =
						if i = w then
							()
						else (
							BinIO.output(oc, colourToBytes (Array.sub(row, i)));
							helper (i + 1)
						)
				in
					helper 0
				end

			fun printData () = 
				let
					fun helper i = 
						if i = h then
							()
						else (
							printRow (Array.sub (data, i));
							helper (i + 1)
						)
				in
					helper 0
				end
		in
			BinIO.output(oc,Byte.stringToBytes("P6 " ^ Int.toString w ^ " " ^ Int.toString h ^ " 255\n"));
			printData ();
			BinIO.closeOut oc
		end

	fun drawAll ((w, h), data) f = Array.appi (fn (y, row) => (Array.modifyi (fn (x, _) => f(x, y)) row)) data
end






