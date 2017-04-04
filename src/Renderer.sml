signature RENDERER = sig
	type camera
	val render : camera * Scene.scene * int -> Image.image -> unit
end

structure Renderer : RENDERER = struct
	type camera = Vector3.vector3 * Vector3.vector3 * Vector3.vector3 * real (*position, direction, up, FOV angle*)

	fun render ((campos, camdir, camup, fov), s, maxRef) (img as ((w, h), _)) =
		let
			val aspectRatio = Real.fromInt(h) / Real.fromInt(w)

			fun helper (x, y) =
				let
					val half_h = Real.fromInt(h) / 2.0
					val half_w = Real.fromInt(w) / 2.0
					val dir =
						Vector3.add(
							camdir,
							Vector3.add(
								Vector3.mult(
									Vector3.normalised(Vector3.cross(camdir, camup)),
									~(half_w - Real.fromInt(x)) / half_w * Math.tan(fov / 2.0)
								),
								Vector3.mult(
									Vector3.normalised(Vector3.cross(Vector3.cross(camdir, camup), camdir)),
									(half_h - Real.fromInt(y)) / half_h * Math.tan(fov / 2.0 * aspectRatio)
								)
							)
						)
				in
					Scene.traceRay(s,
						Line.fromPosAndDir(
							campos,
							dir
						),
						maxRef
					)
				end

			fun toRGB x = Real.round(255.0 * (if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x))

			fun vecToColour ((r, g, b) : Vector3.vector3) = (toRGB r, toRGB g, toRGB b) : Image.colour
		in
			Image.drawAll img (vecToColour o helper)
		end
end