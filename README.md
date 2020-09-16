# RayTracer
RayTracer in Standard ML

Ideally, there is no need to change anything in the "src" forled to run it, but the actual scene has to be described in .sml file, which should be the last file included in the .mlb basis file. An example of the scene description is given in the "main.sml".

To compile I recommend to use MLton (http://mlton.org) and passing the .mlb file to it. Then run the generated executable.

If you want to use other sompiler (like PolyML, SML/NJ, etc.) you need to manually convert .mlb to something the compiler understands (like "use" expressions and/or providing "main" function)

Example output:
![](https://raw.githubusercontent.com/zlotnleo/RayTracer/master/example.png)
