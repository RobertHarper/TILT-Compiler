1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 0.0 point /yellow
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /purple

				% a cylinder
{ /v /u /face			  % bind arguments
  face 0 eqi
  { blue }
  { face 1 eqi
    { purple }
    { green  }
    if
  }
  if	  % surface color
  1.0 0.0 1.0			  % kd ks n
} cylinder /s

				% a matte white plane
s 0.0 -0.5 0.0 translate
  30.0 rotatex
  0.0 0.0 2.0 translate 	  % cylinder at (-1, 0, 3)

/scene		  % compose

				% render
0.4 0.4 0.4 point		  % ambient light
[ ]				  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
100 100				  % image wid and height
"cylinder.ppm"			  % output file
render

