% cylinder.gml
%
% Two matte cylinders over a matte plane.
%

				% a cylinder
{ /v /u /face			  % bind arguments
  0.8 0.8 0.2 point		  % surface color
  1.0 0.0 1.0			  % kd ks n
} cylinder /s

				% a matte white plane
{ /v /u /face
  1.0 1.0 1.0 point
  1.0 0.0 1.0
} plane /p

				% scene consisting of two cylinders
s -1.2 -0.5 3.0 translate 	  % cylinder at (-1, 0, 3)
s  1.2 0.5 3.0 translate	  % cylinder at (1, 1, 3)
p  0.0 -3.0 0.0 translate	  % plane at Y = -3
union union /scene		  % compose

				% directional light
1.0 -1.0 0.0 point		  % direction
1.0  1.0 1.0 point light /l	  % directional light

				% render
0.4 0.4 0.4 point		  % ambient light
[ l ]				  % lights
scene				  % scene to render
3				  % tracing depth
90.0				  % field of view
320 240				  % image wid and height
"cylinders.ppm"			  % output file
render

