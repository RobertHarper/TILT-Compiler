
% Second scene
% Like first scene with dimmed walls
% Add a purple sphere and a white ellipsoid partly into the floor

% /q is used to delete arguments from the stack
% without using them.

{ /x x x } /dup

1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 0.0 point /yellow
1.0 0.0 1.0 point /purple
1.0 1.0 1.0 point /white
1.0 0.0 1.0 point /purple

% this function takes a color and returns a surfaceparm fun

% surfaceparms: kd=1.0 ks=0.0 n=1.0

{ /color { /q /q /q color 1.0 0.0 1.0 } } /solid-color
{ /color color getx 0.7 mulf 
	 color gety 0.7 mulf 
	 color getz 0.7 mulf 
	 point } /dim

% defs are done; the scene:

0.5  0.5  0.5  point   % ambient light
[ ]   % lights (none!)


% little purple sphere
{ /v /u /face			  % bind arguments
  face 0 eqi
  { u v 0.5 point }       % red-blue
  { 0.5 u v point }	  % blue-green 
  if
  1.0 0.2 1.0			  % kd ks n
} cone

	1.0 2.0 1.0 scale
	-95.0 rotatex
	0.0 0.0 2.0 translate
	65.0 rotatey
	0.0 0.0 2.0 translate

% transform here to look around


% final params to render:

1     % depth
90.0  % fov
100   % width
100   % height
"cone.ppm"   % file output

render
