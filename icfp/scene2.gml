
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

% white elippsoid
white solid-color apply sphere
	1.5 2.0 1.0 scale
	2.0 -2.0 4.0 translate 

% little purple sphere
{ /v /u /face			  % bind arguments
  0.8 0.2 v point		  % surface color
  1.0 0.2 1.0			  % kd ks n
} sphere 
	-1.0 1.0 3.0 translate 

% wall facing us (z=0), push back
red dim apply solid-color apply "red back wall" plane 
        90.0 rotatex
        0.0 0.0 5.0 translate 

% wall to our left, (x=0), push left
blue dim apply solid-color apply "blue left wall" plane
        90.0 rotatez
	-3.0 0.0 0.0 translate

% green floor
green dim apply solid-color apply plane
	0.0 -2.0 0.0 translate	

% yellow floor
yellow dim apply solid-color apply plane
	0.0 2.0 0.0 translate 

% make into one object
union
union
union
union
union

% transform here to look around


% final params to render:

1     % depth
90.0  % fov
100   % width
100   % height
"scene2.ppm"   % file output

render
