
% Our first scene! - Tom 7, Team Fixed-point

% yellow ceiling, green floor, red wall ahead, blue wall to left

% Test with: Top.runFile <name of this file>

% /q is used to delete arguments from the stack
% without using them.

{ /x x x } /dup

1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue
1.0 1.0 0.0 point /yellow

% this function takes a color and returns a surfaceparm fun

% surfaceparms: kd=1.0 ks=0.0 n=1.0

{ /color { /q /q /q color 1.0 0.0 1.0 } } /solid-color

% defs are done; the scene:

0.5  0.5  0.5  point   % ambient light
[ ]   % lights (none!)

% wall facing us (z=0), push back
red solid-color apply plane 
        90.0 rotatex
        0.0 0.0 5.0 translate 

% wall to our left, (x=0), push left
blue solid-color apply plane
        90.0 rotatez
	-3.0 0.0 0.0 translate

% green floor
green solid-color apply plane
	0.0 -2.0 0.0 translate	

% yellow floor
yellow solid-color apply plane
	0.0 2.0 0.0 translate 

% make into one object
union
union
union

% transform here to look around


% final params to render:

1     % depth
90.0  % fov
100   % width
100   % height
"scene1.ppm"   % file output

render
