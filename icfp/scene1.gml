
% Our first scene! - Tom 7, Team Fixed-point

% Get file with:
%   val str = (Stdin.stdintostring ())
%   (paste this file)
% Or with:
%   val str = ParseString.file2string "scene1.gml"
% val parse = ParseString.parse string
% val res = Eval.eval (valOf parse) 
%           handle Base.Eval s => (print s; raise Base.Eval s)
% <- probably want to patch this to call render instead of
%    raising an exception!

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
red solid-color apply "red back wall" plane 
        90.0 rotatex
        0.0 0.0 5.0 translate 

% wall to our left, (x=0), push left
blue solid-color apply "blue left wall" plane
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
50   % width
50   % height
"scene1.ppm"   % file output

render
