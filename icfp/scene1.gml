
% Our first scene! - Tom 7, Team Fixed-point

% val parse = ParseString.parse (Stdin.stdintostring ())
% (paste this file)
% val res = Eval.eval (valOf parse) 
%           handle Eval.Eval s => (print s; raise Eval.Eval s)
% <- probably want to patch this to call render instead of
%    raising an exception!

% /q is used to delete arguments from the stack
% without using them.

{ /x x x } /dup

1.0 0.0 0.0 point /red
0.0 1.0 0.0 point /green
0.0 0.0 1.0 point /blue

% this function takes a color and returns a surfaceparm fun

% surfaceparms: kd=1.0 ks=0.0 n=1.0

{ /color { /q /q /q color 1.0 0.0 1.0 } } /solid-color

% defs are done; the scene:

0.2   % ambient light
[ ]   % lights (none!)

% wall facing us (z=0), push back
red solid-color apply plane 
        90.0 rotatex
        0.0 0.0 3.0 translate 

% wall to our left, (x=0), push left
blue solid-color apply plane
        90.0 rotatez
	-3.0 0.0 0.0 translate

% ceiling and floor
green solid-color apply plane
        dup apply
	0.0 12.0 0.0 translate 
	union
	0.0 -4.0 0.0 translate	

% make into one object
union
union

% transform here to look around


% final params to render:

3     % depth
90.0  % fov
320   % width
200   % height
"scene1.ppm"   % file output

render