
% Checkerboarded cube

{ /x x x } /dup

0.0 0.0 1.0 point /blue
1.0 0.0 0.0 point /red

0.2  0.2  0.2  point   % ambient light
[ ]   % lights (none!)

[
  [ red blue red  ]
  [ blue red blue ]
  [ red blue red  ]
] /texture

{ /v /u /face
  {
    3.0 mulf floor /i
    i 3 eqi { 2 } { i } if
  } /toIntCoord
  texture u toIntCoord apply get
    v toIntCoord apply get
  1.0
  0.0
  1.0
} cube

27.0 rotatex
34.0 rotatey


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
