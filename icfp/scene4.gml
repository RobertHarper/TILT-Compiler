
% Checkerboarded cube

{ /x x x } /dup

0.0 0.0 1.0 point /blue1
1.0 0.0 0.0 point /red1

1.0 1.0 0.0 point /red2
0.0 1.0 1.0 point /blue2

1.0 0.0 1.0 point /red3
0.0 1.0 1.0 point /blue3

0.0 1.0 0.0 point /red4
0.0 0.0 1.0 point /blue4

0.0 1.0 1.0 point /red5
0.0 0.0 1.0 point /blue5

1.0 1.0 1.0 point /red6
0.0 1.0 0.0 point /blue6

0.5  0.5  0.5  point   % ambient light
[ ]   % lights (none!)

{ 
/red /blue
  [
  [ red blue red  ]
  [ blue red blue ]
  [ red blue red  ]
  ] 
} /check

[
 red1 blue1 check apply
 red2 blue2 check apply
 red3 blue3 check apply
 red4 blue4 check apply
 red5 blue5 check apply
 red6 blue6 check apply
] /texture

{ /v /u /face
  {
    3.0 mulf floor /i
    i 3 eqi { 2 } { i } if
  } /toIntCoord
  texture face get
    u toIntCoord apply get
    v toIntCoord apply get
  1.0
  0.0
  1.0
} cube

2.5 uscale
-1.0 -1.0 -1.0 translate

/box

box
45.0 rotatex
45.0 rotatey
45.0 rotatez
0.0 0.0 2.0 translate

% transform here to look around


% final params to render:

1     % depth
90.0  % fov
200   % width
150   % height
"scene4.ppm"   % file output

render
