
% Gradient Cube - Tom 7

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

0.8  0.8  0.8  point   % ambient light
[ ]   % lights (none!)

[
 [ red1 blue1 ]
 [ red2 blue2 ]
 [ red3 blue3 ]
 [ red4 blue4 ]
 [ red5 blue5 ]
 [ red6 blue6 ]
] /texture

{ /s /c c getx s mulf c gety s mulf c getz s mulf point } /scalec
{ /a /b a getx b getx addf 0.5 mulf
        a gety b gety addf 0.5 mulf
        a getz b getz addf 0.5 mulf point } /avgc


{ /v /u /face
  texture face get
    0 get u scalec apply
  texture face get
    1 get u scalec apply avgc apply
  1.0
  0.0
  1.0
} cube

1.2 3.0 0.8 scale

27.0 rotatex
34.0 rotatey
-7.0 rotatez

0.0 0.0 3.0 translate

% transform here to look around


% final params to render:

3     % depth
90.0  % fov
320   % width
200   % height
"scene5.ppm"   % file output

render
