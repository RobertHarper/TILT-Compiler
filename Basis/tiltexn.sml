exception Div
exception Overflow
structure TiltExn =
struct
    exception SysErr of string * int option
    exception LibFail of string
end
