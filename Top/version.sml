structure Version :>
sig
    val majorVersion : int
    val minorVersion : int
    val version : string
end =
struct
    val majorVersion = 1
    val minorVersion = 1
    val version = (Int.toString majorVersion ^ "." ^ Int.toString minorVersion) ^ "cvs"
end
