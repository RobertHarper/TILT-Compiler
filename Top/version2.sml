structure Version :>
sig
    val majorVersion : int
    val minorVersion : int
    val version : string
end =
struct
    val majorVersion = 2
    val minorVersion = 0
    val version = (Int.toString majorVersion ^ "." ^ Int.toString minorVersion)
end
