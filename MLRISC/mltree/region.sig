signature REGION = sig
  type region
  val stack : region
  val memory : region
  val toString : region -> string
end
