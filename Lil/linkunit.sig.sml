signature LILLINKUNIT = 
  sig
    val unit_type_label : string -> Lil.label
    val unit_rtype_label : string -> Lil.label
    val linkunit : {imports:string list, name:string} list -> Lil.module
  end