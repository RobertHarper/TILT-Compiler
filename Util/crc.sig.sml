signature CRC =
  sig
    eqtype crc
    val blastOutCrc : Blaster.outstream -> crc -> unit
    val blastInCrc : Blaster.instream -> crc
    val pp_crc : crc -> Formatter.format

    val crc_of_string : string -> crc
    val crc_of_file : string -> crc

    val toString : crc -> string
    val fromString : string -> crc option

  end
