functor HppaShuffle(I:HPPAINSTR) = struct
  structure I = I
  structure Shuffle = Shuffle(I)
  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  fun error msg = MLRiscErrorMsg.impossible ("HppaShuffle." ^ msg)
  val mem = I.Region.memory

  fun move{src=I.Direct rs, dst=I.Direct rt} = [I.ARITH{a=I.OR, r1=rs, r2=0, t=rt}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} =
       [I.LOADI{li=I.LDW, r=base, i=I.IMMED disp, t=rt, mem=mem}]
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
       [I.STORE{st=I.STW, b=base, d=I.IMMED disp, r=rs, mem=mem}]
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = [I.FUNARY{fu=I.FCPY, f=fs, t=fd}]
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} = let
        val tmp = I.C.newCell I.C.GP ()
      in
	[I.LDO{i=I.IMMED disp, b=base, t=tmp},
	 I.FLOADX{flx=I.FLDDX, b=tmp, x=0, t=ft, mem=mem}]
      end
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = let
	val tmp = I.C.newCell I.C.GP ()
      in
	[I.LDO{i=I.IMMED disp, b=base, t=tmp},
	 I.FSTOREX{fstx=I.FSTDX, b=tmp, x=0, r=fs, mem=mem}]
      end
    | fmove _ = error "move"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end


