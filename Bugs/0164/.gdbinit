file /usr5/swasey/t/Bin/sparc/tilt
set environment TILT_LIBDIR=/usr5/swasey/t2
set height 0

set $closure = 0U
set $global = 0U
set $rec1 = 0U
set $rec16 = 0U
set $ref = 0U

define status
	printf "closure: "
	set $statusa = (unsigned)&anonfun__11101
	if $closure != $statusa
		printf "CHANGED: "
		set $closure = $statusa
	end
	x/1w $statusa
	printf "global: "
	set $statusa = (unsigned)&ml__PLUSULinkIl__INT__r__INT
	if $global != $statusa
		printf "CHANGED: "
		set $global = $statusa
	end
	x/1w $statusa
	printf "rec1: "
	set $statusa = (unsigned)*(ptr_t)$statusa
	if $rec1 != $statusa
		printf "CHANGED: "
		set $rec1 = $statusa
	end
	x/1w $statusa
	printf "rec16: "
	set $statusa = (unsigned)*(ptr_t)$statusa
	if $rec16 != $statusa
		printf "CHANGED: "
		set $rec16 = $statusa
	end
	x/1w $statusa
	printf "ref: "
	set $statusa = (unsigned)*(ptr_t)$statusa
	if $ref != $statusa
		printf "CHANGED: "
		set $ref = $statusa
	end
	x/1w $statusa
end

break GC_Gen

disable 1
break code__7232
commands
	silent
	printf "\nInitialized\n"
	status
	enable 1
	continue
end

# @showatgc'='11 @showglobals
run @paranoid'='1 -b
