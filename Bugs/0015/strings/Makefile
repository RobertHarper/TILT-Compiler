all:	A.od B.od

%.o: %.c
	as -o $@ $<

%.od: %.o
	od -b $< >$@

clean:
	rm *.o *.od
