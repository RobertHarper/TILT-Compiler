targ=\
	btimes

tilt=../Bin/tilt-nj

all: $(targ)

clean:
	rm -f $(targ)
	$(tilt) -vvpp project

%: %.sml
	$(tilt) -vv -o $@ -c $@ project
