# Twelf
# Copyright (C) 1997, 1998, Frank Pfenning and Carsten Schuermann

# ---------------------------------------------------------------
# Please edit the following lines
# ---------------------------------------------------------------

# What is SML/NJ called?
sml = SML_VERSION=110 sml

# What is TILT called?
tilt = ../../Bin/tilt-nj

cputype=`../../Bin/cputype`

# Twelf root directory
twelfdir = .

# ---------------------------------------------------------------
# Do not edit the following lines
# ---------------------------------------------------------------

version = "1.2"

default : twelf-server

all : twelf-server

twelf-server:
	$(tilt) -vv -cTwelfServer -obin/twelf-server-$(cputype) project-twelf-server

old-twelf-server: ; 
	@echo "*************************************************"
	@echo "Twelf $(version): Server"
	@echo "*************************************************"	 
	$(sml) < twelf-server.sml ;
	sed -e "s#%TWELFDIR#$(twelfdir)#g" \
	    -e "s#%SML#$(sml)#g" bin/.twelf-server \
	> bin/twelf-server ;
	chmod a+x bin/twelf-server ;

old-twelf-sml: ; 
	@echo "*************************************************"
	@echo "Twelf $(version): SML"
	@echo "*************************************************"	 
	$(sml) < twelf-sml.sml ;
	sed -e "s#%TWELFDIR#$(twelfdir)#g" \
	    -e "s#%SML#$(sml)#g" bin/.twelf-sml \
	> bin/twelf-sml ;
	chmod a+x bin/twelf-sml ;

old-twelf-emacs: ; 
	@echo "*************************************************"
	@echo "Twelf $(version): Emacs"
	@echo "*************************************************"	 
	sed -e "s#%TWELFDIR#$(twelfdir)#g" emacs/.twelf-init.el \
	> emacs/twelf-init.el ;
	@echo "*************************************************"
	@echo "Add"
	@echo ""
	@echo "(load \"$(twelfdir)/emacs/twelf-init.el\")"
	@echo ""
	@echo "to your .emacs file"
	@echo "*************************************************"	


clean: ;
	rm -rf $(twelfdir)/src/*/CM ;
	$(tilt) -pp project-twelf-server
