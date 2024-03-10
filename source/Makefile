# GNU
FC = mpif90
#FFLAGS = -DMPI -cpp -O2
# For debugging
FFLAGS = -g -cpp -DMPI -fbounds-check -Wall -fbacktrace -finit-real=nan -fPIE -fPIC

# Compiler options ###
# Options for linux
OPT= -O2
DEBUG=
CFLAGS= -w

export FC FFLAGS


RPVHOME=../contributions/SPheno-4.0.0
HBHOME=../contributions/higgsbounds-5.10.2/build
HSHOME=../contributions/higgssignals-2.6.2/build

### Setups for the LIB root directory ###
LIB_RPV=../contributions/SPheno-4.0.0/lib
LIB_RPVD=../rpvdec
LIB_NS=../multinest
LIB_HB=../contributions/higgsbounds-5.10.2/build/lib
LIB_HS=../contributions/higgssignals-2.6.2/build/lib
LAPACKL = -L../contributions/lapack-3.6.0 -llapack -lrefblas


OBJFILES = inifile.o utils.o settings.o typedef.o paramdef.o \
	   likedata.o calclike.o propose.o MC_RPV.o nestwrap.o \
	   postprocess.o driver.o 

READFILES = inifile.o utils.o settings.o read.o


PLOTFILES = typedef.o inifile.o utils.o settings.o paramdef.o mcsamples.o matlab.o smplots.o GetPlots.o

FLIBS = -L$(LIB_RPV) -lSPhenomunuSSM3G -lSPheno -L$(LIB_NS) -lnest3 -L$(LIB_HS) -lHS -L$(LIB_HB) -lHB $(LAPACKL)


TYPEDEF = typedef.f90

all:  typedef rpvlib multinestlib hblib hslib rpv getplots


%.o: %.f90
	$(FC) $(FFLAGS) -I$(LIB_NS) -c $*.f90

%.o: %.F90
	$(FC) $(FFLAGS)  -I$(LIB_NS) -c $*.F90

rpvlib: 
	cd $(RPVHOME) ;$(MAKE) all

multinestlib:
	cd ../multinest ;$(MAKE) all

hblib:
	cd $(HBHOME); ../../../../cmake-3.28.0-linux-x86_64/bin/cmake .. ;$(MAKE) all 

hslib:
	cd $(HSHOME); ../../../../cmake-3.28.0-linux-x86_64/bin/cmake .. ;$(MAKE) all

precompile : $(OBJFILES)

rpv:	$(OBJFILES) 
	$(FC) -o  ../rpv.x  $(OBJFILES) $(FLIBS) $(FFLAGS)

getplots: $(PLOTFILES) 
	$(FC) -o  ../getplots.x  $(PLOTFILES) $(LAPACKL) $(FFLAGS) 


typedef	: $(TYPEDEF)
	$(FC) -c $(TYPEDEF)


cleanall:
	$(MAKE) clean clean_multinest clean_rpv clean_hb clean_hs


clean:
	rm -f *.o *.mod *.d *.pc *.obj ../rpv.x ../getplots.x

clean_multinest:
	cd ../multinest; $(MAKE) clean ;

clean_rpv: 
	cd $(RPVHOME); $(MAKE) cleanall ;

clean_hb: 
	cd $(HBHOME); $(MAKE) clean ;

clean_hs: 
	cd $(HSHOME); $(MAKE) clean ;















