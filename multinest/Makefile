PROGS = libnest3.a 

OBJEXT = o

AR = ar r  
LINKLIB = ld -shared  
LIBDIR = ./


NSOBJECTS = utils.$(OBJEXT) utils1.$(OBJEXT) \
	priors.$(OBJEXT) kmeans_clstr.$(OBJEXT) xmeans_clstr.$(OBJEXT) \
	posterior.$(OBJEXT) nested.$(OBJEXT)

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $^ 

%.o: %.F90
	$(FC) $(FFLAGS) -c -o $@ $^ 

all: $(PROGS) 
#	-rm nested.o

libnest3.so: $(NSOBJECTS) 
	$(LINKLIB) -o $(LIBS) $@ $^ 
#	-mv $@ $(LIBDIR)  

libnest3.a: $(NSOBJECTS) 
	$(AR) $@ $^ 

clean: 
	-rm $(LIBDIR)/libnest3.*  *.o *.mod

