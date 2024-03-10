PROGS = libnest3.a 
 
FC = mpif90 -g -ffree-line-length-none -DMPI
AR = ar r  
LINKLIB = ld -shared  
LIBDIR = ./

#CS new
#LAPACKLIB = -llapack
 
NSOBJECTS = utils.o utils1.o priors.o kmeans_clstr.o xmeans_clstr.o posterior.o nested.o

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $^ 

%.o: %.F90
	$(FC) $(FFLAGS) -c -o $@ $^ 

 
all: $(PROGS) 
#	-rm nested.o
 
libnest3.so: $(NSOBJECTS) 
	$(LINKLIB) -o $@ $^ 
#CS new, instead of line above
	$(LINKLIB) -o $(LIBS) $@ $^
 
libnest3.a: $(NSOBJECTS) 
	$(AR) $@ $^ 
 
 
clean: 
	-rm $(LIBDIR)/libnest3.*  *.o *.mod

