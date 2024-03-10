
module nestwrapper

   use Nested
   use priors
   use CalcLike, only: Feedback, ParamSet, num_params, Scales, CountParams_NS, outparams_ns, GetLogLike
   implicit none
   
   !MultiNest parameters
   logical nest_mmodal !multiple modes expected?
   logical nest_ceff !multiple modes expected?
   integer nest_nlive !no. of live points
   integer nest_nPar !tot no. of parameters to be saved along with the sampling parameters
   integer nest_nCdims !no. of parameters for mode separation
   integer nest_seed !seed for MultiNest, -ve means take it from sys clock
   real*8 nest_tol !evidence tolerance factor
   real*8 nest_efr !sampling efficiency
   character*100 nest_root !root for saving posterior files
   integer nest_maxModes !max modes expected, for memory allocation
   logical nest_fb !feedback on the sampling progress?
   logical restart_multinest !resume from previous run?
   integer sdim !dimensionality
   
 contains

!-----*-----------------------------------------------------------------

subroutine nest_Sample
   integer context !total number of clusters found
   integer i,j,k
   integer, allocatable :: nest_pWrap(:)
   
   !set dimensionality
   sdim=0
   do i=1,num_params
   	!don't count the parameters with delta priors
   	if(Scales%PMin(i)==Scales%PMax(i)) cycle
      sdim=sdim+1
   end do
   
   !set total no. of parameters to be saved
   nest_nPar=CountParams_NS()
   
   !no wraparound
   allocate(nest_pWrap(sdim))
   nest_pWrap=0

   if (Feedback > 1) then
      write(*,*) 'Dimensionality of param space = ', sdim
      write(*,*) 'Number of derived parameters  = ', nest_nPar
      write(*,*) 'Location of ln(like) in live points file, column = ', sdim+nest_nPar+1
   endif
	
   call nestRun(nest_mmodal,nest_ceff,nest_nlive,nest_tol,nest_efr,sdim,nest_nPar, &
   nest_nCdims,nest_maxModes,100,-1d90,nest_root,nest_seed,nest_pWrap,nest_fb,restart_multinest, &
   .true.,.false.,-1d10,getLogLikeNS,dumper,context)
    

end subroutine nest_Sample

!-----*-----------------------------------------------------------------

! Wrapper around Likelihood Function
! Cube(1:n_dim) has nonphysical parameters
! scale Cube(1:n_dim) & return the scaled parameters in Cube(1:n_dim) &
! additional parameters in Cube(n_dim+1:nPar)
! return the log-likelihood in lnew
subroutine getLogLikeNS(Cube,n_dim,nPar,lnew,context)

   integer n_dim,nPar,context,i,j
   real*8 lnew,Cube(nPar)
   Type(ParamSet) Params
   real*8 logZero
   parameter(logZero=-huge(1.d0)*epsilon(1.d0))
   
   j=0
   do i=1,num_params
   	if(Scales%PMin(i)==Scales%PMax(i)) then
      		Params%P(i)=Scales%PMin(i)
	else
      		j=j+1
!		if(j==1 .or. j==2) then
!			Cube(j)=LogPrior(Cube(j),dble(Scales%PMin(i)),dble(Scales%PMax(i)))
!		else
   			Cube(j)=Scales%PMin(i)+(Scales%PMax(i)-Scales%PMin(i))*Cube(j)
!		endif
      		Params%P(i)=real(Cube(j))
	end if
   end do
   
   !get the log-like
   lnew=dble(-GetLogLike(Params))

!   if(lnew<=-1.d10) lnew=logZero
   if(lnew<=-1.d30) lnew=logZero 
  
   !get the additional parameters
   call OutParams_NS(Params,Cube)

end subroutine getLogLikeNS

!-----*-----------------------------------------------------------------

! dumper, called after every updInt*10 iterations

subroutine dumper(nSamples, nlive, nPar, physLive, posterior, paramConstr, maxLogLike, logZ, logZerr, context)

	implicit none

	integer nSamples				! number of samples in posterior array
	integer nlive					! number of live points
	integer nPar					! number of parameters saved (physical plus derived)
	double precision, pointer :: physLive(:,:)	! array containing the last set of live points
	double precision, pointer :: posterior(:,:)	! array with the posterior distribution
	double precision, pointer :: paramConstr(:)	! array with mean, sigmas, maxlike & MAP parameters
	double precision maxLogLike			! max loglikelihood value
	double precision logZ				! log evidence
	double precision logZerr			! error on log evidence
	integer context					! not required by MultiNest, any additional information user wants to pass
	
end subroutine dumper

!-----*-----------------------------------------------------------------

end module nestwrapper
