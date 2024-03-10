!
!Module for Grid scan for RPV parameters
!


module mc_rpv

! use ParamDef
 use propose
 use calclike

implicit none


 type(ParamSet)  Trial
 integer :: burn_in = 2 !Minimum to get sensible answers
 integer :: indep_sample = 0

 real, allocatable, dimension(:,:), save :: Rot_param
 !flags to debug/tune the SliceSampler
 logical :: slice_randomsize = .false.
 integer, parameter :: stepping_out = 1, doubling = 2
 
 !number of iterations between dumping full model data. If zero then skip.
 !real :: Temperature  = 1

 !real, allocatable, dimension(:,:), save :: Rot_param
 !flags to debug/tune the SliceSampler

contains

!**********************************************
! GRID ROUTINES
!**********************************************

!----------------------------------------------
! Metropolis sampling
!----------------------------------------------

  subroutine MCMCsample(Params, accept_num)
    real Like
    integer accept_num, bank_id
    Type(ParamSet) Params, CurParams
    real(8) CurLike
    logical accpt
    integer  :: mult, num_mh = 0, num_bank = 0
    logical just_restarted
    real time_per_sample


    GFlags%Debug = .true.
    
    CurLike = StartLike
    CurParams = Params
    just_restarted = .true.
    if (.not. restart) then
       MaxLike = LogZero
       num_accept = 0
       num = 0 
       just_restarted = .false.
       !otherwise already set in driver.f90
    end if
    mult= 1
    !write here code for gridding
    !GRID HERE
    !---------------------------

    do while (num_accept <= accept_num+1)
       !the first point is not written
       
       num = num + 1
       If (propose_grid .and. feedback > 0) write(*,*) 'Chain ', instance, ' - grid points number ', num
       
       if (CurLike /= LogZero .and. slice_sampling) then 
          !Slice sampling - 1D for a start
          !This uses the 'doubling' procedure for maximum efficiency
          !See Radford M Neal, "Slice Sampling", Tech Rep 2005, (2000)
          
          if (num_accept> max(burn_in,1) .and. .not.(just_restarted)) then
             !never writes out the first line since for that I have not
             !computed the derived quantities
             output_lines = output_lines +1
 
             !WARNING! DOUBLE CHECK THIS!!!!!!!!!!!!! 
             call WriteParams(CurParams, dble(mult), dble(CurLike))
          end if
          just_restarted = .false.
          if (Feedback > 1) write (*,*) 'Slicing, Current Like:', CurLike
          mult = 1
          !This is currently untested - RT
          call SliceSampler(CurParams, CurLike) 
          num_accept = num_accept + 1
          if (CurLike < MaxLike) MaxLike = CurLike
       else
          !Metropolis sampling or grid for testing purposes
          !This is the only tested MCMC sampler at the moment
          if (.not. propose_grid) then
             call GetProposal(CurParams, Trial)
          else
             call ProposeGrid(CurParams, Trial)
          end if        
          
          Like = GetLogLike(Trial)
          
          if (Feedback > 1) write (*,*) '- Ln(Like)= chi^2/2 = ', & 
               & Like, 'Current LogLike:', CurLike

          accpt = (Like /= logZero) .and. &
               (CurLike > Like .or. Like < CurLike + 20 .and. ranmar() < exp(min(0.,-Like+CurLike))) 
          !Include the min() so that compilers not doing optimal compilation doen't complain
      
          if (propose_grid .and. Like /= logZero) accpt =.true.
      
          if (accpt) then
             if (num_accept> max(burn_in,1) .and. .not.(just_restarted)) then
                !writes the **previous** point
                call WriteParams(CurParams,dble(mult),dble(CurLike))
             end if
             just_restarted = .false.
             CurLike = Like
             CurParams = Trial
             num_accept = num_accept + 1
             if (Like < MaxLike) MaxLike = Like
             mult=1
             !save the derived quanties calculated for the Current point
             PreviousSpectrum    = CurrentSpectrum
             !PreviousRpvOut   = CurrentRpvOut

             if (Feedback > 1) write (*,*) num, ' accepting. ratio:', real(num_accept)/num, & 
                  ' slow = ', slow_proposals, ' nuisance = ', nuis_proposals 
             if (mod(num_accept,50) == 0 .and. Feedback > 1) then 
                write (*,*) 'rat: ', real(num_accept)/num, ' in ',num,', best: ',real(MaxLike) 
             end if
          else
             mult=mult+1
             if (Feedback > 1) write (*,*) num,' rejecting. ratio:', real(num_accept)/num
          end if
       end if !not sample_slicing 
       if (logfile_unit /=0 .and. mod(num,logsteps) == 0) then
          write (logfile_unit,'(A,I8,A,I10,A, f7.3,A,I5,A,f10.5)') & 
               'accepted:', num_accept, ' in ', num, ' --  % rate:', 100*real(num_accept)/num, &
               ' (nuis: ',  nuis_proposals, '),  best: ', real(MaxLike)
          write (logfile_unit,'(A, F8.2,A, F8.2)') &
               '   > Timing (ms):  Sftsusy  = ', rpv_av, ' +/- ', sqrt(abs(rpv_ssq))
          !putting abs so in case something wrong with the timing it does not crash
          write (logfile_unit,'(A, F8.2,A, F8.2)') &
               '                :  like     = ', like_av, ' +/- ', sqrt(abs(like_ssq))
          write (logfile_unit,'(A, F8.2)') &
               '      Av time per model (s) = ', real((rpv_av + like_av)/1000)
          time_per_sample =  real((rpv_av + like_av))/1000/(real(num_accept)/num)
          write (logfile_unit,'(A, F8.2)') &
               '      Av time per sample (s)= ', time_per_sample
          write (logfile_unit,'(A, F8.2)') &
               '      Estimated time to run (days) = ', (accept_num - num_accept)*time_per_sample/60/60/24
          call FlushFile(logfile_unit)
       end if
       if (propose_grid .and. num > TotGridPoints) exit

    end do !loop on samples
    
  end subroutine MCMCsample

!----------------------------------------------
! Slice sampling (Currently bugged - DO NOT USE)
!----------------------------------------------
 
  subroutine SliceSampler(CurParams, CurLike) 
    !Currently untested! RT
    Type(ParamSet) CurParams
    real(8) CurLike
    integer, save:: loopix = 0
    

    !1D slice sampling using doubling or stepping out technique
    if (sampling_method .eq. slice_sampling_1d) then
       
       if (mod(loopix,num_params_used)==0) then
          !generates a new set of randomly oriented basis vectors
          !after having proposed a change along all directions
          !of the previous basis
          if (.not. allocated(Rot_param)) allocate(Rot_param(num_params_used,num_params_used))
          call RotMatrix(Rot_param,num_params_used)
          loopix = 0
       end if
       loopix = loopix + 1
       If (Feedback > 4) write(*,*) 'Proposing for loopix', loopix
       call SlicePropose1D(CurParams, CurLike, loopix) 
       
    end if
    
  end subroutine SliceSampler

  subroutine SlicePropose1D(SlicePar, Like, i)
    !Slice sampling in 1D for direction i
    !Starting point x0 has coord Params and likelihood Like
    !On exit they contain the new values of the next point x1
    
    Type(ParamSet) SlicePar, ParamsP
    integer, intent(in) :: i
    real(8), intent(inout) :: Like
    real offset,  P
    real L, R, M, OldL, OldR
    real LikL, LikR, Range, LikP, w, tmp_random, LikRT
    integer fevals
    integer, parameter :: MaxK = 4
    integer :: K
    logical :: Dflag, AcceptP, ComputeL, ComputeR 
    
    !using "doubling" procedure
    !w = propose_scale
    w = 1.0
    !just a constant, set from the driver
    if (slice_randomsize) w =w * Gaussian1()
    !step (a), page 8: defininig y (here is the new value for Like)
    Like = Like + randexp1()  !New vertical position (likelihood)  
    if (Feedback > 4) write(*,*) 'New Slice defined by Like = ', Like
    fevals = 0
    
    !step (b): find I=(L,R)
    !Using algorith of Figure 3, page 11
    !out here, x0 = 0 and scale w = 1
    !offset = U
    offset = ranmar() 
    
    L = -offset   !L = x0 - w*U, with x0 = 0, w=1
    R = -offset+1 !R = L + w, with w=1  
    K = maxK
    
    !Doubling procedure, Figure 4
    if (procedure .eq. doubling) then
       ComputeL = .true.; ComputeR = .true.
       
       do
          !both L and R are computed the first time
          !subsequent times only one is updated per loop
          if (ComputeL) then
             LikL = UpdateParamsLike(SlicePar, w*L, i)
             If (Feedback > 4) write(*,*) maxK-K, ' ) Just DoubledL', L, LikL
             
             fevals = fevals + 1
             !if (LikL .eq. LogZero) then
             !outside the boundaries, stop expanding along L
             !go back to previous value for L
             !   L = OldL
             !   StopUpdateL = .true.
             !end if
          end if
          if (ComputeR) then
             LikR = UpdateParamsLike(SlicePar, w*R, i)
             If (Feedback > 4) write(*,*) maxK-K, ' ) Just DoubledR', R, LikR 
             fevals = fevals + 1
             !if (LikR .eq. LogZero) then
             !outside the boundaries, stop expanding along R
             !go back to previous value for R
             !   R = OldR
             !   StopUpdateR = .true.
             !end if
          end if
          
          if ((LikR > Like) .and. (LikL > Like)) exit
          !one of the sides has been doubled
          K = K - 1
          if(K < 1) exit !reached max expansion
          !do another expansion if not both ends outside S
          tmp_random = ranmar()
          if( tmp_random < 0.5d0) then
             L = L - (R-L)
             ComputeL = .true.
             ComputeR = .false.
          else 
             R = R + (R-L)
             ComputeR = .true.
             ComputeL = .false.
             !if both ends out, it has already exited
          end if
          
       end do
    end if !end of doubling
    
    !Stepping out procedure, Figure 3
    if (procedure .eq. stepping_out) then
       do
          LikL = UpdateParamsLike(SlicePar, w*L, i)
          If (Feedback > 4) write(*,*) 'SteppingOut L', L, LikL
          fevals = fevals + 1
          if (LikL < LogZero) then
             if (LikL > Like) exit
             L = L  - 1
          else
             !go back to previous point and stop
             L = L + 1
             exit
          end if
       end do
       
       do
          LikR = UpdateParamsLike(SlicePar, w*R, i)
          If (Feedback > 4) write(*,*) 'SteppingOut R', R, LikR
          fevals = fevals + 1
        if (LikR < LogZero) then
           if (LikR > Like) exit
           R = R + 1
        else
           !you are going outside the boundaries, better stop 
           !at previous point
           R = R - 1
           exit
        end if
       end do
    end if !end of stepping out
    If (Feedback > 4) write(*,*) ' Found new interval : (L,R) = ', L,R
    
    !shrinkage procedure, Figure 5
    !draws a sample from the interval (R,L)
    If (Feedback > 4) write(*,*) ' Now trying to sample point from I...'
    do
       Range =  R - L
       tmp_random = ranmar()
       P  =  L + tmp_random * Range
       !ParamsP corresponds to x1
       ParamsP = SlicePar
       LikP = 0d0
       !updates like value LikP AND ParmamsP to x1
       LikP = UpdateParamsLike(ParamsP, w*P,i, .true.)
       fevals = fevals + 1
       !Now ParamsP contains coordinates of x1 in ParamSpace
     
       if (LikP < Like) then 
          If (Feedback > 4)  write(*,*) 'Test 1: LikP < Like = ', (LikP<Like), LikP, Like
          if (procedure .ne. doubling) then 
             AcceptP = .true.
          else
             !check whether the new point is acceptable
             If (Feedback > 4) write(*,*) 'Checking Acceptable...'
             Dflag = .false.
             AcceptP = .true.
             !this is mantained if the loop goes through
             !whithout disproving it
             do  while ((R-L)>1.1)
                M = (L+R)/2
                !remember, x0 = 0 here
                !x1 = P
                if ((0<M .and. P.ge.M) .or. (0.ge.M .and. P<M)) then
                   Dflag = .true.
                else
                   Dflag = .false.
                end if
                if (P<M) then
                   R = M
                else
                   L = M
                end if
                if (Dflag) then
                   !around x1
                   LikR = UpdateParamsLike(SlicePar,  w*R, i)
                   if (LikR .ge. Like) then
                      LikL = UpdateParamsLike(SlicePar,  w*L, i)  
                      if (LikL .ge. Like) then
                         AcceptP = .false. !point is not acceptable
                      end if
                   end if
                   exit
                end if  !Dflag if
             end do !Acceptance Check Loop
          end if !procedure ne doubling
          
          if (Feedback > 4) write(*,*) 'Test 2: AcceptableP = ', AcceptP
          
          if (AcceptP) exit
          
       end if !LikP < Like
       
       if (P > 0) then
          R = P
       else
          L = P
       end if
       
    end do !do another loop
    !after having found an acceptable point
    Like = LikP
    SlicePar = ParamsP
    num = num + fevals
    if (Feedback > 4) write(*,*) 'Acceptable point found, coords, ', ParamsP%P(1:2), &
         'num = ', num
    
  end subroutine SlicePropose1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!             AUXILIARY ROUTINES 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function UpdateParamsLike(InParams, dist, i, UpdateParams)result(NewLike) 
  !For slice sampling movement and likelihood
  Type(ParamSet) tmp, InParams
  integer, intent(in) :: i
  real, intent(in) :: dist
  real :: NewLike
  logical, optional :: UpdateParams

  tmp = InParams

  tmp%P(params_used) = tmp%P(params_used) +  &
       Rot_param(:,i) * dist *  Scales%PWidth(params_used)
  !tmp%P(params_used(i)) = tmp%P(params_used(i)) +  &
  ! dist *  Scales%PWidth(params_used(i))

  NewLike = GetLogLike(tmp)

  if(present(UpdateParams) .and. UpdateParams) InParams = tmp

end function UpdateParamsLike



  subroutine GRIDsample(Params, accept_num)
    real Like
    integer accept_num
    Type(ParamSet) Params, CurParams
    Type(Error_Out) RPVErr
    real CurLike
    logical accpt
    integer  mult
    logical just_restarted, unphys
    real time_per_sample

    GFlags%Debug = .true.
    
 
    CurParams = Params
    just_restarted = .true.
    if (.not. restart) then
       num_accept = 0
       num = 0 
       just_restarted = .false.
       !otherwise already set in driver.f90
    end if
    mult= 1
    !write here code for gridding
    !GRID HERE
    !---------------------------

    do while (num_accept < accept_num+1)
       !the first point is not written
       
       num = num + 1
       If (propose_grid .and. feedback > 0) write(*,*) 'Chain ', instance, ' - grid points number ', num
       
          !This uses the 'doubling' procedure for maximum efficiency
          !See Radford M Neal, "Slice Sampling", Tech Rep 2005, (2000)
        if(num_params_used /= 0) then 
          call ProposeGrid(CurParams, Trial)
        else
          Trial = CurParams
        endif


        unphys = .false.

        !call GetOut(Trial, unphys)

        accpt = .false.
 
        if(propose_grid .and. .not. unphys) accpt =.true.         

      
!temp: allpoint accepted better pass as option  
!        accpt = .true.
        if (accpt) then
!             if (num_accept> max(burn_in,1) .and. .not.(just_restarted)) then
!              call WriteParams(Trial)
!             end if
             just_restarted = .false.
             CurLike = Like
             CurParams = Trial
             num_accept = num_accept + 1
        endif

        if (propose_grid .and. num > TotGridPoints) exit

     

      enddo
    
 end subroutine GRIDsample

! subroutine GRIDSample(Params)
!   real Like
!   integer accept_num
!   Type(ParamSet) Params, CurParams
!   logical accpt
!   integer  i,j
!   logical just_restarted
!   real time_per_sample
!   real dim_array(num_params_used)

!   GFlags%Debug = .true.


!   CurParams = Params
!   just_restarted = .true.
!   if (.not. restart) then
!      num_accept = 0
!      num = 0 
!      just_restarted = .false.
!   end if

!   If (feedback > 0) write(*,*) 'Grid points to achieve ', TotGridPoints

!   call SamplesGrid(CurParams)


!end subroutine GRIDSample



! subroutine SamplesGrid(CurParams)

!  Type(ParamSet) Trial, CurParams
!  integer :: r = 0, i = 0 , j = 0, k= 0, l = 0
!  real vec(num_nuis)


!  if (num_nuis /= 0) then 
!    do i = 0, GridDim(nuis_params_used(1))
!       vec(1) = i
!       Trial = CurParams
!       call UpdateNuisParams(Trial, vec)
!       if (num_slow /= 0) then 
!         call SlowGrid(Trial)
!       else 
!          call GetOut(Trial)
!          call WriteParams(Trial)
!       endif
!    enddo
!  else
!    if (num_slow /= 0) call SlowGrid(CurParams)
! endif


! end subroutine SamplesGrid


end module mc_rpv


