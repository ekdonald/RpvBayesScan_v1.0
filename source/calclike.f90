module CalcLike !ACA CALCULO LAS LIKELIHOODS
 use Random
 use settings
 use ParamDef
 use likedata

 implicit none

#ifdef MPI
    include "mpif.h"
#endif

 logical :: Init_data

contains

function GetLogLike(Params)

    Type(ParamSet)::  Params
    Type(Input_Params):: munuSSM
    Type(Nuisance_Params):: NuisP
    Type(Susy_spec):: Spectrum
    Type(Collider) :: Coll
    Type(Error_Out) :: Err

    integer :: ifail
    real :: GetLogLike
    real :: dum(1,1)
    logical :: Errors, unphys

    logical, parameter :: DebugSampler = .false.

    unphys = .false.


!    Params%P(1) = 0.035d0
!    Params%P(2) = 0.035d0
!    Params%P(3) = 0.035d0
!    Params%P(4) = -0.035d0
!    Params%P(5) = -0.035d0
!    Params%P(6) = -0.035d0
!    Params%P(7) = 2100d0
!    Params%P(8) = 2100d0
!    Params%P(9) = 2100d0
!    Params%P(10) = 1.0d-4
!    Params%P(11) = 1.0d-4
!    Params%P(12) = 1.0d-4
!    Params%P(13) = 2.91d-7
!    Params%P(14) = 2.91d-7
!    Params%P(15) = 2.91d-7
!    Params%P(16) = 800d0
!    Params%P(17) = 1800d0
!    Params%P(18) = 1300d0
!    Params%P(19) = 1000d0
!    Params%P(20) = 1000d0
!    Params%P(21) = 1000d0
!    Params%P(22) = 1000d0
!    Params%P(23) = 1000d0
!    Params%P(24) = 1000d0
!    Params%P(25) = 1000d0
!    Params%P(26) = 1000d0
!    Params%P(27) = 1000d0
!    Params%P(28) = 1000d0
!    Params%P(29) = 1000d0
!    Params%P(30) = 1000d0
!    Params%P(31) = 0d0
!    Params%P(32) = -2.0d3
!    Params%P(33) = 1.d2
!    Params%P(34) = 0.d1
!    Params%P(35) = -1.05d-4
!    Params%P(36) = 10.d0
!    Params%P(37) = 10.d0
!    Params%P(38) = 10.d0
!    Params%P(39) = -0.22d0
!    Params%P(40) = -0.22d0
!    Params%P(41) = -0.22d0
!    Params%P(42) = 34.d0

    !checks if proposal within prior box
    if ((any(Params%P > Scales%PMax) .or. any(Params%P < Scales%PMin)) .and. .not. postproc) then
     !proposal outside prior range
     GetLogLike = logZero
     if (Feedback > 2) write(*,*) 'out of bounds'
     return
    end if

    !converts Parms to MSSM
    call ParamsTomunuSSMParams(Params%P, munuSSM, NuisP)

    !calls code to get the spectrum
    if (timing) call StartTiming(Hertz,Begin_Clock)

    call GetPredictions(munuSSM, NuisP, Spectrum, Coll, Err)

    Errors = ErrorsPresent(Err)

    if (Feedback > 2) Call DebugErrorOutput(Err)

    if (.not. Errors) then

     CurrentSpectrum  = Spectrum
     CurrentCollider = Coll

     if (Feedback > 2) write(*,*) '... Computing likelihood ... '
     if (timing) call StartTiming(Hertz,Begin_Clock)
     GetLogLike = GetLogLikePost(munuSSM, NuisP, Spectrum, Coll)
     if (timing) then
          call StopTiming(Begin_Clock, End_Clock, Hertz, Elapsed_Time, ms)
          call ComputeAverage(rpv_av, rpv_ssq,ms,num)
     end if

    else
     !if errors in rpv, point is non-physical
     GetLogLike = LogZero
     return
    endif


   end function GetLogLike
!!-------------------------------------------------------------------------

!!-------------------------------------------------------------------------
   function GetLogLikePost(InP, NuisP, SusySp, Coll)

    !here the -Ln(like) = chi^2/2 is computed
    !edit this function to include new data etc
    real GetLogLikePost, NewlogLike
    Type(Input_Params) :: InP
    Type(Nuisance_Params):: NuisP
    Type(Susy_spec) :: SusySp
    Type(Collider) :: Coll
    Type (HiggsLike) :: hlike
    integer :: rank, ierror

    rank = 0

#ifdef MPI
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
#endif

    rank = rank + 1

    GetLogLikePost = 0d0

    if (Use_Nuisance) then
         NewLogLike = GetLogLikeNuisance(NuisP)
         GetLogLikePost = GetLogLikePost + NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike nuisance    = ', NewLogLike
    end if

!   !! for mu decay
    NewLogLike = GetLogLikemudecay(Coll)
    GetLogLikePost = GetLogLikePost + NewLogLike
    if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike mu decays = ', NewLogLike

    !! LEP
    NewLogLike = GetLogLikeMSparticles(SUSYSp)
    GetLogLikePost = GetLogLikePost + NewLogLike
    if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike LEP    = ', NewLogLike
    !! Point already excluded by LEP SUSY searches
    if(NewLogLike .eq. LogZero) return

     !!This for higgsbounds
     if (Use_Higgs) then !si quiero incluir otra likelihood tengo que incluirlo aca.
         NewLogLike = GetLogLikeHiggs(rank, hlike)
         GetLogLikePost = GetLogLikePost + NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike Higgs       = ', NewLogLike

         !!Point already excluded by Higgses searches
         if(NewLogLike .eq. LogZero) return

     endif

     if (Use_Leptons) then
         NewLogLike = GetLogLikeFromData(Melectron, abs(SusySp%mch(1))*1d3)
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike electron mass = ', NewLogLike
         !
         NewLogLike = GetLogLikeFromData(Mmuon, abs(SusySp%mch(2))*1d3)
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike muon mass = ', NewLogLike
         !
         NewLogLike = GetLogLikeFromData(Mtau, abs(SusySp%mch(3)))
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike tau mass = ', NewLogLike
     endif

     if (Use_Neutrinos) then
         if(SusySp%neutrinoh .eq. 1d0) then
          NewLogLike = GetLogLikeFromData(Sol_NH, SusySp%m2sol*1d5)
         else
          NewLogLike = GetLogLikeFromData(Sol_IH, SusySp%m2sol*1d5)
         endif
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike \Delta m^2_{sol}  = ', NewLogLike

         if(SusySp%neutrinoh .eq. 1d0) then
          NewLogLike = GetLogLikeFromData(Atm_NH, SusySp%m2at*1d3)
         else
          NewLogLike = GetLogLikeFromData(Atm_IH, SusySp%m2at*1d3)
         endif
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike \Delta m^2_{atm} = ', NewLogLike

         if(SusySp%neutrinoh .eq. 1d0) then
          NewLogLike = GetLogLikeFromData(Sinth12_NH, SusySp%sth12sq)
         else
          NewLogLike = GetLogLikeFromData(Sinth12_IH, SusySp%sth12sq)
         endif
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike \sin^2 \theta_{12} = ', NewLogLike

         if(SusySp%neutrinoh .eq. 1d0) then
          NewLogLike = GetLogLikeFromData(Sinth13_NH, SusySp%sth13sq)
         else
          NewLogLike = GetLogLikeFromData(Sinth13_IH, SusySp%sth12sq)
         endif
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike \sin^2 \theta_{13}  = ', NewLogLike

         if(SusySp%neutrinoh .eq. 1d0) then
          NewLogLike = GetLogLikeFromData(Sinth23_NH, SusySp%sth23sq)
         else
          NewLogLike = GetLogLikeFromData(Sinth23_IH, SusySp%sth23sq)
         endif
         GetLogLikePost = GetLogLikePost +  NewLogLike
         if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike \sin^2 \theta_{23}  = ', NewLogLike
     end if

     !!Now gm2
     if (Use_Anomalous_Mu) then
          NewLogLike = GetLogLikeFromData(AnomalousMu, coll%gm2*1E10)
          GetLogLikePost = GetLogLikePost + NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike g-2         = ', NewLogLike
     end if

     !!Now B(D)-physics
     if (Use_bsgamma) then
          NewLogLike = GetLogLikeFromData(Bsgamma, coll%bsgamma*1E4)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike Bs -> gamma = ', NewLogLike
     end if

     if (Use_Bsmumu) then
          NewLogLike = GetLogLikeFromData(Bsmumu, coll%Bsmumu*1E7)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike Bs -> mumu  = ', NewLogLike
     end if

     if (Use_Bdmumu) then
          NewLogLike = GetLogLikeFromData(Bdmumu, coll%Bdmumu*1E7)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike Bd -> mumu  = ', NewLogLike
      end if

      if (Use_DeltaMbs) then
          NewlogLike = GetLogLikeFromData(RDeltaMBs, coll%DeltaMbs)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike RDelta MBs   = ', NewLogLike
      end if

      if (Use_RBtaunu) then
          NewlogLike = GetLogLikeFromData(RBtaunu, coll%RBtaunu)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike R(Bu -> taunu) = ', NewLogLike
      end if

      if (Use_Dsmunu) then
          NewlogLike = GetLogLikeFromData(Dsmunu, coll%Dsmunu*1.E3)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike Ds -> mu nu = ', NewLogLike
       end if

       if (Use_Dmunu) then
          NewlogLike = GetLogLikeFromData(Dmunu, coll%Dmunu*1.E4)
          GetLogLikePost = GetLogLikePost +  NewLogLike
          if (Feedback > 2) write(*,'(A,E15.7)') '-lnlike D -> mu nu = ', NewLogLike
       end if


  end function GetLogLikePost
!!-----------------------------------------------------------------------------------------

!!-----------------------------------------------------------------------------------------
  function ErrorsPresent(err)

    Type(Error_Out) :: err

    logical :: ErrorsPresent

    if(err%general_error == 1 .or. err%coll_constraints == 1) then
!    if(err%general_error == 1) then
       ErrorsPresent = .true.
    else
       ErrorsPresent = .false.
    endif

  end function ErrorsPresent

  function SmearedBound(y, x0, sigma, tau,lower)result(lnprob)
    !input
    !y : where to evaluate the lnprob
    !x0 : the lower limit (ideally, the peak of the lower half-Gaussian,
    !or else the 95% limit with very small sigma)
    !sigma : the 1sigma widht of the half-gaussian
    !tau : estimated 1sigma uncertainity of theoretical prediction
    !lnprob : output, -ln(probabability)
    !lower = .true. means a lower bound
    !      = .false. it's an upper bound
    real(8) :: y
    real(8) :: lnprob, x0, sigma, tau, tstar
    logical :: lower
    real :: sgn

    if (lower .eqv. .true.) then
       sgn = 1.0d0
       else
       sgn = -1.0d0
    end if
    tstar = (sigma/tau)*(sgn*(x0-y))/sqrt(sigma**2+tau**2)
    !changed to fix normalization to 1 away from the bound for sigma = 0.0
    lnprob = -LOG(sigma/sqrt((sigma**2+tau**2))*EXP(-0.5*(sgn*(y-x0))**2/(sigma**2+tau**2))*(1-Z_func(tstar)) + &
             Z_func(sgn*(x0-y)/tau))

  end function SmearedBound


  function Z_func(lambda)
    !auxiliary function
    ! Z_func = 1/sqrt(2pi) int_lambda^infty exp(-1/2 t**2) dt
    real (8) :: Z_func, lambda
    Z_func = 0.5*errfc(lambda/sqrt(2.0))
  end function Z_func

  function errfc(x)
    !from Press et al p 164
    !returns the complementary error fct with fractional precision of E-7
    real(8) :: errfc, x,z,t
    z = abs(x)
    t = 1.0/(1.0 + z/2.0)
    errfc = t*EXP(-z*z - 1.26551223 + t*(1.00002368 + t*(0.37409196+ &
        t*(0.09678418 + t*(-0.18628806 +t*(0.27886807 + t*(-1.13520398 + &
        t*(1.48851587 + t*(-0.82215223 +t*0.17087277)))))))))
    if (x .lt. 0.0) errfc = 2.-errfc
    return
  end function errfc


  function GetLogLikeNuisance(NuisP)result(lnlike)
    real  lnlike

    Type(Input_Params) :: munuSSM !! DEK
    Type(Nuisance_params) :: NuisP
    real :: Mtop, si_top
    real :: Mbot, si_bot
    real :: alpha_S, si_S
    real :: alpha_EM_INV, si_EM_INV


!   if (future_errors) then
!   call DoStop('Tomorrow never dies')
!   end if
    Mtop = 172.7
    si_top = 2.9

    Mbot = 4.2
    si_bot = 0.06
    !RT updated March 2006

    alpha_S = 0.1186
    si_S = 0.002

    alpha_EM_INV = 127.958
    si_EM_INV = 0.048

    lnlike = 0.5*(NuisP%mtop - Mtop)**2/(si_top**2)
    lnlike = lnlike + 0.5*(NuisP%mbmb - Mbot)**2/(si_bot**2)
    lnlike = lnlike + 0.5*(NuisP%alphasmz - alpha_S)**2/(si_S**2)
    !the constraints are applied on alpha_EM^INV
    lnlike = lnlike + 0.5*(1d0/NuisP%alphamz - alpha_EM_INV)**2/(si_EM_INV**2)

  end function GetLogLikeNuisance


   function GetLogLikemudecay(Coll)result(lnlike)

    real :: lnlike
    real :: NewLogLike
    Type(Collider) :: Coll

    NewLogLike = 0.

    NewLogLike =  Coll%mueg  !! mu -> e gamma
    if(NewLogLike .gt. 5.7E-13) then
       lnlike = 1.e9
    else
       lnlike = 0.
    endif
    write(*,*) 'lnlike mu->eg:', lnlike

   !!Coll%mu3e
    NewLogLike =  Coll%mu3e  !! mu -> eee
    if(NewLogLike .gt. 1.0E-12) then
       lnlike =  lnlike + 1.e9
    else
       lnlike =  lnlike + 0.
    endif
    write(*,*) 'lnlike mu->eg:', lnlike

   !!Coll%Btaunu
   lnlike = lnlike + 0.5*(Coll%RBtaunu - 1.14E-4)**2/((0.23E-4)**2)

   end function GetLogLikemudecay


!-------------------------------------------
!    Generic multi-purpose likelihood
!------------------------------------------
  function GetLogLikeFromData(Datum, theory)result(lnlike)
    real(8) :: lnlike
    real(8) :: theory
    real(8) :: tau
    Type(LikeDatum) :: datum

    if (datum%tau_percent) then
       tau = datum%tau*theory
    else
       tau = datum%tau
    end if

    !Gaussian likelihood here
    if (datum%datum_type .eq. Gaussian) then
       lnlike = 0.5*(theory-datum%mu)**2/(datum%sigma**2+tau**2)
    else if  (datum%datum_type .eq. UpperLimit) then
       lnlike = SmearedBound(theory, datum%mu, datum%sigma, tau,lower= .false.)
    else if  (datum%datum_type .eq. LowerLimit) then
       lnlike = SmearedBound(theory, datum%mu, datum%sigma, tau,lower= .true.)
    end if
  end function GetLogLikeFromData


!**********************************************
! MSSM specific subroutines
!**********************************************

  subroutine GetPredictions(munuMSSM, NuisPar, Spectrum, Coll, Err)
!  subroutine GetRPVSpectrum(NMSSM, NuisPar, Spect, OutRPV, RPVErr)
      !input:
         !     the parameter values is MSSM
         ! and the Nuisance Parameters in NuisPar
      !output:
        !     the SUSY spectrum


      Type(Input_params) :: munuMSSM
      Type(Nuisance_params):: NuisPar
      Type(Susy_spec) :: Spectrum
      Type(Collider) :: Coll
      Type(Error_Out) :: Err

      integer :: rank, ierror

      If (Feedback > 3) write(*,*) '... Now calling rpv interface ...'

     rank = 0

#ifdef MPI
       call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
#endif

      rank = rank + 1

      call munussm(rank, GFlags, munuMSSM, NuisPar, Spectrum, Coll, Err)


      if(ErrorsPresent(Err)) return

      If (Feedback > 3) then
         write(*,*) '>>> Spectrum in GeV'
         write(*,*) ' >>> gaugino masses '
         write(*,*) '     >>> Lightest Neutralino mass : ', Spectrum%mchi(4)
         write(*,*) '     >>> Lightest Chargino mass   : ', Spectrum%mch(4)
!         write(*,*) '     >>>  Gluino mass             : ', Spectrum%mgluino
         write(*,*) ' >>> scalar masses '
         write(*,*) '     >>> Lightest scalar_1 mass   : ', Spectrum%ms(1)
         write(*,*) '>>> Lightest pseudoscalar_1 mass  : ', Spectrum%mp(2)
         write(*,*) '     >>> Charged scalar_1 mass    : ', Spectrum%mhc(1)
         write(*,*) ' >>> squark masses '
         write(*,*) '     >>>  stop 1 mass             : ', Spectrum%msqu(3)
         write(*,*) '     >>>  sbottom 1 mass          : ', Spectrum%msqd(3)
!         write(*,*) '     >>>  Lights squark mass     : ', minval(outrpv%sfer%msu(1,:))
         write(*,*) ' >>> neutrino Observables '
!         write(*,*) '     >>> mass nu_1                : ', Spectrum%mchi(1)
!         write(*,*) '     >>> delta^2_sol            : ', Outrpv%neut%mneut(2)
!         write(*,*) '     >>> delta^2_atm            : ', Outrpv%neut%mneut(3)
!         write(*,*) '>>> neutrino mixing angles '
!         write(*,*) '     >>>  theta12               : ', Outrpv%neut%theta12
!         write(*,*) '     >>>  theta23               : ', Outrpv%neut%theta23
!         write(*,*) '     >>>  theta13               : ', Outrpv%neut%theta13
!         write(*,*) '>>> neutrino mixing        '
!         write(*,*) '     >>>  sth12sq               : ', Outrpv%neut%sth12sq
!         write(*,*) '     >>>  sth13sq               : ', Outrpv%neut%sth13sq
!         write(*,*) '     >>>  sth23sq               : ', Outrpv%neut%sth23sq
!         write(*,*) '>>>  neutrino composition'
!         write(*,*) '     >>>  (Z_chi/Z_nu)_1  : ', Outrpv%neut%ratio(1)
!         write(*,*) '     >>>  (Z_chi/Z_nu)_2  : ', Outrpv%neut%ratio(2)
!         write(*,*) '     >>>  (Z_chi/Z_nu)_3  : ', Outrpv%neut%ratio(3)
!         write(*,*) '>>>  neutrino mixing'
!         write(*,*) '     >>>  U_mix  : ', Outrpv%neut%Umix
!         write(*,*) '>>>  neutrino hierarchy'
!         write(*,*) '     >>>  direct  : ', Outrpv%neut%direct
!         write(*,*) '>>>  Higgs 1 CP-even composition'
!         write(*,*) '     >>>  down_frac     : ', Outrpv%scal%Scomp(1,1)
!         write(*,*) '     >>>  up_frac       : ', Outrpv%scal%Scomp(2,1)
!         write(*,*) '     >>>  singlet_frac  : ', Outrpv%scal%Scomp(3,1)
!         write(*,*) '>>>  Higgs 2 CP-even composition'
!         write(*,*) '     >>>  down_frac     : ', Outrpv%scal%Scomp(1,2)
!         write(*,*) '     >>>  up_frac       : ', Outrpv%scal%Scomp(2,2)
!         write(*,*) '     >>>  singlet_frac  : ', Outrpv%scal%Scomp(3,2)
!         write(*,*) '>>>  Higgs 3 CP-even composition'
!         write(*,*) '     >>>  down_frac     : ', Outrpv%scal%Scomp(1,3)
!         write(*,*) '     >>>  up_frac       : ', Outrpv%scal%Scomp(2,3)
!         write(*,*) '     >>>  singlet_frac  : ', Outrpv%scal%Scomp(3,3)
!         write(*,*) '>>>  Higgs 4 CP-even composition'
!         write(*,*) '     >>>  down_frac     : ', Outrpv%scal%Scomp(1,4)
!         write(*,*) '     >>>  up_frac       : ', Outrpv%scal%Scomp(2,4)
!         write(*,*) '     >>>  singlet_frac  : ', Outrpv%scal%Scomp(3,4)
!         write(*,*) '>>>  Neutralino 4 composition'
!         write(*,*) '     >>>  gaugino_frac     : ', Outrpv%neut%gf
!         write(*,*) '     >>>  higgsino_frac    : ', Outrpv%neut%hf
!         write(*,*) '     >>>  singlino_frac    : ', Outrpv%neut%sf
!          write(*,*) '     >>> SM Higgs mass     : ', Spectrum%mHSM
!          write(*,*) '     >>> Tau Sneut. mass   : ', Spectrum%mSvL3
!          write(*,*) '     >>> Tau SvL Decay len : ', DMAX1(Spectrum%DecayLenRe,Spectrum%DecayLenIm)
!          write(*,*) '     >>> SM Higgs Mass     : ', Spectrum%mHSM
!          write(*,*) '     >>> SM Higgs Mass     : ', Spectrum%mHSM

!         write(*,*) '    >>> Lgts neutralino:', minval(abs(OutSoft%neut%mneut))
         if(GFlags%Collider_predict) then
          write(*,*) '>>> Collider observables'
          write(*,*) ' >>> B/D-physics '
          write(*,*) '     >>> BR(b -> s gamma)   : ', Coll%bsgamma
          write(*,*) '     >>> BR(D -> mu nu)     : ', Coll%Dmunu
          write(*,*) '     >>> BR(Ds -> mu nu)    : ', Coll%Dsmunu
          write(*,*) '     >>> RBR(B -> tau nu)   : ', Coll%RBtaunu
          write(*,*) '     >>> BR(Bs -> mu mu)    : ', Coll%bsmumu
          write(*,*) '     >>> BR(Bd -> mu mu)    : ', Coll%bdmumu
          write(*,*) '     >>> Delta(mBS)         : ', Coll%deltambs
          write(*,*) '     >>> Delta(mBd)         : ', Coll%deltambd
          write(*,*) ' >>> Flavour violation '
          write(*,*) '     >>> BR(mu -> e gamma)  : ', Coll%mueg
          write(*,*) '     >>> BR(tau -> e gamma) : ', Coll%taueg
          write(*,*) '     >>> BR(tau -> mu gamma): ', Coll%taumug
          write(*,*) '     >>> BR(mu -> 3 e)      : ', Coll%mu3e
          write(*,*) '     >>> BR(tau -> 3 e)     : ', Coll%tau3e
          write(*,*) ' >>> Other  '
          write(*,*) '     >>> Delta a_mu         : ', Coll%gm2
         endif
      end If


    end subroutine GetPredictions
!!--------------------------------------------------------------

!!--------------------------------------------------------------
   function GetLogLikeHiggs(rank, hlike)result(lnlike)

    integer :: rank
    real :: lnlike
    Type (HiggsLike) :: hlike

    integer :: i, indx, higgsbounds
    real :: NewLogLike
    real(8) :: d, dmin
    !real(8) :: higgssignals

    NewLogLike = 0.

    hlike%excl = 0.

    NewLogLike = real(higgsbounds(rank))
    if(NewLogLike .eq. 0.) then
       lnlike = 1.e9   !lnlike = LogZero
       !return
    else
       lnlike = 0.
    endif


    call higgssignals(rank, hlike%Chisq_mu, hlike%Chisq_mh)

    NewLogLike = real(hlike%Chisq_mu + hlike%Chisq_mh)/2d0

    lnlike = lnlike + NewLogLike

   end function GetLogLikeHiggs
!!--------------------------------------------------------------

!!--------------------------------------------------------------
   function GetLogLikeMSparticles(Spectrum)result(lnlike)

     real(8)  :: lnlike, percent_error
     real(8)  :: CharginoMass, MLSP, tanb
     real(8)  :: mstar, sigma, tau, mlsq
     Type(Susy_spec) :: Spectrum

     lnlike = 0d0

     !taking 5% th error on all masses, sigma is unknown
     percent_error = 0.05
     sigma = 0.0

     CharginoMass = Spectrum%mch(4)
     tau = percent_error*CharginoMass
     mstar = 105d0
     lnlike = SmearedBound(CharginoMass,mstar,sigma, tau, lower = .true.)

     if (lnlike > LogZero) lnlike = LogZero
     !to be consistent with everything else

  end function GetLogLikeMSparticles
!!----------------------------------------------------


!**********************************************
! ANCILLARY SUBROUTINES
!**********************************************
    subroutine DebugErrorOutput(EOut)
      Type(Error_Out) :: EOut

     write(*,*) '      >>> Error flags'
     write(*,'("           Error in spectrum calculation :", I2)'), Eout%general_error
     write(*,'("           Error from collider constraints :", I2)'), Eout%coll_constraints

!     write(*,'("            rpv Landau pole     :", I2)'), Eout%irqf
!     write(*,'("            rpv tachyonic sferm :", I2)'), Eout%tachyon
!     write(*,'("            rpv tachyonic CP-even :", I2)'), Eout%tachyon_cpev
!     write(*,'("            rpv tachyonic CP-odd :", I2)'), Eout%tachyon_cpodd
!     write(*,'("            rpv no EW minimum   :", I2)'), Eout%higgsUfb
!     write(*,'("            rpv no perturbative :", I2)'), Eout%nonperturbative
!     write(*,'("            rpv trivial minimum :", I2)'), Eout%trivialminimun
!     write(*,'("            rpv neutrino incos. :", I2)'), Eout%neutrino
     !write(*,'("            rpv neutrino mass out bounds:", I2)'), Eout%numass
     !write(*,'("            rpv neutrino mixing out bounds:", I2)'), Eout%numix

    end subroutine DebugErrorOutput



!**********************************************
! DATA HANDLING SUBROUTINES
!**********************************************



end module CalcLike
