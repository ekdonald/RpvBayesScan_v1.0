! This module contains
!        - the type definitions needed for the MSSM code
!        NEW now moved to typedef.f90 so interface can link to it
!        - parameter read-in/initialization routines
!        - mapping between the MC parameters (params) and the physical
!          MSSM parameters
!        - parameter I/O routines
! Roberto Ruiz de Austri
! Monte Carlo routines based on COSMOMC package by Antony Lewis (http://cosmologist.info)

module ParamDef

 use parameters !all Types definitions are in there
 use Random
 use settings

 implicit none

!#ifdef MPI
!    include "mpif.h"
!#endif

 Type ParamSet
    real :: P(num_params)
 end Type ParamSet

 Type ParamScale
    real :: PMin(num_params), PMax(num_params), PWidth(num_params),center(num_params)
 end Type ParamScale


 integer :: GridDim(num_params)
 integer :: num_accept=0, numtoget
 integer :: TotGridPoints

 Type(ParamScale) :: Scales

 integer :: use_data

 logical :: has_propose_matrix = .false.
 logical :: propose_grid = .false., postproc = .false.
 logical :: estimate_propose_matrix = .false.
 logical :: use_mu = .false.
 logical :: use_log = .false.
 logical :: use_nuisance_splitting = .false.
 logical :: Use_Nuisance = .false.
 !
 logical :: Use_LEP = .false.
 logical :: Use_LHC = .false.
 logical :: Use_Neutrinos = .false.
 logical :: Use_Higgs = .false.
 !
 logical :: Use_Anomalous_Mu = .false.
 logical :: Use_bsgamma = .false.
 logical :: Use_Bsmumu = .false.
 logical :: Use_Bdmumu = .false.
 logical :: Use_RBtaunu = .false.
 logical :: Use_Dsmunu = .false.
 logical :: Use_Dmunu = .false.
 logical :: Use_DeltaMbs = .false.
 logical :: Use_DeltaMbd = .false.
 logical :: Use_epsk = .false.
 logical :: Use_mueg = .false.
 logical :: Use_taueg = .false.
 logical :: Use_taumug = .false.
 logical :: Use_mue3 = .false.
 logical :: Use_tau3e = .false.
 logical :: Use_leptons = .false.


 real, dimension(:,:), allocatable :: propose_matrix, propose_matrix_nuis
 real, dimension(:), allocatable :: propose_diag, propose_diag_nuis
 real, dimension(:), allocatable :: sigmas
 real, dimension(:), allocatable :: grid_mass
 real, dimension(:), allocatable :: grid_sigma
 real, dimension(:,:), allocatable :: grid_lnlike
 real, dimension(2) :: grid_ics
 integer, dimension(:), allocatable :: slow_evecs

 !global variable
 !Type(Leshouches_out) :: Leshouches
 !
 Type(Susy_spec) :: CurrentSpectrum, PreviousSpectrum
 !Type(RPVOut)    :: CurrentRPVOut, PreviousRPVOut
 Type(Collider) :: CurrentCollider, PreviousCollider
 Type(Error_Out) :: ErrorsFromRPV
 Type(ReducedRPVOut) :: ChainReducedRPVOut
 !
 real :: mu

 real :: StartLike = LogZero
 real :: MaxLike = LogZero
 !global variable containing the flags
 Type(Flags) :: GFlags
 !timing variables
 logical, parameter :: timing= .true.
 integer :: Hertz,  Begin_Clock, End_Clock, ms
 character(12) :: Elapsed_Time
 real :: rpv_av = 0d0, like_av = 0d0
 real :: rpv_ssq = 0d0 !standard dev squared
 !standard dev squared
 real :: like_ssq

 !variables needed for the restart
 !used by driver.f90
 !logical :: old_format
 logical :: restart, redo_like, redo_theory, redo_change_like_only
 real :: redo_likeoffset = 0.0
 integer :: skip_lines
 character(LEN=5000) RestartLine

 !MPI variables
 double precision :: MPI_StartTime
contains

subroutine Initialize(Params)
        use IniFile
        implicit none
        type (ParamSet) Params
        integer i
        character(LEN=5000) InLine
 !       character(LEN=120) prop_mat
        real center, wid, mult, like
        real pmat(num_params,num_params)


        Ini_fail_on_not_found = .false.



!        if (restart) then
!           !RestartLine has been saved before, see driver.f90
!           read(RestartLine, *) mult, like, Params%P(1:num_params)
!           StartLike = Like
!           if (Feedback > 1) then
!              write(*,*) ' Restarting from like: ', like
!              write(*,*) ' At params ', Params%P(1:num_params)
!           end if
!        end if

        num_params_used = 0
        num_slow = 0
        num_nuis = 0
        TotGridPoints = 1


        do i=1,num_params

           InLine = Ini_Read_String(numcat('param',i), .true.)
           read(InLine, *, err = 100) center, Scales%PMin(i), Scales%PMax(i), wid, Scales%PWidth(i)
           Scales%center(i) = center
           if (Scales%PMax(i) < Scales%PMin(i)) stop 'You have param Max < Min'
           if (Scales%PWidth(i) /= 0) then
           !those are used params
           num_params_used = num_params_used + 1
           if (i > num_hard) then
             num_nuis = num_nuis + 1
           else
             num_slow = num_slow + 1
           end if
           if (propose_grid) then
             GridDim(i) = nint((Scales%PMax(i) - Scales%PMin(i))/Scales%PWidth(i))
             If (Feedback > 0) then
                write(*,*) 'Grid Points in Param' ,i, ' = ', GridDim(i)
             end If
             TotGridPoints = TotGridPoints*GridDim(i)
           end if
          end if
          if (.not. restart) then

           if (.not. propose_grid) then
             do
                if (wid < 0) then
                   !This case we want half gaussian, width -wid
                   !e.g. for positive definite parameters
                   Params%P(i) = center - abs(Gaussian1())*wid
                else
                   Params%P(i) = center + Gaussian1()*wid
                end if
 !! write(*,*) 'test paramdef b4 = ',Params%P(i)   !! by Donald on 12-02-2016
                !Repeat until get acceptable values in range
                if (Params%P(i)>=  Scales%PMin(i) .and. Params%P(i) <= Scales%PMax(i)) exit

 !! write(*,*) 'test paramdef aft = ',i,Params%P(i)   !! by Donald on 12-02-2016

             end do
           else !if grid
             Params%P(i) = Scales%PMin(i)
          end if !propose grid
       end if !not restart


         end do


        TotGridPoints = nint(1.0*TotGridPoints/(1.0*(nprocs)))

        if (TotGridPoints > numtoget .and. propose_grid) then
           write(*,*) "Grid too large - increase samples to ", TotGridPoints
           stop
        end if

        if (propose_grid .and. Feedback > 0) write(*,*) "Number of grid elements: ", TotGridPoints
        allocate(params_used(num_params_used))
        allocate(nuis_params_used(num_nuis))
        num_params_used = 0
        num_nuis = 0
        !now fill in the values
        do i=1,num_params
           if (Scales%PWidth(i) /= 0) then
              num_params_used = num_params_used + 1
              params_used(num_params_used) = i
              if (i > num_hard) then
                num_nuis = num_nuis + 1
                nuis_params_used(num_nuis) = i

             end if
           end if
        end do



        ! ---- do you want to use the split slow/nuis or not ----
        ! ---- if not, then num_nuis = 0, num_params_used = num_slow
!        if (.not. use_nuisance_splitting) then
!           num_nuis = 0
!           num_slow = num_params_used
!        end if
        if (Feedback > 0 ) &
           write(*, *) 'Varying a total of ', num_params_used, ' parameters'
!           if (use_nuisance_splitting) then
!              write(*,*)  'of which ', num_slow, ' are SUSY ', num_nuis, ' are nuisance'
!            else
!              write(*,*)  'all treated as slow parameters / no splitting '
!           end if
!        end if



        return
100     write(*,*) 'Error reading param details: '//trim(InLIne)
        stop

end subroutine Initialize


subroutine ParamsTomunuSSMParams(Params, munuSSM, NuisP)
     use settings

     implicit none
     real :: Params(num_Params)
     Type(Input_Params) :: munuSSM
     Type(Nuisance_Params) :: NuisP
!aca declaro los parametros

     if (use_log) then
        munuSSM%vL1 = 10**(Params(10))
        munuSSM%vL2 = 10**(Params(11))
        munuSSM%vL3 = 10**(Params(12))
        munuSSM%ynu1 = 10**(Params(13))
        munuSSM%ynu2 = 10**(Params(14))
        munuSSM%ynu3 = 10**(Params(15))
     else
        munuSSM%vL1 = Params(10)
        munuSSM%vL2 = Params(11)
        munuSSM%vL3 = Params(12)
        munuSSM%ynu1 = Params(13)
        munuSSM%ynu2 = Params(14)
        munuSSM%ynu3 = Params(15)
      endif

     munuSSM%lambda1 = Params(1)
     munuSSM%lambda2 = Params(1)
     munuSSM%lambda3 = Params(1)
     munuSSM%kappa1 = Params(4)
     munuSSM%kappa2 = 1.02*Params(4)
     munuSSM%kappa3 = 1.04*Params(4)
     munuSSM%vR1 = Params(7)
     munuSSM%vR2 = Params(7)
     munuSSM%vR3 = Params(7)
!    munuSSM%vL1 = Params(10) !comento los parametros en los que estoy escanenado.
!    munuSSM%vL2 = Params(11)
!    munuSSM%vL3 = Params(12)
!    munuSSM%ynu1 = Params(13)
!    munuSSM%ynu2 = Params(14)
!    munuSSM%ynu3 = Params(15)
     munuSSM%m1 = Params(16)
     munuSSM%m2 = Params(17)
     munuSSM%m3 = Params(18)
     munuSSM%msq1 = Params(19)
     munuSSM%msq2 = Params(20)
     munuSSM%msq3 = Params(21)
     munuSSM%msu1 = Params(22)
     munuSSM%msu2 = Params(23)
     munuSSM%msu3 = Params(24)
     munuSSM%msd1 = Params(25)
     munuSSM%msd2 = Params(26)
     munuSSM%msd3 = Params(27)
     munuSSM%mse1 = Params(28)
     munuSSM%mse2 = Params(29)
     munuSSM%mse3 = Params(30)
     munuSSM%a  =  Params(31)
     munuSSM%at =  Params(32)
     munuSSM%ab =  Params(33)
     munuSSM%atau = Params(34)
     munuSSM%anu =  Params(35)
     munuSSM%al1 = Params(36)
     munuSSM%al2 = Params(37)
     munuSSM%al3 = Params(38)
     munuSSM%ak1 = Params(39)
     munuSSM%ak2 = Params(39)
     munuSSM%ak3 = Params(39)
     munuSSM%tanb = Params(42)
!!
     !nuisance parameters
     NuisP%mbmb = Params(43)
     NuisP%mtop = Params(44)
     NuisP%alphasmz = Params(45)
     !param(23) is alpha_EM^-1
     NuisP%alphamz = 1d0/Params(46)

     if (Feedback > 3)  call PrintOutModelParams(munuSSM, NuisP)

   end subroutine ParamsTomunuSSMParams

subroutine munuSSMParamsToParams(munuSSM, NuisP, Params) !este tambien hay que adaptarlo
     use settings

     implicit none
     real :: Params(num_Params)
     Type(Input_Params) :: munuSSM
     Type(Nuisance_Params) :: NuisP


     if (use_log) then
         Params(10) = log10(munuSSM%vL1)
         Params(11) = log10(munuSSM%vL2)
         Params(12) = log10(munuSSM%vL3)
         Params(13) = log10(munuSSM%ynu1)
         Params(14) = log10(munuSSM%ynu2)
         Params(15) = log10(munuSSM%ynu3)
     else
         Params(10) = munuSSM%vL1
         Params(11) = munuSSM%vL2
         Params(12) = munuSSM%vL3
         Params(13) = munuSSM%ynu1
         Params(14) = munuSSM%ynu2
         Params(14) = munuSSM%ynu3
      endif

      Params(1) = munuSSM%lambda1
      Params(2) = munuSSM%lambda2
      Params(3) = munuSSM%lambda3
      Params(4) = munuSSM%kappa1
      Params(5) = munuSSM%kappa2
      Params(6) = munuSSM%kappa3
      Params(7) = munuSSM%vR1
      Params(8) = munuSSM%vR2
      Params(9) = munuSSM%vR3
!     Params(10) = munuSSM%vL1
!     Params(11) = munuSSM%vL2
!     Params(12) = munuSSM%vL3
!     Params(13) = munuSSM%ynu1
!     Params(14) = munuSSM%ynu2
!     Params(15) = munuSSM%ynu3
      Params(16) = munuSSM%m1
      Params(17) = munuSSM%m2
      Params(18) = munuSSM%m3
      Params(19) = munuSSM%msq1
      Params(20) = munuSSM%msq2
      Params(21) = munuSSM%msq3
      Params(22) = munuSSM%msu1
      Params(23) = munuSSM%msu2
      Params(24) = munuSSM%msu3
      Params(25) = munuSSM%msd1
      Params(26) = munuSSM%msd2
      Params(27) = munuSSM%msd3
      Params(28) = munuSSM%mse1
      Params(29) = munuSSM%mse2
      Params(30) = munuSSM%mse3
      Params(31) = munuSSM%a
      Params(32) = munuSSM%at
      Params(33) = munuSSM%ab
      Params(34) = munuSSM%atau
      Params(35) = munuSSM%anu
      Params(36) = munuSSM%al1
      Params(37) = munuSSM%al2
      Params(38) = munuSSM%al3
      Params(39) = munuSSM%ak1
      Params(40) = munuSSM%ak2
      Params(41) = munuSSM%ak3
      Params(42) = munuSSM%tanb
     !nuisance parameters
      Params(43) = NuisP%mbmb
      Params(44) = NuisP%mtop
      Params(45) = NuisP%alphasmz
      Params(46) = 1d0/NuisP%alphamz

     if (Feedback > 3)  call PrintOutModelParams(munuSSM, NuisP)

   end subroutine munuSSMParamsToParams


   subroutine PrintOutModelParams(InP, InN)
     Type(Input_Params) InP
     Type(Nuisance_Params) InN


       write(*,*) '>>>> Input Params'
       write(*,*) 'lambda1       : ', InP%lambda1
       write(*,*) 'lambda2       : ', InP%lambda2
       write(*,*) 'lambda3       : ', InP%lambda3
       write(*,*) 'kappa1        : ', InP%kappa1
       write(*,*) 'kappa2        : ', InP%kappa2
       write(*,*) 'kappa3        : ', InP%kappa3
       write(*,*) 'vR_1          : ', InP%vR1
       write(*,*) 'vR_2          : ', InP%vR2
       write(*,*) 'vR_3          : ', InP%vR3
       write(*,*) 'vL_1          : ', InP%vL1
       write(*,*) 'vL_2          : ', InP%vL2
       write(*,*) 'vL_3          : ', InP%vL3
       write(*,*) 'ynu_1         : ', InP%ynu1
       write(*,*) 'ynu_2         : ', InP%ynu2
       write(*,*) 'ynu_3         : ', InP%ynu3
       write(*,*) 'm_1           : ', InP%m1
       write(*,*) 'm_2           : ', InP%m2
       write(*,*) 'm_3           : ', InP%m3
       write(*,*) 'msQ1          : ', InP%msq1
       write(*,*) 'msQ2          : ', InP%msq2
       write(*,*) 'msQ3          : ', InP%msq3
       write(*,*) 'msU1          : ', InP%msu1
       write(*,*) 'msU2          : ', InP%msu2
       write(*,*) 'msU3          : ', InP%msu3
       write(*,*) 'msD1          : ', InP%msd1
       write(*,*) 'msD2          : ', InP%msd2
       write(*,*) 'msD3          : ', InP%msd3
       write(*,*) 'msE1          : ', InP%mse1
       write(*,*) 'msE2          : ', InP%mse2
       write(*,*) 'msE3          : ', InP%mse3
       write(*,*) 'A             : ', InP%a
       write(*,*) 'A_t           : ', InP%at
       write(*,*) 'A_b           : ', InP%ab
       write(*,*) 'A_tau         : ', InP%atau
       write(*,*) 'A_nu          : ', InP%anu
       write(*,*) 'A_lambda1     : ', InP%al1
       write(*,*) 'A_lambda2     : ', InP%al2
       write(*,*) 'A_lambda3     : ', InP%al3
       write(*,*) 'A_kappa1      : ', InP%ak1
       write(*,*) 'A_kappa2      : ', InP%ak2
       write(*,*) 'A_kappa3      : ', InP%ak3
       write(*,*) 'tanb         : ', InP%tanb
       write(*,*) '>>>> Nuisance Params'
       write(*,*) 'm_b          : ', InN%mbmb
       write(*,*) 'm_t          : ', InN%mtop
       write(*,*) 'alpha_S      : ', InN%alphasmz
       write(*,*) 'alpha_em     : ', InN%alphamz
       write(*,*) '  or al_em^-1: ', 1.0d0/InN%alphamz
       write(*,*) '>>>> Fixed Nuisance Params'

      if(feedback .gt. 4) then
       write(debug_unit,*) 'lambda1       : ', InP%lambda1
       write(debug_unit,*) 'lambda2       : ', InP%lambda2
       write(debug_unit,*) 'lambda3       : ', InP%lambda3
       write(debug_unit,*) 'kappa1        : ', InP%kappa1
       write(debug_unit,*) 'kappa2        : ', InP%kappa2
       write(debug_unit,*) 'kappa3        : ', InP%kappa3
       write(debug_unit,*) 'vR_1          : ', InP%vR1
       write(debug_unit,*) 'vR_2          : ', InP%vR2
       write(debug_unit,*) 'vR_3          : ', InP%vR3
       write(debug_unit,*) 'vL_1          : ', InP%vL1
       write(debug_unit,*) 'vL_2          : ', InP%vL2
       write(debug_unit,*) 'vL_3          : ', InP%vL3
       write(debug_unit,*) 'ynu_1         : ', InP%ynu1
       write(debug_unit,*) 'ynu_2         : ', InP%ynu2
       write(debug_unit,*) 'ynu_3         : ', InP%ynu3
       write(debug_unit,*) 'm_1           : ', InP%m1
       write(debug_unit,*) 'm_2           : ', InP%m2
       write(debug_unit,*) 'm_3           : ', InP%m3
       write(debug_unit,*) 'msQ1          : ', InP%msq1
       write(debug_unit,*) 'msQ2          : ', InP%msq2
       write(debug_unit,*) 'msQ3          : ', InP%msq3
       write(debug_unit,*) 'msU1          : ', InP%msu1
       write(debug_unit,*) 'msU2          : ', InP%msu2
       write(debug_unit,*) 'msU3          : ', InP%msu3
       write(debug_unit,*) 'msD1          : ', InP%msd1
       write(debug_unit,*) 'msD2          : ', InP%msd2
       write(debug_unit,*) 'msD3          : ', InP%msd3
       write(debug_unit,*) 'msE1          : ', InP%mse1
       write(debug_unit,*) 'msE2          : ', InP%mse2
       write(debug_unit,*) 'msE3          : ', InP%mse3
       write(debug_unit,*) 'A             : ', InP%a
       write(debug_unit,*) 'A_t           : ', InP%at
       write(debug_unit,*) 'A_b           : ', InP%ab
       write(debug_unit,*) 'A_tau         : ', InP%atau
       write(debug_unit,*) 'A_nu          : ', InP%anu
       write(debug_unit,*) 'A_lambda1     : ', InP%al1
       write(debug_unit,*) 'A_lambda2     : ', InP%al2
       write(debug_unit,*) 'A_lambda3     : ', InP%al3
       write(debug_unit,*) 'A_kappa1      : ', InP%ak1
       write(debug_unit,*) 'A_kappa2      : ', InP%ak2
       write(debug_unit,*) 'A_kappa3      : ', InP%ak3
       write(debug_unit,*) 'tanb         : ', InP%tanb
       write(debug_unit,*) '>>>> Nuisance Params'
       write(debug_unit,*) 'm_b          : ', InN%mbmb
       write(debug_unit,*) 'm_t          : ', InN%mtop
       write(debug_unit,*) 'alpha_S      : ', InN%alphasmz
       write(debug_unit,*) 'alpha_em     : ', InN%alphamz
       write(debug_unit,*) '  or al_em^-1: ', 1.0d0/InN%alphamz
       write(debug_unit,*) '>>>> Fixed Nuisance Params'
      endif

    end subroutine PrintOutModelParams

subroutine WriteParams(P, mult, like)
   implicit none

   Type(ParamSet) :: P
   Type(ReducedRPVOut) :: ReducedRPV

   real(8), intent(in) :: mult, like

   if (outfile_unit ==0) return

   !call ReduceOutput(PreviousRPVOut, ReducedRPV)

   write(outfile_unit,trim(fmt_params), ADVANCE='NO') mult, like, P%P

   write (outfile_unit, fmt_susysp, ADVANCE='NO') PreviousSpectrum

   if (GFlags%Collider_predict) write (outfile_unit, fmt_coll, ADVANCE='NO') PreviousCollider

   write (outfile_unit, fmt_rpv, ADVANCE='NO') ReducedRPV


   write (outfile_unit, '(A)') ' ' !to start a new line

end  subroutine WriteParams

subroutine OutParams_NS(P,paramOut)
    implicit none
    Type(ParamSet) :: P
    Type(ReducedRPVOut) :: ReducedRPV
    real*8 :: paramOut(:)
    integer :: i, j

    !quantities which have not been computed should simply be zer
    !as they have been initialized
    !for reflection strategy, takes the abs of the masses

    !call ReduceOutput(CurrentRPVOut, ReducedRPV)

    i=0

    !Which variables to save
    !Customize it to suits your need
    !Remember to adjust the formats in SetFormat
    paramOut(1:num_params)=P%P(1:num_params)
    i=i+num_params

    paramOut(i+1:i+8)=CurrentSpectrum%ms
    i=i+8
    paramOut(i+1:i+7)=CurrentSpectrum%mp
    i=i+7
    paramOut(i+1:i+7)=CurrentSpectrum%mhc
    i=i+7
    paramOut(i+1:i+5)=CurrentSpectrum%mch
    i=i+5
    paramOut(i+1:i+10)=CurrentSpectrum%mchi
    i=i+10
    paramOut(i+1:i+6)=CurrentSpectrum%msqu
    i=i+6
    paramOut(i+1:i+6)=CurrentSpectrum%msqd
    i=i+6
    paramOut(i+1)=CurrentSpectrum%mgluino
    paramOut(i+2)=CurrentSpectrum%m2sol
    paramOut(i+3)=CurrentSpectrum%m2at
    paramOut(i+4)=CurrentSpectrum%sth12sq
    paramOut(i+5)=CurrentSpectrum%sth13sq
    paramOut(i+6)=CurrentSpectrum%sth23sq
    paramOut(i+7)=CurrentSpectrum%neutrinoh
    i=i+7

    if (GFlags%Collider_predict) then
        paramOut(i+1)=CurrentCollider%gm2
        paramOut(i+2)=CurrentCollider%deltarho
        paramOut(i+3)=CurrentCollider%bsgamma
        paramOut(i+4)=CurrentCollider%Dmunu
        paramOut(i+5)=CurrentCollider%Dsmunu
        paramOut(i+6)=CurrentCollider%RBtaunu
        paramOut(i+7)=CurrentCollider%deltambs
        paramOut(i+8)=CurrentCollider%deltambd
        paramOut(i+9)=CurrentCollider%Bsmumu
        paramOut(i+10)=CurrentCollider%Bdmumu
        paramOut(i+11)=CurrentCollider%epsk
        paramOut(i+12)=CurrentCollider%mueg
        paramOut(i+13)=CurrentCollider%taueg
        paramOut(i+14)=CurrentCollider%taumug
        paramOut(i+15)=CurrentCollider%mu3e
        paramOut(i+16)=CurrentCollider%tau3e
      i=i+16
    end if


!    paramOut(i+1:i+3)=ReducedRPV%Scomp(1:3,1)
!    paramOut(i+4:i+6)=ReducedRPV%Scomp(1:3,2)
!    paramOut(i+7:i+9)=ReducedRPV%Scomp(1:3,3)
!    paramOut(i+10:i+12)=ReducedRPV%Scomp(1:3,4)
!    paramOut(i+13)=ReducedRPV%gf
!    paramOut(i+14)=ReducedRPV%hf
!    paramOut(i+15)=ReducedRPV%sf
!    paramOut(i+16)=ReducedRPV%Umix
!    paramOut(i+17)=ReducedRPV%direct


end subroutine OutParams_NS

integer function CountParams_NS()
    !counts the total no. of output parameters including the basis + nuisance +
    !derived parameters
    implicit none
    integer :: i, n

    n=num_params

    !spectrum
    n=n+56

    !collider observables
    if (GFlags%Collider_predict) n = n+16

!    n=n+17

    CountParams_NS=n

end function CountParams_NS

subroutine SetFormat
  character(len=3) :: nstr
  integer :: i,n

  !fmt_params =  trim(numcat(' ',num_params))//'E28.18,'
  fmt_params =  '('//trim(numcat('2E28.18,',num_params))//'E28.18)'
  fmt_susysp = '(56E28.18)'
  fmt_rpv = '(17E28.18)'
  fmt_coll = '(16E28.18)'
  fmt_err = '(15I3)'

  !fmt = '('//trim(fmt_params)//trim(fmt_rpv)//trim(fmt_err)//')'

  n = 0
  !writes .info file
  ParamsNames(1:42) = pack(MunuNames,MunuUsed)
  ParamsNames(43:46) = pack(NuisanceNames,NuisanceUsed)

  write(infofile_unit, '(A)') '### Chain generated by: '
  write(infofile_unit, '(A, I4)') 'action = ', action
  if (action .eq. doPP)   then
     write(infofile_unit, '(A)') '### Original chain generated by action:'
     write(infofile_unit, '(A, I4)')  'previous_action = ', previous_action
  end if
  write(infofile_unit, '(A)') '### header info '
  write(infofile_unit, '(A)') '# First col contains the multiplicity'
  if (((action .eq. doMCMC) .or. (action .eq. doGrid)) .or. ((action.eq.doPP).and.(previous_action.eq.doMCMC))) then
     !MCMC output (or grid output)
     write(infofile_unit, '(A)') '# Second col contains -ln(like)=chisq/2'
  else if ((action .eq. doNS) .or. ((action.eq.doPP).and.(previous_action.eq.doNS)))then
     !NS output
     write(infofile_unit, '(A)') '# Second col contains -2ln(like)=chisq'
  end if
  write(infofile_unit, '(A)') '# Subsequent columns: col i+2 contain parameter with name lab i'
  write(infofile_unit, '(A)') '### Input and nuisance params'
  do i=1,num_params
     n = n+1
     write(nstr,'(I3)') n
     write(infofile_unit, '(4A)') 'lab'//trim(adjustl(nstr))//'= '//trim(ParamsNames(i))
  end do
  write(infofile_unit, '(A)') '### SUSY spectrum'
  do i=1,size(SusySpecNames)
     n = n+1
     write(nstr,'(I3)') n
     write(infofile_unit, '(4A)') 'lab'//trim(adjustl(nstr))//'= '//trim(SusySpecNames(i))
  end do
  if (GFlags%Collider_predict) then
     write(infofile_unit, '(A)') '### Collider quantities'
     do i=1,size(ColliderNames)
        n = n+1
        write(nstr,'(I3)') n
        write(infofile_unit, '(4A)') 'lab'//trim(adjustl(nstr))//'= '//trim(ColliderNames(i))
     end do
  end if

  !do i=1,size(ReducedRPVOutNames)
  !   n = n+1
  !   write(nstr,'(I3)') n
  !   write(infofile_unit, '(4A)') 'lab'//trim(adjustl(nstr))//'= '//trim(ReducedRPVOutNames(i))
  !end do

 write(infofile_unit, '(A)') '### Number of parameters saved'

 write(infofile_unit, '(A, I4)') 'params_saved = ', n

end subroutine SetFormat

subroutine SaveFlags(in_unit)

  integer :: in_unit

  write(in_unit, '(A)') '### Data Included '
  write(in_unit, '(A,L)') 'Use_nuisance  = ', Use_Nuisance
  write(in_unit, '(A,L)') 'Use_LEP = ', Use_LEP
  write(in_unit, '(A,L)') 'Use_LHC = ', Use_LHC
  write(in_unit, '(A,L)') 'Use_Leptons = ', Use_Leptons
  write(in_unit, '(A,L)') 'Use_Higgs = ', Use_Higgs
  write(in_unit, '(A,L)') 'Use_Neutrinos = ', Use_Neutrinos
  write(in_unit, '(A,L)') 'Use_Anomalous_Mu = ', Use_Anomalous_Mu
  write(in_unit, '(A,L)') 'Use_bsgamma = ', Use_bsgamma
  write(in_unit, '(A,L)') 'Use_Bsmumu = ', Use_Bsmumu
  write(in_unit, '(A,L)') 'Use_Bdmumu = ', Use_Bdmumu
  write(in_unit, '(A,L)') 'Use_Btaunu = ',  Use_RBtaunu
  write(in_unit, '(A,L)') 'Use_Dsmunu = ', Use_Dsmunu
  write(in_unit, '(A,L)') 'Use_Dmunu = ', Use_Dmunu
  write(in_unit, '(A,L)') 'Use_DeltaMbs = ', Use_DeltaMbs
  write(in_unit, '(A,L)') 'Use_DeltaMbd = ', Use_DeltaMbd
  write(in_unit, '(A,L)') 'Use_epsk = ', Use_epsk
  write(in_unit, '(A,L)') 'Use_mueg = ', Use_mueg
  write(in_unit, '(A,L)') 'Use_taueg = ', Use_taueg
  write(in_unit, '(A,L)') 'Use_taumug = ', Use_taumug
  write(in_unit, '(A,L)') 'Use_mue3 = ', Use_mue3
  write(in_unit, '(A,L)') 'Use_tau3e = ', Use_tau3e

  write(in_unit, '(A)') '### Current or future data '
  write(in_unit, '(A,I3)') 'use_data = ', use_data

  write(in_unit, '(A)') '### Whether log priors are used '
  write(in_unit, '(A,L)') 'use_log = ', use_log

end subroutine SaveFlags

subroutine ReduceOutput(RPV, Reduced)

  type(RPVOut):: RPV
  type(ReducedRPVOut):: Reduced

  Reduced%Scomp(1,1)=RPV%scal%Scomp(1,1)
  Reduced%Scomp(2,1)=RPV%scal%Scomp(2,1)
  Reduced%Scomp(3,1)=RPV%scal%Scomp(3,1)
  Reduced%Scomp(1,2)=RPV%scal%Scomp(1,2)
  Reduced%Scomp(2,2)=RPV%scal%Scomp(2,2)
  Reduced%Scomp(3,2)=RPV%scal%Scomp(3,2)
  Reduced%Scomp(1,3)=RPV%scal%Scomp(1,3)
  Reduced%Scomp(2,3)=RPV%scal%Scomp(2,3)
  Reduced%Scomp(3,3)=RPV%scal%Scomp(3,3)
  Reduced%Scomp(1,4)=RPV%scal%Scomp(1,4)
  Reduced%Scomp(2,4)=RPV%scal%Scomp(2,4)
  Reduced%Scomp(3,4)=RPV%scal%Scomp(3,4)
  Reduced%gf=RPV%neut%gf
  Reduced%hf=RPV%neut%hf
  Reduced%sf=RPV%neut%sf
  Reduced%Umix=RPV%neut%Umix
  Reduced%direct=RPV%neut%direct

end subroutine ReduceOutput


subroutine  LoadOldFlags(InputFile)

  logical bad
  character(LEN=*) InputFile

  call Ini_Open(InputFile, infofile_unit, bad, .false.)

  if (bad) call DoStop ('Problem loading up file: '//trim(InputFile))

  previous_action = Ini_Read_Int('action')


  close(infofile_unit)
  call Ini_Close

end subroutine  LoadOldFlags

subroutine DoStop(S)
 character(LEN=*), intent(in), optional :: S
 integer ierror
        if (present(S)) write (*,*) trim(S)


#ifdef MPI
!        MPI_StartTime = MPI_WTime() - MPI_StartTime
        if (Feedback > 0 .and. MPIRank==0) then
!           write (*,*) 'Total time:', nint(MPI_StartTime), &
!                                   '(',MPI_StartTime/(60*60),' hours)'
           write (*,*) 'Fct evaluations: ', num
        end if
        call mpi_finalize(ierror)
#endif
       stop
end subroutine DoStop


end module ParamDef
