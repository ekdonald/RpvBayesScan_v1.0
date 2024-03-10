subroutine higgssignals(rank, Chisq, Xi2)


! use usefulbits, only : theo,g2,tevR,res,deallocate_usefulbits,debug, &
!                       inputmethod,HiggsBounds_info,ndat,n_additional,nH, &
!                       allocate_dataset_parts, allocate_sqcouplratio_parts, &
!                       allocate_tevextras_parts,whichanalyses,whichinput
 use input, only : setup_input,do_input
! use S95tables, only : setup_S95tables,deallocate_S95tables
! use theory_BRfunctions, only : setup_BRSM,deallocate_BRSM
! use channels, only : setup_channels,check_channels
! use output, only : setup_output,do_output
! use theo_manip, only : complete_theo
! use parameters

 use usefulbits, only : whichinput, whichanalyses, np, Hneut, Hplus, debug, &
&                       inputmethod, inputmethod, vsmall  !, theo , g2
             
 use usefulbits_hs, only : output_level, Exptdir, pdf, HSresults,HiggsSignals_info, Nparam !, HSres, runmode
 use io, only : do_output_for_hs, setup_input_for_hs
 use pc_chisq, only :  print_peaks_signal_rates_to_file
 use numerics, only: gammp
 
 implicit none

! Type(Susy_spec) :: Spectrum
!Type(Flags) :: LFlags
!Type(Input_params) :: munuMSSM
!Type(Nuisance_params):: NuisPar
!Type(Susy_spec) :: Spectrum
!Type(Collider) :: Coll
!Type(Error_Out) :: Err

    double precision :: Pvalue, Chisq, Chisq_mu, Chisq_mh, Xi2
    double precision :: Chisq_mu_LHCRun1, Chisq_mh_LHCRun1, Chisq_LHCRun1, Pvalue_LHCRun1
    double precision :: Chisq_STXS, Chisq_STXS_rates, Chisq_STXS_mh, Pvalue_STXS
    double precision :: Pvalue_tot
    integer :: nobs, nobs_LHCRun1, nobs_STXS !, mode


! used by HiggsBounds_neutral_input_part
 integer :: rank   !, i, j
 ! integer, parameter :: Npar = 5

 !real(8) :: Pvalue_tot, Chisql, Chisq_mu, Chisq_mh,Chisq 
 integer :: nH, nHplus  !, ndf , mode

 ! logical :: invisible_lsp, just_after_run
 ! logical, save :: first = .true.

 debug = .false.

 !init
 Chisq_mu = 0d0 
 Chisq_mh = 0d0

 ! Set number of neutral and charged Higgs bosons in the MSSM:
 nH = 15
 nHplus = 7

 !Exptdir = 'latestresults'
 !runmode = 'peak'
 pdf = 2

 if(debug)then
   write(*,*)'nH=', nH + nHplus
 endif


 !this part replaces the call to HiggsBounds_input_SLHA
 whichinput='effC'

    inputmethod = 'datfile'
    output_level = 0
    whichanalyses = 'onlyH'

    call setup_input_for_hs(rank)

    call initialize_HiggsSignals(np(Hneut), np(Hplus), Exptdir)

    call do_input(rank)

!  write(*,*) "Run HS..."
    call run_HiggsSignals(Chisq_mu, Chisq_mh, Chisq, nobs, Pvalue)

!  write(*,*) "Run HS Run1 ..."
    call run_HiggsSignals_LHC_Run1_combination(Chisq_mu_LHCRun1, Chisq_mh_LHCRun1, Chisq_LHCRun1, nobs_LHCRun1, Pvalue_LHCRun1)

!  write(*,*) "Run HS STXS ..."
    call run_HiggsSignals_STXS(Chisq_STXS_rates, Chisq_STXS_mh, Chisq_STXS, nobs_STXS, Pvalue_STXS)

!  call calculate_model_predictions_for_STXS

!  call get_chisq_from_STXS(Chisq_STXS, Pvalue_STXS)
!
!  call get_number_of_STXS_observables(nobs_STXS)

!  call print_STXS()

    if ((Chisq + Chisq_LHCRun1 + Chisq_STXS) .gt. vsmall .and. (nobs + nobs_LHCRun1 + nobs_STXS - Nparam) .gt. 0) then
        Pvalue_tot = 1 - gammp(dble(nobs + nobs_LHCRun1 + nobs_STXS - Nparam)/2.0D0, (Chisq + Chisq_LHCRun1 + Chisq_STXS)/2.0D0)
    endif

    call complete_HS_results()

!  write(*,*) "Chisq_mu, Chisq_mu_LHCRun1, Chisq_STXS = ",Chisq_mu,Chisq_mu_LHCRun1,Chisq_STXS
!  write(*,*) "Chisq_mh, Chisq_mh_LHCRun1 = ",Chisq_mh,Chisq_mh_LHCRun1
!  write(*,*) "nobs, nobs_LHCRun1, nobs_STXS, Nparam = ", nobs, nobs_LHCRun1, nobs_STXS, Nparam
!  write(*,*) "Pvalue_tot = ", Pvalue_tot

!  Xi2 =  (Chisq + Chisq_LHCRun1 + Chisq_STXS)
 Xi2 =  Chisq_LHCRun1 + Chisq_STXS
 
!  write(*,*) 'finishing off ...'
  call finish_HiggsSignals

  write(*,*)'finished' 


end subroutine higgssignals
