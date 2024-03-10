function higgsbounds(rank) result(HBresult)


!! use usefulbits, only : theo,g2,tevR,res,deallocate_usefulbits,debug, &
!!                       inputmethod,HiggsBounds_info,ndat,n_additional,nH, &
!!                       allocate_dataset_parts, allocate_sqcouplratio_parts, &
!!                       allocate_tevextras_parts,whichanalyses,whichinput

!! use usefulbits, only : whichinput, whichanalyses, inputsub, theo, np, Hneut, Hplus, debug, &
!!                      inputmethod,HiggsBounds_info,file_id_debug1,file_id_debug2, analysislist

 use usefulbits, only : whichinput, whichanalyses, theo, np, Hneut, Hplus, debug, &
                      inputmethod,HiggsBounds_info,file_id_debug1,file_id_debug2, analysislist
    
 use input, only : setup_input,do_input
 use S95tables, only : setup_S95tables,deallocate_S95tables
 use theory_BRfunctions, only : setup_BRSM,deallocate_BRSM
 use channels, only : setup_channels,check_channels
 use output, only : do_output ,setup_output
!! use theo_manip, only : complete_theo
!! use parameters
!! use Model_Data_munuSSM3G

!#if defined(NAGf90Fortran)
! use F90_UNIX_IO, only : flush
!#endif

 implicit none

!! !Type(Susy_spec) :: Spectrum
!!Type(Flags) :: LFlags
!!Type(Input_params) :: munuMSSM
!!Type(Nuisance_params):: NuisPar
!!Type(Susy_spec) :: Spectrum
!!Type(Collider) :: Coll
!!Type(Error_Out) :: Err


 integer :: rank

!!--- HB output:
 integer :: nH, nHplus, HBresult, chan, ncombined  
 real(8) :: obsratio 
 logical :: invisible_lsp, just_after_run
 logical, save :: first = .true.
 debug = .false.


 inputmethod='datfile'    !(inputmethod='subrout' is also possible, but do not set that here)

  if(inputmethod.eq.'datfile' .and. first) call HiggsBounds_info

  first = .false.

  if(debug)then  
    open(file_id_debug2,file='debug_predratio.txt')
    open(file_id_debug1,file='debug_channels.txt')
  endif   
                             
!  write(*,*)'doing some preliminary tasks...'      ; call flush(6)
  call setup_input(rank)
  
  write(*,*) 'initializing HiggsBounds...'      ; call flush(6)
  call initialize_HiggsBounds(np(Hneut),np(Hplus),whichanalyses)
! endif

  write(*,*)'getting theoretical input...'          ; call flush(6)
  call do_input(rank)          
  
  write(*,*)'compare each data point to the experimental bounds...' ;call flush(6)                                
  call run_HiggsBounds(HBresult,chan,obsratio,ncombined)

! Spectrum%HBoutResults = HBresult
! Type(Susy_spec) :: Spectrum

!!HBoutResults,HSPvalue

  write(*,*)'beginning output...'                   ; call flush(6)
!  call do_output

 write(*,*)'finishing off...'                      ; call flush(6)
 call finish_HiggsBounds
      
 write(*,*)'finished HB'                              ; call flush(6)             


   write(*,*)'HBresult, obsratio  = ', HBresult, obsratio

 if(debug) then
  write(*,*)
  write(*,*)'*************    HiggsBounds Results  **************'
  write(*,*)
  write(*,*)'Is this parameter point excluded by either LEP'
  write(*,*)'or Tevatron data?'
        write(*,*) HBresult, ',  where'
  write(*,*)'               0 = yes, it is excluded'
  write(*,*)'               1 = no, it has not been excluded'
  write(*,*)'              -1 = invalid parameter set'
  write(*,*)
  write(*,*)'The process with the highest statistical sensitivity'
  write(*,*)'is'
        write(*,*) chan,'(see Key.dat)'
  write(*,*)'This process has a theoretical rate vs. limit of'
        write(*,*) obsratio
  write(*,*)
  write(*,*)'The number of Higgs which have contributed to the'
  write(*,*)'theoretical rate of this process was'
        write(*,*) ncombined
  write(*,*)
  write(*,*)'See HiggsBounds documentation for more information.'
  write(*,*)'****Q************************************************'
  write(*,*)
 endif


end function higgsbounds
