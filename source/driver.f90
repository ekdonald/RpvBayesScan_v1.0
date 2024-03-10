!     Code for RPV model spectrum  
!     by R. Ruiz  
!     See readme.html for documentation. 
!     This is a sample driver routine that reads
!     in one set of parameters and produdes the corresponding output. 

program MunuBayeS


!  use iflport

  use mc_rpv, only:  burn_in, mcmcsample      !Monte Carlo routines to implement SUSY params & like
  use postprocess  !Post Processing of chains
  use ParamDef
  use likedata
  use nestwrapper

  implicit none
        
  character(LEN=80) :: InputFile, LogFile
  character(LEN = 200) :: HeaderLine
  logical :: bad
  integer :: numsets, i, seedsb
  character(LEN=150) :: rootname, rootname_ns, out_rootname, out_rootname_ns, &
               filename, infoname, infoname_ns, model, numstr, nprocstr, &
               seedstr, debug_numstr, debug_filename
  Type(ParamSet) :: Params, EstParams
  integer :: num_points
  integer :: ierror, status
  real :: delta_loglike
  character(LEN = 5000) :: InLine
  real :: tmult, tlike
  real(8) :: intime
  integer :: myrank, rc, debug_instance = 0 
 !global timing purposes
  integer :: lines_file_unit = 27, lines
  integer :: MC_Hertz,  MC_Begin_Clock, MC_End_Clock, MC_ms
  character(12) :: MC_Elapsed_Time 

  integer :: istat


  !default values for non-MPI runs (single chain)
  myrank = 0
  nprocs = 1
  instance = 0

 
#ifdef MPI

        call mpi_init(ierror)
        if (ierror/=MPI_SUCCESS) then
           print *,'Error starting MPI program. Terminating.'
           call MPI_ABORT(MPI_COMM_WORLD, rc, ierror)
        end if

        call MPI_COMM_RANK(MPI_COMM_WORLD, myrank,  ierror)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierror)
        if (Feedback >0) write(*,*) 'Number of processors or tasks=', &
             & nprocs,' My rank= ',myrank

        rankg = myrank + 1
#endif


  !The master process gets the file (MPI or not)
!  if (myrank .eq. 0) then
     write(*,*) '*****************************************************'
     write(*,*) 'Welcome to '//trim(version)
     write(*,*) 'Currently linked packages and versions: '
     write(*,*) trim(rpv_ver)
     write(*,*) trim(like_ver)
     write(*,*) '*****************************************************'
     InputFile = GetParam(1)
     if (InputFile == '') call DoStop('No parameter input file')
     write(*,*)  'Read InputFile: ', trim(InputFile)
!  end if

 
!#ifdef MPI
!  call MPI_BCAST(InputFile,79,MPI_CHARACTER,0,MPI_COMM_WORLD,ierror)
!  call MPI_BARRIER(MPI_COMM_WORLD,ierror)
!#endif  

 
!  InputFile = 'inifiles/SampleIniFile.ini' 

  !every processor reads in the InputFile
  call Ini_Open(InputFile, 1, bad, .false.)
  if (bad) call DoStop('Error opening parameter file: '//trim(InputFile))

#ifdef MPI
!  call MPI_BCAST(InputFile,79,MPI_CHARACTER,0,MPI_COMM_WORLD,ierror)
  call MPI_BARRIER(MPI_COMM_WORLD,ierror)
#endif 

  

  Ini_fail_on_not_found = .false.
  rootname = Ini_Read_String('file_root')

  !-----------------------------------
  ! Reading in of settings
  !-----------------------------------

  ! ---- Feedback Level and what to compute ----
  FeedBack = Ini_Read_Int('feedback',0)
  write(*,*) 'Feedback is ', Feedback

  seedstr = Ini_Read_String('rand_seed')
  if (seedstr /= '') then
     read(seedstr,*) seedsb
     call InitRandom(seedsb)
  else
     seedsb = 0
     call InitRandom()
  end if

  if(feedback .gt. 4) then 

    debug_instance = myrank + 1 !start at 1 for chains
  
    write (debug_numstr,*) debug_instance
    debug_filename = trim(adjustl(debug_numstr))

    !call CreateTxtFile(trim(debug_filename)//'.txt', debug_unit)
    !call OpenTxtFile(trim(debug_filename)//'.txt',debug_unit)
    open(unit=debug_unit,file=trim(debug_filename),status='unknown')

  endif


  ! ---- Action type ----
  action = Ini_Read_Int('action',doMCMC)

  if (action == doMCMC .or. action == doPP) then

#ifdef MPI

   instance = myrank + 1 !start at 1 for chains
   write (numstr,*) instance
   write (nprocstr,*) nprocs 

   MPI_StartTime = MPI_WTime()

#endif

  endif

  propose_grid = .false. 

  if(action .eq. doGRID) propose_grid = .true.

  !for a restart, the continuation is always appended to the previous files
  !currently restart only works for MCMC and GRID modes
  !MultiNest has its own restart files
  restart = .false.

  if ((action .eq. doMCMC) .or. (action .eq. doGRID) ) then
        restart = Ini_Read_Logical('restart_and_continue')
        numtoget = Ini_Read_Int('samples')
        out_rootname = rootname
        infoname = rootname
  else if (action .eq. doNS) then
     	!set up MultiNest parameters
	nest_mmodal = Ini_Read_Logical('multimodal',.true.)
	nest_ceff = Ini_Read_Logical('ceff',.true.)
	nest_nlive = Ini_Read_Int('nlive',2000)
	nest_nCdims = Ini_Read_Int('nCdims',2)
	nest_maxmodes = Ini_Read_Int('maxmodes',10)
	nest_efr = Ini_Read_Double('eff',0.5d0)
	nest_tol = Ini_Read_Double('tol',0.5d0)
	nest_root = rootname

        out_rootname = rootname
        infoname = rootname 
	!set up the seed for the MultiNest
	if (seedsb .ne. 0) then
          nest_seed = seedsb
	else
          nest_seed = -1 !take it from system clock
	end if
	!feedback for the MultiNest
	if(FeedBack>0) then
	  nest_fb = .true.
	else
          nest_fb = .false.
	end if
	restart_multinest = Ini_Read_Logical('restart_and_continue',.true.)
        !inquire(file=trim(rootname)//'resume.dat',exist=resume_exists)
	restart = .false.
  else if (action .eq. doPP) then
     postproc = .true.
     !gets version of SB with which the chains have been created             
     !to ensure back compatibility                                           
     open(unit=1,file=trim(rootname)//'.info',form='formatted',status='old')
     read(1,'(a)') HeaderLine
     close(1)

     !post processing: do you want to recompute the theory?
     redo_theory =  Ini_Read_Logical('redo_theory')
     !post processing: do you want to recompute the likelihood?
     redo_like = Ini_Read_Logical('redo_like')
     !offset like by that value
     redo_likeoffset = Ini_Read_Real('redo_likeoffset', 0.0)
     if (redo_theory) redo_like = .true.
     if (.not. redo_theory .and. .not. redo_like) call DoStop('Nothing to do in postprocessing!')
     if(.not. redo_theory .and. redo_like) & 
          write(*, *) 'Warning: if you only recompute the likelihood &
            the compute flags must match with the ones used for the old &
            chains ( ie. look at'//trim(rootname)//'.info file)'       
     !only changing likelihoods  not weights
     redo_change_like_only =  Ini_Read_Logical('redo_change_like_only',.false.)
     if(redo_change_like_only) &
          write (*,*) 'Warning: only changing likelihoods not weights'
     if (redo_theory) redo_like = .true.
     !you'd better recompute the like if you changed your predictions!
     out_rootname = Ini_Read_String('out_root')     
     skip_lines = Ini_Read_Int('skip_lines', 0)
     infoname = out_rootname
     filename = rootname
     infoname_ns = rootname
     rootname_ns = rootname
     out_rootname_ns = out_rootname
  else
     out_rootname = rootname
     infoname = rootname 
     restart = .false.
  end if

  ! ---- MPI file names ----
  if(action .eq. doMCMC) then

   if (instance /= 0) then
     rootname = trim(rootname)//'_'//trim(adjustl(numstr))
     if (out_rootname .ne. '') out_rootname = trim(out_rootname)//'_'//trim(adjustl(numstr))
   end if

   if (action .eq. doMCMC .and. feedback > 0) then
     write(*,*) 'This is chain ', instance
     write(*,*) 'Writing to file: ', trim(rootname)
   end if !feedback if

  endif


  if(action .eq. doPP) then
    
   close(1)

   call Ini_Open(trim(infoname_ns)//'.info',infofile_ns_unit, bad, .false.)

   if (bad) call DoStop ('Problem loading up file: '//trim(infoname_ns)//'.info')
   !use this to enforce consistency between saved values of chi-squared (different for MCMC and NS)
   pp_ns  = Ini_Read_Int('action')

   close(infofile_ns_unit)

   if(pp_ns == 5 .and. instance /= 0) then
#ifdef MPI
     if(myrank == 0) then
      istat = system('./manipulate_files.pl 1 '//trim(adjustl(nprocstr)) &
      //' '//adjustl(trim(rootname))//' '//adjustl(trim(out_rootname)))
      if(istat /= 0) call DoStop('Something wrong with the call to perl script to split NS chain')
     endif
     call MPI_BARRIER(MPI_COMM_WORLD, ierror)
#endif
   endif 

   if (instance /= 0) then
     rootname = trim(rootname)//'_'//trim(adjustl(numstr))
     if (out_rootname .ne. '') out_rootname = trim(out_rootname)//'_'//trim(adjustl(numstr))
   end if

   call CreateTxtFile(trim(out_rootname)//'.txt', outfile_unit)
   call OpenTxtFile(trim(rootname)//'.txt',readin_file_unit) 
   call OpenTxtFile(trim(out_rootname)//'.txt',outfile_unit)

!   if (pp_ns == 5 .or. instance /= 0) then
!     call CreateTxtFile(trim(out_rootname)//'.txt', outfile_unit)
!     call OpenTxtFile(trim(rootname)//'.txt',readin_file_unit) 
!     call OpenTxtFile(trim(out_rootname)//'.txt',outfile_unit)
!   else
!     call OpenTxtFile(trim(rootname)//'.txt',readin_file_unit)   
!   endif

   if (feedback > 0) then
     write(*,*) 'Post-processing chain: ', trim(rootname)
     write(*,*) 'Writing to file: ', trim(out_rootname)
   endif

   !every processor reads in the InputFile
   call Ini_Open(InputFile, 1, bad, .false.)
   if (bad)  call DoStop('Error opening parameter file: '//trim(InputFile))

  endif

  ! --- Observables choice ----
  GFlags%Collider_predict =  Ini_Read_Logical('Collider_predict')

  !opens/creates logfile
  LogFile = trim(out_rootname)//'.log'
  logfile_unit = 49

  if (.not. restart) then
    call CreateTxtFile(LogFile,logfile_unit)
  else !in case of restart
    call OpenFileToAppend(LogFile, logfile_unit,'formatted')
    write(logfile_unit, '(A)') "----- Chain restarted -----"
    call FlushFile(logfile_unit)
  end if

  if (.not. restart) then
     !creates new .txt files for MCMC/GRID/PP
     if (action .eq. doMCMC .or. action .eq. doGRID) call CreateTxtFile(trim(out_rootname)//'.txt', outfile_unit)
  else
     !saves the last line of previous file in RestartLine
     !then used in paramdef.f90, Initialize routine
        if (action .eq. doPP) call DoStop('Sorry - restarting while post-processing not yet supported!')
        InLine = ''
        call OpenTxtFile(trim(rootname)//'.txt',tmp_file_unit)
        do
           read(tmp_file_unit,'(a)', iostat=ierror) InLine
           if (ierror < 0) exit
           read(InLine, *) tmult, tlike
           if (tlike < MaxLike) MaxLike = tlike
           num = num + tmult
           num_accept = num_accept+1
        end do
        close(tmp_file_unit)

        RestartLine = InLine
        num_accept = num_accept+1
        if (num_accept .ge. numtoget) call DoStop('No restart necessary - sampling already completed!')
        num_at_restart = num !used for timing purposes
        !open the file for further writing
        call OpenFileToAppend(trim(rootname)//'.txt', outfile_unit,'formatted')
        if (Feedback > 0) then
           write(*,*) 'Restarting from files ', trim(rootname)
           write(*,*) 'Resuming from:'
           write(*,*) '  num        = ', num
           write(*,*) '  num_accept = ', num_accept
           write(*,*) '  MaxLike    = ', real(MaxLike)
        end if
  end if


  Ini_fail_on_not_found = .false.
  burn_in = Ini_Read_Int('burn_in',0)	

  ! --- Sampling method ---- 
  if (action .eq. doMCMC) then
     !use_nuisance_splitting = Ini_Read_Logical('Use_nuisance_splitting', .false.)
     use_nuisance_splitting = .false.
     oversample_nuisance = Ini_Read_Int('oversample_nuisance',1)
     !sampling_method = Ini_Read_Int('sampling_method', 1)
     sampling_method = 1
     Temperature = Ini_Read_Real('temperature',1.)

     if (sampling_method > 1) then
        !this needs testing !
        slice_sampling = .true.
        procedure = Ini_Read_Int('slicing_procedure')
        call doStop('Sorry - slice sampling untested for the moment')
     end if
  end if

  ! --- Parameterization choice ---- 
  !whether to use log scale 
  use_log = Ini_Read_Logical('use_log')

  ! --- Nuisance Parameters ----
  Use_Nuisance = Ini_Read_Logical('Use_Nuisance',.true.)
  !
  Use_Leptons = Ini_Read_Logical('Use_Leptons',.true.)
  !
  Use_Neutrinos = Ini_Read_Logical('Use_Neutrinos',.true.)
  !
  Use_Higgs = Ini_Read_Logical('Use_Higgs',.true.) 

  Use_LEP = Ini_Read_Logical('Use_LEP',.false.)
  !
  Use_LHC = Ini_Read_Logical('Use_LHC',.false.)
  !
  Use_Anomalous_Mu = Ini_Read_Logical('Use_Anomalous_Mu',.false.)
  Use_bsgamma = Ini_Read_Logical('Use_bsgamma',.false.)
  Use_Bsmumu = Ini_Read_Logical('Use_Bsmumu',.false.)
  Use_Bdmumu = Ini_Read_Logical('Use_Bsmumu',.false.)
  Use_RBtaunu= Ini_Read_Logical('Use_RBtaunu',.false.)
  Use_Dmunu= Ini_Read_Logical('Use_Dmunu',.false.)
  Use_Dsmunu= Ini_Read_Logical('Use_Dsmunu',.false.)
  Use_DeltaMbs = Ini_Read_Logical('Use_DeltaMbs',.false.)
  Use_DeltaMbd = Ini_Read_Logical('Use_DeltaMbd',.false.)
  Use_epsk = Ini_Read_Logical('Use_epsk',.false.)
  Use_mueg = Ini_Read_Logical('Use_mueg',.false.)
  Use_taueg = Ini_Read_Logical('Use_taueg',.false.)
  Use_taumug = Ini_Read_Logical('Use_taumug',.false.)
  Use_mue3 = Ini_Read_Logical('Use_mue3',.false.)
  Use_tau3e = Ini_Read_Logical('Use_tau3e',.false.)

  use_data = Ini_Read_Int('use_data')

  if (Feedback > 1 .and. myrank .eq. 0) then
     if (Use_Neutrinos) write(*,*) 'Including Neutrino data'  
  endif

  if (Feedback > 1) then
     if (Use_LEP) write(*,*) 'Using LEP bounds on Higgs and Chargino Masses'
     if (Use_LHC) write(*,*) 'Using LHC bounds'
     if (Use_Higgs) write(*,*) 'Including Higgs data' 
     if (Use_Neutrinos) write(*,*) 'Including Neutrino data'  
     if (Use_Anomalous_Mu) write(*,*) 'Including Mu Anomalous magnetic moment data'
     if (Use_bsgamma) write(*,*) 'Including b -> s gamma decay data'
     if (Use_Bsmumu) write(*,*) 'Including B_s -> mu+mu- BR bound'
     if (Use_Bdmumu) write(*,*) 'Including B_d -> mu+mu- BR bound'
     if (Use_DeltaMBs) write(*,*) 'Including B_s - bar(B)_s oscillations'
     if (Use_DeltaMBd) write(*,*) 'Including B_d - bar(B)_d oscillations'
     if (Use_Dsmunu) write(*,*) 'Including Ds -> mu nu data'
     if (Use_Dmunu) write(*,*) 'Including D -> mu nu data'
     if (Use_RBtaunu) write(*,*) 'Including RBR(Bu -> tau nu) decay data'
     if (Use_epsk) write(*,*) 'Including epsk data'
     if (Use_mueg) write(*,*) 'Including mu -> e gamma data'
     if (Use_taueg) write(*,*) 'Including tau -> e gamma data'
     if (Use_taumug) write(*,*) 'Including tau -> mu gamma data'
     if (Use_mue3) write(*,*) 'Including mu -> 3e data'
     if (Use_tau3e) write(*,*) 'Including tau -> 3e data'
  end if

  If ((Use_Anomalous_Mu .or. Use_bsgamma &
       .or. Use_DeltaMBs .or. Use_RBtaunu &
       .or. Use_Dsmunu .or. Use_Dmunu) &
       .and. (.not. GFlags%Collider_predict)) then 
     call DoStop('You cannot use collider data if you dont set &
      & compute_Collider_Predictions = T')
  end If


Write(*,*) 'before initialize '

  Ini_fail_on_not_found = .true.

  call Initialize(Params)

Write(*,*) 'end initialize '
  
 call InitializeDataSets

  call Ini_Close


  !PS bug fix 2011-06-23 moved from postprocess.f90 to ensure previous_action is correctly set
  !this determines which derived variables where 
  !saved in the input chain
  if (action .eq. doPP) then    
   if (Feedback > 1) write(*,*) 'Reading flags from: ', trim(filename)//'.info'
    call LoadOldFlags(trim(filename)//'.info')
    if (Feedback > 1) write(*,*) 'Previous info read in'
  endif

  !creates new .info file, regardless if you have restarted or not
  call CreateTxtFile(trim(infoname)//'.info', infofile_unit)
  !saves labels in .info file
  call SetFormat
  !saves flags in .info file
  call SaveFlags(infofile_unit)

  close(infofile_unit)

  num_points = 0
  call StartTiming(MC_Hertz,MC_Begin_Clock)          

  if (action .eq. doMCMC .or. action .eq. doGRID) then
     !-----------------------------------
     ! Starts MCMC or grid
     !-----------------------------------	
     if (Feedback > 0) then
        if (action .eq. doMCMC) then
           write (*,*) 'starting Monte-Carlo'
           write (*,*) 'Sampling method : ', sampling_method
           if (slice_sampling) write(*,*) 'Slicing procedure: ', procedure
        end if
        if (action .eq. doGRID) then
           write(*,*) 'starting with grid scan - all physical points &
          & will be saved'
        end if
     end if

     call MCMCSample(Params, numtoget)

     write (*,*)'Finished'
     write(*,*) 'Chain ', instance, ': Fct  evalutations: ', num
     
  else if (action .eq. doPP) then
     !-----------------------------------
     ! Starts post-processing
     !-----------------------------------
     !computes the number of samples in the chains
     lines_file_unit = 38	
     call CreateTxtFile(trim(rootname)//'_lines', lines_file_unit)
     istat = system ('wc -l '//trim(rootname)//'.txt | awk'//" '{ print $1 }' >"//trim(rootname)//"_lines")
     if(istat /= 0) call DoStop('Something wrong with PP procedure')
     call OpenTxtFile(trim(rootname)//'_lines',lines_file_unit)
     read(lines_file_unit,*) lines
     close(lines_file_unit)
     istat = system('rm '//trim(rootname)//'_lines')
     if(istat /= 0) call DoStop('Something wrong with PP procedure')
     if (Feedback > 0) write (*,*) 'starting post processing'

     Call PostProcChains(trim(filename)//'.info', readin_file_unit, lines)
     close(readin_file_unit)

     write (*,*) 'Post-processing finished (chain ', instance, ')'

     if(pp_ns == 5 .and. instance /= 0) then
      !run the split scr
#ifdef MPI
      call MPI_BARRIER(MPI_COMM_WORLD, ierror)
      if(myrank == 0) then
       istat = system('./manipulate_files.pl 2 '//trim(adjustl(nprocstr)) &
        //' '//adjustl(trim(rootname_ns))//' '//adjustl(trim(out_rootname_ns)))
       if(istat /= 0) call DoStop('Something wrong with the call to perl script to join temporary files')
       istat = system('./manipulate_files.pl 3 '//trim(adjustl(nprocstr)) &
       //' '//adjustl(trim(rootname_ns))//' '//adjustl(trim(out_rootname_ns)))
       if(istat /= 0) call DoStop('Something wrong with the call to perl script to delete temporary files')
      endif
#endif

     endif


  else if(action .eq. doNS) then
   
  !-----------------------------------
  ! Starts MultiNest
  !-----------------------------------

   
   call nest_sample     

  else
     call DoStop('You have chosen an undefined action - exiting ')
  end if

  !-----------------------------------
  ! Wrapping up loose ends
  !-----------------------------------
  if (indepfile_unit .ne. 0) close(indepfile_unit)
  if (outfile_unit .ne. 0) close(outfile_unit)

  call StopTiming(MC_Begin_Clock, MC_End_Clock, MC_Hertz, MC_Elapsed_Time, MC_ms)
  if(action .eq. doMCMC .or. action .eq. doPP) then

   write(*,*) 'Total elapsed time: ', trim(MC_Elapsed_Time)

   if (logfile_unit /=0) then
     write(logfile_unit, '(A)') '***********************************'
     write(logfile_unit, '(A)') '       Run completed             '
     write(logfile_unit, '(A)')'Total running time for this chain (hrs:min:sec:ms): '
     write(logfile_unit, '(A)') trim(MC_Elapsed_Time)
     close(logfile_unit)
   end if

   write(*,*) 'I am chain', instance, ' and I have finished my run - waiting for other chains to finish'

  else if(action .eq. doNs) then
   
   if(myrank .eq. 0) then
    if (logfile_unit /=0) then
     write(logfile_unit, '(A)') '***********************************'
     write(logfile_unit, '(A)') '       Run completed           '
     write(logfile_unit, '(A)') trim(MC_Elapsed_Time) !! DK 18.02.16
     close(logfile_unit)
    endif
   endif

  endif


  call DoStop
             
  
end program MunuBayeS


