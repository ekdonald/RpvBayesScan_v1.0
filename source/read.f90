program GetPlots

        use settings
        use IniFile

        implicit none

        integer, parameter :: gp = KIND(1.d0)

       
        real(gp), dimension(:,:), target, allocatable :: coldata, coldata2  !(col_index, row_index)

        
        integer, parameter :: max_rows = 2000000
        integer, parameter :: max_cols = 200
        integer, parameter :: max_chains = 300   
        integer, parameter :: max_failures = 10
 
        character(LEN=80) rootname
        character(LEN=1000) InputFile, numstr, InfoFile, numstr1, numstr2
        character(LEN=30000) InLine
        real invars(max_cols), invars2(max_cols) 
        real*8 :: highprec(max_cols)
        integer chain_indices(max_chains), num_chains_used, chain_num, first_chain

        integer:: ix, ix1, i, ios, failures = 0
        integer nrows, ncols,nmapped, norig

        logical bad

        character(LEN=1000) filename, infile, in_root,out_root, sm_file, chain_file
        integer :: chain_ix

        logical :: NS_files = .false.

        integer :: ignorerows  
        integer chain_numbers(max_chains)

        !InputFile = GetParam(1)
        !if (InputFile == '') stop 'No parameter input file'

        !Gets chains filename
        !call Ini_Open(InputFile, 1, bad, .false.)
        !if (bad) stop 'Error opening parameter file'
        !Ini_fail_on_not_found = .true.
        !in_root = Ini_Read_String('file_root')
        !rootname = ExtractFileName(in_root)
        !call Ini_Close
        

       !Opens .info file to read lables from it
        !ncols set automatically
        !InfoFile = trim(in_root)//'.info'
        !write(*,*) 'Opening .info file: ', trim(InfoFile)
        !call Ini_Open(InfoFile, 1, bad, .false.)
        !if (bad) then
        !   write(*,*) 'Stop - problem opening file: ', trim(InfoFile)
        !   stop
        !end if
        ncols = 114    !Ini_Read_Int('params_saved')
        write(*,*) 'Number of parameters in MCMC chain:' , ncols
        


        
        call Ini_Close     
        
        
        write(*,*) 'Allocation'
        allocate(coldata(ncols,0:max_rows))


        
        !write (*,*) 'producing files with root '//trim(rootname)
        !call CreateDirectory('output_files/'//trim(out_root),err)


                !Read in the chains     
        nrows = 0
        num_chains_used =0

        infile = "chains/vc_tanb_m2_vev"

        open(unit=300,file=trim(infile)//'r.txt',form='formatted',status='new', iostat=ios)

        do chain_ix = 1, 4

          write (numstr,*)  chain_ix


           write (*,*) 'Trying to read ' // trim(infile)//'_'//trim(adjustl(numstr))//'.txt'
           failures = 0
           ios = 0
           open(unit=50,file=trim(infile)//'_'//trim(adjustl(numstr))//'.txt',form='formatted',status='old', iostat=ios)
!           open(unit=300,file=trim(infile)//'r.txt')

           write(*,*) 'Chain found, please wait...'



           chain_indices(num_chains_used) = nrows
           chain_numbers(num_chains_used) = chain_ix
           do
12            read (50,'(a)',end=1) InLine
              read(InLine,*,end=1, err=2) highprec(1:ncols)
              invars(1:ncols) = highprec(1:ncols)
              !write(*,*) 'here:', invars(1), highprec(1)
              !write(*,*) 'Mapping params'
              !need to reconvert alpha_EM
              !invars(9+2) = (invars(9+2) + alpha_inv_mean)         

              !if (map_params) call MapParameters(invars)
              !if (nmapped > 0) call AddParameters(invars)
              
              nrows = nrows + 1
              if (nrows > max_rows) stop 'need to increase max_rows'

              if(any(invars(100:114) == 1.)) cycle
              !if(invars(113) == 1. .or. invars(114) == 1.) cycle
              write(300,'(114E20.12)')  invars(1:ncols)



           end do
2          write (*,*) 'error reading line ', nrows + ignorerows, ' - trying to continue...'
           write(*,*) 'Offending line ', trim(InLine)
           failures = failures+1
           nrows = nrows + 1
           if (failures<max_failures  ) then
              goto 12
           else
              write(*,*) 'Too many errors, skipping the rest of the file'   
           end if
1          close(50)
           goto 30
28         write (*,'(" chain ",1I4," missing")') chain_ix
30      end do



       if (nrows == 0) stop 'No un-ignored rows!'



!add a column to original

!        open(unit=300,file=trim(infile)//'r.txt')     
         


!        do i = 0, nrows-1

!         write(300,'(114E20.12)') coldata(1:ncols-1,i)      
         
!        enddo 

        close(300)

end program  GetPlots
