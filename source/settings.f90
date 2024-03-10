module settings
  use AMLutils
!  use Random
  use IniFile
  implicit none



!num_norm is number of calibration and normalization parameters (fast)
!num_initpower is the number of initial power parameters
!RTRT those two are irrelevant for SZ at the moment.
!num_hard is the number of `hard' parameters 

  !which version this is and info about packages
  character(LEN = 200), parameter :: version = 'MunuBayeS, v1.0.0, November 2015'
  character(LEN = 200), parameter :: rpv_ver         = 'RpV:        version 1.0'  
  character(LEN = 200), parameter :: ns_ver          = 'Nested sampling: version 2.13'
  character(LEN = 200), parameter :: like_ver        = 'Likelihood code: version 07/2012'

  integer, parameter :: num_hard = 42 !those are true SUSY params
  integer, parameter :: num_soft = 4 !nuisance params 
  !this is analogous to fast/slow split in cmb-case
  
  real, parameter :: propose_scale = 2.4
  real :: Temperature  = 1
  integer :: nprocs

  integer :: oversample_nuisance = 0
  integer :: sampling_method = 1
  integer :: procedure = 1
  integer :: action, previous_action

  !format output/input
  character(LEN = 200) :: fmt_params, fmt_susysp, fmt_rpv, fmt_coll, fmt_err

 !The rest are set up automatically
  integer, parameter :: num_params = num_hard + num_soft !max number allowe
  integer, dimension(:), allocatable :: params_used, nuis_params_used
  integer num_params_used, num_slow, num_nuis
  !num_params_used corresponds to the total number of used params
  !num_slow corresponds to how many num_hard are used
  !num_nuis corresponds to how many num_soft are used

  integer :: num_threads = 0
  integer :: instance = 0
  integer :: slow_proposals = 0, nuis_proposals = 0, num = 0, num_at_restart = 0

  integer :: logfile_unit  = 47
  integer, parameter :: outfile_unit = 48
  integer, parameter :: readin_file_unit = 58
  integer, parameter :: infofile_unit = 68, ppfile_unit = 78, infofile_ns_unit = 88, debug_unit = 25
  integer :: indepfile_unit = 0
  integer :: output_lines = 0
  integer, parameter :: sampling_metropolis = 1, slice_sampling_1d = 2
  logical :: slice_sampling
  integer :: pp_ns

  integer :: oversample_fast = 0
  integer :: directional_grid_steps = 20 

  integer, parameter :: logsteps = 250 !writes a log entry every logsteps trials

  real, parameter :: logZero = 1e30
  character (LEN =120) :: FileChangeIni = '', FileChangeIniAll = ''

  integer, parameter :: current_data = 1, synthetic_data = 2
  integer, parameter :: doMCMC = 0, doGRID = 4, doPP = 1, doNS = 5

  !MPI related variables
  integer :: MPIchains = 1, MPIrank = 0

  !plot related variables
  real :: caxis_min, caxis_max 
  character(LEN=150) :: plot_3D_psname = 'default', colormap_name = 'jet'

contains


 subroutine CheckParamChangeF(F)
   character(LEN=*), intent(in) ::  F
   logical bad, doexit

   if (F /= '') then

       call Ini_Open(F, tmp_file_unit, bad, .false.)
       if (bad) return
       Ini_fail_on_not_found = .false.
       doexit = (Ini_Read_Int('exit',0) == 1) 
       FeedBack = Ini_Read_Int('feedback',Feedback)
       num_threads = Ini_Read_Int('num_threads',num_threads)
       call Ini_Close
       if (F== FileChangeIni) call DeleteFile(FileChangeini)
       if (doexit) stop

   end if

 end subroutine CheckParamChangeF

 subroutine CheckParamChange

   call CheckParamChangeF(FileChangeIni)
   if (FileChangeIni/=FileChangeIniAll) call CheckParamChangeF(FileChangeIniAll)

 end subroutine CheckParamChange


 subroutine ReadMatrix(aname, mat, m,n)
   character(LEN=*), intent(IN) :: aname
   integer, intent(in) :: m,n
   real, intent(out) :: mat(m,n)
   integer j,k
   real tmp

   if (Feedback > 0) write(*,*) 'reading: '//trim(aname)
   call OpenTxtFile(aname, tmp_file_unit)

   do j=1,m
      read (tmp_file_unit,*, end = 200, err=100) mat(j,1:n)
   end do
   goto 120

100 rewind(tmp_file_unit)  !Try other possible format
   do j=1,m 
    do k=1,n
      read (tmp_file_unit,*, end = 200) mat(j,k)
    end do
   end do

120 read (tmp_file_unit,*, err = 150, end =150) tmp
   goto 200

150 close(tmp_file_unit)
    return

 200 write (*,*) 'matrix file '//trim(aname)//' is the wrong size'
     stop

 end subroutine ReadMatrix

  subroutine ReadVector(aname, vec, n)
   character(LEN=*), intent(IN) :: aname
   integer, intent(in) :: n
   real, intent(out) :: vec(n)
   integer j

   if (Feedback > 0) write(*,*) 'reading: '//trim(aname)

   call OpenTxtFile(aname, tmp_file_unit)

   do j=1,n
      read (tmp_file_unit,*, end = 200) vec(j)
   end do


    close(tmp_file_unit)
    return

 200 write (*,*) 'vector file '//trim(aname)//' is the wrong size'
     stop

 end subroutine ReadVector

 subroutine WriteVector(aname, vec, n)
   character(LEN=*), intent(IN) :: aname
   integer, intent(in) :: n
   real, intent(in) :: vec(n)
   integer j
  
   call CreateTxtFile(aname, tmp_file_unit)

   do j=1,n
      write (tmp_file_unit,'(1E15.5)') vec(j)
   end do

   close(tmp_file_unit)
  
 end subroutine WriteVector

  subroutine Matrix_Write(aname, mat, forcetable)
   character(LEN=*), intent(in) :: aname
   real, intent(in) :: mat(:,:)
   logical, intent(in), optional :: forcetable
   integer i,k
   character(LEN=50) fmt
   integer shp(2)
   logical WriteTab

   shp = shape(mat)
   WriteTab = shp(2)<=50
   if (present(forcetable)) then
     if (forcetable) WriteTab = .true.
    end if

   call CreateTxtFile(aname, tmp_file_unit)
   fmt = trim(numcat('(',shp(2)))//'E15.5)'
   do i=1, shp(1)
     if (.not. WriteTab) then
      do k=1, shp(2)
       write (tmp_file_unit, '(1E15.5)') mat(i,k)
      end do
     else
      write (tmp_file_unit, fmt) mat(i,1:shp(2))
     end if
   end do

   close(tmp_file_unit)

 end subroutine Matrix_Write


 subroutine WriteSqMatrix(aname, mat,n)
   character(LEN=*), intent(in) :: aname
   integer, intent(in) :: n
   real, intent(in) :: mat(n,n)
   integer i
   character(LEN=50) fmt

   call CreateTxtFile(aname, tmp_file_unit)
   fmt = trim(numcat('(',n))//'E15.5)'
   do i=1, n
      write (tmp_file_unit, fmt) mat(i,1:n)
   end do

   close(tmp_file_unit)

 end subroutine WriteSqMatrix


       subroutine Diagonalize(m, diag, n)
          !Does m = U diag U^T, returning U in m
          integer, intent(in) :: n
          real m(n,n), diag(n)
          integer ierr, tmpsize
          real tmp(3*n**2)

          tmpsize = 3*n**2
          call SSYEV('V','U',n,m,n,diag,tmp,tmpsize,ierr) !evalues and vectors of symmetric matrix
        
       end subroutine Diagonalize


  subroutine Matrix_Inverse(M)
   !This should not be used in real situations, but useful for quick testing
     real, intent(inout):: M(:,:)
     real w(Size(M,DIM=1)),tmp(Size(M,DIM=1),Size(M,DIM=1))
     integer i, n

     n=Size(M,DIM=1)
     if (n<=1) return
     if (Size(M,DIM=2)/=n) stop 'Matrix_Inverse: non-square matrix'

     call Diagonalize(M,w,n)      
     do i=1, n
        tmp(i,:) = M(:,i)/w(i)
     end do
     M = matmul(M,tmp)     

  end subroutine Matrix_Inverse



end module settings

