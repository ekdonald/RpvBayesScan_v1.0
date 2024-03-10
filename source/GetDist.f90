module MCSamples
 use settings
 implicit none

        integer, parameter :: gp = KIND(1.d0)

        real(gp), dimension(:,:), target, allocatable :: coldata  !(col_index, row_index)
        integer, dimension(:,:), target, allocatable :: errdata 
        integer, dimension(:), allocatable :: thin_ix  
        integer thin_rows

        integer, parameter :: max_rows = 500000
        integer, parameter :: max_cols = 500
        integer, parameter :: max_chains = 100
        integer, parameter :: max_split_tests = 4 

        integer chain_indices(max_chains), num_chains_used

        integer nrows, ncols, nerrcols, nincols
        real numsamp
        integer ND_cont1, ND_cont2
        integer covmat_dimension
        integer colix(max_cols), num_vars  !Parameters with non-blank labels
        real mean(max_cols), sddev(max_cols)
        real, dimension(:,:), allocatable :: corrmatrix
        character(LEN=120) rootname
        character(LEN=240) title
        character(LEN=120) labels(max_cols)
        logical has_limits(max_cols), has_limits_top(max_cols),has_limits_bot(max_cols)
        integer :: tails(max_cols)
        integer num_contours 
        real matlab_version
        real contours(2), contour_levels(2)
        real mean_mult, max_mult
        integer :: indep_thin = 0 
        integer chain_numbers(max_chains)
        logical isused(max_cols)
        logical force_twotail 
        logical smoothing, plot_meanlikes, plot_NDcontours, gplot
        logical shade_meanlikes, make_single_samples
        integer single_thin,cust2DPlots(max_cols**2)
        integer ix_min(max_cols),ix_max(max_cols)
        real center(max_cols),width(max_cols)   
        real, dimension(:,:), allocatable :: TheBins, bins2D, bin2Dlikes, bin2Dmax
        real meanlike, maxlike, maxmult
        integer GridDim(max_cols)
        logical BW,do_shading
        real(gp), dimension(:,:), target, allocatable :: coord
        character(LEN=80) ComparePlots(20)
        integer Num_ComparePlots
        logical ::   prob_label = .false.

            
contains

  subroutine AdjustPriors
   !Can adjust the multiplicity of each sample in coldata(1, rownum) for new priors
   !Be careful as this code is parameterisation dependent
   integer i
   real tau, chisq 

!   stop 'You need to write the AdjustPriors subroutine in GetDist.F90 first!'
  
   write (*,*) 'Adjusting priors'
   do i=0, nrows-1
!E.g. tau prior
      tau = coldata(4+2,i)
      chisq = (tau - 0.1)**2/0.03**2
      coldata(1,i) = coldata(1,i)*exp(-chisq/2) 
      coldata(2,i) = coldata(2,i) + chisq/2
     
   end do
   

  end subroutine AdjustPriors

  subroutine DeleteZeros
    integer i,ii
    ii=0
    do i=0, nrows-1
     if (coldata(1,i)/=0) then
       coldata(:,ii) = coldata(:,i)
       ii=ii+1
     end if
     end do
     if (ii==0) stop 'Prior has removed all models!'
     if (ii /= nrows) write (*,*) 'Prior removed ',nrows-ii,'models'
     nrows = ii
          
  end subroutine DeleteZeros


 subroutine MapParameters(invars)
    real invars(1:ncols)
! map parameters in invars: eg. invars(3)=invars(3)*invars(4) 

!    invars(2+13)=invars(17+2)*exp(-invars(2+4))
   
    stop 'Need to write MapParameters routine first'  
  
  end subroutine MapParameters


  subroutine MakeSingleSamples(single_thin)

  !Make file of weight-1 samples by choosing samples with probability given by their weight

    integer i, single_thin
    character(LEN=20) :: fmt_data, fmt_err, fmt

    
    real maxmult
    Feedback = 0          
  
    fmt_data = trim(numcat('(',ncols))//'E15.5)'
    fmt_err = trim(numcat('(',nerrcols+1))//'I3,)'
    fmt = '('//trim(fmt_data)//trim(fmt_err)//')'
    open(unit=50,file=trim(rootname)//'_single.txt',form='formatted',status='replace')
    open(unit=60,file=trim(rootname)//'_single_err.txt',form='formatted',status='replace')


!    do i= 0, nrows -1
!     write (50,fmt_data) coldata(1:ncols,i)
!    end do

    do i= 0, nrows -1
     if (any(errdata(1:nerrcols,i) /= 0)) then
       write (60,fmt) coldata(1:ncols,i), errdata(1:nerrcols,i)
     else
      write (50,fmt_data) coldata(1:ncols,i)
     endif
    end do         
    close(50)

    



  end subroutine MakeSingleSamples



  subroutine Make1DSamples(col1,col2)


    integer i, col1, col2
    character(LEN=20) :: fmt_data, fmt_err, fmt
    character(LEN=120) :: plotfile


    Feedback = 0          
    fmt_data = trim(numcat('(',2))//'E15.5,)'
    fmt_err = trim(numcat('(',nerrcols+1))//'I3,)'
    fmt = '('//trim(fmt_data)//trim(fmt_err)//')'
    plotfile = numcat(trim(numcat(trim(rootname)//'_',col1))//'_',col2)
    open(unit=40,file='plot_data/'//trim(plotfile)//'.dat',form='formatted',status='replace')

     do i = 0, nrows-1
       write (40,fmt) coldata(col1, i) , coldata(col2,i), errdata(1:nerrcols,i),sum(errdata(1:nerrcols,i))
     enddo

  end subroutine Make1DSamples

  subroutine Make2DSamples(col1,col2,fixed)

  !Make file of weight-1 samples by choosing samples with probability given by their weight

    integer i, j, col1, col2, fixed
    integer, dimension(20) :: rang
    character(LEN=20) :: fmt_data, fmt_err, fmt
    character(LEN=120) :: plotfile

!    real, dimension(:) :: coord     

!    allocate(coord(11))

    Feedback = 0          
    fmt_data = trim(numcat('(',3))//'E15.5,)'
    fmt_err = trim(numcat('(',nerrcols+1))//'I3,)'
    fmt = '('//trim(fmt_data)//trim(fmt_err)//')'
    plotfile = numcat(trim(numcat(trim(rootname)//'_',col1))//'_',col2)
    open(unit=40,file='plot_data/'//trim(plotfile)//'.dat',form='formatted',status='replace')


   !ruiz: fix dimensions
   do i = 1, num_params 
    if(isused(i) .and. i /= col1) fixed = i
   end do  


   if(coldata(col1, 1) == coldata(col1, 2)) then 
     j = 0
!     do i = 1, 11
      do i = 1, gridDim(fixed)
      do while(j <= gridDim(col1)-1)
       if (i == 1) then
        rang(j) = j * (gridDim(fixed)-1+ i) 
        write (40,fmt) coldata(col1, rang(j)) , coldata(col2,rang(j)),  coldata(fixed,rang(j)), errdata(1:nerrcols,rang(j)),sum(errdata(1:nerrcols,rang(j))) 
       else
        write (40,fmt) coldata(col1, rang(j)+i-1) , coldata(col2,rang(j)+i-1),  coldata(fixed,rang(j)+i-1), errdata(1:nerrcols,rang(j)+i-1), sum(errdata(1:nerrcols,rang(j)+i-1))      
       endif
       j = j + 1     
!      coord(i) =  coldata(fixed,rang(j)+i-1)
      enddo
      j = 0
      write (40,*)
     enddo
   else
     j = 1
     do i= 0, nrows-1
      if(coldata(fixed, i) == coldata(fixed, i+1)) then
       write (40,fmt) coldata(col1, i) , coldata(col2,i),  coldata(fixed,i), errdata(1:nerrcols,i), sum(errdata(1:nerrcols,i))
      else
       write (40,fmt) coldata(col1, i) , coldata(col2,i), coldata(fixed,i), errdata(1:nerrcols,i), sum(errdata(1:nerrcols,i))
!       coord(j) =  coldata(fixed,i)
       j = j + 1 
       write (40,*)
      endif 
    end do         
   endif  


!   deallocate(coord)

 
    close(40)


!    do i= 0, nrows-1
!      write (40,fmt) coldata(col1, i) , coldata(col2,i), coldata(fixed,i), errdata(1:nerrcols,i) 
!    end do         
!    close(40)

  end subroutine Make2DSamples



  subroutine Make3DSamples(col1,col2,col3)

  !Make file of weight-1 samples by choosing samples with probability given by their weight

    integer i, col1, col2, col3
    character(LEN=20) :: fmt_data, fmt_err, fmt
    character(LEN=120) :: plotfile
    

    Feedback = 0          
    fmt_data = trim(numcat('(',3))//'E15.5,)'
    fmt_err = trim(numcat('(',nerrcols+1))//'I3,)'
    fmt = '('//trim(fmt_data)//trim(fmt_err)//')'
    plotfile = numcat(trim(numcat(trim(numcat(trim(rootname)//'_',col1))//'_',col2))//'_',col3)
    open(unit=40,file='plot_data/'//trim(plotfile)//'.dat',form='formatted',status='replace')

    
    

    do i= 0, nrows-1
      if(coldata(col1, i) == coldata(col1, i+1)) then
       write (40,fmt) coldata(col1, i) , coldata(col2,i),  coldata(col3,i), errdata(1:nerrcols,i),sum(errdata(1:nerrcols,i))
      else
       write (40,fmt) coldata(col1, i) , coldata(col2,i),  coldata(col3,i), errdata(1:nerrcols,i), sum(errdata(1:nerrcols,i))
       write (40,*)
      endif 
    end do        
  
    close(40)



  end subroutine Make3DSamples


  subroutine SortColData(bycol)
 !Sort coldata in order of likelihood
     Type(double_pointer), dimension(:), allocatable :: rowptrs
     integer, intent(in) :: bycol
     real(gp), dimension(:,:), pointer :: tmp 
     integer i

     allocate(tmp(ncols,0:nrows-1))
  
     allocate(rowptrs(0:nrows-1))
     do i = 0, nrows -1
      rowptrs(i)%p => coldata(1:,i)
     end do

    call QuickSortArr(rowptrs(0), 1, nrows, bycol)

!Make new table using sorted pointers   
    do i = 0, nrows-1
    tmp(1:ncols,i) = rowptrs(i)%p(1:ncols)
    end do

    deallocate(rowptrs)
    do i=0, nrows-1
     !Write this out explicitly to avoid stack overflow problems
     coldata(1:ncols,i) = tmp(1:ncols,i)
    end do
    deallocate(tmp)

  end  subroutine SortColData


  subroutine WriteThinData(fname,cool)
    character(LEN=*), intent(in) :: fname
    real,intent(in) :: cool
 
    character(LEN=20) :: fmt
    integer i
    real MaxL, NewL      

    if (cool /= 1) write (*,*) 'Cooled thinned output with temp: ', cool
  
    MaxL = minval(coldata(2,0:nrows-1))

    fmt = trim(numcat('(',ncols))//'E16.7)'
    open(unit=50,file=fname,form='formatted',status='replace')

    do i=0, thin_rows -1   
      if (cool/=1) then
        newL = coldata(2,thin_ix(i))*cool
        write (50,fmt) exp(-(newL - coldata(2,thin_ix(i))) - MaxL*(1-cool) ), newL, &
               coldata(3:ncols,thin_ix(i))
      else 
        write (50,fmt) 1., coldata(2:ncols,thin_ix(i)) 
      end if
    end do
    write (*,*) 'Wrote ',thin_rows, 'thinned samples'
    close(50)

  end subroutine WriteThinData

  subroutine ThinData(fac, ix1,ix2)
  !Make thinned samples
 
    integer, intent(in) :: fac
    integer, intent(in), optional :: ix1,ix2
    integer i, tot, nout, nend, mult
          
    if (allocated(thin_ix)) deallocate(thin_ix)
    allocate(thin_ix(0:nint(numsamp)/fac))
              
    tot= 0
    nout=0
    i = 0
    if (present(ix1)) i=ix1
    nend = nrows
    if (present(ix2)) nend = ix2+1
 
    mult = coldata(1,i)
    do while (i< nend)
        
    if (abs(nint(coldata(1,i)) - coldata(1,i)) > 1e-4) &
        stop 'non-integer weights in ThinData'

    if (mult + tot < fac) then
       tot = tot + mult
       i=i+1
       if (i< nend) mult = nint(coldata(1,i))
    else
        thin_ix(nout) = i
        nout= nout+1
       if (mult == fac - tot) then  
          i=i+1
          if (i< nend) mult = nint(coldata(1,i))
       else
         mult = mult - (fac -tot)
       end if
       tot = 0
    end if

    end do
       
    thin_rows = nout

    close(50)

  end subroutine ThinData

           function MostCorrelated2D(i1,i2, direc)
            integer, intent(in) :: i1,i2, direc
            integer MostCorrelated2D
            !Find which parameter is most correllated with the degeneracy in ix1, ix2
            integer pars(2)
            real mat2D(2,2), evals(2), u(2,2)
            real corrs(2,ncols-2)
          
            if (direc /= 0 .and. direc /= -1) stop 'Invalid 3D color parameter'

            pars(1)=i1
            pars(2)=i2
            mat2D = corrmatrix(pars,pars)
            u = mat2D

!            call Diagonalize(u, evals, 2)
            corrs = matmul(transpose(u), corrmatrix(pars,:))
            corrs(:,pars) = 0
          
            MostCorrelated2D = MaxIndex(abs(corrs(2+direc,colix(1:num_vars)-2)), num_vars)
            MostCorrelated2D = colix(MostCorrelated2D) -2
         
          end function MostCorrelated2D

          subroutine GetFractionIndices(fraction_indices,n)
             integer, intent(in) :: n
             integer num, fraction_indices(*), i
             real tot, aim

             tot = 0
             aim=numsamp/n
             num = 1
             fraction_indices(1) = 0
             fraction_indices(n+1) = nrows
             do i=0, nrows-1
              tot = tot + coldata(1,i)
              if (tot > aim) then
               num=num+1
               fraction_indices(num) = i
               if (num==n) exit              
               aim=aim+numsamp/n
              end if   
             end do

          end subroutine GetFractionIndices

           function ConfidVal(ix,limfrac,upper,ix1,ix2)
             integer, intent(IN) :: ix
             real, intent(IN) :: limfrac
             logical, intent(IN) :: upper
             integer, intent(IN), optional :: ix1,ix2
             real ConfidVal, samps
             real try, lasttry, try_t, try_b
             integer l,t 
          !find upper and lower bounds
            
             l=0
             t=nrows-1
             if (present(ix1)) l=ix1
             if (present(ix2)) t = ix2
             try_b = minval(coldata(ix,l:t))
             try_t = maxval(coldata(ix,l:t))

             samps = sum(coldata(1,l:t))

             lasttry = -1
             if (upper) then
             do 
               try = sum(coldata(1,l:t),mask = coldata(ix,l:t) > (try_b + try_t)/2) 
               if (try > samps*limfrac) then
                  try_b = (try_b+try_t)/2
               else
                  try_t = (try_b+try_t)/2
               end if
               if (try == lasttry) exit
               lasttry = try
             end do
             else
               do 
               try = sum(coldata(1,l:t),mask = coldata(ix,l:t) < (try_b + try_t)/2) 
               if (try > samps*limfrac) then
                  try_t = (try_b+try_t)/2
               else
                  try_b = (try_b+try_t)/2
               end if
               if (try == lasttry) exit
               lasttry = try
                end do
             end if

             ConfidVal = try_t
            end function ConfidVal

        subroutine GetUsedCols
          integer j
         
                  
           do j = 1, ncols
            isused(j) = any(coldata(j,0:nrows-1)/=coldata(j,0)) 
           end do  
 
         end subroutine GetUsedCols

        subroutine GetDimUsedCols
          integer i, j, k

          allocate(coord(num_hard,0:nrows-1))

           do j = 1, num_hard
             k = 0  
             if(isused(j)) then
               do i = 0, nrows-1
                if(not(any(coldata(j,i) == coldata(j,0:i-1)))) then 
                  k = k + 1
                endif 
                coord(j, k) = coldata(j,i)
                GridDim(j) = k               
               enddo
             endif
           enddo



         end subroutine GetDimUsedCols


 
        subroutine Get2DPlotData(j,j2)
         integer, intent(in) :: j,j2
         integer i,ix1,ix2,wx,wy
         real binweight, dist, norm, maxbin
         real try_b, try_t,try_sum, try_last
         character(LEN=120) :: plotfile, filename,numstr
      
              bins2D = 0
              bin2Dlikes = 0
              bin2Dmax = 1e30        


              do i = 0, nrows-1
                ix1=nint((coldata(colix(j),i)-center(j))/width(j))
                ix2=nint((coldata(colix(j2),i)-center(j2))/width(j2))
       
                if (smoothing) then
                do wx = max(ix_min(j),ix1-2),min(ix_max(j),ix1+2)
                   dist = (coldata(colix(j),i) - (center(j) + wx*width(j)))**2/width(j)**2
                 do wy = max(ix_min(j2),ix2-2),min(ix_max(j2),ix2+2)
                   binweight = coldata(1,i)*exp( - (&
                      dist +(coldata(colix(j2),i) - (center(j2) + wy*width(j2)))**2/width(j2)**2 )/2)
                   bins2D(wx,wy) = bins2D(wx,wy) + binweight
                   bin2Dlikes(wx,wy) = bin2Dlikes(wx,wy) + binweight*exp(meanlike - coldata(2,i))
                 end do
                end do
                else
                  bins2D(ix1,ix2) = bins2D(ix1,ix2) + coldata(1,i) 
                  bin2Dlikes(ix1,ix2) = bin2Dlikes(ix1,ix2) + coldata(1,i)*exp(meanlike-coldata(2,i))
                end if
              bin2Dmax(ix1,ix2) = min(bin2Dmax(ix1,ix2),real(coldata(2,i)))
              end do

             
              do ix1=ix_min(j),ix_max(j)
               do ix2 =ix_min(j2),ix_max(j2) 
                  if (bins2D(ix1,ix2) >0) bin2Dlikes(ix1,ix2) = bin2Dlikes(ix1,ix2)/bins2D(ix1,ix2)
                  if (plot_NDcontours) then
                    !Map maximum likelihood in each bin into a significance value from the full N-D distribution
                    if (bin2DMax(ix1,ix2) == 1e30) then
                     bin2DMax(ix1,ix2) = 0
                      else
                      bin2DMax(ix1,ix2) = sum(coldata(1,0:nrows-1), mask = coldata(2,0:nrows-1) > bin2DMax(ix1,ix2))/numsamp
                    end if
                 end if
                end do 
              end do
     

              if (has_limits(colix(j)) .or. has_limits(colix(j2))) then

               !Fix up underweighting near edges. Note this makes edge pixels noisier.
               do ix1 = ix_min(j), ix_max(j)
                 do ix2 = ix_min(j2),ix_max(j2)
                    if (ix1 ==ix_min(j) .and. has_limits_bot(colix(j))) bins2D(ix1,ix2) = bins2D(ix1,ix2)*2
                    if (ix1 ==ix_max(j) .and. has_limits_top(colix(j))) bins2D(ix1,ix2) = bins2D(ix1,ix2)*2
                    if (ix2 ==ix_min(j2).and. has_limits_bot(colix(j2))) bins2D(ix1,ix2) = bins2D(ix1,ix2)*2
                    if (ix2 ==ix_max(j2).and. has_limits_top(colix(j2))) bins2D(ix1,ix2) = bins2D(ix1,ix2)*2

                    if (ix1 ==ix_min(j)+1.and. has_limits_bot(colix(j))) bins2D(ix1,ix2) = bins2D(ix1,ix2)/0.84
                    if (ix1 ==ix_max(j)-1.and. has_limits_top(colix(j))) bins2D(ix1,ix2) = bins2D(ix1,ix2)/0.84
                    if (ix2 ==ix_min(j2)+1.and. has_limits_bot(colix(j2))) bins2D(ix1,ix2) = bins2D(ix1,ix2)/0.84
                    if (ix2 ==ix_max(j2)-1.and. has_limits_top(colix(j2))) bins2D(ix1,ix2) = bins2D(ix1,ix2)/0.84
                 end do
               end do

             end if

! Get contour containing contours(:) of the probability

             allocate(TheBins(ix_max(j)-ix_min(j)+1,ix_max(j2)-ix_min(j2)+1))
             TheBins = bins2D(ix_min(j):ix_max(j),ix_min(j2):ix_max(j2))
                
             norm = sum(TheBins)
             
             do ix1 = 1, num_contours
             try_t = maxval(TheBins)
             try_b = 0
             try_last = -1
             do
               try_sum = sum(TheBins,mask = TheBins < (try_b + try_t)/2) 
               if (try_sum > (1-contours(ix1))*norm) then
                  try_t = (try_b+try_t)/2
               else
                  try_b = (try_b+try_t)/2
               end if
               if (try_sum == try_last) exit
               try_last = try_sum
         
             end do
             contour_levels(ix1) = (try_b+try_t)/2

             end do
             deallocate(TheBins)


              plotfile = numcat(trim(numcat(trim(rootname)//'_2D_',colix(j)-2))//'_',colix(j2)-2)
              filename = 'plot_data/'//trim(plotfile)
              open(unit=49,file=filename,form='formatted',status='replace')
              do ix1 = ix_min(j), ix_max(j)
                  write (49,trim(numcat('(',ix_max(j2)-ix_min(j2)+1))//'E16.7)') bins2D(ix1,ix_min(j2):ix_max(j2))
              end do
              close(49)

            open(unit=49,file=trim(filename)//'_cont',form='formatted',status='replace')
            write(numstr,*) contour_levels(1:num_contours)
            if (num_contours==1) numstr = trim(numstr)//' '//trim(numstr)
            write (49,*) trim(numstr)
            close(49)

          if (shade_meanlikes) then
                  open(unit=49,file=trim(filename)//'_likes',form='formatted',status='replace')
                  maxbin = maxval(bin2Dlikes(ix_min(j):ix_max(j),ix_min(j2):ix_max(j2)))
                  do ix1 = ix_min(j), ix_max(j)
                    write (49,trim(numcat('(',ix_max(j2)-ix_min(j2)+1))//'E16.7)') &
                           bin2Dlikes(ix1,ix_min(j2):ix_max(j2))/maxbin
                  end do
                 close(49)
          end if

          if (plot_NDcontours) then
                  open(unit=49,file=trim(filename)//'_confid',form='formatted',status='replace')
                  do ix1 = ix_min(j), ix_max(j)
                    write (49,trim(numcat('(',ix_max(j2)-ix_min(j2)+1))//'E16.7)') &
                           bin2Dmax(ix1,ix_min(j2):ix_max(j2))

                  end do
                 close(49)
          end if
     
        end subroutine Get2DPlotData


        function PlotContMATLAB(aunit,aroot,j,j2, DoShade)
         character(LEN=*), intent(in) :: aroot
         integer, intent(in) :: j, j2,aunit
         logical, intent(in) :: DoShade
         character(LEN=120) fname,numstr,plotfile
         logical PlotContMATLAB

             PlotContMATLAB= .false. 
             plotfile = numcat(trim(numcat(trim(aroot)//'_2D_',colix(j)-2))//'_',colix(j2)-2)
             if (.not. FileExists('plot_data/'//trim(plotfile))) return
             PlotContMATLAB= .true.
              write (aunit,'(a)') 'pts=load (fullfile(''plot_data'',''' // trim(plotfile) //'''));'
     
              fname =trim(trim(aroot)//trim(numcat('_p',colix(j2)-2)))
              write (aunit,'(a)') 'tmp = load (fullfile(''plot_data'',''' // trim(fname)//  '.dat''));'
              write (aunit,*) 'x1 = tmp(:,1);'

              fname =trim(trim(aroot)//trim(numcat('_p',colix(j)-2)))
              write (aunit,'(a)') 'tmp = load (fullfile(''plot_data'',''' // trim(fname)//  '.dat''));'
              write (aunit,*) 'x2 = tmp(:,1);'
    
!              fmt = trim(numcat('(''x1 = ['',',ix_max(j2)-ix_min(j2)+1))//'E16.7,''];'')'
!              write (aunit,fmt) (/(I, I=ix_min(j2), ix_max(j2), 1)/)*width(j2)+center(j2)

 !             fmt = trim(numcat('(''x2 = ['',',ix_max(j)-ix_min(j)+1))//'E16.7,''];'')'
 !             write (aunit,fmt) (/(I, I=ix_min(j), ix_max(j), 1)/)*width(j)+center(j)
              if (DoShade) then
              if (shade_meanlikes) then
                write (aunit,'(a)') 'load (fullfile(''plot_data'',''' // trim(plotfile) //'_likes''));'
                write (aunit,*) 'contourf(x1,x2,'//trim(plotfile)//'_likes,64);'
              else     
                write (aunit,*) 'contourf(x1,x2,pts,64);'
              end if

              write (aunit,*) 'set(gca,''climmode'',''manual''); shading flat; hold on;'
              end if
              
           
              if (num_contours /= 0) then

               write (aunit,'(a)') 'cnt = load (fullfile(''plot_data'',''' // trim(plotfile) //'_cont''));'
  
               if (plot_NDcontours) then
                write (aunit,'(a)') 'load (fullfile(''plot_data'',''' // trim(plotfile) //'_confid''));'
                write(numstr,*) 1-contours(1:num_contours)  
                if (num_contours==1) numstr = trim(numstr)//' '//trim(numstr)
               write (aunit,'(a)') 'contour(x1,x2,'//trim(plotfile)//'_confid,['//trim(numstr)//'],''r'');'
               
               end if
              end if

                      
        end function PlotContMATLAB


        subroutine Write2DPlotMATLAB(aunit,j,j2, DoLabelx,DoLabely)
          integer, intent(in) :: aunit,j,j2
          logical, intent(in) :: DoLabelx,DoLabely 
          character(LEN=120) fmt
          integer i
          

              if (PlotContMATLAB(aunit,rootname,j,j2,do_shading) .and. &
                  num_contours /= 0) then

               write (aunit,'(a)') '[C h] = contour(x1,x2,pts,cnt,lineM{1});'
               write (aunit,'(a)') 'set(h,''LineWidth'',lw1);'
               write (aunit,*) 'hold on; axis manual; '
       
               do i = 1, Num_ComparePlots
                fmt = trim(numcat('lineM{',i+1)) // '}'
                if (PlotContMATLAB(aunit,ComparePlots(i),j,j2,.false.)) &
                 write (aunit,'(a)') '[C h] = contour(x1,x2,pts,cnt,'//trim(fmt)//');'
                 write (aunit,'(a)') 'set(h,''LineWidth'',lw2);'
             
               end do

              end if

              write (aunit,*) 'hold off; set(gca,''Layer'',''top'',''FontSize'',axes_fontsize);'
              fmt = ''',''FontSize'',lab_fontsize);'
              if (DoLabelx) write (aunit,*) 'xlabel('''//trim(labels(colix(j2)))//trim(fmt)
              if (DoLabely) write (aunit,*) 'ylabel(''',trim(labels(colix(j)))//trim(fmt)
 
         end subroutine  Write2DPlotMATLAB


         subroutine WriteMatLabInit(unit,sm)
          integer, intent(in) :: unit
          logical, intent(in) :: sm
          if (sm) then
           write(unit,*) 'lab_fontsize = 9; axes_fontsize = 9;'
          else
           write(unit,*) 'lab_fontsize = 12; axes_fontsize = 12;'
          end if
           write(unit,*) 'clf'
          if (BW) then
           if (plot_meanlikes) then
            write(unit,*) 'lineM = {''-k'',''-r'',''-b'',''-m'',''-g'',''-y''};';
            write(unit,*) 'lineL = {'':k'','':r'','':b'','':m'','':g'','':y''};';
            write(unit,*) 'lw1=3;lw2=1;' !Line Widths     
           else
            write(unit,*) 'lineM = {''-k'',''--r'',''-.b'','':m'',''--g'',''-.y''};';
            write(unit,*) 'lw1=1;lw2=1;' !Line Widths     
           end if
           else
            write(unit,*) 'lineM = {''-k'',''-r'',''-b'',''-m'',''-g'',''-y''};';
            write(unit,*) 'lineL = {'':k'','':r'','':b'','':m'','':g'','':y''};';
           write(unit,*) 'lw1=1;lw2=1;' !Line Widths     
          end if

         end subroutine WriteMatLabInit

     subroutine Write1DplotMatLab(aunit,j)
      integer, intent(in) :: aunit, j
      character(LEN=120) fname
       character(LEN=60) fmt
       integer ix1

        fname =trim(trim(rootname)//trim(numcat('_p',colix(j)-2)))
        write (aunit,*) 'load (fullfile(''plot_data'',''' // trim(fname)//  '.dat''));'
        write (aunit,*) 'pts='//trim(fname)//';'
        write (aunit,*) 'plot(pts(:,1),pts(:,2),lineM{1},''LineWidth'',lw1);'
        write (aunit,*) 'axis([-Inf,Inf,0,1.1]);axis manual;'
        write (aunit,*) 'set(gca,''FontSize'',axes_fontsize); hold on;'

        if (plot_meanlikes) then
         write (aunit,*) 'load (fullfile(''plot_data'',''' // trim(fname)//  '.likes''));'
         write (aunit,*) 'pts='//trim(fname)//';'
         write (aunit,*) 'plot(pts(:,1),pts(:,2),lineL{1},''LineWidth'',lw1);'
        end if


       do ix1 = 1, Num_ComparePlots
         fname =  trim(trim(ComparePlots(ix1))//trim(numcat('_p',colix(j)-2)))    
         if (FileExists('plot_data/' // trim(fname)//'.dat')) then

          write (aunit,*) 'pts = load (fullfile(''plot_data'',''' // trim(fname)// '.dat''));'
          fmt = trim(numcat('lineM{',ix1+1)) // '}'        
          write (aunit,*) 'plot(pts(:,1),pts(:,2),'//trim(fmt)//',''LineWidth'',lw2);'
    
         if (plot_meanlikes) then

            write (aunit,*) 'pts=load (fullfile(''plot_data'',''' //trim(fname)//'.likes''));'
           fmt = trim(numcat('lineL{',ix1+1)) // '}'        
            write (aunit,*) 'plot(pts(:,1),pts(:,2),'//trim(fmt)//',''LineWidth'',lw2);'

         end if

         end if
       end do

     end subroutine Write1DplotMatLab
 
end module MCSamples

subroutine CheckMatlabAxes(afile)
 integer, intent(in) :: afile

  write (afile,*) 'ls =get(gca,''XTick'');sz=size(ls,2);'
  write (afile,*) 'if(sz > 4)'
  write (afile,*) ' set(gca,''XTick'',ls(:,1:2:sz));'
  write (afile,*) 'end;'

end subroutine CheckMatlabAxes


program GetDist

        use IniFile
        use MCSamples
        implicit none

        real  bincounts(-1000:1000),binlikes(-1000:1000),binmaxlikes(-1000:1000)
    
        character(LEN=80) InputFile, numstr, coordstr, labxstr, labystr
        character(LEN=300) fname
        character(LEN=10000) InLine  ! ,LastL,OutLine
        real invars(max_cols)
        integer ix, ix1,i,ix2,ix3, nbins, wx
        real ignorerows
   
        logical bad, adjust_priors

        real  amax, amin
        character(LEN=120) filename, infile, in_root,out_root, sm_file, gp_file, RGB
        character(LEN=120) matlab_col, InS1,InS2,fmt, tmpstr
        integer plot_row, plot_col, chain_num, first_chain,chain_ix, plot_num
       
        integer x,y,j,j2, jf, num_2D_plots, num_cust2D_plots
        integer num_3D_plots
        character(LEN=80) contours_str, plot_3D(20), plot_2D(20), bin_limits
        integer plot_x, plot_y, thin_factor, outliers
        real limmin(max_cols),limmax(max_cols), maxbin
        integer plot_2D_param, j2min
        real try_b, try_t, binweight
        real cont_lines(max_cols,2,2), dist, distweight, limfrac 
        integer bestfit_ix
        integer chain_exclude(max_chains), num_exclude
        logical map_params
        logical :: triangle_plot = .false.
        real counts
        real cool
        integer PCA_num, PCA_normparam
        integer PCA_params(max_cols)
        integer plotparams_num, plotparams(max_cols)
        character(LEN=max_cols) PCA_func
        logical Done2D(max_cols,max_cols)
        logical samples_are_chains, no_plots

        real thin_cool
        logical no_tests, auto_label

        integer fixed, k, num_eff
        real labx, laby, ylb1, ylb2, ylb3, ylb4
        real ylb(2, 5)
        integer cut(2, 6)

        cut = RESHAPE((/1, 5, 9, 13, 17, 19, 0, 1, 6, 10, 14, 18/),(/2,6/),ORDER=(/2,1/))

        ylb = RESHAPE((/1.02, 0.98, 0.94, 0.9, 0.86, 1.07, 1.04, 1.01, 0.98,0./),(/2,5/),ORDER=(/2,1/))


!        real, dimension(:),  allocatable :: coord  

        InputFile = GetParam(1)
        if (InputFile == '') stop 'No parameter input file'

        call Ini_Open(InputFile, 1, bad, .false.)
        if (bad) stop 'Error opening parameter file'
        Ini_fail_on_not_found = .true.

        in_root = Ini_Read_String('file_root')
       
        rootname = ExtractFileName(in_root)
        chain_num = Ini_Read_Int('chain_num')

        ncols = Ini_Read_Int('columnnum',0)
        nincols = Ini_Read_Int('incolumnnum',0)
        nerrcols = Ini_Read_Int('errcolumnnum',0)

        RGB = Ini_Read_String('RGB')

        if (ncols==0) then
           if (chain_num == 0) then
            infile = trim(in_root) // '.txt'
          else
           infile = trim(in_root) //'_1.txt'
          end if
          ncols = TxtFileColumns(infile)
          write (*,*) 'Reading ',ncols, 'columns' 
          nerrcols = TxtFileColumns(infile)
          write (*,*) 'Reading ',nerrcols, 'columns'
        end if
        allocate(coldata(ncols,0:max_rows))
        allocate(errdata(nerrcols,0:max_rows))
!error
        

       
  
!        nbins = Ini_Read_Int('num_bins')

        ignorerows = Ini_Read_Double('ignore_rows',0.d0)

        adjust_priors = Ini_Read_Logical('adjust_priors',.false.)
    
        Ini_fail_on_not_found = .false.

        matlab_version = Ini_Read_Real('matlab_version',6.)

!        labels(1) = 'mult'
!        labels(2) = 'likelihood'
!        auto_label = Ini_Read_Logical('auto_label',.false.)
        do ix=1, ncols
           write (numstr,*) ix
!          if (auto_label) then
!             labels(ix) = trim(adjustl(numstr))
!           else
          labels(ix) = Ini_Read_String('lab'//trim(adjustl(numstr)), .false.)  
           tails(ix) = Ini_Read_Int('tails'//trim(adjustl(numstr)),2)
!           end if 
        end do


        prob_label = Ini_Read_Logical('prob_label',.false.)
        triangle_plot = Ini_Read_Logical('triangle_plot',.false.)
        
!        samples_are_chains = Ini_Read_Logical('samples_are_chains',.true.)

        no_tests = Ini_Read_Logical('no_tests',.false.)
!        no_plots = Ini_Read_Logical('no_plots',.false.)

!        thin_factor = Ini_Read_Int('thin_factor',0)
!        thin_cool = Ini_read_Real('thin_cool',1.)

        first_chain = Ini_Read_Int('first_chain',1)

        make_single_samples = Ini_Read_logical('make_single_samples',.false.)
!       single_thin = Ini_Read_Int('single_thin',1)
!       cool = Ini_Read_Real('cool',1.)

        Num_ComparePlots = Ini_Read_Int('compare_num',0)
        do ix = 1, Num_ComparePlots
           ComparePlots(ix) = ExtractFileName(Ini_Read_String(numcat('compare',ix)))
        end do

        has_limits_top = .false.
        has_limits_bot = .false.
        
!        bin_limits = Ini_Read_String('all_limits')
        do ix=1, ncols
           write (numstr,*) ix
!           if (bin_limits /= '') then
!           InLine = bin_limits
!          else
            InLine = Ini_Read_String('limits'//trim(adjustl(numstr)))
!           end if
           if (InLine /= '') then
              read (InLine,*) InS1, InS2 
              if (trim(adjustl(InS1)) /= 'N') then
               has_limits_bot(ix) = .true.
               read(InS1,*) limmin(ix)
              end if
              if (trim(adjustl(InS2)) /= 'N') then
                has_limits_top(ix) = .true.
                read(InS2,*) limmax(ix)
              end if
           end if
        end do

       
 
        has_limits = has_limits_top .or. has_limits_bot

        plot_2D_param = Ini_Read_Int('plot_2D_param',0)
        if (plot_2D_param /= 0) then
        plot_2D_param = plot_2D_param + 2
        num_cust2D_plots = 0
    else
         !Use custom array of specific plots
     num_cust2D_plots = Ini_Read_Int('plot_2D_num',0)
     do ix = 1, num_cust2D_plots
        InLine = Ini_Read_String(numcat('plot',ix))
        read (InLine, *) ix1,ix2
        cust2DPLots(ix) = ix1+2 + (ix2)*100 
     end do
    end if


    InS1 =Ini_Read_String('exclude_chain')
    num_exclude = 0
    chain_exclude = 0
    read (InS1, *, end =20) chain_exclude
   
20  do while (chain_exclude(num_exclude+1)/=0) 
     num_exclude = num_exclude + 1
    end do 
    

    map_params = Ini_Read_Logical('map_params',.false.)
    if (map_params)  &
          write (*,*) 'WARNING: Mapping params - .covmat file is new params.'



    matlab_col = Ini_Read_String('matlab_colscheme')
    smoothing = Ini_Read_Logical('smoothing',.true.)

    out_root = Ini_Read_String('out_root')
    if (out_root /= '') then
     rootname = out_root
     write (*,*) 'producing files with root '//trim(out_root)
    end if
!   sm or gp 
    gplot = Ini_Read_Logical('gnuplot',.true.)
    sm_file = trim(rootname) // '.sm'
    gp_file = trim(rootname) // '.gp'
   


    contours(1) = Ini_Read_Real('contour1',0.)
    contours(2) = Ini_Read_Real('contour2',0.)
    contours_str = trim(Ini_Read_String('contour1')) // ' ; '// &
             trim(Ini_Read_String('contour2'))
    num_contours = count(contours/= 0)
    if (contours(1)==0) contours(1) = contours(2)

    force_twotail = Ini_Read_Logical('force_twotail',.false.)  
    if (force_twotail) then 
        write (*,*) 'Computing two tail limits'
       tails(:) = 2
    end if



    plot_NDcontours = Ini_Read_Logical('plot_NDcontours',.false.)


        plotparams_num = Ini_Read_Int('plotparams_num',0)
        if (plotparams_num /= 0) then
          InLine = Ini_Read_String('plotparams')
          read(InLine,*) plotparams(1:plotparams_num)
        end if

        num_2D_plots = Ini_Read_Int('num_2D_plots',0)
        do ix =1, num_2D_plots
           plot_2D(ix) = Ini_Read_String(numcat('2D_plot',ix))
        end do

        num_3D_plots = Ini_Read_Int('num_3D_plots',0)
        do ix =1, num_3D_plots
           plot_3D(ix) = Ini_Read_String(numcat('3D_plot',ix))
        end do

        BW = Ini_Read_Logical('B&W',.false.)
        do_shading = Ini_Read_Logical('do_shading',.true.)
        call Ini_Close

!        allocate(bins2D(-1000:1000,-1000:1000),bin2Dlikes(-1000:1000,-1000:1000),bin2Dmax(-1000:1000,-1000:1000))
 
        

!Read in the chains     
        nrows = 0
        num_chains_used =0

        do chain_ix = first_chain, first_chain-1 + max(1,chain_num)


         do ix = 1, num_exclude
           if (chain_exclude(ix) == chain_ix) goto 30
         end do  
      
         if (chain_num == 0) then
            infile = trim(in_root) // '.txt'
         else
            write (numstr,*) chain_ix
            infile = trim(in_root) //'_'//trim(adjustl(numstr))// '.txt'
         end if


         write (*,*) 'reading ' // trim(infile)
   
         
         open(unit=50,file=infile,form='formatted',status='old', err=28)


          if (ignorerows >=1) then
           do ix = 1, nint(ignorerows)
             read (50,'(a)',end=1) InLine
           end do
          end if

         num_chains_used = num_chains_used + 1
         if (num_chains_used > max_chains) stop 'Increase max_chains in GetDist'
         chain_indices(num_chains_used) = nrows
         chain_numbers(num_chains_used) = chain_ix
          do
           read (50,'(a)',end=1) InLine
           if (SCAN (InLine, 'N') /=0) then
             write (*,*) 'WARNING: skipping line with probable NaN'
             cycle
           end if
!           if (samples_are_chains) then
            read(InLine,*,end=1, err=2) invars(1:ncols+nerrcols)
!           else
!            read(InLine,*,end=1, err=2) invars(3:ncols) 
!            invars(1) = 1
!            invars(2) = 1
!           end if
           
           if (map_params) call MapParameters(invars)
           coldata(1:ncols, nrows) = invars(1:ncols)
            
! error reading
           errdata(1:nerrcols, nrows) = invars(ncols+1:ncols+1+nerrcols)

           nrows = nrows + 1
           if (nrows > max_rows) stop 'need to increase max_rows'

          end do
 


 2       write (*,*) 'error reading line ', nrows + int(ignorerows), ' - skipping rest of file'
                       
 
 1       close(50)

          

!         if (ignorerows<1 .and. ignorerows/=0) then
!           i = chain_indices(num_chains_used)
!           j = nint((nrows-i-1)*ignorerows)
!           do ix = i,nrows-j-1
!            coldata(:,ix) = coldata(:,ix+j)
!           end do
!           nrows = nrows - j
!         end if
         goto 30
 28      write (*,'(" chain ",1I4," missing")') chain_ix
 30      end do

         
         if (nrows == 0) stop 'No un-ignored rows! (check number of chains/burn in)'
          

!          if (cool /= 1) call CoolChain(cool)
!Adjust weights if requested
         if (adjust_priors) then
             call AdjustPriors
         end if

         call GetUsedCols !See which parameters are fixed

         call GetDimUsedCols

         if (adjust_priors) call DeleteZeros  


         if (make_single_samples) call MakeSingleSamples(single_thin)

!Only use variables whose label's are not empty (and in list of plotparams if plotparams_num /= 0)
          num_vars = 0
          

          do ix = 1,ncols

           if (labels(ix) /= '' .and. isused(ix)) then
                if (plotparams_num == 0 .or. any(plotparams(1:plotparams_num)==ix)) then
                 num_vars = num_vars + 1
                 colix(num_vars) = ix
                 limmin(ix) =  minval(coldata(ix,0:nrows-1))
                 limmax(ix) =  maxval(coldata(ix,0:nrows-1))               
                end if
             end if
          end do

          triangle_plot = triangle_plot .and. (num_vars > 1)

          write (*,*) 'using ',nrows,' rows, processing ',num_vars,' parameters'     


         
!Output files for 1D plots

          plot_col =  nint(sqrt(num_vars/1.4))
          plot_row = (num_vars +plot_col-1)/plot_col 
   
 
          !So we don't forget to run sm, delete current .ps file
      call DeleteFile(trim(rootname)//'.ps')

    if(gplot) then 
      open(unit=50,file=gp_file,form='formatted',status='replace')
       !Gnuplot .gp file for 1D plots
          if(num_2D_plots == 1) then
           write (50,*) 'set terminal postscript enhanced color solid 14'
           else
           write (50,*) 'set terminal postscript enhanced color solid 12'
          endif
          write (50,*)
          write (50,*) 'set output "'//trim(rootname)//'.eps"'
          write (50,*) 
          if(num_2D_plots == 1) then 
             write(50,*) 'set size square 0.8,0.8'
             write(50,*)
             write(50,*) 'set origin 0.2,0.'
             write(50,*)
          endif

!          if (num_vars > 6) then
!               write (50,*) 'set size square 0.8,0.8'
!          else
!               write (50,*) 'set size square 1.1,1.1'
!          endif

          if(num_2D_plots > 1) write (50,*) 'set multiplot'

          write (50,*) 

          write (50,*)                   
          write (50,*) 'set nokey'
          write (50,*)

          write (50,*)  'set mxtics 2'
          write (50,*)  'set mytics 2'
          write (50,*) 

          num_eff = 1 
          if(num_2D_plots >= 2) num_eff = 2  
          j = 1
          k = 0
          do i = 1, nincols-num_soft
           if(.not. isused(i)) then
             write (numstr,'(E10.3)') coldata(i,0)
             if(any(j==cut(num_eff,:))) then 
               title = trim(labels(i))//' = '//trim(adjustl(numstr))//', \'
               write (50,*)
               write (50,*) 'set label "'//trim(title)                        
             elseif(any(j==cut(num_eff,:)-1)) then
               k = k + 1
               write(coordstr,'(E10.3)') ylb(num_eff,k)
               title = trim(labels(i))//' = '//trim(adjustl(numstr))
               write (50,*) trim(title)//' " at screen  0.05,'//trim(coordstr)//' font "0.6 " '
             else
               title = trim(labels(i))//' = '//trim(adjustl(numstr))//', \'
               write (50,*) trim(title)
             endif
             j = j + 1
           endif
          enddo


       else          
!Do 2D bins a la sm    
          
          open(unit=50,file=sm_file,form='formatted',status='replace')
       !SuperMongo .sm file for 1D plots
          write (50,*) 'DEFINE TeX_strings 1'
          write (50,*) 'device postfile '//trim(rootname)//'.ps'
          write (50,*) 'DEFINE default_font rm'
          if (num_vars > 6) then
           write (50,*) 'expand 0.8'
           write (50, *)'define x_gutter 0.6'
           write (50,*) 'define y_gutter 0.8'
          else
           write (50,*) 'expand 1.1'
          endif

          write (50,*) 'location 2500 31000 3500 31000'
          if (BW) then
            write (50,*) 'lweight 4'
          else
             write (50,*) 'lweight 3'
          end if

        endif 

        if (num_2D_plots /=0) then
          write (*,*) 'producing ',num_2D_plots, '2D colored  plots'

         
          do j = 1, num_2D_plots

           read(plot_2D(j),*) ix1, ix2
!2D data file
           if(num_2D_plots == 1) then       
             call Make1DSamples(ix1,ix2)
           else
             call Make2DSamples(ix1,ix2,fixed)  
           endif

!2D contour data file
!rruiz
!            call Get2DPlotData(ix1,ix2, fixed)

            fname = numcat(trim(numcat(trim(rootname)//'_',ix1))//'_',ix2)
            filename = 'plot_data/'// trim(fname) //'.dat'
            plot_x = mod(j+plot_col-1,plot_col)+1
            plot_y = plot_row - (j+plot_col-1)/plot_col + 1

           if(gplot) then

! odd number of plots
            if(num_2D_plots > 1 .and. mod(num_2D_plots,2) > 0) then
             write (50,'(" set size ",E10.3,",",E10.3)') 2./(num_2D_plots+1), 0.5
             write (50,*)
! label coord 
             labx =  2./(num_2D_plots+1)
             laby =  0.5            
             if(j <= (num_2D_plots+1)/2) then
              write (50,'(" set origin",E10.3,",",E10.3)') (j-1)*2./(num_2D_plots+1), 0.47
              write (50,*)
             labx = (j-1)*2./(num_2D_plots+1)
             laby =  0.5   
             else
              write (50, '(" set origin",E10.3,",",E10.3)') (j-(num_2D_plots+1)/2-1)*2./(num_2D_plots+1), -0.05
              write (50,*)
             labx = (j-(num_2D_plots+1)/2-1)*2./(num_2D_plots+1)
             laby =  0.
             endif
! even number of plots
            else if(num_2D_plots > 1) then
              if(num_2D_plots == 2) then
               write (50,'(" set size ",E10.3,",",E10.3)') 0.5, 0.5
               labx = 2./num_2D_plots + 0.3 
               laby =  0.52
              else 
               write (50,'(" set size ",E10.3,",",E10.3)') 2./num_2D_plots, 0.5
               labx = 2./num_2D_plots
               laby =  0.52
              endif
              write (50,*)
              if(j <= num_2D_plots/2) then
               if(num_2D_plots == 2) then              
                write (50,'(" set origin",E10.3,",",E10.3)') 0.25, 0.47
                labx = (j-1)*2./num_2D_plots + 0.25
                laby =  0.52            
               else
                write (50,'(" set origin",E10.3,",",E10.3)') (j-1)*2./num_2D_plots, 0.47
                labx = (j-1)*2./num_2D_plots
                laby =  0.52
               endif
               write (50,*)
              else
               if(num_2D_plots == 2) then              
                write (50,'(" set origin",E10.3,",",E10.3)') 0.25, -0.05
                labx = (j-num_2D_plots/2-1)*2./num_2D_plots + 0.25
                laby =  0.
               else
                write (50, '(" set origin",E10.3,",",E10.3)') (j-num_2D_plots/2-1)*2./num_2D_plots, -0.05
                labx = (j-num_2D_plots/2-1)*2./num_2D_plots
                laby =  0.
               endif
              endif            
            endif

            write (50,*)
            
            write (50,'('' set xrange[ '',E10.5,":",E10.5,"]")') limmin(ix1), limmax(ix1) 
            write (50,'('' set yrange[ '',E10.5,":",E10.5,"]")') 0.9 * limmin(ix2), 1.1 * limmax(ix2)            

            write (50,*)
            write (50,*) 'set xlabel "' // trim(labels(ix1))// ' "'
            write (50,*) 'set ylabel "' // trim(labels(ix2))// ' "'
            write (50,*)

           if (j == 1) then 
             write (50,*)  'set style line  1 lt  1 lw 1'
             write (50,*)  'set style line  2 lt  2 lw 1'
             write (50,*)  'set style line  3 lt  3 lw 1'
             write (50,*)  'set style line  4 lt  4 lw 1'
             write (50,*)  'set style line  5 lt  5 lw 1'
             write (50,*)  'set style line  6 lt  6 lw 1'
             write (50,*)  'set style line  7 lt  7 lw 1'
             write (50,*)  'set style line  8 lt  8 lw 1'
             write (50,*)  'set style line  9 lt  9 lw 1'

             write (50,*)

           endif

           write (labxstr,'(E10.2)') labx + 0.12 

           if(num_2D_plots == 1) then
            write(50,*) 'plot "'//trim(filename)//'" using 1:2 notitle w l linestyle 1'
            write (50,*) 'plot "'//trim(filename)//'" using 1:($13 >0 ? $2:1/0) notitle w p pt 7 ps 1'
           else
            k = 0
            do i = 1, GridDim(fixed), nint(real(GridDim(fixed)/tails(fixed)))+ 1     
             k = k + 1 
             write (numstr,'(I2)') k
             write (coordstr,'(E8.2)') coord(fixed,i)
             write (labystr,'(E10.2)') laby + 0.42 - 0.03 * k
             write(50,*) 'set label "'//trim(labels(fixed))//' = '//trim(coordstr)//' " \'
             write(50,*) 'at screen '//trim(labxstr)//','//trim(labystr)//' tc lt '//trim(numstr)//' font "0.5 " '  
            enddo

            write (50,*)

            k = 0
            do i = 1, GridDim(fixed), nint(real(GridDim(fixed)/tails(fixed)))+1
             k = k + 1 
             write (numstr,'(I2)') k
             write(50,*) trim(numcat(trim(numcat('plot "'// trim(filename) //  '"  every :::',i-1))//'::',i-1))//'\'
             write (50,*) 'notitle w l linestyle '//trim(numstr)
             write(50,*) trim(numcat(trim(numcat('plot "'// trim(filename) //  '"  every :::',i-1))//'::',i-1))//'\'
             write (50,*) ' using 1:($13 >0 ? $2:1/0) notitle w p pt 7 ps 1'
             write (50,*)
            enddo
           
           endif

           write (50,*)


          else
            write (50,'(''window'',4I4)') plot_col,plot_row, plot_x, plot_y
            write (50,*) 'data "' // trim(filename) //'.dat"'
            write (50,*) 'read { x 1 y 2}'
            write (50,'(''limits '',4E15.5)') limmin(ix1), limmax(ix1), limmin(ix2), limmax(ix2)
!           else
!              write (50,*) 'limits x 0 1.1'
!           end if
            write (50,*) 'box 1 3 3 3'
            if (BW) then
             write (50,*) 'lweight 2'
             if (j>1) write (50,*) 'ltype 2'
            else
             write (50,*) 'ctype ',j+2
            end if          
            write (50,*) 'connect x y'
            write (50,*) 'ltype 0'
            write (50,*) 'xlabel '// trim(labels(ix1))
            write (50,*) 'ylabel '// trim(labels(ix2))
            write (50,*)
            write (50,*) '#Next...'
            write (50,*) 'quit'
           endif


          enddo
 


        endif   
          
        if(num_2D_plots > 1) write (50,*) 'unset multiplot' 
        write (50,*)
        write (50,*) 'reset'           
        close(50)


!do 2D bins and produce matlab file
      
         

!Do 3D plots (i.e. 2D scatter plots with coloured points)
       if(num_2D_plots > 1) then 
        if (num_3D_plots /=0) then
          write (*,*) 'producing ',num_3D_plots, '3D colored scatter plots'

          if (gplot) then 
            filename = trim(rootname)//'_3D.gp'
            open(unit=50,file=filename,form='formatted',status='replace')

            if(num_3D_plots == 1) then
             write (50,*) 'set terminal postscript enhanced color solid 14'
            else
             write (50,*) 'set terminal postscript enhanced color solid 12'
            endif
            write (50,*)

            write (50,*) 'set output "'//trim(rootname)//'_3D.eps"'
            write (50,*) 
!            if (num_3D_plots == 2) write (50,*) 'set size square 1.,1.' 
            
            write (50,*)                   
            write (50,*) 'set nokey'
            write (50,*)

            write (50,*)  'set mxtics 2'
            write (50,*)  'set mytics 2'
            write (50,*) 
           
            num_eff = 1 
            if(num_3D_plots >= 2) num_eff = 2
            j = 1
            k = 0
            do i = 1, nincols-num_soft
             if(.not. isused(i)) then
               write (numstr,'(E10.3)') coldata(i,0)
               if(any(j==cut(num_eff,:))) then 
                title = trim(labels(i))//' = '//trim(adjustl(numstr))//', \'
                write (50,*)
                write (50,*) 'set label "'//trim(title)                        
               elseif(any(j==cut(num_eff,:)-1)) then
                k = k + 1
                write(coordstr,'(E10.3)') ylb(num_eff,k)
                title = trim(labels(i))//' = '//trim(adjustl(numstr))
                write (50,*) trim(title)//' " at screen  0.05,'//trim(coordstr)//' font "0.6 " '
               else
                title = trim(labels(i))//' = '//trim(adjustl(numstr))//', \'
                write (50,*) trim(title)
               endif
               j = j + 1
             endif
            enddo


            write (50,*)

            write (50,*) 'set pm3d map'
            write (50,*)

            write (50,*) 'set palette  rgbformulae '//trim(RGB)
            write (50,*)
            write (50,*) 'set colorbox horizontal'
            write (50,*)
            
            if(num_3D_plots > 1) write (50,*) 'set multiplot'
            write (50,*) 
          else
 
            filename = trim(rootname)//'_3D.m'
            open(unit=50,file=filename,form='formatted',status='replace')
            write (50,*) 'clf;colormap(''jet'');'
            if (num_3D_plots ==1 ) then
               write(50,*) 'lab_fontsize = 12; axes_fontsize = 12;'
            else
               write(50,*) 'lab_fontsize = 9; axes_fontsize = 9;'
            end if
            if (mod(num_3D_plots,2)==0 .and. num_3D_plots < 11) then
               plot_col = num_3D_plots/2
            else
               plot_col =  nint(sqrt(1.*num_3D_plots))
            end if
            plot_row = (num_3D_plots +plot_col-1)/plot_col 

            write(50,*) 'pts = load(''//trim(infile)//'');'

          endif

          do j=1, num_3D_plots
            read(plot_3D(j),*) ix1, ix2, ix3  !x, y, color
!3D data file 
            call Make3DSamples(ix1,ix2,ix3)

            fname = numcat(trim(numcat(trim(numcat(trim(rootname)//'_',ix1))//'_',ix2))//'_',ix3)
            filename = 'plot_data/'// trim(fname) //'.dat'
            plot_x = mod(j+plot_col-1,plot_col)+1
            plot_y = plot_row - (j+plot_col-1)/plot_col + 1

            if (gplot) then 
              write(50,*)
! odd number of plots
              if(num_3D_plots == 1) then 
               write (50,*) 'set size 1.,1.'
               write (50,*) 'set origin 0.,0.02' 
               write (50,*)
               write (50,*) 'set colorbox user origin 0.15, 0.07 size 0.7,0.03'
               write (50,*)
               write (50,*) 'set cblabel " '//trim(labels(ix3))//' " 0.,-0.4'
               write (50,*)
              endif
              if(num_3D_plots > 1 .and. mod(num_3D_plots,2) > 0) then
               write (50,'(" set size ",E10.3,",",E10.3)') 2./(num_3D_plots+1), 0.5
               write (50,*)
               if(j <= (num_3D_plots+1)/2) then
                 write (50,'(" set origin",E10.3,",",E10.3)') (j-1)*2./(num_3D_plots+1), 0.53
                 write(50,'(" set colorbox user origin",E10.3,",",E10.3," size",E10.3,",",E10.3)') (j-1)*2./(num_3D_plots+1)+0.05, 0.53, 0.8*2./(num_3D_plots+1) , 0.02                                       

                 write (50,*)

                 write(50,*) 'set cblabel "' //trim(labels(ix3))// ' " 0.,.55'

                 write (50,*)
               else
                 write (50, '(" set origin",E10.3,",",E10.3)') (j-(num_3D_plots+1)/2-1)*2./(num_3D_plots+1), 0.048

                 write(50,'(" set colorbox user origin",E10.3,",",E10.3," size",E10.3,",",E10.3)') (j-(num_3D_plots+1)/2-1)*2./(num_3D_plots+1)+0.05, 0.049,  0.8*2./(num_3D_plots+1) , 0.02


                  write (50,*)
                 
                   write(50,*) 'set cblabel "' //trim(labels(ix3))// ' " 0,-5'

                  write (50,*)

               endif
! even number of plots
              else if(num_3D_plots > 1) then
              if(num_3D_plots == 2) then
                 write (50,'(" set size ",E10.3,",",E10.3)') 0.5, 0.5
              else 
                 write (50,'(" set size ",E10.3,",",E10.3)') 2./num_3D_plots, 0.5
              endif
                write (50,*)
                if(j <= num_3D_plots/2) then
                  if(num_3D_plots == 2) then 
                   write (50,'(" set origin",E10.3,",",E10.3)') 0.3, 0.53
                   write(50,'(" set colorbox user origin",E10.3,",",E10.3," size",E10.3,",",E10.3)') 0.35, 0.53, 0.4 , 0.02 
                 else
                  write (50,'(" set origin",E10.3,",",E10.3)') (j-1)*2./num_3D_plots, 0.53
                  write(50,'(" set colorbox user origin",E10.3,",",E10.3," size",E10.3,",",E10.3)')  (j-1)*2./num_3D_plots+0.05, 0.53,  0.8*2./num_3D_plots , 0.02
                  endif
                  write (50,*)

                 write(50,*) 'set cblabel "' //trim(labels(ix3))// ' " 0,.55'

                 write (50,*)
                else
                 if(num_3D_plots == 2) then 
                   write (50, '(" set origin",E10.3,",",E10.3)') 0.3, 0.048
                   write(50,'(" set colorbox user origin",E10.3,",",E10.3," size",E10.3,",",E10.3)') 0.35, 0.049, 0.4 , 0.02 
                 else
                  write (50, '(" set origin",E10.3,",",E10.3)') (j-num_3D_plots/2-1)*2./num_3D_plots, 0.048
                  write(50,'(" set colorbox user origin",E10.3,",",E10.3," size",E10.3,",",E10.3)')  (j-num_3D_plots/2-1)*2./num_3D_plots+0.05, 0.049, 0.8*2./num_3D_plots , 0.02
                  endif 
                 write (50,*)

                  write(50,*) 'set cblabel "' //trim(labels(ix3))// ' " 0,-5'
                  write (50,*)

                endif            
!ruiz
              endif



              write (50,'('' set xrange[ '',E10.5,":",E10.5,"]")') limmin(ix1), limmax(ix1) 
              write (50,'('' set yrange[ '',E10.5,":",E10.5,"]")') limmin(ix2), limmax(ix2) 

              write (50,*) 

              if(num_3D_plots > 2) then 
               write (50, '(" set cbtics",I4)') int((limmax(ix3) - limmin(ix3))/5.)
               write (50,*)
              endif

              write (50,*) 'set xlabel "' // trim(labels(ix1))// ' "'
              write (50,*) 'set ylabel "' // trim(labels(ix2))// ' "'
              write (50,*) 


              write (50,*) 'splot "'// trim(filename) //'",\'
              write (50,*) '      "'// trim(filename) //'" using 1:2:($13 > 0 ? $3:1/0)\' 
              write (50,*) 'notitle w p pt 7 ps 1'

              write (50,*)


            !matlab
            else

              if (ix3<1) ix3 = MostCorrelated2D(ix1,ix2,ix3)
                 
              write (50,'(''subplot('',1I5,'','',1I5,'','',1I5,'');'')') plot_row,plot_col,j   
              write (50,*) '%Do params ',ix1,ix2,ix3
              write (50,*) 'scatter(pts(:,',ix1,'),pts(:,',ix2,'),3,pts(:,',ix3,'));'
              fmt = ''',''FontSize'',lab_fontsize);'
              write (50,*) 'xlabel('''//trim(labels(ix1))//trim(fmt)
              write (50,*) 'ylabel('''//trim(labels(ix2))//trim(fmt)
              write (50,*) 'set(gca,''FontSize'',axes_fontsize); ax = gca;'
              write (50,*) 'hbar = colorbar(''horiz'');axes(hbar);'

              write (50,*) 'xlabel('''//trim(labels(ix3))//trim(fmt)
              write (50,*) 'set(gca,''FontSize'',axes_fontsize);'
              if (num_3D_plots > 2 .and. matlab_version < 7) then
                write (50,*) ' p = get(ax,''Position'');'
                write (50,*) 'set(ax,''Position'',[p(1) (p(2)+p(4)/8) p(3) p(4)]);'
              elseif (matlab_version==7) then
!workaround for colorbar/label overlap bug
                write (50,*) 'fix_colorbar(hbar,ax); axes(ax);'
              end if
                  
            endif
          end do


           
          if (gplot) then 
             if(num_3D_plots > 1) write (50,*) 'unset multiplot' 
             write (50,*)

             write (50,*) 'reset'           
             close(50)
          else
 
             write (50,*)  'set(gcf, ''PaperUnits'',''inches'');'
     
             tmpstr = RealToStr((plot_col*8.)/plot_row)
             write (50,*) 'set(gcf, ''PaperPosition'',[ 0 0 '//  &
                            trim(tmpstr) //' 8]);'

             write (50,*) 'print -dpsc2 '//trim(rootname)//'_3D.ps;'

          endif
          close(50)
        end if
       endif
        


end program GetDist

