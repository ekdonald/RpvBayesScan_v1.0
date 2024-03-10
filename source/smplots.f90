!
! SuperBayes Package
! 
! by Farhan Feroz (f.feroz@mrao.cam.ac.uk), Roberto Ruiz de Austri (rruiz@ific.uv.es), Pat Scott (patscott@physics.mcgill.ca) and Roberto Trotta (r.trotta@imperial.ac.uk)
!
! This version Dec 2012
!
! Module containing 
!       - plotting routines for SM (1D)
!
! Based on GetDist by Anthony Lewis & Sarah Bridle (cosmologist.info/cosmomc)


module smplots

  use mcsamples
  implicit none  

  contains

    subroutine WriteSM1DPlotInit(unit, tag)
      integer, intent(in) :: unit
      character(LEN=*), intent(in) :: tag

      write (unit,'(A)') 'DEFINE TeX_strings 1'
      write (unit,'(A)') 'device postfile ../'//trim(rootname)//trim(tag)//'.ps'
      write (unit,'(A)') 'DEFINE default_font rm'
      if (num_vars > 6) then
         write (unit,'(A)') 'define ex_st 0.8'
         write (unit,'(A)') 'define lw_curves 5'
         write (unit,'(A)') 'define ex_label 0.8'
         write (unit,'(A)') 'define x_gutter 0.9'
         write (unit,'(A)') 'define y_gutter 0.8'
      else
         !RT write (unit,'(A)') 'expand 1.1'
         write (unit,'(A)') 'define ex_st 1.5'
         write (unit,'(A)') 'define lw_curves 8'
         write (unit,'(A)') 'define ex_label 1.1'
         write (unit,'(A)') 'define x_gutter 0.5'
         write (unit,'(A)') 'define y_gutter 0.8'
      endif
      write (unit,'(A)') 'location 2500 31000 3500 31000'
      write (unit,'(A)') 'define lw_st 5 lweight $lw_st expand $ex_st'
      !plotting colours and styles 
      write (unit,'(A)') 'define lt_pdf 2 define ct_pdf  "blue"'
      write (unit,'(A)') 'define lt_mlike 1 define ct_mlike  "red"'
      write (unit,'(A)') 'define lt_mchi 2 define ct_mchi  "green"'
      write (unit,'(A)') 'define lt_prof 0 define ct_prof  "red"'
      write (unit,'(A)') 'define lt_like 1 define ct_like  "black"'
      write (unit,'(A)') 'define prof_ones_err "green" define prof_twos_err "red" '
      write (unit,'(A)') 'define pdf_ones_err "green" define pdf_twos_err "blue" '
      write (unit,'(A)') 'define lw_ones_err 18  define lw_twos_err 18 '
      !separation between bar 
      write (unit,'(A)') 'define ymax 1.3'
      write (unit,'(A)') 'set yloc_pdf_err = 1.12'
      write (unit,'(A)') 'define yloc_pdf_err (yloc_pdf_err)'
      write (unit,'(A)') 'define yloc_prof_err 1.25'     

      !if (plot_1D_likelihood) call InitializeDataSets

    end subroutine WriteSM1DPlotInit

  
    subroutine Write1DPlotSM(unit, aroot, j)

      integer :: unit, j, cc, ii
      character(LEN=*), intent(in) :: aroot
      character(LEN=20) :: ptype
      logical :: plot_like_in_this_panel

      write (unit,'(''window'',4I4)') plot_col,plot_row, plot_x, plot_y
      write (unit,'(A)') 'data "../../'//trim(aroot)//'.dat"'
      write (unit,'(A)') 'read {x 1 pdf 2 mlike 3 mchi 4 prof 5}'
      !if (ix_min(j) == ix_max(j)) then
      !   write (unit,'(''limits '',2E15.5,'' y'')') center(j) - abs(center(j))/2-0.1, &
      !        center(j) + abs(center(j))/2+0.1
      !else
      if (has_plot_limits(colix(j))) then
         !write(*,*) 'I have found limits for j = ', j
         write (unit,'(''limits '',2E15.5,'' 0 $ymax'')')  plot_limits_min(colix(j)),  plot_limits_max(colix(j))
      else
         write (unit,'(A)') 'limits x 0 $ymax'
      end if

      if (smoothing) then
         ptype = ' connect '
      else
         ptype = ' histogram '
      end if

      if (plot_1D_pdf) then
         write (unit,'(A)') 'ltype $lt_pdf ctype $ct_pdf lweight $lw_curves'
         write (unit,'(2A)') trim(ptype)//' x pdf'
      end if
      if (plot_1D_meanlike) then
         write (unit,'(A)') 'ltype $lt_mlike ctype $ct_mlike lweight $lw_curves '
         write (unit,'(2A)') trim(ptype)//' x mlike'
      end if
      if (plot_1D_meanchisq) then
         write (unit,'(A)') 'ltype $lt_mchi ctype $ct_mchi lweight $lw_curves'
         write (unit,'(A)') 'lim x mchi'
         write (unit,'(2A)') trim(ptype)//' x mchi'
         write (unit,'(A)') 'ltype 0 ctype black'
         !adds chi-sq value on the right-hand y-axis
         write (unit,'(A)') 'box 4 4 4 2'
         write (unit,'(A)') 'lim x 0 $ymax'
      end if
      if (plot_1D_profile) then
         write (unit,'(A)') 'ltype $lt_prof ctype $ct_prof lweight $lw_curves'
         write (unit,'(2A)') trim(ptype)//' x prof'
      end if

      if (plot_1D_likelihood) then
         !Warning, the stuff below depends on the order of the params 
         plot_like_in_this_panel = .true.
         select case(colix(j)-2)
         case(6)
            !call Gaussian_1DPlot(unit,Mbottom,6)
         case(7)
            !call Gaussian_1DPlot(unit,Mtop,7)
         case(8)
            !call Gaussian_1DPlot(unit,alpha_S,8)
         case(9)
            !call Gaussian_1DPlot(unit,alpha_em_inv,9)
         case(10)
            !call Gaussian_1DPlot(unit,CDM,10)
         case(15)
            !call Gaussian_1DPlot(unit,AnomalousMu,15)
         case(16)
            !call Gaussian_1DPlot(unit,Bsgamma,16)
         case(20)
            !call Gaussian_1DPlot(unit,MW,20)
         case(21)
            !call Gaussian_1DPlot(unit,SinEff,21)
         case(22)
            !call Gaussian_1DPlot(unit,RDeltaMBs,22)
         case(23)
            !call Gaussian_1DPlot(unit,RBtaunu,23)
         case(24) !Higgs limit 
            !call SmearedBound_1DPlot(unit,sigma=real(MHiggs%sigma), tau=real(MHiggs%tau), limit=real(MHiggs%mu) ) 
         case default
                plot_like_in_this_panel = .false.
         end select

         if (plot_like_in_this_panel) then
            write (unit,'(A)') 'ltype $lt_like ctype $ct_like lweight $($lw_curves*1.5) '
            write (unit,'(2A)')'connect xx like'
         end if
      end if

      if (plot_mean) then
         write (unit,'(A)') 'ltype 0 ctype $ct_pdf lweight $($lw_curves/2)'
         write (unit,'(A, 2E15.5, A)') 'set xm = {', mean(j),mean(j),' } ' 
         write (unit,'(A)') 'set ym = {-10000 10000}'
         write (unit,'(A)') 'connect xm ym'
      end if
      if (plot_1D_pdf) then
         !plot 1D posterior credible intervals
         !2sigma errors
         write (unit,'(A)') 'ctype $pdf_twos_err ltype 0 lweight  $lw_twos_err  '
         !uncomment below if you want errorbars rather than lines
         !write (unit,'(A, E15.5, A, E15.5, A)') 'errorbar (', mean(j), ') (yloc_pdf_err) (', mean(j)-cont_lines(j,1,2), ') 3'
         !write (unit,'(A, E15.5, A, E15.5, A)') 'errorbar (', mean(j), ') (yloc_pdf_err) (', cont_lines(j,2,2)-mean(j), ') 1'
         write (unit,'(A, 2E15.5, A)') 'set xs = {',  cont_lines(j,1,2),  cont_lines(j,2,2), '}'
         write (unit,'(A)') 'set ys = {$!!yloc_pdf_err $!!yloc_pdf_err}'
         write (unit,'(A)') 'con xs ys'         
         !1sigma errors
         write (unit,'(A)') 'ctype $pdf_ones_err lweight $lw_ones_err '
         !uncomment below if you want errorbars rather than lines
         !write (unit,'(A, E15.5, A, E15.5, A)') 'errorbar (', mean(j), ') (yloc_pdf_err) (', mean(j)-cont_lines(j,1,1), ') 3'
         !write (unit,'(A, E15.5, A, E15.5, A)') 'errorbar (', mean(j), ') (yloc_pdf_err) (', cont_lines(j,2,1)-mean(j), ') 1'
         write (unit,'(A, 2E15.5, A)') 'set xs = {',  cont_lines(j,1,1),  cont_lines(j,2,1), '}'
         write (unit,'(A)') 'con xs ys'         
         write (unit,'(A)') 'lweight $lw_st'        
      end if

      if (plot_bestfit) then
         write (unit,'(A)') 'ltype 0 ctype $ct_prof lweight $lw_curves'
         write (unit,'(A, 1E15.5, A)') 'set xbf = {', coldata(colix(j),0),' } ' 
         write (unit,'(A)') 'set ybf = {0}'
         write (unit,'(A)') 'lweight 50 ptype 4 1 points xbf ybf lweight $lw_st '
      end if
      if (plot_1D_profile) then
         !plot 1D confidence regions (likelihood ratio test)
         do cc  = 2,1,-1
            do ii = 1, int_num(j,cc)
               write (unit,'(A)') 'ltype 0'
               if (cc .eq. 1) then
                  write (unit,'(A)') 'ctype $prof_ones_err lweight $lw_ones_err'
               else
                  write (unit,'(A)') 'ctype $prof_twos_err lweight $lw_twos_err '
               end if
               write (unit,'(A, 2E15.5, A)') 'set xs = {',  profl_interval(j, 1, ii, cc),  profl_interval(j, 2, ii, cc), '}'
               !write (*,*) 'ii (which intvl) = ', ii, 'cc = ', cc, ' inteval = ',  profl_interval(j, 1, ii, cc),  profl_interval(j, 2, ii, cc), '}'
               
               ! write (unit,'(A, 2E15.5, A)') 'set ys = {', profl_cont(cc), profl_cont(cc), '}'
               write (unit,'(A)') 'set ys = {$!!yloc_prof_err $!!yloc_prof_err}'
               write (unit,'(A)') 'con xs ys lweight $lw_st'
            end do
         end do
      end if
      if (plot_reference) then
         call DefineRefPoint(rf)
         write (unit,'(A)') 'ltype 0 ctype green'
         write (unit,'(A, 1E15.5, A)') 'set xrf = {', rf(colix(j)-2),' } ' 
         write (unit,'(A)') 'set yrf = {0}'
        write (unit,'(A)') 'ptype 20 3 lweight 5 expand 2 points xrf yrf lweight $lw_st expand $ex_st '     
      end if
      write (unit,'(A)') 'lweight $lw_st '       
      !         write (unit,'(A)') 'ltype 1'
      !         do ix1 = 1, num_contours
      !         do i2 =1,2
      !         write (numstr,'(A)') cont_lines(j,i2,ix1)
      !         write (unit,*) 'set x = {'//trim(numstr)//' '//trim(numstr)//'}'
      !         write (unit,*) 'set y = { 0 1.1}'
      !!         write (unit,*) 'connect x y'
      !uncomment this if you want dooted lines on 1D plots for condifence regions
      !Note 1D confidence regions are computed from full samples using different definition to
      !2D confidence regions
      !         end do
      !         end do

      write (unit,'(A)') 'ltype 0 ctype black'
      write (unit,'(A)') 'xlabel '// trim(lables(colix(j)))
      if (plot_1D_meanchisq) then
         write (unit,'(A)') 'box 1 3 3 4'
      else
         write (unit,'(A)') 'box 1 3 3 3'
      end if
      write (unit,'(A)')
      write (unit,'(A)') '###### Next panel ########'

    end subroutine Write1DPlotSM

    subroutine Gaussian_1DPlot(unit, datum,j) 
      integer :: unit
      Type(LikeDatum) :: datum
      integer :: j
      write (unit,'(A,E15.5)') 'define mean ', datum%mu 
      if ((datum%tau .ne. 0) .and. (.not. datum%tau_percent)) then
         write (unit,'(A,E15.5)') 'define sig  ', sqrt(datum%sigma**2 + datum%tau**2)  
      else
         write (unit,'(A,E15.5)') 'define sig  ', datum%sigma 
      end if
      !Gaussian normalized to the peak
      if (.not. has_plot_limits(j+2)) then
          write (unit,'(A)') 'set xx = x'
      else
        
          write (unit,'(A,E15.5,A,E15.5,A,E15.5)') 'set xx = ', &
            plot_limits_min(2+j),',', plot_limits_max(2+j),',', &
            (plot_limits_max(2+j)- plot_limits_min(2+j))/100           
      end if
      write (unit,'(A)') 'set like = (gauss(xx)*SQRT(2*PI)*$sig)'

    end subroutine Gaussian_1DPlot

    subroutine SmearedBound_1DPlot(unit,sigma, tau, limit ) 
      integer :: unit
      real :: tau, sigma, limit

      write (unit,'(A,E15.5)') 'set tau = ', tau
      write (unit,'(A,E15.5)') 'set sigma = ', sigma   
      write (unit,'(A,E15.5)') 'set mstar = ', limit      
      write (unit,'(A)') 'set tstar = (sigma/tau)*(mstar-x)/sqrt(sigma**2+tau**2)'
      write (unit,'(A)') 'set t2 = (mstar-x)/tau'
      write (unit,'(A)') 'set like = 1/sqrt(2*PI*(sigma**2+tau**2)) & 
         *EXP(-0.5*(x-mstar)**2/(sigma**2+tau**2))*(1- 1/2*erfc(tstar/sqrt(2))) + 1/2*erfc(t2/sqrt(2))' 
      write (unit,'(A)') 'set xx = x'
    end subroutine SmearedBound_1DPlot

    subroutine FinalTouches1DPlotSM(unit)
      integer, intent(in) :: unit
      integer :: i
      real :: delta = 0.36 , y = 1.00, line = 0.07

      !if there is anything else you need to tweak...
      write(unit, '(A)') 'window 1 1 1 1 location 2500 32000 3500 32000 lim 0 1 0 1'
      i = 0
      if (plot_1D_pdf) then
         write (unit,'(A)') 'ltype $lt_pdf ctype $ct_pdf  expand $ex_label'
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  Posterior pdf"'
         i = i+1
      end if
      if (plot_1D_meanlike) then
         write (unit,'(A)') 'ltype $lt_mlike ctype $ct_mlike expand $ex_label '
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  mean like"'
         i = i+1
      end if
      if (plot_1D_meanchisq) then
         write (unit,'(A)') 'ltype $lt_mchi ctype $ct_mchi expand $ex_label '
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  mean \chi^2"'
         i = i+1
      end if
      if (plot_1D_profile) then
         write (unit,'(A)') 'ltype $lt_prof ctype $ct_prof expand $ex_label'
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  Profile likelihood"'
         i = i+1
      end if
      write (unit,'(A)') 'ctype black expand $ex_label'
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  1.00 ', y
         if (use_log) then
            write(unit, '(A)') 'putlabel 4 " (Log priors)"'
         else
            write(unit, '(A)') 'putlabel 4 " (Flat priors)"'
         end if
      write (unit,'(A)') 'expand $ex_st'
      write(unit, '(A)') 'quit'

    end subroutine FinalTouches1DPlotSM
    

!-----------------------------------------------------
!  Routines to plot the position of the chain as a fct of step number
!-----------------------------------------------------
  subroutine WriteSMChainsInit(unit, tag)
      integer, intent(in) :: unit
      character(LEN=*), intent(in) :: tag

      write (unit,'(A)') 'DEFINE TeX_strings 1'
      write (unit,'(A)') 'DEFINE default_font rm'
      if (num_vars > 6) then
         write (unit,'(A)') 'expand 0.8'
         write (unit,'(A)') 'define x_gutter 0.9'
         write (unit,'(A)') 'define y_gutter 0.9'
      else
         write (unit,'(A)') 'expand 1.1'
      endif
      write (unit,'(A)') 'define lw_st 3 lweight $lw_st'
      write (unit,'(A)') 'define lt_ref 0 define ct_ref  "black"'
      write (unit,'(A)') 'define lt_mean 1 define ct_mean  "green"'
      write (unit,'(A)') 'define lt_bf 2 define ct_bf  "blue"'
      write (unit,'(A)') 'define lt_prof 3 define ct_prof  "blue"'

      !write (unit,'(A)') 'verbose 0'     
      write (unit,'(A)')
      write (unit,'(A)') '###### data loading  ########'  

    end subroutine WriteSMChainsInit


    subroutine  WriteSMLoadChains(unit, aroot)
      character(LEN=*), intent(in) :: aroot
      integer :: unit, j, i, ch, ic
      character(LEN = 500) :: read_string, datafile
      character(LEN=80) :: chstr, colstr

      !opens and reads chain, all variables read in at once
      !loops over chains

      do ic=1, num_chains_used
         ch = chain_numbers(ic)
         if (chain_num .eq. 0) then
            datafile = trim(aroot)
            write (chstr,*) 0
         else

            write (chstr,*) ch
            datafile = trim(aroot) //'_'//trim(adjustl(chstr))
         end if

         write (unit,'(A)') 'data "../../'//trim(datafile)//'.txt"'
         read_string = ''
         do i=1,num_vars
            write(colstr, *) colix(i)
            read_string = trim(read_string)//' ch'//trim(adjustl(chstr))//'y'//trim(adjustl(colstr))//' '//trim(adjustl(colstr))
         end do
         write (unit,'(A,A,A)') 'read {ch'//trim(adjustl(chstr))//'chisq 2 ', trim(read_string), '}'
         write (unit,'(A)') 'set ch'//trim(adjustl(chstr))//'n =1,DIMEN(ch'//trim(adjustl(chstr))//'chisq)'
         write (unit,'(A)') 'set ch'//trim(adjustl(chstr))// &
            'm = ch'//trim(adjustl(chstr))//'n % 10  set ch'// &
            trim(adjustl(chstr))//'nn = ch'//trim(adjustl(chstr))// &
            'n IF(ch'//trim(adjustl(chstr))//'m == 0)'
         !thinning of vectors for plotting
         write (unit,'(9A)') 'set ch'//trim(adjustl(chstr))//'chisqyy' & 
                 & //' = 2*ch'//trim(adjustl(chstr))//'chisq'//' IF(ch'//trim(adjustl(chstr))//'m == 0)'
         do i=1,num_vars   
            write(colstr, *) colix(i) 
            write (unit,'(9A)') 'set ch'//trim(adjustl(chstr))//'yy'//trim(adjustl(colstr)) & 
                 & //' = ch'//trim(adjustl(chstr))//'y'//trim(adjustl(colstr))//' IF(ch'//trim(adjustl(chstr))//'m == 0)'
         end do
         
      end do !chains loop

    end subroutine WriteSMLoadChains


    subroutine ChainPositionPlot(unit, ch, j)

      integer :: unit, j, ch
      character(LEN=150) :: colstr, chstr
      write (unit,'(A)') 'window 1 1 1 1 location 2500 31000 3500 30000'
      write (unit,'(''window'',4I4)') plot_col,plot_row, plot_x, plot_y
      write(colstr, *) colix(j)
      write(chstr, *) ch
      write (unit,'(2A)') 'limits ch'//trim(adjustl(chstr))//'nn ch'//trim(adjustl(chstr))//'yy'//trim(adjustl(colstr))
      write (unit,'(2A)') 'ltype 0 ctype red con ch' &
        //trim(adjustl(chstr))//'nn ch'//trim(adjustl(chstr))//'yy'//trim(adjustl(colstr))
      write (unit,'(A)') 'ltype 0 ctype black'
      write (unit,'(A)') 'ylabel '// trim(lables(colix(j)))
      write (unit,'(A)') 'xlabel Step number '      
      write (unit,'(A)') 'box '
      if (plot_reference) then
         call DefineRefPoint(rf)
         write (unit,'(A)') 'ltype $lt_ref ctype $ct_ref'
         write (unit,'(A, 2E15.5, A)') 'set yrf = {', rf(colix(j)-2), rf(colix(j)-2),' } ' 
         write (unit,'(A)') 'set xrf = {-1E6 1E6}'
         write (unit,'(A)') 'lweight $(3*$lw_st) con xrf yrf lweight  $lw_st'        
      end if
      if (plot_mean) then
         write (unit,'(A)') 'ltype $lt_mean ctype $ct_mean'
         write (unit,'(A, 2E15.5, A)') 'set yrf = {', mean(j), mean(j),' } ' 
         write (unit,'(A)') 'set xrf = {-1E6 1E6}'
         write (unit,'(A)') 'lweight $(3*$lw_st) con xrf yrf lweight $lw_st'        
      end if        
      if (plot_bestfit) then
         write (unit,'(A)') 'ltype $lt_bf ctype $ct_bf'
         write (unit,'(A, 2E15.5, A)') 'set yrf = {',  coldata(colix(j),0),  coldata(colix(j),0),' } ' 
         write (unit,'(A)') 'set xrf = {-1E6 1E6}'
         write (unit,'(A)') 'lweight $(3*$lw_st) con xrf yrf  lweight $lw_st'        
      end if
      !burn in 
      write (unit,'(A)') 'ltype 0 ctype black'
      write (unit,'(A, 2I10, A)') 'set xb = {', ignorerows, ignorerows,' } ' 
      write (unit,'(A)') 'vecminmax  ch'//trim(adjustl(chstr))//'yy'//trim(adjustl(colstr))//' min max'
      write (unit,'(A)') 'set yb = {$!!min $!!max}'
      write (unit,'(A)') 'con xb yb '       
      write (unit,'(A)')
      write (unit,'(A)') '###### Next panel ########'      

    end subroutine ChainPositionPlot

   subroutine FinalTouchesChainPlot(unit, ch)
      integer, intent(in) :: unit
      integer :: i, ch
      real :: delta = 0.23 , y = 1.00, line = 0.1
      character(LEN=150) :: chstr

      write(chstr, *) ch
      !if there is anything else you need to tweak...
      write (unit,'(A)')
      write (unit,'(A)') '###### Legend  ########'           
      write(unit, '(A)') 'window 1 1 1 1 location 2500 32000 3500 32000 lim 0 1 0 1'
      i = 0
      write (unit,'(A)') 'ltype 0 ctype black'
      write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
      write(unit, '(A)') 'putlabel 6 " CHAIN #"'//trim(adjustl(chstr))     
      i = i+1

      if (plot_reference) then
         write (unit,'(A)') 'ltype $lt_ref ctype $ct_ref'
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  Ref point"'
         i = i+1
      end if
      if (plot_mean) then
         write (unit,'(A)') 'ltype $lt_mean ctype $ct_mean'
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  mean"'
         i = i+1
      end if
      if (plot_bestfit) then
         write (unit,'(A)') 'ltype $lt_bf ctype $ct_bf'
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'relocate  $(0.01+', i*delta, ') ', y
         write(unit, '(A, 1E10.5, A, 1E10.5)') 'draw  $(0.01+', i*delta+line, ') ', y
         write(unit, '(A)') 'putlabel 6 "  best fit"'
      end if

    end subroutine FinalTouchesChainPlot
    

    subroutine BurnInPlot(unit, ch)

      integer :: unit, ch
      character(LEN=150) :: colstr, chstr, minstr
      write (unit,'(A)') 'window 1 1 1 1 location 2500 31000 3500 30000'
      write (unit,'(''window'',4I4)') plot_col,plot_row, plot_x, plot_y
      write(chstr, *) ch
      write (unit,'(2A)') 'limits ch'//trim(adjustl(chstr))//'nn ch'//trim(adjustl(chstr))//'chisqyy'
      write (unit,'(2A)') 'ltype 0 ctype red con ch'//trim(adjustl(chstr))//'nn ch'//trim(adjustl(chstr))//'chisqyy'
      write (unit,'(A)') 'ltype 0 ctype black'
      write (unit,'(A)') 'ylabel \chi^2'
      write (unit,'(A)') 'xlabel Step number '      
      write (unit,'(A)') 'box '
      if (plot_mean) then
         write (unit,'(A)') 'ltype $lt_mean ctype $ct_mean'
         write (unit,'(A, 2E15.5, A)') 'set yrf = {', 2*meanchisq, 2*meanchisq,' } ' 
         write (unit,'(A)') 'set xrf = {-1E6 1E6}'
         write (unit,'(A)') 'lweight $(3*$lw_st) con xrf yrf lweight $lw_st'        
      end if        
      if (plot_bestfit) then
         write (unit,'(A)') 'ltype $lt_bf ctype $ct_bf'
         write (unit,'(A, 2E15.5, A)') 'set yrf = {',  2*maxlike, 2*maxlike,' } ' 
         write (unit,'(A)') 'set xrf = {-1E6 1E6}'
         write (unit,'(A)') 'lweight $(3*$lw_st) con xrf yrf  lweight $lw_st'        
      end if
      !burn in 
      write (unit,'(A)') 'ltype 0 ctype black'
      write (unit,'(A, 2I10, A)') 'set xb = {', ignorerows, ignorerows,' } ' 
      write (unit,'(A)') 'set yb = {-1E6 1E6}'
      write (unit,'(A)') 'con xb yb '       
      write (unit,'(A)')
      write (unit,'(A)') '###### chain number  ########'           
      write (unit,'(A)') 'ltype 0 ctype black'
      write(unit, '(A, 1E10.5, A, 1E10.5)') 'lim 0 1 0 1 relocate 0.3 0.9 '
      write(unit, '(A)') 'putlabel 6 " CHAIN #"'//trim(adjustl(chstr))     
      !chisqyy has already been multiplied by two
      !but here we need the min over the whole samples in the chain
      write (unit,'(A)') 'vecminmax  ch'//trim(adjustl(chstr))//'chisq'//' min max'
      write(unit, '(A)') 'define minv (sprintf(''%10.5f'',(2*$min)))'
      write(unit, '(A)') 'relocate 0.3 0.8 putlabel 6 "best-fit \chi^2 = "'
      write(unit, '(A)') 'putlabel 6 $minv'
      write (unit,'(A)') '###### Next panel ########'      

    end subroutine BurnInPlot

 subroutine FinalTouchesBurnInPlot(unit)
      integer, intent(in) :: unit
      integer :: i
      real :: deltay = 0.03 , y = 1.00, line = 0.1, x = 0.01
      character(LEN=150) :: chstr

      !if there is anything else you need to tweak...
      write (unit,'(A)')
      write (unit,'(A)') '###### Legend  ########'           
      write(unit, '(A)') 'window 1 1 1 1 location 2500 32000 3500 32000 lim 0 1 0 1'

      i = 0
      if (plot_mean) then
         write (unit,'(A)') 'ltype $lt_mean ctype $ct_mean'
         write(unit, '(A, 2E10.5)') 'relocate  ', x,  y-i*deltay
         write(unit, '(A, 2E10.5)') 'draw  ', x+line,  y-i*deltay
         write(unit, '(A)') 'putlabel 6 " global mean \chi^2 = "'
         write(unit, '(A,F15.5)') 'putlabel 6 ', 2*meanchisq
         i = i+1
      end if
      if (plot_bestfit) then
         write (unit,'(A)') 'ltype $lt_bf ctype $ct_bf'
         write(unit, '(A, 2E10.5)') 'relocate  ', x,  y-i*deltay
         write(unit, '(A, 2E10.5)') 'draw  ', x+line,  y-i*deltay
         write(unit, '(A)') 'putlabel 6 " global best fit \chi^2 = "'
         write(unit, '(A,F15.5)') 'putlabel 6 ', 2*maxlike
      end if

    end subroutine FinalTouchesBurnInPlot


  end module smplots
