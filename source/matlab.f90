!
! SuperBayes Package
! 
! by Farhan Feroz (f.feroz@mrao.cam.ac.uk), Roberto Ruiz de Austri (rruiz@ific.uv.es), Pat Scott (patscott@physics.mcgill.ca) and Roberto Trotta (r.trotta@imperial.ac.uk)
!
! This version Dec 2012
!
! This module contains 
!        - plotting routines for matlab (2D and 3D plots)
!
! Based on GetDist by Anthony Lewis & Sarah Bridle (cosmologist.info/cosmomc)


module matlab

  use mcsamples
  implicit none  

  character (LEN = 150) :: matlab_string = 'SuperBayeS v 2.0.0'
  !this will appear on the top right of matlab plots. Set it to = '' to disable it 
  character (LEN = 80) :: credit, othercomments
  integer, parameter :: smooth = 1
  integer, parameter :: sharp = 2
  integer :: color_scheme
  !To customize the look & size of the printed ps files, look for FinalTouchesMATLAB
  !and  FinalTouches_SingleFig_MATLAB below
  contains

!---------------------------------------------------------
    subroutine WriteMatLabInit(unit,D)
!---------------------------------------------------------
      integer, intent(in) :: unit, D
      !D=1 means 1D plot, D=2 means 2D plot, D=3 is for 3D plot
      write (unit, '(A)') 'clear;'
      write (unit, '(A)') 'gclim=[0 1e31];'
      if ((together) .and. (D.eq.2)) then
         write (unit, '(A,I4,A)') 'figure(',ShadeMargProb,');  clf(''reset'')'
         if (plot_2D_meanlike) write (unit, '(A,I4,A)') 'figure(',ShadeMeanLike,');  clf(''reset'')'
         if (plot_2D_meanchisq) write (unit, '(A,I4,A)') 'figure(',ShadeMeanChisq,'); clf(''reset'')'
         write (unit, '(A,I4,A)') 'figure(',ShadeProfLike,');  clf(''reset'')'
         write (unit, '(A,I4,A)') 'figure(',SingleSamples,');  clf(''reset'')'
         write(unit, '(A,I4,A)') 'redf = (1+0.35*',plot_row,');'
         !line width and size of the points (eg bestfit, mean)
         write (unit, '(A)') 'bflw = 2.5;'
         write (unit, '(A)') 'bfsize = 15;'
      else if ((together) .and. (D.eq.1)) then
         write(unit, '(A,I4,A)') 'redf = (1+0.35*',plot_row,');'
         write (unit, '(A)') 'bflw = 1.5;'
         write (unit, '(A)') 'bfsize = 15;'
         write (unit, '(A)') 'figure'
      else 
         write (unit, '(A)') 'redf = 1.00;'
         write (unit, '(A)') 'bflw = 2.5;'
         write (unit, '(A)') 'bfsize = 25;'
      end if
      write(unit, '(A)') 'lab_fontsize = 24/redf; axes_fontsize = 24/redf; &
        &   names_fontsize = 10/redf;  cbar_fontsize = 16/redf;  &
        &   textbox_fontsize = 30/redf; twolines_fontsize = 30/redf; &
        &   exp_fontsize = 14/redf;  '

      !good format for screen viewing 
      !write(unit, '(A)') 'lab_fontsize = 14/redf; axes_fontsize = 10/redf; names_fontsize = 14/redf;  cbar_fontsize = 12/redf;  textbox_fontsize = 20/redf; twolines_fontsize = 20/redf;exp_fontsize = 14/redf;  '
      !format for contours 
      write(unit, '(A)') ' l1=''-k''; lw1=0.8; lwaxes = 2.0;'
      !DD limits 
      write(unit, '(A)') ' lwdd= 3.0; '
      !format for contours for compared models
      write(unit, '(A)') ' l2=''.r''; lw1=1.2; '
      if (plot_reference) call DefineRefPoint(rf)

      if (D.eq.2) then
         write (unit,'(A)') 'load(''../../colormaps/greenmap'',''likegree'');'
         write (unit,'(A)') 'load(''../../colormaps/ModInverseHot'',''modrhot'');'
         write (unit,'(A)') 'load(''../../colormaps/yellowblue'',''yellowblue'');'
         write (unit,'(A)') 'load(''../../colormaps/yellowred'',''yellowred'');'
      write (unit,'(A)') 'load(''../../colormaps/CoolBlue'',''icool'');'
      write (unit,'(A)') 'load(''../../colormaps/SunsetOrange'',''sunset'');'
      else if (D .eq. 3) then
         write(unit,'(A)')  'load(''../../colormaps/Redmap'',''redmap'');'
         write(unit,'(A)')  'load(''../../colormaps/fsystmap'',''fsystmap'');'        
      end if

    end subroutine WriteMatLabInit

!---------------------------------------------------------
    subroutine PlotContMATLAB(aunit,aroot,j,j2, DoContours, ShadeWhat)
!---------------------------------------------------------
      character(LEN=*), intent(in) :: aroot
      integer, intent(in) :: j, j2,aunit, ShadeWhat
      logical, intent(in) :: DoContours
      character(LEN=520) fname,numstr,plotfile, fnroot, rnroot, dir, reldir

      
      fnroot =  ExtractFileName(aroot)
      rnroot =  ExtractFilePath(aroot)
      dir = '../plot_data/'//trim(rnroot)
      reldir = '../'//trim(dir)

      !best fit point
      if (plot_bestfit) write (aunit,'(a, 2E15.5,a)') 'bestfit = [', coldata(colix(j2),0),  coldata(colix(j),0), '];'
      !mean 
      if (plot_mean) write (aunit,'(a, 2E15.5,a)') 'mean = [', mean(j2), mean(j), '];'
      !any other point you might want to plot. Edit coordinates at the beginning of this file
      if (plot_reference) write (aunit,'(a, 2E15.5,a)') 'refpoint = [', rf(colix(j2)-2), rf(colix(j)-2), '];'


      fname =trim(trim(fnroot)//trim(numcat('_p',colix(j2)-2)))
      write (aunit,'(a)') 'tmp = load (fullfile('''//trim(reldir)// ''',''' // trim(fname)//  '.dat''));'
      !xaxis
      write (aunit,*) 'x1 = tmp(:,1);'
      fname =trim(trim(fnroot)//trim(numcat('_p',colix(j)-2)))
      write (aunit,'(a)') 'tmp = load (fullfile('''//trim(reldir)// ''',''' // trim(fname)//  '.dat''));'
      !yaxis
      write (aunit,*) 'x2 = tmp(:,1);'

      plotfile = numcat(trim(numcat(trim(fnroot)//'_2D_',colix(j)-2))//'_',colix(j2)-2)

      write (aunit,'(a)') 'cnt = load (fullfile('''//trim(reldir)//''',''' // trim(plotfile) //'_cont''));'
      if (plot_NDcontours) write (aunit,'(a)') 'load (fullfile('''//trim(reldir)//''',''' // trim(plotfile) //'_confid''));'

      !double checks that the file does exist 
      if (.not. FileExists(trim(dir)//trim(plotfile)//'_marg')) then
         write(*,*) 'Warning (Matlab): data file not found: ', trim(dir)//trim(plotfile)
         return !nothing to plot
      end if

      !print*,ShadeWhat,ShadeMargProb

      if (ShadeWhat .eq.  ShadeMargProb) then
         !shades marginal distribution
         write (aunit,'(a)') 'pdfpts=load (fullfile('''//trim(reldir)// ''',''' // trim(plotfile) //'_marg''));'
         write(numstr,*) ShadeMargProb
         if (color_scheme .eq. smooth) then
                write (aunit,'(a)') 'contourf(x1,x2,pdfpts,64);'
            else
                write (aunit,'(a)') 'pdflvs = [cnt('//trim(numstr)//',:) 0];'
                write (aunit,'(a)') 'contourf(x1,x2,pdfpts,pdflvs);'
         end if
      else if (ShadeWhat .eq. ShadeMeanLike) then
         !shades meanlike
         write (aunit,'(a)') 'load (fullfile('''//trim(reldir)//''',''' // trim(plotfile) //'_likes''));'
         write (aunit,'(a)') 'ah=imagesc(x1,x2,'//trim(plotfile)//'_likes);'
      else if (ShadeWhat .eq. ShadeMeanChisq) then
         !shades mean chisquare
         write (aunit,'(a)') 'load (fullfile('''//trim(reldir)//''',''' // trim(plotfile) //'_chisq''));'
         write (aunit,'(a)') 'ah=imagesc(x1,x2,'//trim(plotfile)//'_chisq);'
      else if (ShadeWhat .eq. ShadeProfLike) then
         !shades profile likelihood
         write(numstr,*) ShadeProfLike
         write (aunit,'(a)') 'profpts = load (fullfile('''//trim(reldir)//''',''' // trim(plotfile) //'_profl''));'
         if (color_scheme .eq. smooth) then
            !write (aunit,'(a)') 'ah=imagesc(x1,x2,profpts);'
            write (aunit,'(a)') 'contourf(x1,x2,profpts,64);'            
         else
            write (aunit,'(a)') 'lvsprof = [cnt('//trim(numstr)//',:) 0];'
            write (aunit,'(a)') 'contourf(x1,x2,profpts,lvsprof);'
         end if
               
      else if (ShadeWhat .eq. SingleSamples) then
         !scatter plot with single samples instead (analogous to 3D plot but with no colour)
         write (aunit,'(a)') 'scpts = load('''//trim(rootname)//'_single.txt'');'         
         !write (aunit, '(A,I4,A,I4,A)') 'scatter(scpts(:,',colix(j2), &
         write (aunit, '(A,I4,A,I4,A)') 'scatter(scpts(:,',j2+2, &
         '),scpts(:,',j+2,'),''r'', ''o'', ''filled'',''SizeData'', 15);'
      end if

      if (ShadeWhat .ne. SingleSamples) write (aunit,'(a)') 'shading flat;'
      write (aunit,'(a)') 'hold on;'
     
    end subroutine PlotContMATLAB


!---------------------------------------------------------
    subroutine PlotContMATLAB2(aunit,aroot,j,j2, DoContours, ShadeWhat)
!---------------------------------------------------------
      character(LEN=*), intent(in) :: aroot
      integer, intent(in) :: j, j2,aunit, ShadeWhat
      logical, intent(in) :: DoContours
      character(LEN=520) fname,numstr,plotfile, fnroot, rnroot, dir, reldir

      
      fnroot =  ExtractFileName(aroot)
      rnroot =  ExtractFilePath(aroot)
      dir = 'plot_data/'//trim(rnroot)
      reldir = '../../'//trim(dir)

         if (ShadeWhat .eq. ShadeMargProb) then
           !puts contours for marg distribution
           if (DoContours) then
              write(numstr,*) ShadeMargProb
              write (aunit,'(a)') '[C h] = contour(x1,x2,pdfpts,cnt('//trim(numstr)//',:),l1);set(h,''LineWidth'',lw1);'
              if (plot_NDcontours) then
                 write(numstr,*) 1-contours(1:num_contours)  
                 if (num_contours==1) numstr = trim(numstr)//' '//trim(numstr)
                 write (aunit,'(a)') '[Cnd hnd] = contour(x1,x2,'//trim(plotfile)//'_confid,['//trim(numstr)//'],''r'');'
              end if
           end if
         else if (ShadeWhat .eq. ShadeProfLike) then
           if (DoContours) then
              write(numstr,*) ShadeProfLike
              write (aunit,'(a)') '[C h] = contour(''v6'',x1,x2,profpts,cnt('//trim(numstr)//',:),l1);set(h,''LineWidth'',lw1);'
           end if
         endif

     
    end subroutine PlotContMATLAB2

!---------------------------------------------------------
    subroutine Write2DPlotMATLAB(aunit,j,j2, ShadeChoice)
!---------------------------------------------------------
      integer, intent(in) :: aunit, j, j2
      integer, intent(in) :: ShadeChoice
      character(LEN=150) gfmt
      
      if (together) then
         !all plots in one file
         write (aunit, '(A,I4,A)') 'figure(',ShadeChoice,')' 
        
         write (aunit,'(''subplot('',1I5,'','',1I5,'','',1I5,'');'')') plot_row,plot_col,plot_num
         !write(pn, *) plot_num
         if (ShadeChoice .eq. ShadeMeanChisq)  write (aunit, '(A,I4,A)') 'ax(',plot_num,') = gca;' 
      else
         write (aunit, '(A)') 'figure'
         write (aunit, '(A)') 'set(gcf, ''NextPlot'', ''replacechildren'');'
         !RRwrite (aunit, '(A)') 'posf=get(0,''DefaultFigurePosition'');'
         !RRwrite (aunit, '(A)') 'set(0,''DefaultFigurePosition'', [posf(1) posf(2) 605 605]);'
         !write (aunit, '(A)') 'axes(''position'', [0.1 0.1 1.0 1.0]   );'
      end if

      write (aunit, '(A)') 'box on; axis square; hold on;'
      
      call PlotContMATLAB(aunit,rootname,j,j2, plot_contours, ShadeChoice) 

         !compare with other chains (contours only)
         !do i = 1, Num_ComparePlots
         !   if (PlotContMATLAB(aunit,ComparePlots(i),j,j2,.false., ShadeMargProb)) &
         !        write (aunit,'(a)') '[C h] = contour(''v6'',x1,x2,pts,cnt,l2);'
         !   write (aunit,'(a)') 'set(h,''LineWidth'',lw2);'
         !end do

      write(aunit,'(A)') 'text(''String'', ''\it{'//trim(matlab_string)// &
         & '}'',''FontSize'',names_fontsize, ''Units'', ''normalized'', &
         & ''Position'', [1.0 1.05], ''HorizontalAlignment'', ''right'');'

      gfmt = ''',''FontSize'',lab_fontsize);'
      !labels, axis 
      !xlabel, slightly more sophisticated
      write (aunit,'(a)') ' text(''String'','''//trim(lables(colix(j2)))// &
       & ''',''FontSize'',lab_fontsize, ''Units'', ''normalized'', &
       & ''Position'', [0.5 -0.18], ''HorizontalAlignment'', ''center'');'
      write (aunit,'(a)') 'ylabel('''//trim(lables(colix(j)))//trim(gfmt)
      write (aunit,'(a)') 'axis tight;' 
      !x-axis limits
      if (has_plot_limits(colix(j2))) write (aunit,'(A,2E15.5,A)') &
         'set(gca,''XLim'', [',  plot_limits_min(colix(j2)),  plot_limits_max(colix(j2)),']);'
      !y-axis limits
      if (has_plot_limits(colix(j))) write (aunit,'(A,2E15.5,A)') &
         'set(gca,''YLim'', [',  plot_limits_min(colix(j)),  plot_limits_max(colix(j)),']);'
      write (aunit,'(a)') 'set(gca,''Layer'',''top'',''FontSize'',axes_fontsize,  ''LineWidth'', lwaxes);'
      write (aunit,'(a)') 'set(gca,''ticklength'',2*get(gca,''ticklength''));'            

      !add to plot some points (best_fit, mean, reference)
      call AddPointsToGraph(aunit)
      
      if (ShadeChoice .eq. ShadeMargProb) then
         !shaded marg probability
         !write(50,'(A)') 'text(''String'', ''Relative probability density'',''FontSize'',cbar_fontsize, ''Units'', ''normalized'', ''Position'', [0.5 2.5], ''HorizontalAlignment'', ''center'');'
         if (color_scheme .eq. smooth) then
               write (aunit,'(A)') 'chosencolor = modrhot;'
            else
               write (aunit,'(A)') 'chosencolor = yellowblue;'
            end if
         write (aunit,'(A)') 'description = ''Posterior pdf'';'
!         write (aunit,'(A)') ' set(get(hbar,''xlabel''),''String'', ''Posterior pdf'', ''FontSize'',cbar_fontsize );'
      else if (ShadeChoice .eq. ShadeMeanLike) then
         !shaded mean likelihood
         !write(50,'(A)') 'text(''String'', ''Mean exp(-chisquare/2)'',''FontSize'',cbar_fontsize, ''Units'', ''normalized'', ''Position'', [0.5 2.5], ''HorizontalAlignment'', ''center'');'
         write (aunit,'(A)') 'chosencolor = likegree;'
         write (aunit,'(A)') 'description = ''Mean [exp(-\chi^2/2)] (norm.d within panel)'';'
      else if (ShadeChoice .eq. ShadeMeanChisq) then
         !shaded mean chi square
         !write(50,'(A)') 'text(''String'', ''exp(-Mean chisquare/2)'',''FontSize'',cbar_fontsize, ''Units'', ''normalized'', ''Position'', [0.5 2.5], ''HorizontalAlignment'', ''center'');'
         write (aunit,'(A)') 'chosencolor = ''jet'';'
         write (aunit,'(A)') 'description = ''Mean [\chi^2/2 - \chi^2/2_{max}]'';'
         write (aunit,'(A)') 'current_clim  = get(gca,''CLim'');' 
         write (aunit,'(A)') 'gclim(1) = max([current_clim(1), gclim(1)]);gclim(2) = min([current_clim(2), gclim(2)]); '
      else if (ShadeChoice .eq. ShadeProfLike) then
         !shaded profile likelihood
         !write(50,'(A)') 'text(''String'', ''exp(-max(chisquare/2)))'',''FontSize'',cbar_fontsize, ''Units'', ''normalized'', ''Position'', [0.5 2.5], ''HorizontalAlignment'', ''center'');'
         if (color_scheme .eq. smooth) then
               write (aunit,'(A)') 'chosencolor = likegree;'
            else
               write (aunit,'(A)') 'chosencolor = yellowred;'
            end if
         write (aunit,'(A)') 'description = ''Profile likelihood'';'
      else if (ShadeChoice .eq. SingleSamples) then
         !single samples scatter plot
         write (aunit,'(A)') 'description = ''Uniformly weighted samples'';'

      end if

      !descriptive text
      !write(aunit,'(A)') 'text(''String'', description ,''FontSize'',cbar_fontsize, ''Units'', ''normalized'', ''Position'', [1.0 1.05], ''HorizontalAlignment'', ''right'');'
      !if ((colix(j) .ne. 12+2) .and. (colix(j2) .ne. 30+2)) then
         
      if ((.not. together) .or. (plot_num .eq. 1)) then
            write(aunit,'(A)') 'text(''String'', description ,''FontSize'', &
             & cbar_fontsize, ''Units'', ''normalized'', ''Position'', &
             & [0.97 0.25], ''HorizontalAlignment'', ''right'');'
            
            if (use_log) then 
               write (aunit,'(A)') 'description = ''Log priors'';'
            else
               write (aunit,'(A)') 'description = ''Flat priors'';'            
            end if
            write(aunit,'(A)') 'text(''String'', description ,''FontSize'', &
             & cbar_fontsize, ''Units'', ''normalized'', ''Position'', &
             & [0.97 0.15], ''HorizontalAlignment'', ''right'');'
         
            write(aunit,'(A)') 'text(''String'', ''CMSSM, \mu>0'' , &
             & ''FontSize'',cbar_fontsize, ''Units'', ''normalized'', &
             & ''Position'', [0.97 0.05], ''HorizontalAlignment'', ''right'');'

           write(aunit,'(A,A,A)') 'text(''String'',''',trim(othercomments), &
             & ''',''FontSize'',cbar_fontsize, ''Units'', ''normalized'', & 
             & ''Position'', [0.97 0.65], ''HorizontalAlignment'', ''right'');'

            write(aunit,'(A,A,A)') 'text(''String'',''',trim(credit), & 
             & ''',''FontSize'',13, ''Units'', ''normalized'', ''Position'', &
             & [0.97 1.025], ''HorizontalAlignment'', ''right'', &
             & ''FontAngle'', ''italic'');'
 
         end if
      !else
         !adds DD limits 
         !call Add_DD_Limits_MATLAB(aunit) 
      !end if

         if (ShadeChoice .ne. SingleSamples) write (aunit,'(A)') 'colormap(chosencolor);'

      call PlotContMATLAB2(aunit,rootname,j,j2, plot_contours, ShadeChoice) 
      
    end subroutine  Write2DPlotMATLAB

!---------------------------------------------------------
    subroutine Write1DPlotMATLAB(aunit,aroot,j)
!---------------------------------------------------------
      integer, intent(in) :: aunit,j
      character(LEN=*), intent(in) :: aroot
      character(LEN=150) :: gfmt
      character(LEN=520) :: fname, fnroot, rnroot, dir, reldir  
      logical :: colorbar_status

      if (together) then
         write (aunit,'(''subplot('',1I5,'','',1I5,'','',1I5,'');'')') plot_row,plot_col,plot_num
      else
         write (aunit, '(A)') 'figure'        
      end if
      !write(pn, *) plot_num
      write (aunit, '(A)') 'set(gcf, ''NextPlot'', ''replacechildren'');'
      write (aunit, '(A)') 'box on; axis square; hold on;'
    
      fnroot =  ExtractFileName(aroot)
      rnroot =  ExtractFilePath(aroot)
      dir = 'plot_data/'//trim(rnroot)
      reldir = '../../'//trim(dir)

      !best fit point
      if (plot_bestfit) write (aunit,'(a, E15.5,a)') 'bestfit = [', coldata(colix(j),0), ' 0.0];'
      !mean 
      if (plot_mean) write (aunit,'(a, E15.5,a)') 'mean = [', mean(j),', 0.0];'
      !any other point you might want to plot. Edit coordinates at the beginning of this file
      if (plot_reference) write (aunit,'(a, E15.5,a)') 'refpoint = [', rf(colix(j)-2), ' 0.0];'


      fname =trim(trim(fnroot)//trim(numcat('_p',colix(j)-2)))
      write (aunit,'(a)') 'tmp = load (fullfile('''//trim(reldir)// ''',''' // trim(fname)//  '.dat''));'
      !xaxis
      write (aunit,'(a)') 'x = tmp(:,1);'
      write (aunit,'(a)') 'pdf = tmp(:,2);' 
      write (aunit,'(a)') 'mlike = tmp(:,3);' 
      write (aunit,'(a)') 'profl = tmp(:,5);' 
      if (plot_1D_pdf .and. plot_1D_profile) then
         write (aunit,'(a)')  'Transparency = 0.2;'
      else
         write (aunit,'(a)')  'Transparency = 1.0;'
      end if
      if (plot_1D_pdf) then
         !produces histogram of pdf
         write (aunit,'(a)')  'hbar = bar(x,pdf);'
         !colour of the histogram
         write (aunit,'(a)')  'set(hbar, ''EdgeColor'', ''b'',''FaceColor'', [0.4 0.8 1.0],''BarWidth'', 1.0);'
         !transparency of the histogram
         write (aunit,'(a)')  'hpatch = get(hbar(1),''children''); set(hpatch,''FaceAlpha'',Transparency);' 
         write (aunit,'(a)')  'hold on;'
      end if
      if (plot_1D_profile) then
         !produces histogram of pdf
         write (aunit,'(a)')  'hbar = bar(x,profl);'
         !colour of the histogram
         write (aunit,'(a)')  'set(hbar, ''EdgeColor'', ''r'',''FaceColor'', [1.0 0.4 0.4],''BarWidth'', 1.0);'
         !transparency of the histogram
         write (aunit,'(a)')  'hpatch = get(hbar(1),''children''); set(hpatch,''FaceAlpha'',Transparency);' 
         write (aunit,'(a)')  'hold on;'
      end if

      gfmt = ''',''FontSize'',lab_fontsize);'
      !xlabel, slightly more sophisticated
      write (aunit,'(a)') ' text(''String'','''//trim(lables(colix(j)))// &
         & ''',''FontSize'',lab_fontsize, ''Units'', ''normalized'', &
         &  ''Position'', [0.5 -0.18], ''HorizontalAlignment'', ''center'');'
      write (aunit,'(a)') 'ylabel(''Probability'', ''FontSize'',lab_fontsize);'
      write (aunit,'(a)') 'axis tight;' 
      !x-axis limits
      if (has_plot_limits(colix(j))) write (aunit,'(A,2E15.5,A)') &
         'set(gca,''XLim'', [',  plot_limits_min(colix(j)),  plot_limits_max(colix(j)),']);'
      !y-axis limits
      write (aunit,'(A,2E15.5,A)') 'set(gca,''YLim'', [0.0 1.1]);'
      write (aunit,'(a)') 'set(gca,''Layer'',''top'',''FontSize'',axes_fontsize,  ''LineWidth'', lwaxes);'
      write (aunit,'(a)') 'set(gca,''ticklength'',2*get(gca,''ticklength''));'            
      if(plot_contours) call Plot1DContMATLAB(aunit,j) 
      !add to plot some points (best_fit, mean, reference)
      call AddPointsToGraph(aunit)
      colorbar_status = colorbar_on
      colorbar_on = .false.
      if (.not. together) call FinalTouches_SingleFig_MATLAB(60, '_1D_'//trim(IntToStr(plot_num)))
      colorbar_on = colorbar_status
    end subroutine Write1DPlotMATLAB

    subroutine Plot1DContMATLAB(aunit,j) 

      integer :: aunit, j
      integer :: i
 
      do i = 1, num_contours
       write(aunit,'(A,2E15.5,A)') 'line([',cont_lines(j,1,i),cont_lines(j,1,i),'],[0 1.1],''Color'',''b'');'
       write(aunit,'(A,2E15.5,A)') 'line([',cont_lines(j,2,i),cont_lines(j,2,i),'],[0 1.1],''Color'',''b'');'
      enddo 

      
    end subroutine Plot1DContMATLAB



!---------------------------------------------------------
    subroutine AddPointsToGraph(aunit)
!---------------------------------------------------------
      integer :: aunit

      if (plot_bestfit) then
         write (aunit,'(A)') 'plot(bestfit(1), bestfit(2),''x'', &
          & ''MarkerSize'',bfsize,''MarkerEdgeColor'',''k'', &
          &  ''LineWidth'', bflw/redf);'
         write (aunit,'(A)') 'plot(bestfit(1), bestfit(2),''o'', &
          & ''MarkerSize'',bfsize,''MarkerFaceColor'', ''none'', &
          & ''MarkerEdgeColor'',''k'', ''LineWidth'', bflw/redf);'
      end if
      if (plot_mean) write (aunit,'(A)') 'plot(mean(1), mean(2),''o'', &
          & ''MarkerSize'',bfsize/2,''MarkerFaceColor'',''k'', &
          & ''MarkerEdgeColor'',''k'', ''LineWidth'', bflw/redf*0.75);'   
      if (plot_reference) write (aunit,'(A)') 'plot(refpoint(1), &
          & refpoint(2),''d'',''MarkerSize'',8,''MarkerFaceColor'',''r'', &
          & ''MarkerEdgeColor'',''y'', ''LineWidth'', 2.0);'   

    end subroutine AddPointsToGraph

!---------------------------------------------------------
    subroutine Add_DD_Limits_MATLAB(aunit)
!---------------------------------------------------------
      integer, intent(in) :: aunit

      !loading of data files
      write(aunit,'(A)') 'ddb1=load (fullfile(''../../data/'',''DDbound_CDMS_update.dat''));'
      write(aunit,'(A)') 'mlsp1=ddb1(:,1)/1000; sigma1=ddb1(:,2); '
      write(aunit,'(A)') 'ddb2=load (fullfile(''../../data/'',''ZeplinIII2009.dat''));'
      write(aunit,'(A)') 'mlsp2=ddb2(:,1)/1000; sigma2=ddb2(:,2)/1.6;' 
      write(aunit,'(A)') 'ddb3=load (fullfile(''../../data/'',''DDbound_EDELWEISS.dat''));'
      write(aunit,'(A)') 'mlsp3=ddb3(:,1)/1000; sigma3=ddb3(:,2); '
      write(aunit,'(A)') 'ddb4=load (fullfile(''../../data/'',''Xe10_136kgd_2007.dat''));'
      write(aunit,'(A)') 'mlsp4=ddb4(:,1)/1000; sigma4=ddb4(:,2)*1.e36;'
      write(aunit,'(A)') 'ddb5=load (fullfile(''data'',''CDMS2009.dat''));'
      write(aunit,'(A)') 'mlsp5=log10(ddb5(:,1)); sigma5=ddb5(:,2);'

      !write(aunit,'(A)') 'text(''String'', ''Trotta et al (2008)'' ,''FontSize'',cbar_fontsize, ''Units'', ''normalized'', ''Position'', [0.0 1.05], ''HorizontalAlignment'', ''left'');'

      !Plotting of limits and description strings
      !%CDMS II
      write(aunit,'(A)') 'plot(mlsp1*1000,log10(sigma1),''-.k'', ''Linewidth'', lwdd); '
      write(aunit,'(A)') 'text(''String'', ''CDMS-II (2008)'',''FontSize'', &
        & exp_fontsize, ''Units'', ''normalized'', ''Position'', &
        & [0.9 0.68],''Color'',''k'',''HorizontalAlignment'', ''right'')' 
      !%CDMS Final Soudan 
      write(aunit,'(A)') 'plot(mlsp5*1000,log10(sigma5),''-.k'', ''Linewidth'', lwdd); '
      write(aunit,'(A)') 'text(''String'', ''CDMS Final (2009)'', &
        & ''FontSize'',exp_fontsize, ''Units'', ''normalized'', ''Position'', &
        & [0.9 0.78],''Color'',''k'',''HorizontalAlignment'', ''right'')' 
      !%XENON10
      write(aunit,'(A)') 'plot(mlsp4*1000,log10(sigma4),''k'', ''Linewidth'',lwdd); '
      write(aunit,'(A)') 'text(''String'', ''XENON-10 (2007)'', &
        & ''FontSize'',exp_fontsize, ''Units'', ''normalized'', ''Position'', &
        & [0.9 0.85],''Color'',''k'',''HorizontalAlignment'', ''right'')'  
      !%ZEPLIN-III 
      write(aunit,'(A)') 'plot(mlsp2*1000,log10(sigma2),'':k'', ''Linewidth'',lwdd); '
      write(aunit,'(A)') 'text(''String'', ''ZEPLIN-III (2009)'',''FontSize'', &
        & exp_fontsize, ''Units'', ''normalized'', ''Position'', [0.4 0.95], &
        & ''Color'',''k'',''HorizontalAlignment'', ''right'')'  

    end subroutine Add_DD_Limits_MATLAB


!---------------------------------------------------------
    subroutine Write3DPlotMATLAB(aunit, ix1, ix2, ix3, filename)
!---------------------------------------------------------
      integer, intent(in) :: aunit,ix1, ix2, ix3
      character(LEN=150) gfmt    
      character(LEN=*) :: filename

      write (aunit,'(A)') 'figure'
      write (aunit,'(A)') 'set(gcf, ''NextPlot'', ''replacechildren'');'
      write (aunit,'(A)') 'axes(''position'', [0.3 0.2 0.5 0.5]   );'
      write (aunit,'(A)') 'box on; axis square; hold on;'
      !x-axis limits
      !write(*,*) '**** ix1, colix = ', ix1, colix(ix1), plot_limits_max
      if (has_plot_limits((colix(ix1)))) write (aunit,'(A,2E15.5,A)') &
        'set(gca,''XLim'', [',  plot_limits_min(colix(ix1)),  plot_limits_max(colix(ix1)),']);'


      !y-axis limits
      if (has_plot_limits((colix(ix2)))) write (aunit,'(A,2E15.5,A)') &
       'set(gca,''YLim'', [',  plot_limits_min(colix(ix2)),  plot_limits_max(colix(ix2)),&
       ']);'


      write( aunit,'(A)') 'set(gca, ''TickLength'', [0.03 0.03]);'
      !if (ix2 .eq. 62) write( aunit,'(A)') 'set(gca,''ytick'',[-10 -5 0 3]);'
 
      write (aunit,'(A,3I8)') '%Do params ',ix1,ix2,ix3
      
      !fine for most plots
      write (aunit,'(A,I5,A,I5,A,I5,A)') 'scatter(pts(:,',ix1+2,'),pts(:,',ix2+2,'),18,pts(:,',ix3+2,'),''filled'');'


      !bigger dots
      !write (aunit,'(A,I5,A,I5,A,I5,A)') 'scatter(pts(:,',2+ix1,'),pts(:,',2+ix2,'),38,pts(:,',2+ix3,'),''filled'');'
      !best fit point
      !write (aunit,'(a, 2E15.5,a)') 'bestfit = [', coldata(colix(ix1),0),  coldata(colix(ix2),0), '];'

      write (aunit,'(a, 2E15.5,a)') 'bestfit = [', coldata(colix(ix1),0),  coldata(colix(ix2),0), '];'
      write (aunit,'(a, 2E15.5,a)') 'mean = [', mean(colix(ix1)), mean(colix(ix2)), '];'
      write (aunit,'(a, 2E15.5,a)') 'refpoint = [', rf(colix(ix1)-2), rf(colix(ix2)-2), '];'

      !plotted here 
      call AddPointsToGraph(aunit)
      gfmt = ''',''FontSize'',lab_fontsize);'
      !xlabel, slightly more sophisticated
      write (aunit,'(A)') ' text(''String'','''//trim(lables(colix(ix1)))// &
         & ''',''FontSize'',lab_fontsize, ''Units'', ''normalized'', &
         & ''Position'', [0.5 -0.18], ''HorizontalAlignment'', ''center'');'
      !!ylabel
      write (aunit,'(A)') 'ylabel('''//trim(lables(colix(ix2)))//trim(gfmt)
      write (aunit,'(A)') 'text(''String'', ''\it{'//trim(matlab_string)// &
         & '}'',''FontSize'',names_fontsize, ''Units'', ''normalized'', &
         & ''Position'', [1.0 1.05], ''HorizontalAlignment'', ''right'');'

      write (aunit,'(A)') 'set(gca,''FontSize'',axes_fontsize,  ''LineWidth'', lwaxes); pa = gca;'

      !Color axis limits
      if (caxis_min .ne. caxis_max) then
         write (aunit,'(A, 2F8.4, A)') 'caxis manual; caxis([', caxis_min, caxis_max ,']);' 
      end if
      !colorbar
      write(aunit,'(A)') 'cname  = ''$'//trim(lables(colix(ix3)))//'$'';'
      write (aunit,'(A)') 'hbar = colorbar(''horiz'');axes(hbar); '

      if (trim(colormap_name) .eq. 'jet') then
         write(50,*) 'colormap(''', trim(colormap_name), ''');'
      else
         write(50,*) 'colormap(', trim(colormap_name), ');'
      end if

      !Colorbar limits
      if (caxis_min .ne. caxis_max) then
         write (aunit,'(A, 2F8.4, A)') 'set(gca, ''CLim'', [', caxis_min, caxis_max ,']);' 
      end if
      !colorbar custom ticks 
      if (ix2 .eq. 62) write( aunit,'(A)')  'set(gca, ''Xtick'', [-2, -1, 0, 1,2],''XTickLabel'', [0.01 0.1 1 10 100]);'
     
      !final repositioning
      write(aunit,'(A)') 'hpos = get(hbar, ''Position'');'
      write(aunit,'(A)') 'text(''interpreter'',''latex'',''String'', cname, &
        & ''FontSize'',axes_fontsize, ''Units'', ''normalized'', ''Position'', &
        & [0.5 2.5], ''HorizontalAlignment'', ''center'');'
      write(aunit,'(A)') 'set(gca,''FontSize'',axes_fontsize);'
      !Position params: 1. horizontal position 2. vertical position  3. width of bar 4: height of bar 
      write(aunit,'(A)') 'set(hbar, ''Position'',[hpos(1)*0.75 hpos(2)*0.52 hpos(3)*1.7 hpos(4)/3]);'

      write (aunit,'(A)')  'set(gcf, ''PaperUnits'',''centimeters'');'
      write (aunit,'(A)') 'set(gcf, ''PaperPosition'',[ 0 0 20 30]);' 
      write (aunit,'(A)') 'set(gcf, ''PaperType'', ''A4'');'
      write (aunit,'(A)') 'set(gcf, ''PaperOrientation'', ''portrait''); '       
      write (aunit,'(A)') 'print -dpsc2 '//trim(filename)

    end subroutine Write3DPlotMATLAB

!---------------------------------------------------------
    subroutine FinalTouchesMATLAB(afile, choice)
!---------------------------------------------------------
      integer, intent(in) :: afile, choice
      !if choice > 0 then this applies to 2D plots
      !if choice < 0 then it is for 1D plots, hence no colorbar nor multiple figures
      !
      !colorbar for the whole file
      !
      if (choice>0) then
         write (afile, '(A,I4,A)') 'figure(',choice,')' 
         if (colorbar_on .and. (choice .ne. SingleSamples)) then
            write (afile,'(A)') 'subplot(''Position'',[0 0 0.99 0.01]); set(gca, ''Visible'', ''off'');'
            write (afile,'(A)') 'mylim=[0 1];'
            if (choice .eq. ShadeMeanChisq) then
               write (afile,'(A)') 'mylim=gclim;'
               write (afile, '(A)') 'set(ax,''CLim'', gclim);'
            end if
            write (afile,'(A)') 'set(gca,''CLim'',mylim); hbar = colorbar(''horiz'');' 
            write(afile,'(A)') 'set(gca,''FontSize'',cbar_fontsize);'
            write(afile,'(A)') 'hpos = get(hbar, ''Position'');'
            !makes colorbar thinner
            !write(50,'(A)') 'set(hbar, ''Position'',[hpos(1) hpos(2)*0.9 hpos(3) hpos(4)/3],''Units'',''normalized'');'
            write(afile,'(A)') 'set(hbar, ''Position'',[0.3 0.05 0.5 0.015], ''Units'', ''normalized'');'
            !write(afile,'(A)') 'axis off;'              
         end if
      end if

      write (afile,'(A)') 'set(gcf, ''PaperUnits'',''centimeters'');'
      if (plot_row*plot_col > 4 .or. colorbar_on) then
         !makes extra space for colorbar at the bottom
         write (afile,'(A)') 'set(gcf, ''PaperPosition'',[ 0 0 20 27]);' 
      else
         write (afile,'(A)') 'set(gcf, ''PaperPosition'',[ 0 0 20 20]);'
      end if
      write (afile,'(A)') 'set(gcf, ''PaperType'', ''A4'');'
      write (afile,'(A)') 'set(gcf, ''PaperOrientation'', ''portrait''); '     
      if (plot_row*plot_col > 4) then
         write (afile,'(A)') 'ls =get(gca,''XTick'');sz=size(ls,2);'
         write (afile,'(A)') 'if(sz > 4)'
         write (afile,'(A)') ' set(gca,''XTick'',ls(:,1:2:sz));'
         write (afile,'(A)') 'end;'
      end if

    end subroutine FinalTouchesMATLAB

!---------------------------------------------------------
    subroutine FinalTouches_SingleFig_MATLAB(afile, fig_id)
!---------------------------------------------------------
      integer, intent(in) :: afile
      character (*):: fig_id

      !this should fix the colourbar
      !
      if (colorbar_on) then
         write (afile,'(A)') 'subplot(''Position'',[0 0 0.99 0.01]); set(gca, ''Visible'', ''off'');'
         write (afile,'(A)') 'mylim=[0 1];'
         write (afile,'(A)') 'set(gca,''CLim'',mylim); hbar = colorbar(''horiz'');' 
         write(afile,'(A)') 'set(gca,''FontSize'',cbar_fontsize);'
         write(afile,'(A)') 'hpos = get(hbar, ''Position'');'
         !!makes colorbar thinner
         !!write(50,'(A)') 'set(hbar, ''Position'',[hpos(1) hpos(2)*0.9 hpos(3) hpos(4)/3],''Units'',''normalized'');'
         write(afile,'(A)') 'set(hbar, ''Position'',[0.3 0.05 0.5 0.015], ''Units'', ''normalized'');'
         !!write(afile,'(A)') 'axis off;'              
      end if

      !With PaperPositionMode set to auto MATLAB does not resize 
      !the figure to fit the current value of the PaperPosition.
      !disables resizing when writing to file
      write (afile,'(A)') 'set(gcf,''paperpositionmode'',''auto'');' 
      !resizes on screen figure to absolute size
      write (afile,'(A)') 'set(gcf, ''Units'',''centimeters'');'
      write (afile,'(A)') 'set(gcf, ''Position'',[ 0.0 0.0 16.5 16.5]);'
      !changes axis units to abs value
      write (afile,'(A)') 'set(gca, ''Units'',''centimeters'');'
      write(afile,'(A)')'set(gca,''position'',[4.0 4.0 11.0 11.0]);'
      !makes size of the paper the same for all figures 
      write (afile,'(A)') 'set(gcf, ''PaperUnits'',''centimeters'');'
      write (afile,'(A)') 'set(gcf, ''PaperSize'',[16.5 16.5]);' 
      write(afile, '(A)') 'hold off;'
      !-loose option in order to have an uncropped output 
      !so all figures have exactly the same size 
      write(afile,'(A)') 'print -dpsc2 -loose  ../'//trim(outn)//trim(rootname)//trim(fig_id)//'.ps;'
  
    end subroutine FinalTouches_SingleFig_MATLAB

    end module matlab
