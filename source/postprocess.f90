!module to perform post-processing of chains
!useful to add data/recompute predictions/add new observables
!SuperBayes Package - by Roberto Ruiz de Austri (rruiz@delta.ft.uam.es) and Roberto Trotta (rxt@astro.ox.ac.uk)
!This version February 2011
module postprocess

   use ParamDef
   use calclike, only: GetLogLikePost, ErrorsPresent, getpredictions

  implicit none

contains 

  subroutine PostProcChains(filen, in_unit, lines)
    integer, intent(IN) :: in_unit, lines

    !outfile_unit is already open and globally defined
    integer :: i, ierr, line, rejected, l, error, ifail
    real(8) :: mult, like,weight, NewLike
    real(8) :: Xt, Xb, mu, tan_2thetat, tan_2thetab, E, step
    character (LEN=80) :: InLine
    character (LEN=*) :: filen

    Type(ParamSet) :: P
    Type(Input_params) :: ChainMSSM
    Type(Nuisance_params) :: ChainNuisP
    Type(Susy_spec) :: ChainSusySp, NewChainSusySp, CurChainSusySp
    Type(RPVOut) :: NewChainSoftOut, CurChainSoftOut 


    !this determines which derived variables where 
    !saved in the input chain
    
    !if (Feedback > 1) write(*,*) 'Reading flags from: ', trim(filen)
    !call  LoadOldFlags(filen)
    !if (Feedback > 1) write(*,*) 'Previous info read in'

    line = 0
    rejected = 0
    if (skip_lines > 0) then
       if (Feedback > 1) write(*,*) 'Skipping ', skip_lines, ' lines'
       do l=1,skip_lines
          read(in_unit, *, iostat=ierr) mult
          if (ierr < 0) call DoStop('Skipped to many lines while post-processing!')
       end do
    end if


    do !loops over lines in file 

!
       read (in_unit, fmt_params, ADVANCE='NO', END = 77) mult, like, P%P

       !RT: MN saves the chisquare, not the lnlike. Therefore you need to divide like by a factor of 2
       if (pp_ns .eq. 5) like = like/2.0

       line = line + 1
       if (Feedback > 1) write(*,*) 'reading in line ', line, ' over ', lines
       if (Feedback > 2) write(*,*) 'params: ', P%P
 
       call ReadChains(in_unit, ChainSusySp)

      !This advances to the next line 
       read(in_unit, '(A)', ADVANCE='YES', END = 77) InLine
      !need to convert back some of the quantities - this has been disabled as default from v 1.36 onward
       call ParamsTomunuSSMParams(P%P, ChainMSSM, ChainNuisP)
       if(redo_theory) then

          !call PrintOutModelParams(ChainMSSM, ChainNuisP)
          call GetTheoryInfo(ChainMSSM, ChainNuisP, NewChainSusySp, NewChainSoftOut, error, ifail)


       else !loads up values from file
          NewChainSusySp  = ChainSusySp
          !NewChainSoftOut = ChainSoftOut 

          error = 0
          ifail = 0
       endif

       CurChainSusySp  = NewChainSusySp
       !CurChainSoftOut = NewChainSoftOut

       if (error == 0 .and. ifail == 0) then
          if (redo_like) then
             !NewLike = GetLogLikePost(ChainMSSM, ChainNuisP, NewChainSusySp, NewChainSoftOut)

             if (NewLike == logZero) then
                weight = 0
             else
                weight = exp(Like-NewLike)
             endif
             if (.not. redo_change_like_only)  mult = mult*weight

             if (Feedback > 0 .and. NewLike < logZero) then
                write(*,*) ' ---- Changes in Like and Multiplicity  -----'
                write(*,'(A,F20.5,A,F20.5)') 'NewLike  = ', NewLike, ' DeltaLike = ', Like-NewLike
                if(.not. redo_change_like_only) & 
                     write(*,*) 'New mult = ', mult, ' Old mult  = ', mult/weight
!                     write(*,'(A,F20.5,A,F20.5)') 'New mult = ', mult, ' Old mult  = ', mult/weight   

                if (Feedback > 1 .and. redo_theory) then 
                 write(*,*) ' ---- Changes in some of the observable values  -----'
!                 if(GFlags_old%Collider_predict .and. GFlags%Collider_predict) then  
!                  write(*,'(A,F10.6)') 'Percent Higgs = ',  &
!                      abs(1d0 - CurChainSusySp%ms4/ChainSusySp%ms4)
!                  write(*,'(A,F10.6)') 'Percent \Delta m^2_{sol} = ',  &
!                      abs(1d0 - CurChainSusySp%mneut2/ChainSusySp%mneut2)
!                  write(*,'(A,F10.6)') 'Percent \Delta m^2_{atm} = ',  &
!                      abs(1d0 - CurChainSusySp%mneut3/ChainSusySp%mneut3)
!                  write(*,'(A,F10.6)') 'Percent \sin^2 \theta_{12} = ',  &
!                      abs(1d0 - CurChainSusySp%sth12sq/ChainSusySp%sth12sq)
!                  write(*,'(A,F10.6)') 'Percent \sin^2 \theta_{13} = ',  &
!                      abs(1d0 - CurChainSusySp%sth13sq/ChainSusySp%sth13sq)
!                  write(*,'(A,F10.6)') 'Percent \sin^2 \theta_{23} = ',  &
!                      abs(1d0 - CurChainSusySp%sth23sq/ChainSusySp%sth23sq)


!                  end if
                endif
             endif
          else
             NewLike = like
             weight = 1
          end if

          if(NewLike < logZero) then
             !write the point only if its allowed under the new like
             !MN saves the chisquare, so I need to multiply the NewLike (which actually is chi-square/2) by
             !a factor of 2 to make sure it is consistent
             if (pp_ns .eq. 5) NewLike = 2.0*NewLike
             !
             call WritePosParams(P, mult, NewLike, NewChainSusySp, NewChainSoftOut)
          else
             rejected = rejected + 1
             if (Feedback >1 ) write (*,*) 'New like = ', NewLike
             cycle
          end if
       else
          rejected = rejected + 1
          if (Feedback >1 ) write (*,*) 'Error in spectrum...'
          cycle
       endif
    end do

77  continue

    write(*,*)  'Points rejected under new like:', rejected 
    write(*,*)  'Chain ', instance, ': post-processed models: ', line

    if(line+skip_lines /=  lines) &
         write(*,*) 'Warning: Not all the samples required have been PP !'

  end subroutine PostProcChains


  subroutine GetTheoryInfo(NMSSM, NuisP, SUSYSpec, OutSoft, error, ifail)

    Type(Input_Params):: NMSSM
    Type(Nuisance_Params):: NuisP
    Type(Susy_spec):: SUSYSpec
    Type(RPVOut) :: OutSoft
    Type(Error_Out) :: RPVErr

    integer :: ifail, error

    error = 0
    ifail = 0
    !computing SUSY spectrum 
    !call GetRPVSpectrum(NMSSM, NuisP, SUSYSpec, OutSoft, RPVErr)

    if (.not.  ErrorsPresent(RPVErr)) then
       !call GetPredictions(NMSSM, NuisP, ifail)
    else
       error = 1
    endif

  end subroutine GetTheoryInfo


  subroutine ReadChains(in_unit, ChainSusySp)

    Type(Susy_spec) :: ChainSusySp
    Type(ReducedRPVOut) :: ChainReducedRPV

    integer :: in_unit


    read(in_unit, fmt_susysp, ADVANCE='NO') ChainSusySp
    read(in_unit, fmt_rpv, ADVANCE='NO') ChainReducedRPV

    !if(.not.redo_theory) then

    !endif

66  continue


  end subroutine ReadChains


  subroutine WritePosParams(P, mult, like, ChainSusySp, NewChainSoftOut)
    implicit none

    Type(ParamSet) ::P
     Type(RPVOut) :: NewChainSoftOut 
    Type(ReducedRPVOut) :: ReducedRPV

    real(8), intent(in) :: mult, like

    Type(Susy_spec) :: ChainSusySp   

    if (outfile_unit == 0) return


    call ReduceOutput(NewChainSoftOut, ReducedRPV)

    !quantities which have not been computed should simply be zero
    !as they have been initialized

    !Which variables to save
    !Customize it to suits your need
    !Remember to adjust the formats in SetFormat
    write (outfile_unit, trim(fmt_params), ADVANCE='NO') mult,like, P%P

    write (outfile_unit, fmt_susysp, ADVANCE='NO')  ChainSusySp

    write (outfile_unit, fmt_rpv, ADVANCE='NO') ReducedRPV
    
    write (outfile_unit, '(A)') ' ' !to start a new line
    
    if (flush_write) call FlushFile(outfile_unit)

 end  subroutine WritePosParams



end module postprocess
