module propose
  use settings
  use Random
  use ParamDef
  use calclike
  implicit none

 logical :: propose_rand_directions = .true.
 logical :: fast_slicing = .false. !slice fast parameters when using Metropolis 
 logical :: slice_stepout = .true.

 real, allocatable, dimension(:,:), save :: Rot_slow, Rot_nuis

contains


 subroutine RotMatrix(M,n)
  integer, intent(in) :: n
  real M(n,n)
  integer i
  
   if (propose_rand_directions .and. n > 1) then
      call RandRotation(M, n)      
   else
      M = 0
      do i = 1, n
           M(i,i) = sign(1., real(ranmar())-0.5)
      end do
   end if
 
 end subroutine RotMatrix


 function Propose_r(in_n) result(r_fac)
  !distance proposal function (scale is 1)
   integer, intent(in) :: in_n
   integer i,n
   real r_fac

      if (ranmar() < 0.33d0) then
       r_fac = randexp1()
      else
       n = min(in_n,2)
       r_fac = 0
       do i = 1, n
        r_fac = r_fac + Gaussian1()**2
       end do
       r_fac = sqrt( r_fac / n )
      end if

 end function Propose_r

 subroutine UpdateParamsDirection(tmp, nuis, dist,i)
  !Change parameters in tmp by dist in the direction of the ith random e-vector
   Type(ParamSet) tmp
   real vec(num_params_used)
   integer, intent(in) :: i
   logical, intent(in) :: nuis
   real, intent(in) :: dist

  if (has_propose_matrix) then
    vec = 0
    if (nuis) then
      vec(1:num_nuis) =  Rot_nuis(:,i) * dist * propose_diag_nuis  
      tmp%P(nuis_params_used) =  tmp%P(nuis_params_used) + & 
        sigmas(num_slow+1:num_slow+ num_nuis) * matmul (propose_matrix_nuis, vec(1:num_nuis))
    else
      vec(1:num_slow) =  Rot_slow(:,i) * dist * propose_diag(slow_evecs) 
      tmp%P(params_used) =  tmp%P(params_used) + &
            sigmas * matmul (propose_matrix(:,slow_evecs), vec(1:num_slow))
    end if
  else !no propose matrix
    if (nuis) then
     tmp%P(nuis_params_used) = tmp%P(nuis_params_used) + &
              Rot_nuis(:,i) * dist *  Scales%PWidth(nuis_params_used)
    else
     tmp%P(params_used(1:num_slow)) = tmp%P(params_used(1:num_slow)) +  &
        Rot_slow(:,i) * dist *  Scales%PWidth(params_used(1:num_slow))
    end if
  end if
 
 end subroutine UpdateParamsDirection


 subroutine ProposeGrid(In,Out)
    type (ParamSet) In, Out
    integer :: ix, k, inner_points, j
    !if you want to propose a grid for testing purposes
    !in this case, Scales%PWidth(ix) is the step length for param ix

    Out=In

    if(instance == 0) instance = 1

    ix = params_used(num_params_used)

    k = num + TotGridPoints*(instance-1)
    !write(*,*) 'Im chain', instance
    !write(*,*) 'num = ', num
    !write(*,*) 'k = ', k
    !write(*,*) 'grid offset = ', TotGridPoints*(instance-1)
    ix = params_used(1)
    Out%P(ix) = Scales%PMin(ix) + mod(k,GridDim(ix))*Scales%PWidth(ix)
    !write(*,*) 'OutP1 ', Out%P(ix)
    inner_points = 1
    do j=2, num_params_used
       ix = params_used(j)
       inner_points = inner_points*GridDim(params_used(j-1))
       Out%P(ix) = Scales%PMin(ix)  + mod(int(k/inner_points), GridDim(ix))*Scales%PWidth(ix)
       !write(*,*) 'OutPix ', Out%P(ix)
    end do
  end subroutine ProposeGrid


subroutine GetProposal(In, Out)

  type (ParamSet) In, Out
  integer, save :: nuis_ix = 0
  nuis_ix = nuis_ix + 1
  if (num_nuis /= 0 .and. &
       mod(nuis_ix, 2*(1 + (num_nuis*oversample_nuisance)/num_slow)) /= 0) then
   call GetProposalProjNuis(In, Out)
  else
   call GetProposalProjSlow(In, Out)
  end if

end subroutine GetProposal

subroutine GetProposalProjSlow(In, Out)
  use settings
  use Random
  use ParamDef
  implicit none
  type (ParamSet) In, Out
  real  wid
  integer, save :: loopix = 0
 
  slow_proposals = slow_proposals + 1
  !Change only slow (ie MSSM) params, or eigenvectors of covmat which most change the MSSM params
  Out= In
  wid = propose_scale
  if (mod(loopix,num_slow)==0) then
     if (.not. allocated(Rot_slow)) allocate(Rot_slow(num_slow,num_slow))
     call RotMatrix(Rot_slow, num_slow)      
     loopix = 0
  end if
  loopix = loopix + 1
  
  call UpdateParamsDirection(Out,.false.,Propose_r(num_slow) * wid, loopix)

end subroutine GetProposalProjSlow


subroutine GetProposalProjNuis(In, Out, ascale)
  type (ParamSet) In, Out
  real wid
  real, intent(in), optional :: ascale
  integer, save :: loopix = 0
  
  Out= In
  nuis_proposals = nuis_proposals + 1
  wid = propose_scale
  if (present(ascale)) wid = ascale
  
  if (mod(loopix,num_nuis)==0) then
     if (.not. allocated(Rot_nuis)) allocate(Rot_nuis(num_nuis,num_nuis))
        call RotMatrix(Rot_nuis, num_nuis)      
        loopix = 0
     end if
     loopix = loopix + 1

     call UpdateParamsDirection(Out,.true.,Propose_r(num_nuis) * wid, loopix)
     
end subroutine GetProposalProjNuis


! subroutine RotMatrix(M,n)
!  integer, intent(in) :: n
!  real M(n,n)
!  integer, save :: i = 0
  
!   i = i + 1 

!   M = 0

!   M(i,i) = 1.d0

! end  subroutine RotMatrix



! subroutine UpdateSlowParams(tmp, vec)
  !Change parameters in tmp by dist in the direction of the ith random e-vector
!   Type(ParamSet) tmp
!   real vec(num_slow)

!     print*,tmp%P(params_used(1:num_slow))
!     tmp%P(params_used(1:num_slow)) = tmp%P(params_used(1:num_slow)) +  &
!        vec * Scales%PWidth(params_used(1:num_slow))


! end subroutine UpdateSlowParams


! subroutine UpdateNuisParams(tmp, vec)
  !Change parameters in tmp by dist in the direction of the ith random e-vector
!   Type(ParamSet) tmp
!   real vec(num_nuis)
!     tmp%P(nuis_params_used) = tmp%P(nuis_params_used) + &
!              vec * Scales%PWidth(nuis_params_used)
! end subroutine UpdateNuisParams


! subroutine SlowGrid(In)

!  Type(ParamSet) In, Trial
!  integer :: r = 0, i = 0 , j = 0, k= 0, l = 0
!  integer, save ::  ist = 0, jst = 0, kst= 0, lst = 0, rst = 0
!  real vec(num_slow)
!  Type(RPVOut) :: OutRPV 
!Embeed this inside nuis_parameters scan 


!  do i = 0, GridDim(params_used(1))
!    vec(1) = i
!    ist = ist + 1
!    if(num_slow.eq.1) then
!       Trial = In
!       call UpdateSlowParams(Trial, vec)
!       call GetOut(Trial)
!       call WriteParams(Trial)
!    endif
!    j = 0
!    do while (num_slow.gt.1 .and. j <= GridDim(params_used(2)))
!      vec(2) = j 
!      if(num_slow.lt.3) then
!        Trial = In
!        call UpdateSlowParams(Trial, vec)
!        call GetOut(Trial)
!        call WriteParams(Trial)
!      endif
!      k = 0
!      do while (num_slow.gt.2 .and. k <= GridDim(params_used(3)))   
!        vec(3) = k 
!        if(num_slow.lt.4) then 
!          Trial = In   
!          r = r + 1 
!          rst = rst + 1
!          call UpdateSlowParams(Trial, vec)
!          call GetOut(Trial)  
!          call WriteParams(Trial) 
!        endif       
!        l = 0
!        do while (num_slow.gt.3 .and. l <= GridDim(params_used(4))) 
!          vec(4) = l
!          Trial = In 
!          call UpdateSlowParams(Trial, vec)
!          call GetOut(Trial)
!          call WriteParams(Trial)
!          l = l + 1 
!          lst = lst + 1
!        enddo   
!        k = k + 1
!        kst = kst + 1
!      enddo
!      j = j + 1
!      jst = jst + 1  
!    enddo
!  enddo


!  if(ist /= 0) num = ist 
!  if(jst /= 0) num = jst 
!  if(kst /= 0) num = kst
!  if(lst /= 0) num = lst
!  if(rst /= 0) num = rst


! end subroutine SlowGrid

! subroutine ProposeGrid(In,Out)
!    type (ParamSet) In, Out
!    integer :: ix, k, inner_points, j
    !if you want to propose a grid for testing purposes
    !in this case, Scales%PWidth(ix) is the step length for param ix

!    Out=In

    
!    ix = params_used(num_params_used)

!    if(instance == 0) instance = 1

   
!    k = num + TotGridPoints*(instance-1)
!    write(*,*) 'Im chain', instance
!    write(*,*) 'num = ', num
!    write(*,*) 'k = ', k!
!    write(*,*) 'grid offset = ', TotGridPoints*(instance-1)
!    ix = params_used(1)
!    Out%P(ix) = Scales%PMin(ix) + mod(k,GridDim(ix))*Scales%PWidth(ix)
!    inner_points = 1
!    do j=2, num_params_used
!       ix = params_used(j)
!       inner_points = inner_points*GridDim(params_used(j-1))
!       Out%P(ix) = Scales%PMin(ix)  + mod(int(k/inner_points), GridDim(ix))*Scales%PWidth(ix)
!       write(*,*) 'OutPix ', Out%P(ix)
!    end do
!  end subroutine ProposeGrid


end module propose
