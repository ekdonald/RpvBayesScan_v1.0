module parameters

!**********************************************
! NEW TYPES DEFINITIONS
!**********************************************

 integer, parameter:: lmax = 250
 integer :: rankg = 1 

!--------- Input parameters ---------

  Type Input_params
   real*8 :: lambda1, lambda2, lambda3, kappa1, kappa2, &
        kappa3, vR1, vR2, vR3, vL1, vL2, vL3, ynu1, ynu2, ynu3, &
        m1, m2, m3, msq1, msq2, msq3, msu1, msu2, msu3, &
        msd1, msd2, msd3, mse1, mse2, mse3, a, at, ab, atau, anu, &
        al1, al2, al3, ak1, ak2, ak3, tanb
  end Type Input_params


  character(LEN=lmax), dimension(42):: MunuNames = (/ & 
   '\lambda_1          ', '\lambda_2          ', '\lambda_3          ', & 
   '\kappa_1           ', '\kappa_2           ', '\kappa_3           ', &      
   '\nu^R_1 (GeV)      ', '\nu^R_2 (GeV)      ', '\nu^R_3 (GeV)      ', &
   '\nu^L_1 (GeV)      ', '\nu^L_2 (GeV)      ', '\nu^L_3 (GeV)      ', &
   'Y_{\nu_1}          ', 'Y_{\nu_2}          ', 'Y_{\nu_3}          ', &
   'M_1 (GeV)          ', 'M_2 (GeV)          ', 'M_3 (GeV)          ', &
   'm_{Q_1} (GeV)      ', 'm_{Q_2} (GeV)      ', 'm_{Q_3} (GeV)      ', &
   'm_{U_1} (GeV)      ', 'm_{U_2} (GeV)      ', 'm_{U_3} (GeV)      ', &
   'm_{D_1} (GeV)      ', 'm_{D_2} (GeV)      ', 'm_{D_3} (GeV)      ', &
   'm_{E_1} (GeV)      ', 'm_{E_2} (GeV)      ', 'm_{E_3} (GeV)      ', &
   'A (GeV)            ', 'A_\t (GeV)         ', 'A_b (GeV)          ', &
   'A_\tau (GeV)       ', 'A_{NU} (GeV)       ', 'A_{\lambda_1} (GeV)', &
   'A_{\lambda_2} (GeV)', 'A_{\lambda_3} (GeV)', 'A_{\kappa_1} (GeV) ', &
   'A_{\kappa_2} (GeV) ', 'A_{\kappa_3} (GeV) ', 'tan \beta          ' /)

  logical, dimension(42), parameter :: MunuUsed = (/ & 
    .true. , .true., .true., .true., .true., .true. , .true., .true., &
    .true. , .true., .true., .true., .true., .true. , .true., .true., &
    .true. , .true., .true., .true., .true., .true. , .true., .true., &
    .true. , .true., .true., .true., .true., .true. , .true., .true., &
    .true. , .true., .true., .true., .true., .true. , .true., .true., &
    .true. , .true. /)

  Type Nuisance_params
     real (8) :: mbmb, mtop, alphamz, alphasmz
  end Type Nuisance_params

  character(LEN=lmax), dimension(4):: NuisanceNames = (/ &
   'm_b(m_b) (GeV)   ', 'm_t (GeV)        ', '\alpha_s         ', &
   '\alpha_{em}      ' /)

  logical, dimension(4), parameter :: NuisanceUsed = (/ &
   .true. , .true., .true., .true. /)

  character(LEN=lmax), dimension(46) :: ParamsNames

!---------  Output from SoftSusy ------------
!Higgs sector
 Type scalars
   real (8) :: mscal(8)=0.d0, mpscal(7)=0.d0,  mhscal(7)=0.d0
   real (8) :: Scomp(3,4)=0.d0
 end Type scalars

!Sfermion sector
 Type sfermions
   real (8) :: msu(3,2)=0.d0, msd(3,2)=0.d0
   real (8) :: thetat=0.d0, thetab=0.d0 
 end Type sfermions

!Neutralino sector
 Type neutralino
   real (8) :: mneut(10)=0.d0, ratio(3) = 0.d0
!  real (8) :: theta12=0.d0, theta23=0.d0, theta13=0.d0
   real (8) :: sth12sq=0.d0, sth13sq=0.d0, sth23sq=0.d0
   real (8) :: gf=0.d0, hf=0.d0, sf=0.d0, Umix=0.d0, direct=0.d0
 end Type neutralino

!Chargino sector
 Type chargino
   real (8) :: mcha(5)=0.d0
 end Type chargino

!soft terms
 Type Softpar
   real (8) :: mu=0.d0, m1=0.d0, m2=0.d0, mass2hd=0.d0, mass2hu=0.d0
   real (8) :: asoftnu(3)=0.d0, mass2e(3)=0.d0, mass2nuc(3)=0.d0
 end Type Softpar

!Soft parameters
 Type RPVout
   Type(Softpar) :: soft
   Type(scalars) :: scal
   Type(sfermions) :: sfer
   Type(neutralino) :: neut
   Type(chargino) :: charg
   real (8) :: mgluino
 End Type RPVout

 Type ReducedRPVout
  real (8) :: Scomp(3,4)=0.d0
  real (8) :: gf=0.d0, hf=0.d0, sf=0.d0, Umix=0.d0, direct=0.d0
 End Type ReducedRPVout

 character (LEN=lmax), dimension(17):: ReducedRPVOutNames= (/ &
   'S1_d                      ', 'S1_u                      ', &  
   'S1_r                      ', 'S2_d                      ', &  
   'S2_u                      ', 'S2_r                      ', &  
   'S3_d                      ', 'S3_u                      ', &  
   'S3_r                      ', 'S4_d                      ', &  
   'S4_u                      ', 'S4_r                      ', &  
   'g_f                       ', 'h_f                       ', &
   's_f                       ', 'U_{mix}                   ', &
   'hierarchy                 '  /)

! Some physical masses - intermediate spectrum
 Type Susy_spec
   real (8) :: ms(8), mp(7), mhc(7), &
      mch(5), mchi(10), msqu(6), msqd(6), mgluino, & 
      m2sol, m2at, sth12sq, sth13sq, sth23sq, neutrinoh
 end Type Susy_spec


 character (LEN=lmax), dimension(56):: SusySpecNames= (/ &
   'm_S1 (GeV)                ', 'm_S2 (GeV)                ', &
   'm_S3 (GeV)                ', 'm_S4 (GeV)                ', &
   'm_S5 (GeV)                ', 'm_S6 (GeV)                ', &
   'm_S7 (GeV)                ', 'm_S8 (GeV)                ', &
   'm_P1 (GeV)                ', 'm_P2 (GeV)                ', &
   'm_P3 (GeV)                ', 'm_P4 (GeV)                ', &
   'm_P5 (GeV)                ', 'm_P6 (GeV)                ', &
   'm_P7 (GeV)                ', 'm_C1 (GeV)                ', &
   'm_C2 (GeV)                ', 'm_C3 (GeV)                ', &
   'm_C4 (GeV)                ', 'm_C5 (GeV)                ', &
   'm_C6 (GeV)                ', 'm_C7 (GeV)                ', &
   'm_{\chi_1^\pm} (GeV)      ', 'm_{\chi_2^\pm} (GeV)      ', &
   'm_{\chi_3^\pm} (GeV)      ', 'm_{\chi_4^\pm} (GeV)      ', &
   'm_{\chi_5^\pm} (GeV)      ', 'm_{\chi_1^0} (GeV)        ', &
   'm_{\chi_2^0} (GeV)        ', 'm_{\chi_3^0} (GeV)        ', &
   'm_{\chi_4^0} (GeV)        ', 'm_{\chi_5^0} (GeV)        ', &
   'm_{\chi_6^0} (GeV)        ', 'm_{\chi_7^0} (GeV)        ', &
   'm_{\chi_8^0} (GeV)        ', 'm_{\chi_9^0} (GeV)        ', &
   'm_{\chi_10^0} (GeV)       ', 'm_{\tilde{u}_R} (GeV)     ', &
   'm_{\tilde{u}_L} (GeV)     ', 'm_{\tilde{s}_R} (GeV)     ', &
   'm_{\tilde{s}_L} (GeV)     ', 'm_{\tilde{t}_1} (GeV)     ', &
   'm_{\tilde{t}_2} (GeV)     ', 'm_{\tilde{d}_R} (GeV)     ', &
   'm_{\tilde{d}_L} (GeV)     ', 'm_{\tilde{c}_R} (GeV)     ', &
   'm_{\tilde{c}_L} (GeV)     ', 'm_{\tilde{b}_1} (GeV)     ', &
   'm_{\tilde{b}_2} (GeV)     ', 'm_{\tilde{g}} (GeV)       ', &
   '\Delta m^2_{sol} (GeV)    ', '\Delta m^2_{atm} (GeV)    ', &
   '\sin^2 \theta_{12}        ', '\sin^2 \theta_{13}        ', &
   '\sin^2 \theta_{23}        ', 'Neutrino hierarchy        ' /)
    
 Type Collider
  real (8) :: gm2, deltarho, bsgamma, Dmunu, Dsmunu, RBtaunu, deltambs, &
    deltambd, bsmumu, bdmumu, epsk, mueg, taueg, taumug, mu3e, tau3e 
  End Type Collider

 character(LEN=lmax), dimension(16):: ColliderNames= (/ &
    '\delta a_{\mu}^{SUSY}                 ', &
    '\Delta \rho                           ', &
    'BR(B->X_s\gamma)                      ', &
    'BR(D -> \mu \nu)                      ', &
    'BR(D_s -> \mu \nu)                    ', &
    'RBR(B_u-> \tau \nu)                   ', &
    '\Delta M_{B_s}                        ', &
    '\Delta M_{B_d}                        ', &
    'BR(B_s->\mu^+\mu^-)                   ', &
    'BR(B_d->\mu^+\mu^-)                   ', &
    'BR(\epsilon->\mu^+\mu^-)              ', &
    'BR(\mu->e \gamma)                     ', &
    'BR(\tau->e \gamma)                    ', &
    'BR(\tau->\mu \gamma)                  ', &
    'BR(\mu->3e)                           ', &
    'BR(\tau->3e)                          ' /)

! Type MunuSSMOut
!  Type (Susy_spec) :: Spectrum
!  Type (Collider) :: Coll
! end Type MunuSSMOut

!------------- SLHA structure for passing par. to FH ----

 Type SM
   real(8) :: GFermi, alpha_em, alpha_s, MZ, mbmb, mb, mtop, mtau
 end Type SM

 Type physical
   real(8) :: MW, mgluino, mscal(8), mpscal(8), mchscal(8) &
              , mneut(10), mcha(5), msu(3,2), msd(3,2)
 end Type physical

 Type mixings
   real(8) :: StopMix(2,2), SbotMix(2,2), StauMix(2,2) &
             , NMix(10,10), UMix(5,5), VMix(5,5) &
             , mixScal(8,8), mixPscal(8,8), mixChscal(8,8)
 end Type mixings

 Type couplings
   real(8) :: gp, g2, g3, yuQ, ycQ, ydQ, ysQ, ytq, ybq, ytauq &
            , lambda(3), kappa(3), ynu(3), ye(3) &
            , lambdaq(3), kappaq(3), ynuq(3), yeq(3)
 end Type couplings

 Type softpars
   real(8) :: m3sq, mhu2, mhd2, m1, m2, m3  &
        , mel, mmul, mtaul, mer, mmur, mtaur &
        , mnuR1, mnuR2, mnuR3 &
        , mql1, mql2, mql3  &
        , mur, mcr, mtr, mdr, msr, mbr &
        , aup, ach, at, ado, ast, ab, ael, amu, atau &
        , al(3), ak(3), anu(3), alq(3), akq(3), anuq(3)
 end Type softpars

 Type susypars
   real(8) :: mueff, mueffQ, tanb, tanbQ, vu, vd, vuq, vdq &
             , nu(3), nuc(3), nuq(3), nucq(3)
 end Type susypars

 Type renormalization
   real(8) :: Q
 end Type renormalization

! Type Leshouches_out
!   Type(SM) :: SMinputs
!   Type(physical) ::masses
!   Type(mixings) :: mix
!   Type(couplings) :: coup
!   Type(softpars) :: soft
!   Type(susypars) :: susy
!   Type(renormalization) :: scale
! end Type Leshouches_out

!---------- Error messages from SoftSusy and Dark Susy about physical 
!and unphysical points ------------
! Type Error_Out
    !all the other are set by rpv, see doc/cmssm.ps for explantions
!    integer :: irqfp=0, noRhoConvergence=0, tachyon=0, nonperturbative=0, &
!               noConvergence=0, nosoftConvergence=0, notadpolConvergence=0, &
!               higgsUfb=0, trivialminimun=0, neutrino=0, lepton=0, &
!               tachyon_cpev=0, tachyon_cpodd=0, numass=0, numix=0
! end Type Error_Out

 Type Error_Out
  integer :: general_error, coll_constraints 
 end Type Error_Out
  
!----- Higgs likelihood

 Type HiggsLike
  real(8) :: type, excl, Chisq_mu, Chisq_mh
 End Type HiggsLike

 character(LEN=lmax), dimension(4):: HiggsLikeNames= & 
(/ 'type        ', 'excl        ', '\chi^2_{\mu}', '\chi^2_{m_h}' /)


!-------- Some flags to decide what to compute ------------

 Type Flags
   logical :: Debug, Collider_predict
 End Type Flags

!---------- Format for likelihood data structure ----------
 integer, parameter :: Gaussian = 1, LowerLimit = 2, UpperLimit = 3

 Type LikeDatum
    real(8) :: mu, sigma, tau
    integer :: datum_type
    logical :: tau_percent
 end Type LikeDatum

 Type AccelDatum
    real(8) :: mu, sigma, tau, theory
    integer :: datum_type
    logical :: sigma_percent, tau_percent, used
    character(LEN=20) :: Name
 end Type AccelDatum



end module parameters
