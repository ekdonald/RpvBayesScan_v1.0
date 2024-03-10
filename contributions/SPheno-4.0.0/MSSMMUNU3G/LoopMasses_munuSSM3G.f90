! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.5.9b3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:30 on 26.8.2015   
! ----------------------------------------------------------------------  
 
 
Module LoopMasses_munuSSM3G 
 
Use Control 
Use Couplings_munuSSM3G 
Use LoopFunctions 
Use Mathematics 
Use MathematicsQP 
Use Model_Data_munuSSM3G 
Use StandardModel 
Use Tadpoles_munuSSM3G 
Use EffectivePotential_munuSSM3G 
Use Pole2L_munuSSM3G 
Use SusyMasses_munuSSM3G 
 
Real(dp), Private :: MSd_1L(6), MSd2_1L(6)  
Complex(dp), Private :: ZD_1L(6,6)  
Real(dp), Private :: MSu_1L(6), MSu2_1L(6)  
Complex(dp), Private :: ZU_1L(6,6)  
Real(dp), Private :: Mhh_1L(8), Mhh2_1L(8)  
Complex(dp), Private :: ZH_1L(8,8)  
Real(dp), Private :: MAh_1L(8), MAh2_1L(8)  
Complex(dp), Private :: ZA_1L(8,8)  
Real(dp), Private :: MHpm_1L(8), MHpm2_1L(8)  
Complex(dp), Private :: ZP_1L(8,8)  
Real(dp), Private :: MChi_1L(10), MChi2_1L(10)  
Complex(dp), Private :: UV_1L(10,10)  
Real(dp), Private :: MCha_1L(5), MCha2_1L(5)  
Complex(dp), Private :: ZER_1L(5,5),ZEL_1L(5,5)
Real(dp), Private :: MGlu_1L, MGlu2_1L  
Real(dp), Private :: MVZ_1L, MVZ2_1L  
Real(dp), Private :: MVWm_1L, MVWm2_1L  
Real(dp), save :: rMS = 0._dp 
Real(dp) :: pi2A0  
Real(dp) :: ti_ep2L(8)  
Real(dp) :: pi_ep2L(8,8)
Real(dp) :: Pi2S_EffPot(8,8)
Real(dp) :: PiP2S_EffPot(8,8)

!!*******************
Real(dp) :: DeltaM_squarks, DeltaMSu(6), DeltaMSd(6) !! DK precisions 01.03.16
Integer  :: iSQ, iii5, iii6
Real(dp) :: DeltaM_CH, DeltaMCH(8), DeltaMGlu
Real(dp) :: DeltaM_neu, DeltaMneu(10), DeltaM_cha, DeltaMcha(5)


!!*******************
Contains 
 
Subroutine OneLoopMasses(MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,            & 
& MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSu,MSu2,MVWm,MVWm2,MVZ,MVZ2,pG,               & 
& TW,ZER,ZEL,ZA,ZD,ZDL,ZDR,ZH,UV,ZP,ZU,ZUL,ZUR,ZW,ZZ,vd,vu,vR,vL,g1,g2,g3,               & 
& Yd,Ye,lam,Yv,Yu,kap,Td,Te,Tlam,Tv,Tu,Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,             & 
& mlHd2,M1,M2,M3,kont)

Implicit None 
Real(dp),Intent(inout) :: g1,g2,g3,mHd2,mHu2,mlHd2(3)

Complex(dp),Intent(inout) :: Yd(3,3),Ye(3,3),lam(3),Yv(3,3),Yu(3,3),kap(3,3,3),Td(3,3),Te(3,3),Tlam(3),            & 
& Tv(3,3),Tu(3,3),Tk(3,3,3),mq2(3,3),ml2(3,3),md2(3,3),mu2(3,3),me2(3,3),mv2(3,3),M1,M2,M3

Real(dp),Intent(inout) :: MAh(8),MAh2(8),MCha(5),MCha2(5),MChi(10),MChi2(10),MFd(3),MFd2(3),MFu(3),             & 
& MFu2(3),MGlu,MGlu2,Mhh(8),Mhh2(8),MHpm(8),MHpm2(8),MSd(6),MSd2(6),MSu(6),              & 
& MSu2(6),MVWm,MVWm2,MVZ,MVZ2,TW,ZA(8,8),ZH(8,8),ZP(8,8),ZZ(2,2)

Complex(dp),Intent(inout) :: pG,ZER(5,5),ZEL(5,5),ZD(6,6),ZDL(3,3),ZDR(3,3),UV(10,10),ZU(6,6),ZUL(3,3),            & 
& ZUR(3,3),ZW(2,2)

Real(dp),Intent(inout) :: vd,vu,vR(3),vL(3)

Complex(dp) :: cplAhAhcVWmVWm(8,8),cplAhAhUhh(8,8,8),cplAhAhUhhUhh(8,8,8,8),cplAhAhUHpmcUHpm(8,8,8,8),& 
& cplAhAhUSdcUSd(8,8,6,6),cplAhAhUSucUSu(8,8,6,6),cplAhAhVZVZ(8,8),cplAhcUHpmVWm(8,8),   & 
& cplAhhhVZ(8,8),cplAhHpmcUHpm(8,8,8),cplAhHpmcVWm(8,8),cplAhSdcUSd(8,6,6),              & 
& cplAhSucUSu(8,6,6),cplAhUhhhh(8,8,8),cplAhUhhVZ(8,8),cplcChaChaUAhL(5,5,8),            & 
& cplcChaChaUAhR(5,5,8),cplcChaChaUhhL(5,5,8),cplcChaChaUhhR(5,5,8),cplcChaChaVZL(5,5),  & 
& cplcChaChaVZR(5,5),cplcChacUFuSdL(5,3,6),cplcChacUFuSdR(5,3,6),cplcChaFdcUSuL(5,3,6),  & 
& cplcChaFdcUSuR(5,3,6),cplcFdFdUAhL(3,3,8),cplcFdFdUAhR(3,3,8),cplcFdFdUhhL(3,3,8),     & 
& cplcFdFdUhhR(3,3,8),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFdcUHpmL(3,3,8),           & 
& cplcFuFdcUHpmR(3,3,8),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplcFuFuUAhL(3,3,8),       & 
& cplcFuFuUAhR(3,3,8),cplcFuFuUhhL(3,3,8),cplcFuFuUhhR(3,3,8),cplcFuFuVZL(3,3),          & 
& cplcFuFuVZR(3,3),cplcgAgWmcVWm,cplcgWmgWmUAh(8),cplcgWmgWmUhh(8),cplcgWmgWmVZ,         & 
& cplcgWmgZUHpm(8),cplcgWpCgAcVWm,cplcgWpCgWpCUAh(8),cplcgWpCgWpCUhh(8),cplcgWpCgWpCVZ,  & 
& cplcgWpCgZcUHpm(8),cplcgWpCgZcVWm,cplcgZgWmcUHpm(8),cplcgZgWmcVWm,cplcgZgWpCUHpm(8),   & 
& cplcgZgZUhh(8),cplChaFucUSdL(5,3,6),cplChaFucUSdR(5,3,6),cplChiChacUHpmL(10,5,8),      & 
& cplChiChacUHpmR(10,5,8),cplChiChacVWmL(10,5),cplChiChacVWmR(10,5),cplChiChiUAhL(10,10,8),& 
& cplChiChiUAhR(10,10,8),cplChiChiUhhL(10,10,8),cplChiChiUhhR(10,10,8),cplChiChiVZL(10,10),& 
& cplChiChiVZR(10,10),cplChiFdcUSdL(10,3,6),cplChiFdcUSdR(10,3,6),cplChiFucUSuL(10,3,6), & 
& cplChiFucUSuR(10,3,6),cplcUChacFuSdL(5,3,6),cplcUChacFuSdR(5,3,6),cplcUChaChaAhL(5,5,8),& 
& cplcUChaChaAhR(5,5,8),cplcUChaChahhL(5,5,8),cplcUChaChahhR(5,5,8),cplcUChaChaVPL(5,5), & 
& cplcUChaChaVPR(5,5),cplcUChaChaVZL(5,5),cplcUChaChaVZR(5,5),cplcUChaChiHpmL(5,10,8),   & 
& cplcUChaChiHpmR(5,10,8),cplcUChaChiVWmL(5,10),cplcUChaChiVWmR(5,10),cplcUChaFdcSuL(5,3,6),& 
& cplcUChaFdcSuR(5,3,6),cplcUFdChaSuL(3,5,6),cplcUFdChaSuR(3,5,6),cplcUFdChiSdL(3,10,6), & 
& cplcUFdChiSdR(3,10,6),cplcUFdFdAhL(3,3,8),cplcUFdFdAhR(3,3,8),cplcUFdFdhhL(3,3,8),     & 
& cplcUFdFdhhR(3,3,8),cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),             & 
& cplcUFdFdVPR(3,3),cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHpmL(3,3,8),            & 
& cplcUFdFuHpmR(3,3,8),cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFdGluSdL(3,6),         & 
& cplcUFdGluSdR(3,6),cplcUFuChiSuL(3,10,6),cplcUFuChiSuR(3,10,6),cplcUFuFdcHpmL(3,3,8),  & 
& cplcUFuFdcHpmR(3,3,8),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuAhL(3,3,8),     & 
& cplcUFuFuAhR(3,3,8),cplcUFuFuhhL(3,3,8),cplcUFuFuhhR(3,3,8),cplcUFuFuVGL(3,3),         & 
& cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),               & 
& cplcUFuFuVZR(3,3),cplcUFuGluSuL(3,6),cplcUFuGluSuR(3,6),cplcUHpmVPVWm(8),              & 
& cplcUHpmVWmVZ(8),cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,             & 
& cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplcVWmVPVWm,cplcVWmVWmVZ,             & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplGluFdcSdL(3,6),cplGluFdcSdR(3,6),   & 
& cplGluFdcUSdL(3,6),cplGluFdcUSdR(3,6),cplGluFucSuL(3,6),cplGluFucSuR(3,6),             & 
& cplGluFucUSuL(3,6),cplGluFucUSuR(3,6),cplGluGluVGL,cplGluGluVGR,cplhhcUHpmVWm(8,8),    & 
& cplhhcVWmVWm(8),cplhhhhcVWmVWm(8,8),cplhhhhUHpmcUHpm(8,8,8,8),cplhhhhUSdcUSd(8,8,6,6)

Complex(dp) :: cplhhhhUSucUSu(8,8,6,6),cplhhhhVZVZ(8,8),cplhhHpmcUHpm(8,8,8),cplhhHpmcVWm(8,8),       & 
& cplhhSdcUSd(8,6,6),cplhhSucUSu(8,6,6),cplhhVZVZ(8),cplHpmcHpmcVWmVWm(8,8),             & 
& cplHpmcHpmVZ(8,8),cplHpmcHpmVZVZ(8,8),cplHpmcUHpmVP(8,8),cplHpmcUHpmVZ(8,8),           & 
& cplHpmcVWmVP(8),cplHpmcVWmVZ(8),cplHpmSucUSd(8,6,6),cplHpmUSdcHpmcUSd(8,6,8,6),        & 
& cplHpmUSucHpmcUSu(8,6,8,6),cplSdcHpmcUSu(6,8,6),cplSdcSdcVWmVWm(6,6),cplSdcSdVZ(6,6),  & 
& cplSdcSdVZVZ(6,6),cplSdcSucVWm(6,6),cplSdcUHpmcSu(6,8,6),cplSdcUSdVG(6,6),             & 
& cplSdcUSdVP(6,6),cplSdcUSdVZ(6,6),cplSdcUSucVWm(6,6),cplSdUSucSdcUSu(6,6,6,6),         & 
& cplSucSucVWmVWm(6,6),cplSucSuVZ(6,6),cplSucSuVZVZ(6,6),cplSucUSdVWm(6,6),              & 
& cplSucUSuVG(6,6),cplSucUSuVP(6,6),cplSucUSuVZ(6,6),cplUAhAhAh(8,8,8),cplUAhAhhh(8,8,8),& 
& cplUAhhhhh(8,8,8),cplUAhhhVZ(8,8),cplUAhHpmcHpm(8,8,8),cplUAhHpmcVWm(8,8),             & 
& cplUAhSdcSd(8,6,6),cplUAhSucSu(8,6,6),cplUAhUAhAhAh(8,8,8,8),cplUAhUAhcVWmVWm(8,8),    & 
& cplUAhUAhhhhh(8,8,8,8),cplUAhUAhHpmcHpm(8,8,8,8),cplUAhUAhSdcSd(8,8,6,6),              & 
& cplUAhUAhSucSu(8,8,6,6),cplUAhUAhVZVZ(8,8),cplUChiChacHpmL(10,5,8),cplUChiChacHpmR(10,5,8),& 
& cplUChiChacVWmL(10,5),cplUChiChacVWmR(10,5),cplUChiChiAhL(10,10,8),cplUChiChiAhR(10,10,8),& 
& cplUChiChihhL(10,10,8),cplUChiChihhR(10,10,8),cplUChiChiVZL(10,10),cplUChiChiVZR(10,10),& 
& cplUChiFdcSdL(10,3,6),cplUChiFdcSdR(10,3,6),cplUChiFucSuL(10,3,6),cplUChiFucSuR(10,3,6),& 
& cplUhhcVWmVWm(8),cplUhhhhhh(8,8,8),cplUhhHpmcHpm(8,8,8),cplUhhHpmcVWm(8,8),            & 
& cplUhhSdcSd(8,6,6),cplUhhSucSu(8,6,6),cplUhhUhhcVWmVWm(8,8),cplUhhUhhhhhh(8,8,8,8),    & 
& cplUhhUhhHpmcHpm(8,8,8,8),cplUhhUhhSdcSd(8,8,6,6),cplUhhUhhSucSu(8,8,6,6),             & 
& cplUhhUhhVZVZ(8,8),cplUhhVZVZ(8),cplUHpmcUHpmcVWmVWm(8,8),cplUHpmcUHpmVPVP(8,8),       & 
& cplUHpmcUHpmVZVZ(8,8),cplUHpmHpmcUHpmcHpm(8,8,8,8),cplUHpmSdcUHpmcSd(8,6,8,6),         & 
& cplUHpmSucUHpmcSu(8,6,8,6),cplUSdcUSdcVWmVWm(6,6),cplUSdcUSdVGVG(6,6),cplUSdcUSdVPVP(6,6),& 
& cplUSdcUSdVZVZ(6,6),cplUSdSdcUSdcSd(6,6,6,6),cplUSdSucUSdcSu(6,6,6,6),cplUSucUSucVWmVWm(6,6),& 
& cplUSucUSuVGVG(6,6),cplUSucUSuVPVP(6,6),cplUSucUSuVZVZ(6,6),cplUSuSucUSucSu(6,6,6,6)

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1, j2, j3, j4, il, i_count, ierr 
Integer :: i2L, iFin 
Logical :: Convergence2L 
Real(dp) :: Pi2S_EffPot_save(8,8), diff(8,8)
Complex(dp) :: Tad1Loop(8), dmz2  
Real(dp) :: comp(8), tanbQ, vev2

Complex(dp) ::tempM1,tempM2 
Complex(dp),DImension(8) ::tempZA1,temPZA2
Complex(dp),DImension(8) ::tempZP1,tempZP2
Integer :: j0,  pos , ipos
Real(dp) :: MAhtemp(8) 
Complex(dp) :: ZAhtemp(8,8),ZAhtemp2(8,8) 
Real(dp) :: MHpmtemp(8) 
Complex(dp) :: ZHpmtemp(8,8), ZHpmtemp2(8,8)


Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMasses' 
 
kont = 0 
 
tanbQ=vu/vd

Call TreeMasses(MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,MGlu,MGlu2,          & 
& Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSu,MSu2,MVWm,MVWm2,MVZ,MVZ2,pG,TW,ZER,ZEL,               & 
& ZA,ZD,ZDL,ZDR,ZH,UV,ZP,ZU,ZUL,ZUR,ZW,ZZ,vd,vu,vR,vL,g1,g2,g3,Yd,Ye,lam,Yv,             & 
& Yu,kap,Td,Te,Tlam,Tv,Tu,Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,M1,M2,              & 
& M3,GenerationMixing,kont)


mHd2Tree  = mHd2
mHu2Tree  = mHu2
ml2Tree  = ml2
mv2Tree  = mv2
 
 If (CalculateOneLoopMasses) Then 
 
Call CouplingsForVectorBosons(g1,g2,ZH,ZA,TW,ZER,ZEL,UV,vd,vu,vL,ZP,ZD,               & 
& ZU,ZDL,ZUL,cplAhhhVZ,cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,            & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,           & 
& cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,    & 
& cplhhhhVZVZ,cplHpmcHpmVZVZ,cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,  & 
& cplcVWmVWmVZVZ3,cplAhHpmcVWm,cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,              & 
& cplcFuFdcVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,               & 
& cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplSdcSucVWm,cplcVWmVPVWm,cplAhAhcVWmVWm,       & 
& cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSucSucVWmVWm,cplcVWmVPVPVWm1,      & 
& cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)

if(General_Error == 1) return


Call Pi1LoopVZ(mZ2,Mhh,Mhh2,MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,              & 
& MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhhhVZ,cplcChaChaVZL,         & 
& cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVZL,           & 
& cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,           & 
& cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHpmcHpmVZVZ,             & 
& cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,             & 
& kont,dmZ2)
if(General_Error == 1) return

vev2=4._dp*Real(mZ2+dmz2,dp)/(g1**2+g2**2) -2*(vL(1)**2 + vL(2)**2 + vL(3)**2) 
vd=Sqrt(vev2/(1._dp+tanbQ**2))
vu=tanbQ*vd
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,lam,Yv,Yu,kap,Td,Te,Tlam,Tv,Tu,             & 
& Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,M1,M2,M3,vd,vu,vL,vR,(/ ZeroC, ZeroC, ZeroC, ZeroC, ZeroC, ZeroC, ZeroC, ZeroC /))
if(General_Error == 1) return

! Set Gauge fixing parameters 
RXi= RXiNew 
RXiG = RXi 
RXiP = RXi 
RXiWm = RXi 
RXiZ = RXi 
Call TreeMasses(MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,MGlu,MGlu2,          & 
& Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSu,MSu2,MVWm,MVWm2,MVZ,MVZ2,pG,TW,ZER,ZEL,               & 
& ZA,ZD,ZDL,ZDR,ZH,UV,ZP,ZU,ZUL,ZUR,ZW,ZZ,vd,vu,vR,vL,g1,g2,g3,Yd,Ye,lam,Yv,             & 
& Yu,kap,Td,Te,Tlam,Tv,Tu,Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,M1,M2,              & 
& M3,GenerationMixing,kont)
if(General_Error == 1) return

Call CouplingsForLoopMasses(Yd,Td,lam,vu,vR,ZD,ZA,g2,Yu,ZER,ZEL,ZUL,ZUR,              & 
& g1,UV,ZDL,ZDR,g3,pG,vd,vL,ZH,Ye,Yv,Tu,ZU,ZP,TW,Tlam,Tv,kap,Tk,Te,cplAhSdcUSd,          & 
& cplChaFucUSdL,cplChaFucUSdR,cplChiFdcUSdL,cplChiFdcUSdR,cplGluFdcUSdL,cplGluFdcUSdR,   & 
& cplhhSdcUSd,cplHpmSucUSd,cplSdcUSdVG,cplSdcUSdVP,cplSdcUSdVZ,cplSucUSdVWm,             & 
& cplAhAhUSdcUSd,cplhhhhUSdcUSd,cplHpmUSdcHpmcUSd,cplUSdSdcUSdcSd,cplUSdSucUSdcSu,       & 
& cplUSdcUSdVGVG,cplUSdcUSdVPVP,cplUSdcUSdcVWmVWm,cplUSdcUSdVZVZ,cplAhSucUSu,            & 
& cplChiFucUSuL,cplChiFucUSuR,cplcChaFdcUSuL,cplcChaFdcUSuR,cplGluFucUSuL,               & 
& cplGluFucUSuR,cplhhSucUSu,cplSdcHpmcUSu,cplSdcUSucVWm,cplSucUSuVG,cplSucUSuVP,         & 
& cplSucUSuVZ,cplAhAhUSucUSu,cplhhhhUSucUSu,cplHpmUSucHpmcUSu,cplSdUSucSdcUSu,           & 
& cplUSuSucUSucSu,cplUSucUSuVGVG,cplUSucUSuVPVP,cplUSucUSucVWmVWm,cplUSucUSuVZVZ,        & 
& cplAhAhUhh,cplAhUhhhh,cplAhUhhVZ,cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,          & 
& cplChiChiUhhR,cplcFdFdUhhL,cplcFdFdUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,       & 
& cplcgWpCgWpCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpmcHpm,cplUhhHpmcVWm,cplUhhSdcSd,        & 
& cplUhhSucSu,cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpmcHpm,     & 
& cplUhhUhhSdcSd,cplUhhUhhSucSu,cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,cplUAhAhAh,               & 
& cplUAhAhhh,cplcChaChaUAhL,cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,     & 
& cplcFdFdUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,       & 
& cplUAhhhVZ,cplUAhHpmcHpm,cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,          & 
& cplUAhUAhhhhh,cplUAhUAhHpmcHpm,cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,         & 
& cplUAhUAhVZVZ,cplAhHpmcUHpm,cplAhcUHpmVWm,cplChiChacUHpmL,cplChiChacUHpmR,             & 
& cplcFuFdcUHpmL,cplcFuFdcUHpmR,cplcgZgWmcUHpm,cplcgWmgZUHpm,cplcgWpCgZcUHpm,            & 
& cplcgZgWpCUHpm,cplhhHpmcUHpm,cplhhcUHpmVWm,cplHpmcUHpmVP,cplHpmcUHpmVZ,cplSdcUHpmcSu,  & 
& cplcUHpmVPVWm,cplcUHpmVWmVZ,cplAhAhUHpmcUHpm,cplhhhhUHpmcUHpm,cplUHpmHpmcUHpmcHpm,     & 
& cplUHpmSdcUHpmcSd,cplUHpmSucUHpmcSu,cplUHpmcUHpmVPVP,cplUHpmcUHpmcVWmVWm,              & 
& cplUHpmcUHpmVZVZ,cplUChiChiAhL,cplUChiChiAhR,cplUChiChacHpmL,cplUChiChacHpmR,          & 
& cplUChiChacVWmL,cplUChiChacVWmR,cplUChiChihhL,cplUChiChihhR,cplUChiChiVZL,             & 
& cplUChiChiVZR,cplUChiFdcSdL,cplUChiFdcSdR,cplUChiFucSuL,cplUChiFucSuR,cplcUChaChaAhL,  & 
& cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,cplcUChaChaVPR,            & 
& cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,cplcUChaChiVWmL,         & 
& cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,cplcUChacFuSdR,           & 
& cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdChaSuL,cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,     & 
& cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,         & 
& cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHpmL,cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,     & 
& cplcUFdGluSdL,cplcUFdGluSdR,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuChiSuL,cplcUFuChiSuR,     & 
& cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,              & 
& cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,         & 
& cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,cplcChacUFuSdR,cplGluFdcSdL,   & 
& cplGluFdcSdR,cplGluFucSuL,cplGluFucSuR,cplGluGluVGL,cplGluGluVGR,cplAhhhVZ,            & 
& cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,         & 
& cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ,cplHpmcHpmVZ,            & 
& cplHpmcVWmVZ,cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,               & 
& cplHpmcHpmVZVZ,cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,              & 
& cplcVWmVWmVZVZ3,cplAhHpmcVWm,cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,              & 
& cplcFuFdcVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,               & 
& cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplSdcSucVWm,cplcVWmVPVWm,cplAhAhcVWmVWm,       & 
& cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSucSucVWmVWm,cplcVWmVPVPVWm1,      & 
& cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)

Call OneLoopTadpoleshh(vd,vL,vR,vu,MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,           & 
& MFu,MFu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSu,MSu2,MVWm,MVWm2,MVZ,MVZ2,cplAhAhUhh,         & 
& cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,cplcFdFdUhhR,   & 
& cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,cplUhhhhhh,        & 
& cplUhhHpmcHpm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,cplUhhVZVZ,Tad1Loop(1:8))
if(General_Error == 1) return

mHd2Tree  = mHd2
mHu2Tree  = mHu2
ml2Tree  = ml2
mv2Tree  = mv2
If (CalculateTwoLoopHiggsMasses) Then 
    If(GaugelessLimit) Then 
  vdFix = 0._dp 
  vuFix = 0._dp 
  vRFix = 0._dp 
  vLFix = 0._dp 
   g1_saveEP =g1
   g1 = 0._dp 
   g2_saveEP =g2
   g2 = 0._dp 
     Else 
  vdFix = vd 
  vuFix = vu 
  vRFix = vR 
  vLFix = vL 
     End if 

SELECT CASE (TwoLoopMethod) 
CASE ( 1 , 2 ) 
  ! Make sure that there are no exactly degenerated masses! 
   Yd_saveEP =Yd
   where (aint(Abs(Yd)).eq.Yd) Yd=Yd*(1 + 1*1.0E-12_dp)
   Ye_saveEP =Ye
   where (aint(Abs(Ye)).eq.Ye) Ye=Ye*(1 + 2*1.0E-12_dp)
   Yv_saveEP =Yv
   where (aint(Abs(Yv)).eq.Yv) Yv=Yv*(1 + 3*1.0E-12_dp)
   Yu_saveEP =Yu
   where (aint(Abs(Yu)).eq.Yu) Yu=Yu*(1 + 4*1.0E-12_dp)
   Td_saveEP =Td
   where (aint(Abs(Td)).eq.Td) Td=Td*(1 + 5*1.0E-12_dp)
   Te_saveEP =Te
   where (aint(Abs(Te)).eq.Te) Te=Te*(1 + 6*1.0E-12_dp)
   Tv_saveEP =Tv
   where (aint(Abs(Tv)).eq.Tv) Tv=Tv*(1 + 7*1.0E-12_dp)
   Tu_saveEP =Tu
   where (aint(Abs(Tu)).eq.Tu) Tu=Tu*(1 + 8*1.0E-12_dp)
   mq2_saveEP =mq2
   where (aint(Abs(mq2)).eq.mq2) mq2=mq2*(1 + 9*1.0E-12_dp)
   ml2_saveEP =ml2
   where (aint(Abs(ml2)).eq.ml2) ml2=ml2*(1 + 10*1.0E-12_dp)
   md2_saveEP =md2
   where (aint(Abs(md2)).eq.md2) md2=md2*(1 + 11*1.0E-12_dp)
   mu2_saveEP =mu2
   where (aint(Abs(mu2)).eq.mu2) mu2=mu2*(1 + 12*1.0E-12_dp)
   me2_saveEP =me2
   where (aint(Abs(me2)).eq.me2) me2=me2*(1 + 13*1.0E-12_dp)
   mv2_saveEP =mv2
   where (aint(Abs(mv2)).eq.mv2) mv2=mv2*(1 + 14*1.0E-12_dp)

If (TwoLoopSafeMode) Then 
  iFin = 12 
  Convergence2L = .false. 
  hstep_pn = 2.0_dp 
  hstep_sa = 2.0_dp 
Else 
  iFin = 1 
  Convergence2L = .true. 
End if 

Pi2S_EffPot_save = 0._dp 
Pi2S_EffPot = 0._dp 

Do i2L = 1, iFin 
Call CalculateCorrectionsEffPot(ti_ep2L,pi_ep2L,vd,vu,vR,vL,g1,g2,g3,Yd,              & 
& Ye,lam,Yv,Yu,kap,Td,Te,Tlam,Tv,Tu,Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,          & 
& M1,M2,M3,kont)
if(General_Error == 1) return

Pi2S_EffPot(1,1) = pi_ep2L(1,1)!-ti_ep2L(1)/vd
Pi2S_EffPot(1,2) = pi_ep2L(1,2)
Pi2S_EffPot(1,3) = pi_ep2L(1,3)
Pi2S_EffPot(1,4) = pi_ep2L(1,4)
Pi2S_EffPot(1,5) = pi_ep2L(1,5)
Pi2S_EffPot(1,6) = pi_ep2L(1,6)
Pi2S_EffPot(1,7) = pi_ep2L(1,7)
Pi2S_EffPot(1,8) = pi_ep2L(1,8)
Pi2S_EffPot(2,1) = pi_ep2L(2,1)
Pi2S_EffPot(2,2) = pi_ep2L(2,2)!-ti_ep2L(2)/vu
Pi2S_EffPot(2,3) = pi_ep2L(2,3)
Pi2S_EffPot(2,4) = pi_ep2L(2,4)
Pi2S_EffPot(2,5) = pi_ep2L(2,5)
Pi2S_EffPot(2,6) = pi_ep2L(2,6)
Pi2S_EffPot(2,7) = pi_ep2L(2,7)
Pi2S_EffPot(2,8) = pi_ep2L(2,8)
Pi2S_EffPot(3,1) = pi_ep2L(3,1)
Pi2S_EffPot(3,2) = pi_ep2L(3,2)
Pi2S_EffPot(3,3) = pi_ep2L(3,3)!-ti_ep2L(3)/vR(1)
Pi2S_EffPot(3,4) = pi_ep2L(3,4)
Pi2S_EffPot(3,5) = pi_ep2L(3,5)
Pi2S_EffPot(3,6) = pi_ep2L(3,6)
Pi2S_EffPot(3,7) = pi_ep2L(3,7)
Pi2S_EffPot(3,8) = pi_ep2L(3,8)
Pi2S_EffPot(4,1) = pi_ep2L(4,1)
Pi2S_EffPot(4,2) = pi_ep2L(4,2)
Pi2S_EffPot(4,3) = pi_ep2L(4,3)
Pi2S_EffPot(4,4) = pi_ep2L(4,4)!-ti_ep2L(4)/vR(2)
Pi2S_EffPot(4,5) = pi_ep2L(4,5)
Pi2S_EffPot(4,6) = pi_ep2L(4,6)
Pi2S_EffPot(4,7) = pi_ep2L(4,7)
Pi2S_EffPot(4,8) = pi_ep2L(4,8)
Pi2S_EffPot(5,1) = pi_ep2L(5,1)
Pi2S_EffPot(5,2) = pi_ep2L(5,2)
Pi2S_EffPot(5,3) = pi_ep2L(5,3)
Pi2S_EffPot(5,4) = pi_ep2L(5,4)
Pi2S_EffPot(5,5) = pi_ep2L(5,5)!-ti_ep2L(5)/vR(3)
Pi2S_EffPot(5,6) = pi_ep2L(5,6)
Pi2S_EffPot(5,7) = pi_ep2L(5,7)
Pi2S_EffPot(5,8) = pi_ep2L(5,8)
Pi2S_EffPot(6,1) = pi_ep2L(6,1)
Pi2S_EffPot(6,2) = pi_ep2L(6,2)
Pi2S_EffPot(6,3) = pi_ep2L(6,3)
Pi2S_EffPot(6,4) = pi_ep2L(6,4)
Pi2S_EffPot(6,5) = pi_ep2L(6,5)
Pi2S_EffPot(6,6) = pi_ep2L(6,6)!-ti_ep2L(6)/vL(1)
Pi2S_EffPot(6,7) = pi_ep2L(6,7)
Pi2S_EffPot(6,8) = pi_ep2L(6,8)
Pi2S_EffPot(7,1) = pi_ep2L(7,1)
Pi2S_EffPot(7,2) = pi_ep2L(7,2)
Pi2S_EffPot(7,3) = pi_ep2L(7,3)
Pi2S_EffPot(7,4) = pi_ep2L(7,4)
Pi2S_EffPot(7,5) = pi_ep2L(7,5)
Pi2S_EffPot(7,6) = pi_ep2L(7,6)
Pi2S_EffPot(7,7) = pi_ep2L(7,7)!-ti_ep2L(7)/vL(2)
Pi2S_EffPot(7,8) = pi_ep2L(7,8)
Pi2S_EffPot(8,1) = pi_ep2L(8,1)
Pi2S_EffPot(8,2) = pi_ep2L(8,2)
Pi2S_EffPot(8,3) = pi_ep2L(8,3)
Pi2S_EffPot(8,4) = pi_ep2L(8,4)
Pi2S_EffPot(8,5) = pi_ep2L(8,5)
Pi2S_EffPot(8,6) = pi_ep2L(8,6)
Pi2S_EffPot(8,7) = pi_ep2L(8,7)
Pi2S_EffPot(8,8) = pi_ep2L(8,8)!-ti_ep2L(8)/vL(3)
 diff=(Pi2S_EffPot-Pi2S_EffPot_save)/MaxVal(Abs(Pi2S_EffPot)) 
  If (MaxVal(Abs(diff)).lt.1.0E-4_dp) Then 
    Convergence2L = .True. 
    Exit 
  Else 
    Pi2S_EffPot_save = Pi2S_EffPot 
  hstep_pn = hstep_pn/2._dp 
  hstep_sa = hstep_sa/2._dp 
  End If 
End do 
If (.not.Convergence2L) Then 
 Write(*,*) "WARNING: Two-Loop corrections are numerically unstable! "  
 Call TerminateProgram 
    return
End If  
 Pi2A0 = 0._dp 
   Yd =Yd_saveEP 
   Ye =Ye_saveEP 
   Yv =Yv_saveEP 
   Yu =Yu_saveEP 
   Td =Td_saveEP 
   Te =Te_saveEP 
   Tv =Tv_saveEP 
   Tu =Tu_saveEP 
   mq2 =mq2_saveEP 
   ml2 =ml2_saveEP 
   md2 =md2_saveEP 
   mu2 =mu2_saveEP 
   me2 =me2_saveEP 
   mv2 =mv2_saveEP 


 CASE ( 3 ) ! Diagrammatic method 
  ! Make sure that there are no exactly degenerated masses! 
   Yd_saveEP =Yd
   where (aint(Abs(Yd)).eq.Yd) Yd=Yd*(1 + 1*1.0E-12_dp)
   Ye_saveEP =Ye
   where (aint(Abs(Ye)).eq.Ye) Ye=Ye*(1 + 2*1.0E-12_dp)
   Yv_saveEP =Yv
   where (aint(Abs(Yv)).eq.Yv) Yv=Yv*(1 + 3*1.0E-12_dp)
   Yu_saveEP =Yu
   where (aint(Abs(Yu)).eq.Yu) Yu=Yu*(1 + 4*1.0E-12_dp)
   Td_saveEP =Td
   where (aint(Abs(Td)).eq.Td) Td=Td*(1 + 5*1.0E-12_dp)
   Te_saveEP =Te
   where (aint(Abs(Te)).eq.Te) Te=Te*(1 + 6*1.0E-12_dp)
   Tv_saveEP =Tv
   where (aint(Abs(Tv)).eq.Tv) Tv=Tv*(1 + 7*1.0E-12_dp)
   Tu_saveEP =Tu
   where (aint(Abs(Tu)).eq.Tu) Tu=Tu*(1 + 8*1.0E-12_dp)
   mq2_saveEP =mq2
   where (aint(Abs(mq2)).eq.mq2) mq2=mq2*(1 + 9*1.0E-12_dp)
   ml2_saveEP =ml2
   where (aint(Abs(ml2)).eq.ml2) ml2=ml2*(1 + 10*1.0E-12_dp)
   md2_saveEP =md2
   where (aint(Abs(md2)).eq.md2) md2=md2*(1 + 11*1.0E-12_dp)
   mu2_saveEP =mu2
   where (aint(Abs(mu2)).eq.mu2) mu2=mu2*(1 + 12*1.0E-12_dp)
   me2_saveEP =me2
   where (aint(Abs(me2)).eq.me2) me2=me2*(1 + 13*1.0E-12_dp)
   mv2_saveEP =mv2
   where (aint(Abs(mv2)).eq.mv2) mv2=mv2*(1 + 14*1.0E-12_dp)

Call CalculatePi2S(0._dp,vd,vu,vR,vL,g1,g2,g3,Yd,Ye,lam,Yv,Yu,kap,Td,Te,              & 
& Tlam,Tv,Tu,Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,M1,M2,M3,kont,ti_ep2L,           & 
& Pi2S_EffPot,PiP2S_EffPot)
if(General_Error == 1) return

   Yd =Yd_saveEP 
   Ye =Ye_saveEP 
   Yv =Yv_saveEP 
   Yu =Yu_saveEP 
   Td =Td_saveEP 
   Te =Te_saveEP 
   Tv =Tv_saveEP 
   Tu =Tu_saveEP 
   mq2 =mq2_saveEP 
   ml2 =ml2_saveEP 
   md2 =md2_saveEP 
   mu2 =mu2_saveEP 
   me2 =me2_saveEP 
   mv2 =mv2_saveEP 


 CASE ( 8 , 9 ) ! Hard-coded routines 
  
 END SELECT
 
   If(GaugelessLimit) Then 
   g1 =g1_saveEP 
   g2 =g2_saveEP 
   End if 

Else ! Two loop turned off 
Pi2S_EffPot = 0._dp 

Pi2A0 = 0._dp 

ti_ep2L = 0._dp 

End if 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,lam,Yv,Yu,kap,Td,Te,Tlam,Tv,Tu,             & 
& Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,M1,M2,M3,vd,vu,vL,vR,Tad1Loop)
if(General_Error == 1) return

mHd21L = mHd2
mHu21L = mHu2
ml21L = ml2
mv21L = mv2
Tad1Loop(1:8) = Tad1Loop(1:8) - ti_ep2L 
Call SolveTadpoleEquations(g1,g2,g3,Yd,Ye,lam,Yv,Yu,kap,Td,Te,Tlam,Tv,Tu,             & 
& Tk,mq2,ml2,mHd2,mHu2,md2,mu2,me2,mv2,mlHd2,M1,M2,M3,vd,vu,vL,vR,Tad1Loop)
if(General_Error == 1) return

mHd22L = mHd2
mHu22L = mHu2
ml22L = ml2
mv22L = mv2
Call OneLoopSd(g1,g2,Yd,Td,lam,mq2,md2,vd,vu,vL,vR,MSd,MSd2,MAh,MAh2,MFu,             & 
& MFu2,MCha,MCha2,MFd,MFd2,MChi,MChi2,MGlu,MGlu2,Mhh,Mhh2,MSu,MSu2,MHpm,MHpm2,           & 
& MVZ,MVZ2,MVWm,MVWm2,cplAhSdcUSd,cplChaFucUSdL,cplChaFucUSdR,cplChiFdcUSdL,             & 
& cplChiFdcUSdR,cplGluFdcUSdL,cplGluFdcUSdR,cplhhSdcUSd,cplHpmSucUSd,cplSdcUSdVG,        & 
& cplSdcUSdVP,cplSdcUSdVZ,cplSucUSdVWm,cplAhAhUSdcUSd,cplhhhhUSdcUSd,cplHpmUSdcHpmcUSd,  & 
& cplUSdSdcUSdcSd,cplUSdSucUSdcSu,cplUSdcUSdVGVG,cplUSdcUSdVPVP,cplUSdcUSdcVWmVWm,       & 
& cplUSdcUSdVZVZ,0.1_dp*delta_mass,MSd_1L,MSd2_1L,ZD_1L,kont)
if(General_Error == 1) return

Call OneLoopSu(g1,g2,lam,Yv,Yu,Tu,mq2,mu2,vd,vu,vL,vR,MSu,MSu2,MAh,MAh2,              & 
& MFu,MFu2,MChi,MChi2,MCha,MCha2,MFd,MFd2,MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,            & 
& MSd2,MVWm,MVWm2,MVZ,MVZ2,cplAhSucUSu,cplChiFucUSuL,cplChiFucUSuR,cplcChaFdcUSuL,       & 
& cplcChaFdcUSuR,cplGluFucUSuL,cplGluFucUSuR,cplhhSucUSu,cplSdcHpmcUSu,cplSdcUSucVWm,    & 
& cplSucUSuVG,cplSucUSuVP,cplSucUSuVZ,cplAhAhUSucUSu,cplhhhhUSucUSu,cplHpmUSucHpmcUSu,   & 
& cplSdUSucSdcUSu,cplUSuSucUSucSu,cplUSucUSuVGVG,cplUSucUSuVPVP,cplUSucUSucVWmVWm,       & 
& cplUSucUSuVZVZ,0.1_dp*delta_mass,MSu_1L,MSu2_1L,ZU_1L,kont)
if(General_Error == 1) return

Call OneLoophh(g1,g2,lam,Tlam,Yv,Tv,kap,Tk,ml22L,mlHd2,mHd22L,mHu22L,mv22L,           & 
& vd,vu,vL,vR,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,             & 
& MFu2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVZ,         & 
& cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,cplcFdFdUhhR,   & 
& cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,cplUhhhhhh,        & 
& cplUhhHpmcHpm,cplUhhHpmcVWm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,cplUhhVZVZ,          & 
& cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpmcHpm,cplUhhUhhSdcSd,cplUhhUhhSucSu,            & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,0.1_dp*delta_mass,Mhh_1L,Mhh2_1L,ZH_1L,kont)
if(General_Error == 1) return

If (TwoLoopMethod.gt.2) Then 
Call OneLoopAh(g1,g2,lam,Tlam,Yv,Tv,kap,Tk,ml22L,mlHd2,mHd22L,mHu22L,mv22L,           & 
& vd,vu,vL,vR,TW,MAh,MAh2,Mhh,Mhh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,              & 
& MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplUAhAhAh,cplUAhAhhh,cplcChaChaUAhL, & 
& cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,cplcFdFdUAhR,cplcFuFuUAhL,     & 
& cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,cplUAhhhVZ,cplUAhHpmcHpm,        & 
& cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHpmcHpm,    & 
& cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,0.1_dp*delta_mass,        & 
& MAh_1L,MAh2_1L,ZA_1L,kont)
if(General_Error == 1) return
Else 
Call OneLoopAh(g1,g2,lam,Tlam,Yv,Tv,kap,Tk,ml21L,mlHd2,mHd21L,mHu21L,mv21L,           & 
& vd,vu,vL,vR,TW,MAh,MAh2,Mhh,Mhh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,              & 
& MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplUAhAhAh,cplUAhAhhh,cplcChaChaUAhL, & 
& cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,cplcFdFdUAhR,cplcFuFuUAhL,     & 
& cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,cplUAhhhVZ,cplUAhHpmcHpm,        & 
& cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHpmcHpm,    & 
& cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,0.1_dp*delta_mass,        & 
& MAh_1L,MAh2_1L,ZA_1L,kont)
if(General_Error == 1) return
End if 

Call OneLoopHpm(g1,g2,Ye,Te,lam,Tlam,Yv,Tv,kap,ml21L,mlHd2,mHd21L,mHu21L,             & 
& me2,vd,vu,vL,vR,MHpm,MHpm2,MAh,MAh2,MVWm,MVWm2,MChi,MChi2,MCha,MCha2,MFu,              & 
& MFu2,MFd,MFd2,Mhh,Mhh2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcUHpm,cplAhcUHpmVWm,         & 
& cplChiChacUHpmL,cplChiChacUHpmR,cplcFuFdcUHpmL,cplcFuFdcUHpmR,cplcgZgWmcUHpm,          & 
& cplcgWmgZUHpm,cplcgWpCgZcUHpm,cplcgZgWpCUHpm,cplhhHpmcUHpm,cplhhcUHpmVWm,              & 
& cplHpmcUHpmVP,cplHpmcUHpmVZ,cplSdcUHpmcSu,cplcUHpmVPVWm,cplcUHpmVWmVZ,cplAhAhUHpmcUHpm,& 
& cplhhhhUHpmcUHpm,cplUHpmHpmcUHpmcHpm,cplUHpmSdcUHpmcSd,cplUHpmSucUHpmcSu,              & 
& cplUHpmcUHpmVPVP,cplUHpmcUHpmcVWmVWm,cplUHpmcUHpmVZVZ,0.1_dp*delta_mass,               & 
& MHpm_1L,MHpm2_1L,ZP_1L,kont)
if(General_Error == 1) return

Call OneLoopChi(g1,g2,lam,Yv,kap,M1,M2,vd,vu,vL,vR,MChi,MChi2,MAh,MAh2,               & 
& MHpm,MHpm2,MCha,MCha2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MSd,MSd2,MFd,MFd2,MSu,              & 
& MSu2,MFu,MFu2,cplUChiChiAhL,cplUChiChiAhR,cplUChiChacHpmL,cplUChiChacHpmR,             & 
& cplUChiChacVWmL,cplUChiChacVWmR,cplUChiChihhL,cplUChiChihhR,cplUChiChiVZL,             & 
& cplUChiChiVZR,cplUChiFdcSdL,cplUChiFdcSdR,cplUChiFucSuL,cplUChiFucSuR,0.1_dp*delta_mass,& 
& MChi_1L,MChi2_1L,UV_1L,kont)
if(General_Error == 1) return

Call OneLoopCha(g2,Ye,lam,Yv,M2,vd,vu,vL,vR,MCha,MCha2,MAh,MAh2,Mhh,Mhh2,             & 
& MVZ,MVZ2,MHpm,MHpm2,MChi,MChi2,MVWm,MVWm2,MSu,MSu2,MFd,MFd2,MFu,MFu2,MSd,              & 
& MSd2,cplcUChaChaAhL,cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,       & 
& cplcUChaChaVPR,cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,          & 
& cplcUChaChiVWmL,cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,          & 
& cplcUChacFuSdR,0.1_dp*delta_mass,MCha_1L,MCha2_1L,ZER_1L,ZEL_1L,kont)
if(General_Error == 1) return


Call OneLoopGlu(M3,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,MGlu,MGlu2,cplGluFdcSdL,       & 
& cplGluFdcSdR,cplGluFucSuL,cplGluFucSuR,cplGluGluVGL,cplGluGluVGR,0.1_dp*delta_mass,    & 
& MGlu_1L,MGlu2_1L,kont)
if(General_Error == 1) return



MSd = MSd_1L
MSd2 = MSd2_1L
ZD = ZD_1L
MSu = MSu_1L
MSu2 = MSu2_1L 
ZU = ZU_1L
Mhh = Mhh_1L
Mhh2 = Mhh2_1L 
ZH = ZH_1L 
MAh = MAh_1L 
MAh2 = MAh2_1L 
ZA = ZA_1L
MHpm = MHpm_1L
MHpm2 = MHpm2_1L
ZP = ZP_1L
MChi = MChi_1L
MChi2 = MChi2_1L
UV = UV_1L
MCha = MCha_1L
MCha2 = MCha2_1L
ZER = ZER_1L
ZEL = ZEL_1L
MGlu = MGlu_1L
MGlu2 = MGlu2_1L
End If 
 
Call SortGoldstones(MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,MGlu,            & 
& MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSu,MSu2,MVWm,MVWm2,MVZ,MVZ2,pG,TW,ZER,             & 
& ZEL,ZA,ZD,ZDL,ZDR,ZH,UV,ZP,ZU,ZUL,ZUR,ZW,ZZ,kont)
if(General_Error == 1) return

!!DK
ipos = 1
do j0=1,8
!write(*,*) ZA(j0,1)**2 +ZA(j0,2)**2
IF ( ZA(j0,2)**2 .gt. 0.5 )then
 ipos = j0
! write (*,*) ipos
 go to 997
 Endif
enddo
997 continue

!write (*,*) ipos, MAh(1), MAh(ipos)
if (ipos .eq. 1) then
!write (*,*) ipos!, MAh(1), MAh(ipos)
go to 1000
else if (ipos .eq. 2) then
!write (*,*) ipos, MAh2(1), MAh2(2), MAh2(ipos)
  MAhtemp = MAh2 
  ZAhtemp = ZA 
  MAh2(1) = MAhtemp(ipos) 
  ZA(1,:) = ZAhtemp(ipos,:) 
  MAh2(ipos) = MAhtemp(1) 
  ZA(ipos,:) = ZAhtemp(1,:)
!write (*,*) ipos,  MAh2(1), MAh2(ipos)
else
  MAhtemp = MAh2 
  ZAhtemp = ZA 
  MAh2(1) = MAhtemp(ipos) 
  ZA(1,:) = ZAhtemp(ipos,:) 
  MAh2(ipos) = MAhtemp(1) 
  ZA(ipos,:) = ZAhtemp(1,:)
!write (*,*) ipos,  MAh2(1), MAh2(ipos)
 ! Reorder the physical states by their mass 
  Do i1=2,8
  Do i2 = i1, 8
  If(MAh2(i1) .gt. MAh2(i2) ) then
  tempM2 = MAh2(i2) 
  ZAhtemp2(i2,:) = ZA(i2,:)

  MAh2(i2) = MAh2(i1) 
  ZA(i2,:) = ZA(i1,:) 

  MAh2(i1) = tempM2 
  ZA(i1,:) = ZAhtemp2(i2,:) 
  Endif
  Enddo
  Enddo
endif
MAh = sqrt(MAh2)
1000 continue


!write(*,*) ' After Loop sorting Goldstones '
!write(*,*) Mhh_1L(1),Mhh_1L(2),Mhh_1L(3),Mhh_1L(4),Mhh_1L(5)
!write(*,*) Mhh(1),Mhh(2),Mhh(3),Mhh(4),Mhh(5)
!write(*,*) '   '
!write(*,*) MAh_1L(1),MAh_1L(2),MAh_1L(3),MAh_1L(4),MAh_1L(5)
!write(*,*) MAh(1),MAh(2),MAh(3),MAh(4),MAh(5)
!write(*,*) '   '


!!DK
ipos = 1
do j0=1,8
!write(*,*) ZP(j0,1)**2 +ZP(j0,2)**2
IF ( ZP(j0,2)**2 .gt. 0.5 )then
 ipos = j0
! write (*,*) ipos
 go to 999
 Endif
enddo
999 continue

!write (*,*) ipos, MHpm(1), MHpm(ipos)
if (ipos .eq. 1) then
!write (*,*) ipos!, MHpm(1), MHpm(ipos)
go to 1001
else if (ipos .eq. 2) then
!write (*,*) ipos, MHpm2(1), MHpm2(2), MHpm2(ipos)
  MHpmtemp = MHpm2 
  ZHpmtemp = ZP 
  MHpm2(1) = MHpmtemp(ipos) 
  ZP(1,:) = ZHpmtemp(ipos,:) 
  MHpm2(ipos) = MHpmtemp(1) 
  ZP(ipos,:) = ZHpmtemp(1,:)
!write (*,*) ipos,  MHpm2(1), MHpm2(ipos)
else
  MHpmtemp = MHpm2 
  ZHpmtemp = ZP 
  MHpm2(1) = MHpmtemp(ipos) 
  ZP(1,:) = ZHpmtemp(ipos,:) 
  MHpm2(ipos) = MHpmtemp(1) 
  ZP(ipos,:) = ZHpmtemp(1,:)
!write (*,*) ipos,  MHpm2(1), MHpm2(ipos)
 ! Reorder the physical states by their mass 
  Do i1=2,8
  Do i2 = i1, 8
  If(MHpm2(i1) .gt. MHpm2(i2) ) then
  tempM2 = MHpm2(i2) 
  ZHpmtemp2(i2,:) = ZP(i2,:)

  MHpm2(i2) = MHpm2(i1) 
  ZP(i2,:) = ZP(i1,:) 

  MHpm2(i1) = tempM2 
  ZP(i1,:) = ZHpmtemp2(i2,:) 
  Endif
  Enddo
  Enddo
endif
MHpm = sqrt(MHpm2)
1001 continue

! Set pole masses 
MVWm = mW 
MVWm2 = mW2 
MVZ = mZ 
MVZ2 = mZ2 
MCha(1:3) = mf_l 
MCha2(1:3) = mf_l**2 
MFu(1:3) = mf_u 
MFu2(1:3) = mf_u**2 
MFd(1:3) = mf_d 
MFd2(1:3) = mf_d**2 
! Shift Everything to t'Hooft Gauge
RXi=  1._dp 
RXiG = 1._dp 
RXiP = 1._dp 
RXiWm = 1._dp 
RXiZ = 1._dp 
MAh(1)=MVZ
MAh2(1)=MVZ2
MHpm(1)=MVWm
MHpm2(1)=MVWm2
mf_u2 = mf_u**2 
mf_d2 = mf_d**2 
mf_l2 = mf_l**2 
 

 If (WriteTreeLevelTadpoleSolutions) Then 
! Saving tree-level parameters for output
mHd2  = mHd2Tree 
mHu2  = mHu2Tree 
ml2  = ml2Tree 
mv2  = mv2Tree 
End if 


Iname = Iname -1 
End Subroutine OneLoopMasses 
 
Subroutine OneLoopTadpoleshh(vd,vL,vR,vu,MAh,MAh2,MCha,MCha2,MChi,MChi2,              & 
& MFd,MFd2,MFu,MFu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MSu,MSu2,MVWm,MVWm2,MVZ,MVZ2,           & 
& cplAhAhUhh,cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,     & 
& cplcFdFdUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,      & 
& cplUhhhhhh,cplUhhHpmcHpm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,cplUhhVZVZ,tadpoles)

Implicit None 
Real(dp), Intent(in) :: MAh(8),MAh2(8),MCha(5),MCha2(5),MChi(10),MChi2(10),MFd(3),MFd2(3),MFu(3),             & 
& MFu2(3),Mhh(8),Mhh2(8),MHpm(8),MHpm2(8),MSd(6),MSd2(6),MSu(6),MSu2(6),MVWm,            & 
& MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhUhh(8,8,8),cplcChaChaUhhL(5,5,8),cplcChaChaUhhR(5,5,8),cplChiChiUhhL(10,10,8), & 
& cplChiChiUhhR(10,10,8),cplcFdFdUhhL(3,3,8),cplcFdFdUhhR(3,3,8),cplcFuFuUhhL(3,3,8),    & 
& cplcFuFuUhhR(3,3,8),cplcgWmgWmUhh(8),cplcgWpCgWpCUhh(8),cplcgZgZUhh(8),cplUhhhhhh(8,8,8),& 
& cplUhhHpmcHpm(8,8,8),cplUhhSdcSd(8,6,6),cplUhhSucSu(8,6,6),cplUhhcVWmVWm(8),           & 
& cplUhhVZVZ(8)

Real(dp), Intent(in) :: vd,vL(3),vR(3),vu

Integer :: i1,i2, gO1, gO2 
Complex(dp) :: coupL, coupR, coup, temp, res, A0m, sumI(8)  
Real(dp) :: m1 
Complex(dp), Intent(out) :: tadpoles(8) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopTadpoleshh'
 
tadpoles = 0._dp 
 
!------------------------ 
! Ah 
!------------------------ 
Do i1 = 1, 8
 A0m = A0(MAh2(i1)) 
  Do gO1 = 1, 8
   coup = cplAhAhUhh(i1,i1,gO1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End Do 
 !------------------------ 
! bar[Cha] 
!------------------------ 
Do i1 = 1, 5
 A0m = 2._dp*MCha(i1)*A0(MCha2(i1)) 
  Do gO1 = 1, 8
   coupL = cplcChaChaUhhL(i1,i1,gO1)
   coupR = cplcChaChaUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 !------------------------ 
! Chi 
!------------------------ 
Do i1 = 1, 10
 A0m = 2._dp*MChi(i1)*A0(MChi2(i1)) 
  Do gO1 = 1, 8
   coupL = cplChiChiUhhL(i1,i1,gO1)
   coupR = cplChiChiUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End Do 
 !------------------------ 
! bar[Fd] 
!------------------------ 
Do i1 = 1, 3
 A0m = 2._dp*MFd(i1)*A0(MFd2(i1)) 
  Do gO1 = 1, 8
   coupL = cplcFdFdUhhL(i1,i1,gO1)
   coupR = cplcFdFdUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 !------------------------ 
! bar[Fu] 
!------------------------ 
Do i1 = 1, 3
 A0m = 2._dp*MFu(i1)*A0(MFu2(i1)) 
  Do gO1 = 1, 8
   coupL = cplcFuFuUhhL(i1,i1,gO1)
   coupR = cplcFuFuUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 !------------------------ 
! bar[gWm] 
!------------------------ 
A0m = 1._dp*A0(MVWm2*RXi) 
  Do gO1 = 1, 8
    coup = cplcgWmgWmUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! bar[gWmC] 
!------------------------ 
A0m = 1._dp*A0(MVWm2*RXi) 
  Do gO1 = 1, 8
    coup = cplcgWpCgWpCUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! bar[gZ] 
!------------------------ 
A0m = 1._dp*A0(MVZ2*RXi) 
  Do gO1 = 1, 8
    coup = cplcgZgZUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! hh 
!------------------------ 
Do i1 = 1, 8
 A0m = A0(Mhh2(i1)) 
  Do gO1 = 1, 8
   coup = cplUhhhhhh(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End Do 
 !------------------------ 
! conj[Hpm] 
!------------------------ 
Do i1 = 1, 8
 A0m = A0(MHpm2(i1)) 
  Do gO1 = 1, 8
   coup = cplUhhHpmcHpm(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 !------------------------ 
! conj[Sd] 
!------------------------ 
Do i1 = 1, 6
 A0m = A0(MSd2(i1)) 
  Do gO1 = 1, 8
   coup = cplUhhSdcSd(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 !------------------------ 
! conj[Su] 
!------------------------ 
Do i1 = 1, 6
 A0m = A0(MSu2(i1)) 
  Do gO1 = 1, 8
   coup = cplUhhSucSu(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
A0m = 3._dp*A0(MVWm2)+RXi*A0(MVWm2*RXi) - 2._dp*MVWm2*rMS 
  Do gO1 = 1, 8
    coup = cplUhhcVWmVWm(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
!------------------------ 
! VZ 
!------------------------ 
A0m = 3._dp*A0(MVZ2)+RXi*A0(MVZ2*RXi) - 2._dp*MVZ2*rMS 
  Do gO1 = 1, 8
    coup = cplUhhVZVZ(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 



tadpoles = oo16pi2*tadpoles 
Iname = Iname - 1 
End Subroutine OneLoopTadpoleshh 
 
Subroutine OneLoopSd(g1,g2,Yd,Td,lam,mq2,md2,vd,vu,vL,vR,MSd,MSd2,MAh,MAh2,           & 
& MFu,MFu2,MCha,MCha2,MFd,MFd2,MChi,MChi2,MGlu,MGlu2,Mhh,Mhh2,MSu,MSu2,MHpm,             & 
& MHpm2,MVZ,MVZ2,MVWm,MVWm2,cplAhSdcUSd,cplChaFucUSdL,cplChaFucUSdR,cplChiFdcUSdL,       & 
& cplChiFdcUSdR,cplGluFdcUSdL,cplGluFdcUSdR,cplhhSdcUSd,cplHpmSucUSd,cplSdcUSdVG,        & 
& cplSdcUSdVP,cplSdcUSdVZ,cplSucUSdVWm,cplAhAhUSdcUSd,cplhhhhUSdcUSd,cplHpmUSdcHpmcUSd,  & 
& cplUSdSdcUSdcSd,cplUSdSucUSdcSu,cplUSdcUSdVGVG,cplUSdcUSdVPVP,cplUSdcUSdcVWmVWm,       & 
& cplUSdcUSdVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MSd(6),MSd2(6),MAh(8),MAh2(8),MFu(3),MFu2(3),MCha(5),MCha2(5),MFd(3),MFd2(3),         & 
& MChi(10),MChi2(10),MGlu,MGlu2,Mhh(8),Mhh2(8),MSu(6),MSu2(6),MHpm(8),MHpm2(8),          & 
& MVZ,MVZ2,MVWm,MVWm2

Real(dp), Intent(in) :: g1,g2,vd,vu,vL(3),vR(3)

Complex(dp), Intent(in) :: Yd(3,3),Td(3,3),lam(3),mq2(3,3),md2(3,3)

Complex(dp), Intent(in) :: cplAhSdcUSd(8,6,6),cplChaFucUSdL(5,3,6),cplChaFucUSdR(5,3,6),cplChiFdcUSdL(10,3,6),   & 
& cplChiFdcUSdR(10,3,6),cplGluFdcUSdL(3,6),cplGluFdcUSdR(3,6),cplhhSdcUSd(8,6,6),        & 
& cplHpmSucUSd(8,6,6),cplSdcUSdVG(6,6),cplSdcUSdVP(6,6),cplSdcUSdVZ(6,6),cplSucUSdVWm(6,6),& 
& cplAhAhUSdcUSd(8,8,6,6),cplhhhhUSdcUSd(8,8,6,6),cplHpmUSdcHpmcUSd(8,6,8,6),            & 
& cplUSdSdcUSdcSd(6,6,6,6),cplUSdSucUSdcSu(6,6,6,6),cplUSdcUSdVGVG(6,6),cplUSdcUSdVPVP(6,6),& 
& cplUSdcUSdcVWmVWm(6,6),cplUSdcUSdVZVZ(6,6)

Complex(dp) :: mat2a(6,6), mat2(6,6),  PiSf(6,6,6)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(6), test_m2(6),p2, test(6) 
Real(dp), Intent(out) :: mass(6), mass2(6) 
Complex(dp), Intent(out) ::  RS(6,6) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopSd'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)-(g1**2*vd**2)/24._dp
mat2a(1,1) = mat2a(1,1)-(g2**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g1**2*vu**2)/24._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)+mq2(1,1)
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)-(g1**2*vL(j1)**2)/24._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(vd**2*Conjg(Yd(j1,1))*Yd(j1,1))/2._dp
End Do 
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+mq2(1,2)
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(vd**2*Conjg(Yd(j1,1))*Yd(j1,2))/2._dp
End Do 
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)+mq2(1,3)
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vd**2*Conjg(Yd(j1,1))*Yd(j1,3))/2._dp
End Do 
mat2a(1,4) = 0._dp 
mat2a(1,4) = mat2a(1,4)+(vd*Conjg(Td(1,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(Yd(1,1))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(1,5) = 0._dp 
mat2a(1,5) = mat2a(1,5)+(vd*Conjg(Td(2,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(Yd(2,1))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(1,6) = 0._dp 
mat2a(1,6) = mat2a(1,6)+(vd*Conjg(Td(3,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vu*Conjg(Yd(3,1))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)-(g1**2*vd**2)/24._dp
mat2a(2,2) = mat2a(2,2)-(g2**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g1**2*vu**2)/24._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+mq2(2,2)
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g1**2*vL(j1)**2)/24._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(vd**2*Conjg(Yd(j1,2))*Yd(j1,2))/2._dp
End Do 
mat2a(2,3) = 0._dp 
mat2a(2,3) = mat2a(2,3)+mq2(2,3)
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd**2*Conjg(Yd(j1,2))*Yd(j1,3))/2._dp
End Do 
mat2a(2,4) = 0._dp 
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(Td(1,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vu*Conjg(Yd(1,2))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(2,5) = 0._dp 
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(Td(2,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vu*Conjg(Yd(2,2))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(2,6) = 0._dp 
mat2a(2,6) = mat2a(2,6)+(vd*Conjg(Td(3,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(vu*Conjg(Yd(3,2))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)-(g1**2*vd**2)/24._dp
mat2a(3,3) = mat2a(3,3)-(g2**2*vd**2)/8._dp
mat2a(3,3) = mat2a(3,3)+(g1**2*vu**2)/24._dp
mat2a(3,3) = mat2a(3,3)+(g2**2*vu**2)/8._dp
mat2a(3,3) = mat2a(3,3)+mq2(3,3)
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(g1**2*vL(j1)**2)/24._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd**2*Conjg(Yd(j1,3))*Yd(j1,3))/2._dp
End Do 
mat2a(3,4) = 0._dp 
mat2a(3,4) = mat2a(3,4)+(vd*Conjg(Td(1,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yd(1,3))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(3,5) = 0._dp 
mat2a(3,5) = mat2a(3,5)+(vd*Conjg(Td(2,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yd(2,3))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(3,6) = 0._dp 
mat2a(3,6) = mat2a(3,6)+(vd*Conjg(Td(3,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yd(3,3))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(4,4) = 0._dp 
mat2a(4,4) = mat2a(4,4)-(g1**2*vd**2)/12._dp
mat2a(4,4) = mat2a(4,4)+(g1**2*vu**2)/12._dp
mat2a(4,4) = mat2a(4,4)+md2(1,1)
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(g1**2*vL(j1)**2)/12._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd**2*Conjg(Yd(1,j1))*Yd(1,j1))/2._dp
End Do 
mat2a(4,5) = 0._dp 
mat2a(4,5) = mat2a(4,5)+md2(1,2)
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd**2*Conjg(Yd(2,j1))*Yd(1,j1))/2._dp
End Do 
mat2a(4,6) = 0._dp 
mat2a(4,6) = mat2a(4,6)+md2(1,3)
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vd**2*Conjg(Yd(3,j1))*Yd(1,j1))/2._dp
End Do 
mat2a(5,5) = 0._dp 
mat2a(5,5) = mat2a(5,5)-(g1**2*vd**2)/12._dp
mat2a(5,5) = mat2a(5,5)+(g1**2*vu**2)/12._dp
mat2a(5,5) = mat2a(5,5)+md2(2,2)
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(g1**2*vL(j1)**2)/12._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd**2*Conjg(Yd(2,j1))*Yd(2,j1))/2._dp
End Do 
mat2a(5,6) = 0._dp 
mat2a(5,6) = mat2a(5,6)+md2(2,3)
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vd**2*Conjg(Yd(3,j1))*Yd(2,j1))/2._dp
End Do 
mat2a(6,6) = 0._dp 
mat2a(6,6) = mat2a(6,6)-(g1**2*vd**2)/12._dp
mat2a(6,6) = mat2a(6,6)+(g1**2*vu**2)/12._dp
mat2a(6,6) = mat2a(6,6)+md2(3,3)
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)-(g1**2*vL(j1)**2)/12._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(vd**2*Conjg(Yd(3,j1))*Yd(3,j1))/2._dp
End Do 

 
 Do i1=2,6
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = Conjg(mat2a(i2,i1)) 
  End do 
End do 

 
Do i1=1,6
PiSf(i1,:,:) = ZeroC 
p2 = MSd2(i1)
Call Pi1LoopSd(p2,MSd,MSd2,MAh,MAh2,MFu,MFu2,MCha,MCha2,MFd,MFd2,MChi,MChi2,          & 
& MGlu,MGlu2,Mhh,Mhh2,MSu,MSu2,MHpm,MHpm2,MVZ,MVZ2,MVWm,MVWm2,cplAhSdcUSd,               & 
& cplChaFucUSdL,cplChaFucUSdR,cplChiFdcUSdL,cplChiFdcUSdR,cplGluFdcUSdL,cplGluFdcUSdR,   & 
& cplhhSdcUSd,cplHpmSucUSd,cplSdcUSdVG,cplSdcUSdVP,cplSdcUSdVZ,cplSucUSdVWm,             & 
& cplAhAhUSdcUSd,cplhhhhUSdcUSd,cplHpmUSdcHpmcUSd,cplUSdSdcUSdcSd,cplUSdSucUSdcSu,       & 
& cplUSdcUSdVGVG,cplUSdcUSdVPVP,cplUSdcUSdcVWmVWm,cplUSdcUSdVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=6,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
    return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
   Call TerminateProgram 
   return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,6
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses' 
     !Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram
       return 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,6
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1LoopSd(p2,MSd,MSd2,MAh,MAh2,MFu,MFu2,MCha,MCha2,MFd,MFd2,MChi,MChi2,          & 
& MGlu,MGlu2,Mhh,Mhh2,MSu,MSu2,MHpm,MHpm2,MVZ,MVZ2,MVWm,MVWm2,cplAhSdcUSd,               & 
& cplChaFucUSdL,cplChaFucUSdR,cplChiFdcUSdL,cplChiFdcUSdR,cplGluFdcUSdL,cplGluFdcUSdR,   & 
& cplhhSdcUSd,cplHpmSucUSd,cplSdcUSdVG,cplSdcUSdVP,cplSdcUSdVZ,cplSucUSdVWm,             & 
& cplAhAhUSdcUSd,cplhhhhUSdcUSd,cplHpmUSdcHpmcUSd,cplUSdSdcUSdcSd,cplUSdSucUSdcSu,       & 
& cplUSdcUSdVGVG,cplUSdcUSdVPVP,cplUSdcUSdcVWmVWm,cplUSdcUSdVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=6,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
   Call TerminateProgram
   return
  endif 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
    return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,6
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     !Write(*,*) 'generation: ',i1 
     !Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  !Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopSd
 
 
Subroutine Pi1LoopSd(p2,MSd,MSd2,MAh,MAh2,MFu,MFu2,MCha,MCha2,MFd,MFd2,               & 
& MChi,MChi2,MGlu,MGlu2,Mhh,Mhh2,MSu,MSu2,MHpm,MHpm2,MVZ,MVZ2,MVWm,MVWm2,cplAhSdcUSd,    & 
& cplChaFucUSdL,cplChaFucUSdR,cplChiFdcUSdL,cplChiFdcUSdR,cplGluFdcUSdL,cplGluFdcUSdR,   & 
& cplhhSdcUSd,cplHpmSucUSd,cplSdcUSdVG,cplSdcUSdVP,cplSdcUSdVZ,cplSucUSdVWm,             & 
& cplAhAhUSdcUSd,cplhhhhUSdcUSd,cplHpmUSdcHpmcUSd,cplUSdSdcUSdcSd,cplUSdSucUSdcSu,       & 
& cplUSdcUSdVGVG,cplUSdcUSdVPVP,cplUSdcUSdcVWmVWm,cplUSdcUSdVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MSd(6),MSd2(6),MAh(8),MAh2(8),MFu(3),MFu2(3),MCha(5),MCha2(5),MFd(3),MFd2(3),         & 
& MChi(10),MChi2(10),MGlu,MGlu2,Mhh(8),Mhh2(8),MSu(6),MSu2(6),MHpm(8),MHpm2(8),          & 
& MVZ,MVZ2,MVWm,MVWm2

Complex(dp), Intent(in) :: cplAhSdcUSd(8,6,6),cplChaFucUSdL(5,3,6),cplChaFucUSdR(5,3,6),cplChiFdcUSdL(10,3,6),   & 
& cplChiFdcUSdR(10,3,6),cplGluFdcUSdL(3,6),cplGluFdcUSdR(3,6),cplhhSdcUSd(8,6,6),        & 
& cplHpmSucUSd(8,6,6),cplSdcUSdVG(6,6),cplSdcUSdVP(6,6),cplSdcUSdVZ(6,6),cplSucUSdVWm(6,6),& 
& cplAhAhUSdcUSd(8,8,6,6),cplhhhhUSdcUSd(8,8,6,6),cplHpmUSdcHpmcUSd(8,6,8,6),            & 
& cplUSdSdcUSdcSd(6,6,6,6),cplUSdSucUSdcSu(6,6,6,6),cplUSdcUSdVGVG(6,6),cplUSdcUSdVPVP(6,6),& 
& cplUSdcUSdcVWmVWm(6,6),cplUSdcUSdVZVZ(6,6)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(6,6) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(6,6) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Sd, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 8
 B0m2 = B0(p2,MSd2(i1),MAh2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplAhSdcUSd(i2,i1,gO1)
coup2 = Conjg(cplAhSdcUSd(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Fu, Cha 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 5
 G0m2 = Gloop(p2,MFu2(i1),MCha2(i2)) 
B0m2 = -2._dp*MFu(i1)*MCha(i2)*B0(p2,MFu2(i1),MCha2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coupL1 = cplChaFucUSdL(i2,i1,gO1)
coupR1 = cplChaFucUSdR(i2,i1,gO1)
coupL2 =  Conjg(cplChaFucUSdL(i2,i1,gO2))
coupR2 =  Conjg(cplChaFucUSdR(i2,i1,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Fd, Chi 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 10
 G0m2 = Gloop(p2,MFd2(i1),MChi2(i2)) 
B0m2 = -2._dp*MFd(i1)*MChi(i2)*B0(p2,MFd2(i1),MChi2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coupL1 = cplChiFdcUSdL(i2,i1,gO1)
coupR1 = cplChiFdcUSdR(i2,i1,gO1)
coupL2 =  Conjg(cplChiFdcUSdL(i2,i1,gO2))
coupR2 =  Conjg(cplChiFdcUSdR(i2,i1,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Glu, Fd 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 G0m2 = Gloop(p2,MGlu2,MFd2(i2)) 
B0m2 = -2._dp*MGlu*MFd(i2)*B0(p2,MGlu2,MFd2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coupL1 = cplGluFdcUSdL(i2,gO1)
coupR1 = cplGluFdcUSdR(i2,gO1)
coupL2 =  Conjg(cplGluFdcUSdL(i2,gO2))
coupR2 =  Conjg(cplGluFdcUSdR(i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +4._dp/3._dp* SumI  
    End Do 
 !------------------------ 
! Sd, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 8
 B0m2 = B0(p2,MSd2(i1),Mhh2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplhhSdcUSd(i2,i1,gO1)
coup2 = Conjg(cplhhSdcUSd(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Su, Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 8
 B0m2 = B0(p2,MSu2(i1),MHpm2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplHpmSucUSd(i2,i1,gO1)
coup2 = Conjg(cplHpmSucUSd(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VG, Sd 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSd2(i2),0._dp) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSdcUSdVG(i2,gO1)
coup2 =  Conjg(cplSdcUSdVG(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +4._dp/3._dp* SumI  
    End Do 
 !------------------------ 
! VP, Sd 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSd2(i2),0._dp) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSdcUSdVP(i2,gO1)
coup2 =  Conjg(cplSdcUSdVP(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Sd 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSd2(i2),MVZ2) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSdcUSdVZ(i2,gO1)
coup2 =  Conjg(cplSdcUSdVZ(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VWm, Su 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSu2(i2),MVWm2) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSucUSdVWm(i2,gO1)
coup2 =  Conjg(cplSucUSdVWm(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MAh2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplAhAhUSdcUSd(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(Mhh2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplhhhhUSdcUSd(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MHpm2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplHpmUSdcHpmcUSd(i1,gO2,i1,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSd2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSdSdcUSdcSd(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSu2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSdSucUSdcSu(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! VG, VG 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVWm2) + 0.25_dp*RXi*A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSdcUSdcVWmVWm(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVZ2) + 0.25_dp*RXi*A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSdcUSdVZVZ(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 6
  Do gO1 = gO2+1, 6
     res(gO1,gO2) = Conjg(res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopSd 
 
Subroutine OneLoopSu(g1,g2,lam,Yv,Yu,Tu,mq2,mu2,vd,vu,vL,vR,MSu,MSu2,MAh,             & 
& MAh2,MFu,MFu2,MChi,MChi2,MCha,MCha2,MFd,MFd2,MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,           & 
& MSd,MSd2,MVWm,MVWm2,MVZ,MVZ2,cplAhSucUSu,cplChiFucUSuL,cplChiFucUSuR,cplcChaFdcUSuL,   & 
& cplcChaFdcUSuR,cplGluFucUSuL,cplGluFucUSuR,cplhhSucUSu,cplSdcHpmcUSu,cplSdcUSucVWm,    & 
& cplSucUSuVG,cplSucUSuVP,cplSucUSuVZ,cplAhAhUSucUSu,cplhhhhUSucUSu,cplHpmUSucHpmcUSu,   & 
& cplSdUSucSdcUSu,cplUSuSucUSucSu,cplUSucUSuVGVG,cplUSucUSuVPVP,cplUSucUSucVWmVWm,       & 
& cplUSucUSuVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MSu(6),MSu2(6),MAh(8),MAh2(8),MFu(3),MFu2(3),MChi(10),MChi2(10),MCha(5),              & 
& MCha2(5),MFd(3),MFd2(3),MGlu,MGlu2,Mhh(8),Mhh2(8),MHpm(8),MHpm2(8),MSd(6),             & 
& MSd2(6),MVWm,MVWm2,MVZ,MVZ2

Real(dp), Intent(in) :: g1,g2,vd,vu,vL(3),vR(3)

Complex(dp), Intent(in) :: lam(3),Yv(3,3),Yu(3,3),Tu(3,3),mq2(3,3),mu2(3,3)

Complex(dp), Intent(in) :: cplAhSucUSu(8,6,6),cplChiFucUSuL(10,3,6),cplChiFucUSuR(10,3,6),cplcChaFdcUSuL(5,3,6), & 
& cplcChaFdcUSuR(5,3,6),cplGluFucUSuL(3,6),cplGluFucUSuR(3,6),cplhhSucUSu(8,6,6),        & 
& cplSdcHpmcUSu(6,8,6),cplSdcUSucVWm(6,6),cplSucUSuVG(6,6),cplSucUSuVP(6,6),             & 
& cplSucUSuVZ(6,6),cplAhAhUSucUSu(8,8,6,6),cplhhhhUSucUSu(8,8,6,6),cplHpmUSucHpmcUSu(8,6,8,6),& 
& cplSdUSucSdcUSu(6,6,6,6),cplUSuSucUSucSu(6,6,6,6),cplUSucUSuVGVG(6,6),cplUSucUSuVPVP(6,6),& 
& cplUSucUSucVWmVWm(6,6),cplUSucUSuVZVZ(6,6)

Complex(dp) :: mat2a(6,6), mat2(6,6),  PiSf(6,6,6)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(6), test_m2(6),p2, test(6) 
Real(dp), Intent(out) :: mass(6), mass2(6) 
Complex(dp), Intent(out) ::  RS(6,6) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopSu'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)-(g1**2*vd**2)/24._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g1**2*vu**2)/24._dp
mat2a(1,1) = mat2a(1,1)-(g2**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)+mq2(1,1)
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)-(g1**2*vL(j1)**2)/24._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(vu**2*Conjg(Yu(j1,1))*Yu(j1,1))/2._dp
End Do 
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+mq2(1,2)
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(vu**2*Conjg(Yu(j1,1))*Yu(j1,2))/2._dp
End Do 
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)+mq2(1,3)
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu**2*Conjg(Yu(j1,1))*Yu(j1,3))/2._dp
End Do 
mat2a(1,4) = 0._dp 
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(Tu(1,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vd*Conjg(Yu(1,1))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(Conjg(Yu(1,1))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(1,5) = 0._dp 
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(Tu(2,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vd*Conjg(Yu(2,1))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(Conjg(Yu(2,1))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(1,6) = 0._dp 
mat2a(1,6) = mat2a(1,6)+(vu*Conjg(Tu(3,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vd*Conjg(Yu(3,1))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)+(Conjg(Yu(3,1))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)-(g1**2*vd**2)/24._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g1**2*vu**2)/24._dp
mat2a(2,2) = mat2a(2,2)-(g2**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+mq2(2,2)
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g1**2*vL(j1)**2)/24._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(vu**2*Conjg(Yu(j1,2))*Yu(j1,2))/2._dp
End Do 
mat2a(2,3) = 0._dp 
mat2a(2,3) = mat2a(2,3)+mq2(2,3)
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vu**2*Conjg(Yu(j1,2))*Yu(j1,3))/2._dp
End Do 
mat2a(2,4) = 0._dp 
mat2a(2,4) = mat2a(2,4)+(vu*Conjg(Tu(1,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(Yu(1,2))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yu(1,2))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(2,5) = 0._dp 
mat2a(2,5) = mat2a(2,5)+(vu*Conjg(Tu(2,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(Yu(2,2))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yu(2,2))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(2,6) = 0._dp 
mat2a(2,6) = mat2a(2,6)+(vu*Conjg(Tu(3,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(vd*Conjg(Yu(3,2))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(Yu(3,2))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)-(g1**2*vd**2)/24._dp
mat2a(3,3) = mat2a(3,3)+(g2**2*vd**2)/8._dp
mat2a(3,3) = mat2a(3,3)+(g1**2*vu**2)/24._dp
mat2a(3,3) = mat2a(3,3)-(g2**2*vu**2)/8._dp
mat2a(3,3) = mat2a(3,3)+mq2(3,3)
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(g1**2*vL(j1)**2)/24._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu**2*Conjg(Yu(j1,3))*Yu(j1,3))/2._dp
End Do 
mat2a(3,4) = 0._dp 
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Tu(1,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(Yu(1,3))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Yu(1,3))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(3,5) = 0._dp 
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Tu(2,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(Yu(2,3))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Yu(2,3))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(3,6) = 0._dp 
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Tu(3,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(Yu(3,3))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yu(3,3))*vL(j1)*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(4,4) = 0._dp 
mat2a(4,4) = mat2a(4,4)+(g1**2*vd**2)/6._dp
mat2a(4,4) = mat2a(4,4)-(g1**2*vu**2)/6._dp
mat2a(4,4) = mat2a(4,4)+mu2(1,1)
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(g1**2*vL(j1)**2)/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu**2*Conjg(Yu(1,j1))*Yu(1,j1))/2._dp
End Do 
mat2a(4,5) = 0._dp 
mat2a(4,5) = mat2a(4,5)+mu2(1,2)
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(Yu(2,j1))*Yu(1,j1))/2._dp
End Do 
mat2a(4,6) = 0._dp 
mat2a(4,6) = mat2a(4,6)+mu2(1,3)
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu**2*Conjg(Yu(3,j1))*Yu(1,j1))/2._dp
End Do 
mat2a(5,5) = 0._dp 
mat2a(5,5) = mat2a(5,5)+(g1**2*vd**2)/6._dp
mat2a(5,5) = mat2a(5,5)-(g1**2*vu**2)/6._dp
mat2a(5,5) = mat2a(5,5)+mu2(2,2)
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(g1**2*vL(j1)**2)/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu**2*Conjg(Yu(2,j1))*Yu(2,j1))/2._dp
End Do 
mat2a(5,6) = 0._dp 
mat2a(5,6) = mat2a(5,6)+mu2(2,3)
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu**2*Conjg(Yu(3,j1))*Yu(2,j1))/2._dp
End Do 
mat2a(6,6) = 0._dp 
mat2a(6,6) = mat2a(6,6)+(g1**2*vd**2)/6._dp
mat2a(6,6) = mat2a(6,6)-(g1**2*vu**2)/6._dp
mat2a(6,6) = mat2a(6,6)+mu2(3,3)
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(g1**2*vL(j1)**2)/6._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(vu**2*Conjg(Yu(3,j1))*Yu(3,j1))/2._dp
End Do 

 
 Do i1=2,6
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = Conjg(mat2a(i2,i1)) 
  End do 
End do 

 
Do i1=1,6
PiSf(i1,:,:) = ZeroC 
p2 = MSu2(i1)
Call Pi1LoopSu(p2,MSu,MSu2,MAh,MAh2,MFu,MFu2,MChi,MChi2,MCha,MCha2,MFd,               & 
& MFd2,MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MVWm,MVWm2,MVZ,MVZ2,cplAhSucUSu,          & 
& cplChiFucUSuL,cplChiFucUSuR,cplcChaFdcUSuL,cplcChaFdcUSuR,cplGluFucUSuL,               & 
& cplGluFucUSuR,cplhhSucUSu,cplSdcHpmcUSu,cplSdcUSucVWm,cplSucUSuVG,cplSucUSuVP,         & 
& cplSucUSuVZ,cplAhAhUSucUSu,cplhhhhUSucUSu,cplHpmUSucHpmcUSu,cplSdUSucSdcUSu,           & 
& cplUSuSucUSucSu,cplUSucUSuVGVG,cplUSucUSuVPVP,cplUSucUSucVWmVWm,cplUSucUSuVZVZ,        & 
& kont,PiSf(i1,:,:))

End Do 
Do i1=6,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
    return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
    return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,6
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses' 
     !Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram 
   return
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,6
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1LoopSu(p2,MSu,MSu2,MAh,MAh2,MFu,MFu2,MChi,MChi2,MCha,MCha2,MFd,               & 
& MFd2,MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MVWm,MVWm2,MVZ,MVZ2,cplAhSucUSu,          & 
& cplChiFucUSuL,cplChiFucUSuR,cplcChaFdcUSuL,cplcChaFdcUSuR,cplGluFucUSuL,               & 
& cplGluFucUSuR,cplhhSucUSu,cplSdcHpmcUSu,cplSdcUSucVWm,cplSucUSuVG,cplSucUSuVP,         & 
& cplSucUSuVZ,cplAhAhUSucUSu,cplhhhhUSucUSu,cplHpmUSucHpmcUSu,cplSdUSucSdcUSu,           & 
& cplUSuSucUSucSu,cplUSucUSuVGVG,cplUSucUSuVPVP,cplUSucUSucVWmVWm,cplUSucUSuVZVZ,        & 
& kont,PiSf(i1,:,:))

End Do 
Do i1=6,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif  
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return 
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,6
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     !Write(*,*) 'generation: ',i1 
     !Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopSu
 
 
Subroutine Pi1LoopSu(p2,MSu,MSu2,MAh,MAh2,MFu,MFu2,MChi,MChi2,MCha,MCha2,             & 
& MFd,MFd2,MGlu,MGlu2,Mhh,Mhh2,MHpm,MHpm2,MSd,MSd2,MVWm,MVWm2,MVZ,MVZ2,cplAhSucUSu,      & 
& cplChiFucUSuL,cplChiFucUSuR,cplcChaFdcUSuL,cplcChaFdcUSuR,cplGluFucUSuL,               & 
& cplGluFucUSuR,cplhhSucUSu,cplSdcHpmcUSu,cplSdcUSucVWm,cplSucUSuVG,cplSucUSuVP,         & 
& cplSucUSuVZ,cplAhAhUSucUSu,cplhhhhUSucUSu,cplHpmUSucHpmcUSu,cplSdUSucSdcUSu,           & 
& cplUSuSucUSucSu,cplUSucUSuVGVG,cplUSucUSuVPVP,cplUSucUSucVWmVWm,cplUSucUSuVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MSu(6),MSu2(6),MAh(8),MAh2(8),MFu(3),MFu2(3),MChi(10),MChi2(10),MCha(5),              & 
& MCha2(5),MFd(3),MFd2(3),MGlu,MGlu2,Mhh(8),Mhh2(8),MHpm(8),MHpm2(8),MSd(6),             & 
& MSd2(6),MVWm,MVWm2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhSucUSu(8,6,6),cplChiFucUSuL(10,3,6),cplChiFucUSuR(10,3,6),cplcChaFdcUSuL(5,3,6), & 
& cplcChaFdcUSuR(5,3,6),cplGluFucUSuL(3,6),cplGluFucUSuR(3,6),cplhhSucUSu(8,6,6),        & 
& cplSdcHpmcUSu(6,8,6),cplSdcUSucVWm(6,6),cplSucUSuVG(6,6),cplSucUSuVP(6,6),             & 
& cplSucUSuVZ(6,6),cplAhAhUSucUSu(8,8,6,6),cplhhhhUSucUSu(8,8,6,6),cplHpmUSucHpmcUSu(8,6,8,6),& 
& cplSdUSucSdcUSu(6,6,6,6),cplUSuSucUSucSu(6,6,6,6),cplUSucUSuVGVG(6,6),cplUSucUSuVPVP(6,6),& 
& cplUSucUSucVWmVWm(6,6),cplUSucUSuVZVZ(6,6)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(6,6) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(6,6) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Su, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 8
 B0m2 = B0(p2,MSu2(i1),MAh2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplAhSucUSu(i2,i1,gO1)
coup2 = Conjg(cplAhSucUSu(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Fu, Chi 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 10
 G0m2 = Gloop(p2,MFu2(i1),MChi2(i2)) 
B0m2 = -2._dp*MFu(i1)*MChi(i2)*B0(p2,MFu2(i1),MChi2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coupL1 = cplChiFucUSuL(i2,i1,gO1)
coupR1 = cplChiFucUSuR(i2,i1,gO1)
coupL2 =  Conjg(cplChiFucUSuL(i2,i1,gO2))
coupR2 =  Conjg(cplChiFucUSuR(i2,i1,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Cha], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 5
       Do i2 = 1, 3
 G0m2 = Gloop(p2,MCha2(i1),MFd2(i2)) 
B0m2 = -2._dp*MCha(i1)*MFd(i2)*B0(p2,MCha2(i1),MFd2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coupL1 = cplcChaFdcUSuL(i1,i2,gO1)
coupR1 = cplcChaFdcUSuR(i1,i2,gO1)
coupL2 =  Conjg(cplcChaFdcUSuL(i1,i2,gO2))
coupR2 =  Conjg(cplcChaFdcUSuR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Glu, Fu 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 3
 G0m2 = Gloop(p2,MGlu2,MFu2(i2)) 
B0m2 = -2._dp*MGlu*MFu(i2)*B0(p2,MGlu2,MFu2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coupL1 = cplGluFucUSuL(i2,gO1)
coupR1 = cplGluFucUSuR(i2,gO1)
coupL2 =  Conjg(cplGluFucUSuL(i2,gO2))
coupR2 =  Conjg(cplGluFucUSuR(i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +4._dp/3._dp* SumI  
    End Do 
 !------------------------ 
! Su, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 8
 B0m2 = B0(p2,MSu2(i1),Mhh2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplhhSucUSu(i2,i1,gO1)
coup2 = Conjg(cplhhSucUSu(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[Hpm], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 6
 B0m2 = B0(p2,MHpm2(i1),MSd2(i2)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSdcHpmcUSu(i2,i1,gO1)
coup2 = Conjg(cplSdcHpmcUSu(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Sd 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSd2(i2),MVWm2) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSdcUSucVWm(i2,gO1)
coup2 =  Conjg(cplSdcUSucVWm(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VG, Su 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSu2(i2),0._dp) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSucUSuVG(i2,gO1)
coup2 =  Conjg(cplSucUSuVG(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +4._dp/3._dp* SumI  
    End Do 
 !------------------------ 
! VP, Su 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSu2(i2),0._dp) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSucUSuVP(i2,gO1)
coup2 =  Conjg(cplSucUSuVP(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Su 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 6
 F0m2 = FloopRXi(p2,MSu2(i2),MVZ2) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSucUSuVZ(i2,gO1)
coup2 =  Conjg(cplSucUSuVZ(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MAh2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplAhAhUSucUSu(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(Mhh2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplhhhhUSucUSu(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MHpm2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplHpmUSucHpmcUSu(i1,gO2,i1,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSd2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplSdUSucSdcUSu(i1,gO2,i1,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSu2(i1)) 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSuSucUSucSu(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! VG, VG 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVWm2) + 0.25_dp*RXi*A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSucUSucVWmVWm(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVZ2) + 0.25_dp*RXi*A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 6
  Do gO2 = gO1, 6
coup1 = cplUSucUSuVZVZ(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 6
  Do gO1 = gO2+1, 6
     res(gO1,gO2) = Conjg(res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopSu 
 
Subroutine OneLoophh(g1,g2,lam,Tlam,Yv,Tv,kap,Tk,ml2,mlHd2,mHd2,mHu2,mv2,             & 
& vd,vu,vL,vR,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,             & 
& MFu2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhAhUhh,cplAhUhhhh,cplAhUhhVZ,         & 
& cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,cplcFdFdUhhR,   & 
& cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,cplUhhhhhh,        & 
& cplUhhHpmcHpm,cplUhhHpmcVWm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,cplUhhVZVZ,          & 
& cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpmcHpm,cplUhhUhhSdcSd,cplUhhUhhSucSu,            & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MAh(8),MAh2(8),Mhh(8),Mhh2(8),MVZ,MVZ2,MCha(5),MCha2(5),MChi(10),MChi2(10),           & 
& MFd(3),MFd2(3),MFu(3),MFu2(3),MHpm(8),MHpm2(8),MVWm,MVWm2,MSd(6),MSd2(6),              & 
& MSu(6),MSu2(6)

Real(dp), Intent(in) :: g1,g2,mlHd2(3),mHd2,mHu2,vd,vu,vL(3),vR(3)

Complex(dp), Intent(in) :: lam(3),Tlam(3),Yv(3,3),Tv(3,3),kap(3,3,3),Tk(3,3,3),ml2(3,3),mv2(3,3)

Complex(dp), Intent(in) :: cplAhAhUhh(8,8,8),cplAhUhhhh(8,8,8),cplAhUhhVZ(8,8),cplcChaChaUhhL(5,5,8),            & 
& cplcChaChaUhhR(5,5,8),cplChiChiUhhL(10,10,8),cplChiChiUhhR(10,10,8),cplcFdFdUhhL(3,3,8),& 
& cplcFdFdUhhR(3,3,8),cplcFuFuUhhL(3,3,8),cplcFuFuUhhR(3,3,8),cplcgWmgWmUhh(8),          & 
& cplcgWpCgWpCUhh(8),cplcgZgZUhh(8),cplUhhhhhh(8,8,8),cplUhhHpmcHpm(8,8,8),              & 
& cplUhhHpmcVWm(8,8),cplUhhSdcSd(8,6,6),cplUhhSucSu(8,6,6),cplUhhcVWmVWm(8),             & 
& cplUhhVZVZ(8),cplAhAhUhhUhh(8,8,8,8),cplUhhUhhhhhh(8,8,8,8),cplUhhUhhHpmcHpm(8,8,8,8), & 
& cplUhhUhhSdcSd(8,8,6,6),cplUhhUhhSucSu(8,8,6,6),cplUhhUhhcVWmVWm(8,8),cplUhhUhhVZVZ(8,8)

Complex(dp) :: mat2a(8,8), mat2(8,8),  PiSf(8,8,8)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(8), test_m2(8),p2, test(8) 
Real(dp), Intent(out) :: mass(8), mass2(8) 
Complex(dp), Intent(out) ::  RS(8,8) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoophh'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+mHd2
mat2a(1,1) = mat2a(1,1)+(3*g1**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(3*g2**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)-(g1**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)-(g2**2*vu**2)/8._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,1) = mat2a(1,1)+(Conjg(lam(j2))*vR(j1)*vR(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(vu**2*Conjg(lam(j1))*lam(j1))/2._dp
End Do 
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)-(g1**2*vd*vu)/4._dp
mat2a(1,2) = mat2a(1,2)-(g2**2*vd*vu)/4._dp
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(Tlam(j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+vd*vu*Conjg(lam(j1))*lam(j1)
End Do 
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(vR(j1)*Tlam(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(vu*Conjg(lam(j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(vu*Conjg(Yv(j2,j1))*vL(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*lam(j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*lam(j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*lam(j1))/12._dp
End Do 
End Do 
End Do 
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(Tlam(1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(lam(j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vd*Conjg(lam(j1))*vR(j1)*lam(1))/2._dp
End Do 
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vd*Conjg(lam(1))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(lam(1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(lam(j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(lam(j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(lam(j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(lam(j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(lam(j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(lam(j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*lam(1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(kap(1,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(kap(1,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(kap(j1,1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(kap(j1,j2,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(kap(j2,1,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(kap(j2,j1,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
mat2a(1,3) = mat2a(1,3)-(vu*Tlam(1))/(2._dp*sqrt(2._dp))
mat2a(1,4) = 0._dp 
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(Tlam(2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(lam(j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vd*Conjg(lam(j1))*vR(j1)*lam(2))/2._dp
End Do 
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vd*Conjg(lam(2))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(lam(2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(lam(j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(lam(j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(lam(j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(lam(j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(lam(j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(lam(j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*lam(2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(kap(2,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(kap(2,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(kap(j1,2,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(kap(j1,j2,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(kap(j2,2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(kap(j2,j1,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
mat2a(1,4) = mat2a(1,4)-(vu*Tlam(2))/(2._dp*sqrt(2._dp))
mat2a(1,5) = 0._dp 
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(Tlam(3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(lam(j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vd*Conjg(lam(j1))*vR(j1)*lam(3))/2._dp
End Do 
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vd*Conjg(lam(3))*vR(j1)*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(lam(3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(lam(j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(lam(j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(lam(j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(lam(j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(lam(j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(lam(j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*lam(3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(kap(3,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(kap(3,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(kap(j1,3,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(kap(j1,j2,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(kap(j2,3,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(kap(j2,j1,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
mat2a(1,5) = mat2a(1,5)-(vu*Tlam(3))/(2._dp*sqrt(2._dp))
mat2a(1,6) = 0._dp 
mat2a(1,6) = mat2a(1,6)+mlHd2(1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,6) = mat2a(1,6)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,6) = mat2a(1,6)-(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vu**2*Conjg(lam(j1))*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vu**2*Conjg(Yv(1,j1))*lam(j1))/4._dp
End Do 
mat2a(1,6) = mat2a(1,6)+(g1**2*vd*vL(1))/4._dp
mat2a(1,6) = mat2a(1,6)+(g2**2*vd*vL(1))/4._dp
mat2a(1,7) = 0._dp 
mat2a(1,7) = mat2a(1,7)+mlHd2(2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,7) = mat2a(1,7)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,7) = mat2a(1,7)-(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,7) = mat2a(1,7)-(vu**2*Conjg(lam(j1))*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(1,7) = mat2a(1,7)-(vu**2*Conjg(Yv(2,j1))*lam(j1))/4._dp
End Do 
mat2a(1,7) = mat2a(1,7)+(g1**2*vd*vL(2))/4._dp
mat2a(1,7) = mat2a(1,7)+(g2**2*vd*vL(2))/4._dp
mat2a(1,8) = 0._dp 
mat2a(1,8) = mat2a(1,8)+mlHd2(3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,8) = mat2a(1,8)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,8) = mat2a(1,8)-(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,8) = mat2a(1,8)-(vu**2*Conjg(lam(j1))*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(1,8) = mat2a(1,8)-(vu**2*Conjg(Yv(3,j1))*lam(j1))/4._dp
End Do 
mat2a(1,8) = mat2a(1,8)+(g1**2*vd*vL(3))/4._dp
mat2a(1,8) = mat2a(1,8)+(g2**2*vd*vL(3))/4._dp
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+mHu2
mat2a(2,2) = mat2a(2,2)-(g1**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)-(g2**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(3*g1**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(3*g2**2*vu**2)/8._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(lam(j2))*vR(j1)*vR(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(vd**2*Conjg(lam(j1))*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(vd*Conjg(lam(j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(vd*Conjg(Yv(j2,j1))*vL(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(Yv(j1,j3))*vR(j2)*vR(j3)*Yv(j1,j2))/2._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(Yv(j3,j1))*vL(j2)*vL(j3)*Yv(j2,j1))/2._dp
End Do 
End Do 
End Do 
mat2a(2,3) = 0._dp 
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(Tlam(1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Tv(j1,1))*vL(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vu*Conjg(lam(j1))*vR(j1)*lam(1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vu*Conjg(lam(1))*vR(j1)*lam(j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vL(j1)*Tv(j1,1))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vu*Conjg(Yv(j1,j2))*vR(j2)*Yv(j1,1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vu*Conjg(Yv(j1,1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(lam(j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(lam(j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(lam(j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(lam(j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(lam(j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(lam(j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(kap(1,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(kap(1,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(kap(j1,1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(kap(j1,j2,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(kap(j2,1,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(kap(j2,j1,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(kap(1,j1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(kap(1,j3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(kap(j1,1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(kap(j1,j3,1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(kap(j3,1,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(kap(j3,j1,1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,3) = mat2a(2,3)-(vd*Tlam(1))/(2._dp*sqrt(2._dp))
mat2a(2,4) = 0._dp 
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(Tlam(2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Tv(j1,2))*vL(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vu*Conjg(lam(j1))*vR(j1)*lam(2))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vu*Conjg(lam(2))*vR(j1)*lam(j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vL(j1)*Tv(j1,2))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vu*Conjg(Yv(j1,j2))*vR(j2)*Yv(j1,2))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vu*Conjg(Yv(j1,2))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(lam(j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(lam(j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(lam(j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(lam(j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(lam(j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(lam(j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(kap(2,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(kap(2,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(kap(j1,2,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(kap(j1,j2,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(kap(j2,2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(kap(j2,j1,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(kap(2,j1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(kap(2,j3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(kap(j1,2,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(kap(j1,j3,2))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(kap(j3,2,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(kap(j3,j1,2))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
End Do 
mat2a(2,4) = mat2a(2,4)-(vd*Tlam(2))/(2._dp*sqrt(2._dp))
mat2a(2,5) = 0._dp 
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(Tlam(3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Tv(j1,3))*vL(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vu*Conjg(lam(j1))*vR(j1)*lam(3))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vu*Conjg(lam(3))*vR(j1)*lam(j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vL(j1)*Tv(j1,3))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vu*Conjg(Yv(j1,j2))*vR(j2)*Yv(j1,3))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vu*Conjg(Yv(j1,3))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(lam(j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(lam(j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(lam(j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(lam(j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(lam(j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(lam(j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(kap(3,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(kap(3,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(kap(j1,3,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(kap(j1,j2,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(kap(j2,3,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(kap(j2,j1,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(kap(3,j1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(kap(3,j3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(kap(j1,3,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(kap(j1,j3,3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(kap(j3,3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(kap(j3,j1,3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
End Do 
mat2a(2,5) = mat2a(2,5)-(vd*Tlam(3))/(2._dp*sqrt(2._dp))
mat2a(2,6) = 0._dp 
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(Tv(1,j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(vd*vu*Conjg(lam(j1))*Yv(1,j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(vd*vu*Conjg(Yv(1,j1))*lam(j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(vR(j1)*Tv(1,j1))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(vu*Conjg(Yv(j2,j1))*vL(j2)*Yv(1,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(vu*Conjg(Yv(1,j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(Yv(1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(Yv(1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)+(Conjg(Yv(1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,6) = mat2a(2,6)-(g1**2*vu*vL(1))/4._dp
mat2a(2,6) = mat2a(2,6)-(g2**2*vu*vL(1))/4._dp
mat2a(2,7) = 0._dp 
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(Tv(2,j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(vd*vu*Conjg(lam(j1))*Yv(2,j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(vd*vu*Conjg(Yv(2,j1))*lam(j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(vR(j1)*Tv(2,j1))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(vu*Conjg(Yv(j2,j1))*vL(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(vu*Conjg(Yv(2,j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(Yv(2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(Yv(2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)+(Conjg(Yv(2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,7) = mat2a(2,7)-(g1**2*vu*vL(2))/4._dp
mat2a(2,7) = mat2a(2,7)-(g2**2*vu*vL(2))/4._dp
mat2a(2,8) = 0._dp 
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(Tv(3,j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(vd*vu*Conjg(lam(j1))*Yv(3,j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(vd*vu*Conjg(Yv(3,j1))*lam(j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(vR(j1)*Tv(3,j1))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(vu*Conjg(Yv(j2,j1))*vL(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(vu*Conjg(Yv(3,j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(Yv(3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(Yv(3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)+(Conjg(Yv(3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,8) = mat2a(2,8)-(g1**2*vu*vL(3))/4._dp
mat2a(2,8) = mat2a(2,8)-(g2**2*vu*vL(3))/4._dp
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)+mv2(1,1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(Yv(j2,1))*vL(j1)*vL(j2)*Yv(j1,1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(Tk(1,1,j1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(Tk(1,j1,1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(Tk(j1,1,1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu**2*Conjg(Yv(j1,1))*Yv(j1,1))/2._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*Conjg(lam(1))*vL(j1)*Yv(j1,1))/2._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*vu*Conjg(lam(j1))*kap(1,1,j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*vu*Conjg(lam(j1))*kap(1,j1,1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*vu*Conjg(lam(j1))*kap(j1,1,1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*Conjg(Yv(j1,1))*vL(j1)*lam(1))/2._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*vu*Conjg(kap(1,1,j1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*vu*Conjg(kap(1,j1,1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*vu*Conjg(kap(j1,1,1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vR(j1)*Tk(1,1,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vR(j1)*Tk(1,j1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vR(j1)*Tk(j1,1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu*Conjg(kap(1,1,j1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu*Conjg(kap(1,j1,1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu*Conjg(kap(j1,1,1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,1,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,j1,1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,1,1))/6._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
mat2a(3,3) = mat2a(3,3)+(vd**2*Conjg(lam(1))*lam(1))/2._dp
mat2a(3,3) = mat2a(3,3)+(vu**2*Conjg(lam(1))*lam(1))/2._dp
mat2a(3,4) = 0._dp 
mat2a(3,4) = mat2a(3,4)+mv2(1,2)/2._dp
mat2a(3,4) = mat2a(3,4)+mv2(2,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Yv(j2,2))*vL(j1)*vL(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Yv(j2,1))*vL(j1)*vL(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Tk(1,2,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Tk(1,j1,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Tk(2,1,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Tk(2,j1,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Tk(j1,1,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Tk(j1,2,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(Yv(j1,2))*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(lam(2))*vL(j1)*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(Yv(j1,1))*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(lam(1))*vL(j1)*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(lam(j1))*kap(1,2,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(lam(j1))*kap(1,j1,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(lam(j1))*kap(2,1,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(lam(j1))*kap(2,j1,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(lam(j1))*kap(j1,1,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(lam(j1))*kap(j1,2,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(Yv(j1,2))*vL(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(Yv(j1,1))*vL(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(kap(1,2,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(kap(1,j1,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(kap(2,1,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(kap(2,j1,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(kap(j1,1,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*vu*Conjg(kap(j1,2,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vR(j1)*Tk(1,2,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vR(j1)*Tk(1,j1,2))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vR(j1)*Tk(2,1,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vR(j1)*Tk(2,j1,1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vR(j1)*Tk(j1,1,2))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vR(j1)*Tk(j1,2,1))/(6._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(kap(1,2,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(kap(1,j1,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(kap(2,1,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(kap(2,j1,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(kap(j1,1,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(kap(j1,2,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,2,1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
mat2a(3,4) = mat2a(3,4)+(vd**2*Conjg(lam(2))*lam(1))/4._dp
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(lam(2))*lam(1))/4._dp
mat2a(3,4) = mat2a(3,4)+(vd**2*Conjg(lam(1))*lam(2))/4._dp
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(lam(1))*lam(2))/4._dp
mat2a(3,5) = 0._dp 
mat2a(3,5) = mat2a(3,5)+mv2(1,3)/2._dp
mat2a(3,5) = mat2a(3,5)+mv2(3,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Yv(j2,3))*vL(j1)*vL(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Yv(j2,1))*vL(j1)*vL(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Tk(1,3,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Tk(1,j1,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Tk(3,1,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Tk(3,j1,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Tk(j1,1,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Tk(j1,3,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(Yv(j1,3))*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(lam(3))*vL(j1)*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(Yv(j1,1))*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(lam(1))*vL(j1)*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(lam(j1))*kap(1,3,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(lam(j1))*kap(1,j1,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(lam(j1))*kap(3,1,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(lam(j1))*kap(3,j1,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(lam(j1))*kap(j1,1,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(lam(j1))*kap(j1,3,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(Yv(j1,3))*vL(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(Yv(j1,1))*vL(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(kap(1,3,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(kap(1,j1,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(kap(3,1,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(kap(3,j1,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(kap(j1,1,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*vu*Conjg(kap(j1,3,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vR(j1)*Tk(1,3,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vR(j1)*Tk(1,j1,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vR(j1)*Tk(3,1,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vR(j1)*Tk(3,j1,1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vR(j1)*Tk(j1,1,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vR(j1)*Tk(j1,3,1))/(6._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(kap(1,3,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(kap(1,j1,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(kap(3,1,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(kap(3,j1,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(kap(j1,1,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(kap(j1,3,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,3,1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,3,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
mat2a(3,5) = mat2a(3,5)+(vd**2*Conjg(lam(3))*lam(1))/4._dp
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(lam(3))*lam(1))/4._dp
mat2a(3,5) = mat2a(3,5)+(vd**2*Conjg(lam(1))*lam(3))/4._dp
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(lam(1))*lam(3))/4._dp
mat2a(3,6) = 0._dp 
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Tv(1,1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yv(1,j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(lam(j1))*vR(j1)*Yv(1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(lam(1))*vR(j1)*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(Yv(1,j1))*vR(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(Yv(1,1))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(1,1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(kap(1,j1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(kap(1,j2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(kap(j1,1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(kap(j1,j2,1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(kap(j2,1,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(kap(j2,j1,1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yv(1,1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
mat2a(3,6) = mat2a(3,6)+(vu*Tv(1,1))/(2._dp*sqrt(2._dp))
mat2a(3,7) = 0._dp 
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Tv(2,1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,7) = mat2a(3,7)+(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,7) = mat2a(3,7)+(Conjg(Yv(2,j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vd*Conjg(lam(j1))*vR(j1)*Yv(2,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vd*Conjg(lam(1))*vR(j1)*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vd*Conjg(Yv(2,j1))*vR(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vd*Conjg(Yv(2,1))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(2,1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(kap(1,j1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(kap(1,j2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(kap(j1,1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(kap(j1,j2,1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(kap(j2,1,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(kap(j2,j1,1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(Conjg(Yv(2,1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
mat2a(3,7) = mat2a(3,7)+(vu*Tv(2,1))/(2._dp*sqrt(2._dp))
mat2a(3,8) = 0._dp 
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Tv(3,1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,8) = mat2a(3,8)+(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,8) = mat2a(3,8)+(Conjg(Yv(3,j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vd*Conjg(lam(j1))*vR(j1)*Yv(3,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vd*Conjg(lam(1))*vR(j1)*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vd*Conjg(Yv(3,j1))*vR(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vd*Conjg(Yv(3,1))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(3,1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(kap(1,j1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(kap(1,j2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(kap(j1,1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(kap(j1,j2,1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(kap(j2,1,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(kap(j2,j1,1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(Conjg(Yv(3,1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
mat2a(3,8) = mat2a(3,8)+(vu*Tv(3,1))/(2._dp*sqrt(2._dp))
mat2a(4,4) = 0._dp 
mat2a(4,4) = mat2a(4,4)+mv2(2,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(Yv(j2,2))*vL(j1)*vL(j2)*Yv(j1,2))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(Tk(2,2,j1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(Tk(2,j1,2))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(Tk(j1,2,2))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu**2*Conjg(Yv(j1,2))*Yv(j1,2))/2._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*Conjg(lam(2))*vL(j1)*Yv(j1,2))/2._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*vu*Conjg(lam(j1))*kap(2,2,j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*vu*Conjg(lam(j1))*kap(2,j1,2))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*vu*Conjg(lam(j1))*kap(j1,2,2))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*Conjg(Yv(j1,2))*vL(j1)*lam(2))/2._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*vu*Conjg(kap(2,2,j1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*vu*Conjg(kap(2,j1,2))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*vu*Conjg(kap(j1,2,2))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vR(j1)*Tk(2,2,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vR(j1)*Tk(2,j1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vR(j1)*Tk(j1,2,2))/(3._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu*Conjg(kap(2,2,j1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu*Conjg(kap(2,j1,2))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu*Conjg(kap(j1,2,2))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,j1,2))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,2,2))/6._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
mat2a(4,4) = mat2a(4,4)+(vd**2*Conjg(lam(2))*lam(2))/2._dp
mat2a(4,4) = mat2a(4,4)+(vu**2*Conjg(lam(2))*lam(2))/2._dp
mat2a(4,5) = 0._dp 
mat2a(4,5) = mat2a(4,5)+mv2(2,3)/2._dp
mat2a(4,5) = mat2a(4,5)+mv2(3,2)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Yv(j2,3))*vL(j1)*vL(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Yv(j2,2))*vL(j1)*vL(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Tk(2,3,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Tk(2,j1,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Tk(3,2,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Tk(3,j1,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Tk(j1,2,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Tk(j1,3,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(Yv(j1,3))*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(lam(3))*vL(j1)*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(Yv(j1,2))*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(lam(2))*vL(j1)*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(lam(j1))*kap(2,3,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(lam(j1))*kap(2,j1,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(lam(j1))*kap(3,2,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(lam(j1))*kap(3,j1,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(lam(j1))*kap(j1,2,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(lam(j1))*kap(j1,3,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(Yv(j1,3))*vL(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(Yv(j1,2))*vL(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(kap(2,3,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(kap(2,j1,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(kap(3,2,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(kap(3,j1,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(kap(j1,2,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*vu*Conjg(kap(j1,3,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vR(j1)*Tk(2,3,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vR(j1)*Tk(2,j1,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vR(j1)*Tk(3,2,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vR(j1)*Tk(3,j1,2))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vR(j1)*Tk(j1,2,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vR(j1)*Tk(j1,3,2))/(6._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(kap(2,3,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(kap(2,j1,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(kap(3,2,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(kap(3,j1,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(kap(j1,2,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(kap(j1,3,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,3,2))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,3,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
mat2a(4,5) = mat2a(4,5)+(vd**2*Conjg(lam(3))*lam(2))/4._dp
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(lam(3))*lam(2))/4._dp
mat2a(4,5) = mat2a(4,5)+(vd**2*Conjg(lam(2))*lam(3))/4._dp
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(lam(2))*lam(3))/4._dp
mat2a(4,6) = 0._dp 
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Tv(1,2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,6) = mat2a(4,6)+(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,6) = mat2a(4,6)+(Conjg(Yv(1,j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vd*Conjg(lam(j1))*vR(j1)*Yv(1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vd*Conjg(lam(2))*vR(j1)*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vd*Conjg(Yv(1,j1))*vR(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vd*Conjg(Yv(1,2))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(1,2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(kap(2,j1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(kap(2,j2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(kap(j1,2,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(kap(j1,j2,2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(kap(j2,2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(kap(j2,j1,2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(Conjg(Yv(1,2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
mat2a(4,6) = mat2a(4,6)+(vu*Tv(1,2))/(2._dp*sqrt(2._dp))
mat2a(4,7) = 0._dp 
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Tv(2,2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,7) = mat2a(4,7)+(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,7) = mat2a(4,7)+(Conjg(Yv(2,j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vd*Conjg(lam(j1))*vR(j1)*Yv(2,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vd*Conjg(lam(2))*vR(j1)*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vd*Conjg(Yv(2,j1))*vR(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vd*Conjg(Yv(2,2))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(2,2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(kap(2,j1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(kap(2,j2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(kap(j1,2,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(kap(j1,j2,2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(kap(j2,2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(kap(j2,j1,2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(Conjg(Yv(2,2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
mat2a(4,7) = mat2a(4,7)+(vu*Tv(2,2))/(2._dp*sqrt(2._dp))
mat2a(4,8) = 0._dp 
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Tv(3,2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,8) = mat2a(4,8)+(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,8) = mat2a(4,8)+(Conjg(Yv(3,j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vd*Conjg(lam(j1))*vR(j1)*Yv(3,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vd*Conjg(lam(2))*vR(j1)*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vd*Conjg(Yv(3,j1))*vR(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vd*Conjg(Yv(3,2))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(3,2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(kap(2,j1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(kap(2,j2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(kap(j1,2,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(kap(j1,j2,2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(kap(j2,2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(kap(j2,j1,2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(Conjg(Yv(3,2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
mat2a(4,8) = mat2a(4,8)+(vu*Tv(3,2))/(2._dp*sqrt(2._dp))
mat2a(5,5) = 0._dp 
mat2a(5,5) = mat2a(5,5)+mv2(3,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(Yv(j2,3))*vL(j1)*vL(j2)*Yv(j1,3))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(Tk(3,3,j1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(Tk(3,j1,3))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(Tk(j1,3,3))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu**2*Conjg(Yv(j1,3))*Yv(j1,3))/2._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*Conjg(lam(3))*vL(j1)*Yv(j1,3))/2._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*vu*Conjg(lam(j1))*kap(3,3,j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*vu*Conjg(lam(j1))*kap(3,j1,3))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*vu*Conjg(lam(j1))*kap(j1,3,3))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*Conjg(Yv(j1,3))*vL(j1)*lam(3))/2._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*vu*Conjg(kap(3,3,j1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*vu*Conjg(kap(3,j1,3))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*vu*Conjg(kap(j1,3,3))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vR(j1)*Tk(3,3,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vR(j1)*Tk(3,j1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vR(j1)*Tk(j1,3,3))/(3._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu*Conjg(kap(3,3,j1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu*Conjg(kap(3,j1,3))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu*Conjg(kap(j1,3,3))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,3,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,j1,3))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,3,3))/6._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,3,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
mat2a(5,5) = mat2a(5,5)+(vd**2*Conjg(lam(3))*lam(3))/2._dp
mat2a(5,5) = mat2a(5,5)+(vu**2*Conjg(lam(3))*lam(3))/2._dp
mat2a(5,6) = 0._dp 
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Tv(1,3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,6) = mat2a(5,6)+(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,6) = mat2a(5,6)+(Conjg(Yv(1,j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vd*Conjg(lam(j1))*vR(j1)*Yv(1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vd*Conjg(lam(3))*vR(j1)*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vd*Conjg(Yv(1,j1))*vR(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vd*Conjg(Yv(1,3))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(1,3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(kap(3,j1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(kap(3,j2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(kap(j1,3,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(kap(j1,j2,3))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(kap(j2,3,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(kap(j2,j1,3))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(Conjg(Yv(1,3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
mat2a(5,6) = mat2a(5,6)+(vu*Tv(1,3))/(2._dp*sqrt(2._dp))
mat2a(5,7) = 0._dp 
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Tv(2,3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,7) = mat2a(5,7)+(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,7) = mat2a(5,7)+(Conjg(Yv(2,j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vd*Conjg(lam(j1))*vR(j1)*Yv(2,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vd*Conjg(lam(3))*vR(j1)*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vd*Conjg(Yv(2,j1))*vR(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vd*Conjg(Yv(2,3))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(2,3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(kap(3,j1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(kap(3,j2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(kap(j1,3,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(kap(j1,j2,3))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(kap(j2,3,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(kap(j2,j1,3))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(Conjg(Yv(2,3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
mat2a(5,7) = mat2a(5,7)+(vu*Tv(2,3))/(2._dp*sqrt(2._dp))
mat2a(5,8) = 0._dp 
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Tv(3,3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,8) = mat2a(5,8)+(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,8) = mat2a(5,8)+(Conjg(Yv(3,j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vd*Conjg(lam(j1))*vR(j1)*Yv(3,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vd*Conjg(lam(3))*vR(j1)*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vd*Conjg(Yv(3,j1))*vR(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vd*Conjg(Yv(3,3))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(3,3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(kap(3,j1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(kap(3,j2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(kap(j1,3,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(kap(j1,j2,3))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(kap(j2,3,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(kap(j2,j1,3))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(Conjg(Yv(3,3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
mat2a(5,8) = mat2a(5,8)+(vu*Tv(3,3))/(2._dp*sqrt(2._dp))
mat2a(6,6) = 0._dp 
mat2a(6,6) = mat2a(6,6)+(g1**2*vd**2)/8._dp
mat2a(6,6) = mat2a(6,6)+(g2**2*vd**2)/8._dp
mat2a(6,6) = mat2a(6,6)-(g1**2*vu**2)/8._dp
mat2a(6,6) = mat2a(6,6)-(g2**2*vu**2)/8._dp
mat2a(6,6) = mat2a(6,6)+ml2(1,1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,6) = mat2a(6,6)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(1,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(vu**2*Conjg(Yv(1,j1))*Yv(1,j1))/2._dp
End Do 
mat2a(6,6) = mat2a(6,6)+(g1**2*vL(1)**2)/4._dp
mat2a(6,6) = mat2a(6,6)+(g2**2*vL(1)**2)/4._dp
mat2a(6,7) = 0._dp 
mat2a(6,7) = mat2a(6,7)+ml2(1,2)/2._dp
mat2a(6,7) = mat2a(6,7)+ml2(2,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,7) = mat2a(6,7)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,7) = mat2a(6,7)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,7) = mat2a(6,7)+(vu**2*Conjg(Yv(2,j1))*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(6,7) = mat2a(6,7)+(vu**2*Conjg(Yv(1,j1))*Yv(2,j1))/4._dp
End Do 
mat2a(6,7) = mat2a(6,7)+(g1**2*vL(1)*vL(2))/4._dp
mat2a(6,7) = mat2a(6,7)+(g2**2*vL(1)*vL(2))/4._dp
mat2a(6,8) = 0._dp 
mat2a(6,8) = mat2a(6,8)+ml2(1,3)/2._dp
mat2a(6,8) = mat2a(6,8)+ml2(3,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,8) = mat2a(6,8)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,8) = mat2a(6,8)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,8) = mat2a(6,8)+(vu**2*Conjg(Yv(3,j1))*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(6,8) = mat2a(6,8)+(vu**2*Conjg(Yv(1,j1))*Yv(3,j1))/4._dp
End Do 
mat2a(6,8) = mat2a(6,8)+(g1**2*vL(1)*vL(3))/4._dp
mat2a(6,8) = mat2a(6,8)+(g2**2*vL(1)*vL(3))/4._dp
mat2a(7,7) = 0._dp 
mat2a(7,7) = mat2a(7,7)+(g1**2*vd**2)/8._dp
mat2a(7,7) = mat2a(7,7)+(g2**2*vd**2)/8._dp
mat2a(7,7) = mat2a(7,7)-(g1**2*vu**2)/8._dp
mat2a(7,7) = mat2a(7,7)-(g2**2*vu**2)/8._dp
mat2a(7,7) = mat2a(7,7)+ml2(2,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,7) = mat2a(7,7)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(vu**2*Conjg(Yv(2,j1))*Yv(2,j1))/2._dp
End Do 
mat2a(7,7) = mat2a(7,7)+(g1**2*vL(2)**2)/4._dp
mat2a(7,7) = mat2a(7,7)+(g2**2*vL(2)**2)/4._dp
mat2a(7,8) = 0._dp 
mat2a(7,8) = mat2a(7,8)+ml2(2,3)/2._dp
mat2a(7,8) = mat2a(7,8)+ml2(3,2)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,8) = mat2a(7,8)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,8) = mat2a(7,8)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(7,8) = mat2a(7,8)+(vu**2*Conjg(Yv(3,j1))*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(7,8) = mat2a(7,8)+(vu**2*Conjg(Yv(2,j1))*Yv(3,j1))/4._dp
End Do 
mat2a(7,8) = mat2a(7,8)+(g1**2*vL(2)*vL(3))/4._dp
mat2a(7,8) = mat2a(7,8)+(g2**2*vL(2)*vL(3))/4._dp
mat2a(8,8) = 0._dp 
mat2a(8,8) = mat2a(8,8)+(g1**2*vd**2)/8._dp
mat2a(8,8) = mat2a(8,8)+(g2**2*vd**2)/8._dp
mat2a(8,8) = mat2a(8,8)-(g1**2*vu**2)/8._dp
mat2a(8,8) = mat2a(8,8)-(g2**2*vu**2)/8._dp
mat2a(8,8) = mat2a(8,8)+ml2(3,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(8,8) = mat2a(8,8)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(vu**2*Conjg(Yv(3,j1))*Yv(3,j1))/2._dp
End Do 
mat2a(8,8) = mat2a(8,8)+(g1**2*vL(3)**2)/4._dp
mat2a(8,8) = mat2a(8,8)+(g2**2*vL(3)**2)/4._dp

 
 Do i1=2,8
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = (mat2a(i2,i1)) 
  End do 
End do 

 
Do i1=1,8
PiSf(i1,:,:) = ZeroC 
p2 = Mhh2(i1)
Call Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MCha,MCha2,MChi,MChi2,MFd,               & 
& MFd2,MFu,MFu2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhAhUhh,cplAhUhhhh,           & 
& cplAhUhhVZ,cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,     & 
& cplcFdFdUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,      & 
& cplUhhhhhh,cplUhhHpmcHpm,cplUhhHpmcVWm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,          & 
& cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpmcHpm,cplUhhUhhSdcSd,cplUhhUhhSucSu, & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=8,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,8
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses' 
     !Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram
   return 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,8
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MCha,MCha2,MChi,MChi2,MFd,               & 
& MFd2,MFu,MFu2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhAhUhh,cplAhUhhhh,           & 
& cplAhUhhVZ,cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,     & 
& cplcFdFdUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,      & 
& cplUhhhhhh,cplUhhHpmcHpm,cplUhhHpmcVWm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,          & 
& cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpmcHpm,cplUhhUhhSdcSd,cplUhhUhhSucSu, & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=8,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,8
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     !Write(*,*) 'generation: ',i1 
     !Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
 ! Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoophh
 
 
Subroutine Pi1Loophh(p2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MCha,MCha2,MChi,MChi2,             & 
& MFd,MFd2,MFu,MFu2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhAhUhh,cplAhUhhhh,       & 
& cplAhUhhVZ,cplcChaChaUhhL,cplcChaChaUhhR,cplChiChiUhhL,cplChiChiUhhR,cplcFdFdUhhL,     & 
& cplcFdFdUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcgWmgWmUhh,cplcgWpCgWpCUhh,cplcgZgZUhh,      & 
& cplUhhhhhh,cplUhhHpmcHpm,cplUhhHpmcVWm,cplUhhSdcSd,cplUhhSucSu,cplUhhcVWmVWm,          & 
& cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpmcHpm,cplUhhUhhSdcSd,cplUhhUhhSucSu, & 
& cplUhhUhhcVWmVWm,cplUhhUhhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(8),MAh2(8),Mhh(8),Mhh2(8),MVZ,MVZ2,MCha(5),MCha2(5),MChi(10),MChi2(10),           & 
& MFd(3),MFd2(3),MFu(3),MFu2(3),MHpm(8),MHpm2(8),MVWm,MVWm2,MSd(6),MSd2(6),              & 
& MSu(6),MSu2(6)

Complex(dp), Intent(in) :: cplAhAhUhh(8,8,8),cplAhUhhhh(8,8,8),cplAhUhhVZ(8,8),cplcChaChaUhhL(5,5,8),            & 
& cplcChaChaUhhR(5,5,8),cplChiChiUhhL(10,10,8),cplChiChiUhhR(10,10,8),cplcFdFdUhhL(3,3,8),& 
& cplcFdFdUhhR(3,3,8),cplcFuFuUhhL(3,3,8),cplcFuFuUhhR(3,3,8),cplcgWmgWmUhh(8),          & 
& cplcgWpCgWpCUhh(8),cplcgZgZUhh(8),cplUhhhhhh(8,8,8),cplUhhHpmcHpm(8,8,8),              & 
& cplUhhHpmcVWm(8,8),cplUhhSdcSd(8,6,6),cplUhhSucSu(8,6,6),cplUhhcVWmVWm(8),             & 
& cplUhhVZVZ(8),cplAhAhUhhUhh(8,8,8,8),cplUhhUhhhhhh(8,8,8,8),cplUhhUhhHpmcHpm(8,8,8,8), & 
& cplUhhUhhSdcSd(8,8,6,6),cplUhhUhhSucSu(8,8,6,6),cplUhhUhhcVWmVWm(8,8),cplUhhUhhVZVZ(8,8)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(8,8) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(8,8) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,MAh2(i1),MAh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhAhUhh(i1,i2,gO1)
coup2 = Conjg(cplAhAhUhh(i1,i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,Mhh2(i1),MAh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhUhhhh(i2,gO1,i1)
coup2 = Conjg(cplAhUhhhh(i2,gO2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VZ, Ah 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,MAh2(i2),MVZ2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhUhhVZ(i2,gO1)
coup2 =  Conjg(cplAhUhhVZ(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! bar[Cha], Cha 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 5
       Do i2 = 1, 5
 G0m2 = Gloop(p2,MCha2(i1),MCha2(i2)) 
B0m2 = -2._dp*MCha(i1)*MCha(i2)*B0(p2,MCha2(i1),MCha2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcChaChaUhhL(i1,i2,gO1)
coupR1 = cplcChaChaUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcChaChaUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcChaChaUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Chi, Chi 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 10
       Do i2 = 1, 10
 G0m2 = Gloop(p2,MChi2(i1),MChi2(i2)) 
B0m2 = -2._dp*MChi(i1)*MChi(i2)*B0(p2,MChi2(i1),MChi2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplChiChiUhhL(i1,i2,gO1)
coupR1 = cplChiChiUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplChiChiUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplChiChiUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Gloop(p2,MFd2(i1),MFd2(i2)) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*B0(p2,MFd2(i1),MFd2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcFdFdUhhL(i1,i2,gO1)
coupR1 = cplcFdFdUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFdFdUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFdFdUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Gloop(p2,MFu2(i1),MFu2(i2)) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*B0(p2,MFu2(i1),MFu2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcFuFuUhhL(i1,i2,gO1)
coupR1 = cplcFuFuUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFuUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFuUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgWmgWmUhh(gO1)
coup2 =  cplcgWmgWmUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgWpCgWpCUhh(gO1)
coup2 =  cplcgWpCgWpCUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gZ 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVZ2*RXi,MVZ2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgZgZUhh(gO1)
coup2 =  cplcgZgZUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,Mhh2(i1),Mhh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhhhhh(gO1,i1,i2)
coup2 = Conjg(cplUhhhhhh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,MHpm2(i1),MHpm2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhHpmcHpm(gO1,i2,i1)
coup2 = Conjg(cplUhhHpmcHpm(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,MHpm2(i2),MVWm2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhHpmcVWm(gO1,i2)
coup2 =  Conjg(cplUhhHpmcVWm(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B0m2 = B0(p2,MSd2(i1),MSd2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhSdcSd(gO1,i2,i1)
coup2 = Conjg(cplUhhSdcSd(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B0m2 = B0(p2,MSu2(i1),MSu2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhSucSu(gO1,i2,i1)
coup2 = Conjg(cplUhhSucSu(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = SVVloop(p2,MVWm2,MVWm2)   
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhcVWmVWm(gO1)
coup2 =  Conjg(cplUhhcVWmVWm(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
F0m2 = SVVloop(p2,MVZ2,MVZ2)   
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhVZVZ(gO1)
coup2 =  Conjg(cplUhhVZVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MAh2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhAhUhhUhh(i1,i1,gO1,gO2)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(Mhh2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhUhhhhhh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MHpm2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhUhhHpmcHpm(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSd2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhUhhSdcSd(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSu2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhUhhSucSu(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVWm2) + 0.25_dp*RXi*A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhUhhcVWmVWm(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVZ2) + 0.25_dp*RXi*A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUhhUhhVZVZ(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 8
  Do gO1 = gO2+1, 8
     res(gO1,gO2) = (res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1Loophh 
 
Subroutine OneLoopAh(g1,g2,lam,Tlam,Yv,Tv,kap,Tk,ml2,mlHd2,mHd2,mHu2,mv2,             & 
& vd,vu,vL,vR,TW,MAh,MAh2,Mhh,Mhh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,MFu2,              & 
& MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplUAhAhAh,cplUAhAhhh,cplcChaChaUAhL, & 
& cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,cplcFdFdUAhR,cplcFuFuUAhL,     & 
& cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,cplUAhhhVZ,cplUAhHpmcHpm,        & 
& cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHpmcHpm,    & 
& cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,delta,mass,               & 
& mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MAh(8),MAh2(8),Mhh(8),Mhh2(8),MCha(5),MCha2(5),MChi(10),MChi2(10),MFd(3),             & 
& MFd2(3),MFu(3),MFu2(3),MVZ,MVZ2,MHpm(8),MHpm2(8),MVWm,MVWm2,MSd(6),MSd2(6),            & 
& MSu(6),MSu2(6)

Real(dp), Intent(in) :: g1,g2,mlHd2(3),mHd2,mHu2,vd,vu,vL(3),vR(3),TW

Complex(dp), Intent(in) :: lam(3),Tlam(3),Yv(3,3),Tv(3,3),kap(3,3,3),Tk(3,3,3),ml2(3,3),mv2(3,3)

Complex(dp), Intent(in) :: cplUAhAhAh(8,8,8),cplUAhAhhh(8,8,8),cplcChaChaUAhL(5,5,8),cplcChaChaUAhR(5,5,8),      & 
& cplChiChiUAhL(10,10,8),cplChiChiUAhR(10,10,8),cplcFdFdUAhL(3,3,8),cplcFdFdUAhR(3,3,8), & 
& cplcFuFuUAhL(3,3,8),cplcFuFuUAhR(3,3,8),cplcgWmgWmUAh(8),cplcgWpCgWpCUAh(8),           & 
& cplUAhhhhh(8,8,8),cplUAhhhVZ(8,8),cplUAhHpmcHpm(8,8,8),cplUAhHpmcVWm(8,8),             & 
& cplUAhSdcSd(8,6,6),cplUAhSucSu(8,6,6),cplUAhUAhAhAh(8,8,8,8),cplUAhUAhhhhh(8,8,8,8),   & 
& cplUAhUAhHpmcHpm(8,8,8,8),cplUAhUAhSdcSd(8,8,6,6),cplUAhUAhSucSu(8,8,6,6),             & 
& cplUAhUAhcVWmVWm(8,8),cplUAhUAhVZVZ(8,8)

Complex(dp) :: mat2a(8,8), mat2(8,8),  PiSf(8,8,8)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(8), test_m2(8),p2, test(8) 
Real(dp), Intent(out) :: mass(8), mass2(8) 
Complex(dp), Intent(out) ::  RS(8,8) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopAh'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+mHd2
mat2a(1,1) = mat2a(1,1)+(g1**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)-(g1**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)-(g2**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vd**2*Cos(TW)**2*RXiZ)/4._dp
mat2a(1,1) = mat2a(1,1)+(g1*g2*vd**2*Cos(TW)*RXiZ*Sin(TW))/2._dp
mat2a(1,1) = mat2a(1,1)+(g1**2*vd**2*RXiZ*Sin(TW)**2)/4._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,1) = mat2a(1,1)+(Conjg(lam(j2))*vR(j1)*vR(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(vu**2*Conjg(lam(j1))*lam(j1))/2._dp
End Do 
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)-(g2**2*vd*vu*Cos(TW)**2*RXiZ)/4._dp
mat2a(1,2) = mat2a(1,2)-(g1*g2*vd*vu*Cos(TW)*RXiZ*Sin(TW))/2._dp
mat2a(1,2) = mat2a(1,2)-(g1**2*vd*vu*RXiZ*Sin(TW)**2)/4._dp
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(Tlam(j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(vR(j1)*Tlam(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*lam(j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*lam(j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*lam(j1))/12._dp
End Do 
End Do 
End Do 
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)-(vu*Conjg(Tlam(1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,3) = mat2a(1,3)+(Conjg(lam(j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,3) = mat2a(1,3)+(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*lam(j1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(lam(1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(lam(j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(lam(j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(lam(j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(lam(j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(lam(j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(lam(j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*lam(1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(kap(1,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(kap(1,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(kap(j1,1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(kap(j1,j2,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(kap(j2,1,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)+(vu*Conjg(kap(j2,j1,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
mat2a(1,3) = mat2a(1,3)-(vu*Tlam(1))/(2._dp*sqrt(2._dp))
mat2a(1,4) = 0._dp 
mat2a(1,4) = mat2a(1,4)-(vu*Conjg(Tlam(2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,4) = mat2a(1,4)+(Conjg(lam(j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,4) = mat2a(1,4)+(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*lam(j1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(lam(2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(lam(j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(lam(j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(lam(j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(lam(j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(lam(j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(lam(j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*lam(2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(kap(2,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(kap(2,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(kap(j1,2,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(kap(j1,j2,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(kap(j2,2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)+(vu*Conjg(kap(j2,j1,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
mat2a(1,4) = mat2a(1,4)-(vu*Tlam(2))/(2._dp*sqrt(2._dp))
mat2a(1,5) = 0._dp 
mat2a(1,5) = mat2a(1,5)-(vu*Conjg(Tlam(3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,5) = mat2a(1,5)+(Conjg(lam(j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,5) = mat2a(1,5)+(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*lam(j1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(lam(3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(lam(j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(lam(j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(lam(j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(lam(j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(lam(j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(lam(j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*lam(3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(kap(3,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(kap(3,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(kap(j1,3,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(kap(j1,j2,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(kap(j2,3,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)+(vu*Conjg(kap(j2,j1,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
mat2a(1,5) = mat2a(1,5)-(vu*Tlam(3))/(2._dp*sqrt(2._dp))
mat2a(1,6) = 0._dp 
mat2a(1,6) = mat2a(1,6)+mlHd2(1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,6) = mat2a(1,6)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,6) = mat2a(1,6)-(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vu**2*Conjg(lam(j1))*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vu**2*Conjg(Yv(1,j1))*lam(j1))/4._dp
End Do 
mat2a(1,6) = mat2a(1,6)+(g2**2*vd*Cos(TW)**2*RXiZ*vL(1))/4._dp
mat2a(1,6) = mat2a(1,6)+(g1*g2*vd*Cos(TW)*RXiZ*Sin(TW)*vL(1))/2._dp
mat2a(1,6) = mat2a(1,6)+(g1**2*vd*RXiZ*Sin(TW)**2*vL(1))/4._dp
mat2a(1,7) = 0._dp 
mat2a(1,7) = mat2a(1,7)+mlHd2(2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,7) = mat2a(1,7)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,7) = mat2a(1,7)-(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,7) = mat2a(1,7)-(vu**2*Conjg(lam(j1))*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(1,7) = mat2a(1,7)-(vu**2*Conjg(Yv(2,j1))*lam(j1))/4._dp
End Do 
mat2a(1,7) = mat2a(1,7)+(g2**2*vd*Cos(TW)**2*RXiZ*vL(2))/4._dp
mat2a(1,7) = mat2a(1,7)+(g1*g2*vd*Cos(TW)*RXiZ*Sin(TW)*vL(2))/2._dp
mat2a(1,7) = mat2a(1,7)+(g1**2*vd*RXiZ*Sin(TW)**2*vL(2))/4._dp
mat2a(1,8) = 0._dp 
mat2a(1,8) = mat2a(1,8)+mlHd2(3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,8) = mat2a(1,8)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,8) = mat2a(1,8)-(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*lam(j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,8) = mat2a(1,8)-(vu**2*Conjg(lam(j1))*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(1,8) = mat2a(1,8)-(vu**2*Conjg(Yv(3,j1))*lam(j1))/4._dp
End Do 
mat2a(1,8) = mat2a(1,8)+(g2**2*vd*Cos(TW)**2*RXiZ*vL(3))/4._dp
mat2a(1,8) = mat2a(1,8)+(g1*g2*vd*Cos(TW)*RXiZ*Sin(TW)*vL(3))/2._dp
mat2a(1,8) = mat2a(1,8)+(g1**2*vd*RXiZ*Sin(TW)**2*vL(3))/4._dp
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+mHu2
mat2a(2,2) = mat2a(2,2)-(g1**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)-(g2**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g1**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vu**2*Cos(TW)**2*RXiZ)/4._dp
mat2a(2,2) = mat2a(2,2)+(g1*g2*vu**2*Cos(TW)*RXiZ*Sin(TW))/2._dp
mat2a(2,2) = mat2a(2,2)+(g1**2*vu**2*RXiZ*Sin(TW)**2)/4._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(lam(j2))*vR(j1)*vR(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(vd**2*Conjg(lam(j1))*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(vd*Conjg(lam(j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(vd*Conjg(Yv(j2,j1))*vL(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(Yv(j1,j3))*vR(j2)*vR(j3)*Yv(j1,j2))/2._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(Yv(j3,j1))*vL(j2)*vL(j3)*Yv(j2,j1))/2._dp
End Do 
End Do 
End Do 
mat2a(2,3) = 0._dp 
mat2a(2,3) = mat2a(2,3)-(vd*Conjg(Tlam(1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(Conjg(Tv(j1,1))*vL(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vL(j1)*Tv(j1,1))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(lam(j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(lam(j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(lam(j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(lam(j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(lam(j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(lam(j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(kap(1,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(kap(1,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(kap(j1,1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(kap(j1,j2,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(kap(j2,1,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*Conjg(kap(j2,j1,1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(1,j1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(1,j3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j1,1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j1,j3,1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j3,1,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j3,j1,1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,3) = mat2a(2,3)-(vd*Tlam(1))/(2._dp*sqrt(2._dp))
mat2a(2,4) = 0._dp 
mat2a(2,4) = mat2a(2,4)-(vd*Conjg(Tlam(2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(Conjg(Tv(j1,2))*vL(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vL(j1)*Tv(j1,2))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(lam(j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(lam(j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(lam(j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(lam(j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(lam(j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(lam(j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(kap(2,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(kap(2,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(kap(j1,2,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(kap(j1,j2,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(kap(j2,2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*Conjg(kap(j2,j1,2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(2,j1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(2,j3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j1,2,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j1,j3,2))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j3,2,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j3,j1,2))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
End Do 
mat2a(2,4) = mat2a(2,4)-(vd*Tlam(2))/(2._dp*sqrt(2._dp))
mat2a(2,5) = 0._dp 
mat2a(2,5) = mat2a(2,5)-(vd*Conjg(Tlam(3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(Conjg(Tv(j1,3))*vL(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vL(j1)*Tv(j1,3))/(2._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(lam(j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(lam(j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(lam(j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(lam(j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(lam(j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(lam(j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(kap(3,j1,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(kap(3,j2,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(kap(j1,3,j2))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(kap(j1,j2,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(kap(j2,3,j1))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*Conjg(kap(j2,j1,3))*vR(j2)*lam(j1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(3,j1,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(3,j3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j1,3,j3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j1,j3,3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j3,3,j1))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j3,j1,3))*vL(j2)*vR(j3)*Yv(j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(Yv(j3,j1))*vL(j3)*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
End Do 
mat2a(2,5) = mat2a(2,5)-(vd*Tlam(3))/(2._dp*sqrt(2._dp))
mat2a(2,6) = 0._dp 
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(Tv(1,j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(vR(j1)*Tv(1,j1))/(2._dp*sqrt(2._dp))
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(1,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(Yv(1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(Yv(1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(Yv(1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,6) = mat2a(2,6)-(g2**2*vu*Cos(TW)**2*RXiZ*vL(1))/4._dp
mat2a(2,6) = mat2a(2,6)-(g1*g2*vu*Cos(TW)*RXiZ*Sin(TW)*vL(1))/2._dp
mat2a(2,6) = mat2a(2,6)-(g1**2*vu*RXiZ*Sin(TW)**2*vL(1))/4._dp
mat2a(2,7) = 0._dp 
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(Tv(2,j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(vR(j1)*Tv(2,j1))/(2._dp*sqrt(2._dp))
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(2,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(Yv(2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(Yv(2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(Yv(2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,7) = mat2a(2,7)-(g2**2*vu*Cos(TW)**2*RXiZ*vL(2))/4._dp
mat2a(2,7) = mat2a(2,7)-(g1*g2*vu*Cos(TW)*RXiZ*Sin(TW)*vL(2))/2._dp
mat2a(2,7) = mat2a(2,7)-(g1**2*vu*RXiZ*Sin(TW)**2*vL(2))/4._dp
mat2a(2,8) = 0._dp 
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(Tv(3,j1))*vR(j1))/(2._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(vR(j1)*Tv(3,j1))/(2._dp*sqrt(2._dp))
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(3,j1))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(Yv(3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(Yv(3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/12._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(Yv(3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/12._dp
End Do 
End Do 
End Do 
mat2a(2,8) = mat2a(2,8)-(g2**2*vu*Cos(TW)**2*RXiZ*vL(3))/4._dp
mat2a(2,8) = mat2a(2,8)-(g1*g2*vu*Cos(TW)*RXiZ*Sin(TW)*vL(3))/2._dp
mat2a(2,8) = mat2a(2,8)-(g1**2*vu*RXiZ*Sin(TW)**2*vL(3))/4._dp
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)+mv2(1,1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(Yv(j2,1))*vL(j1)*vL(j2)*Yv(j1,1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(Tk(1,1,j1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(Tk(1,j1,1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(Tk(j1,1,1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vu**2*Conjg(Yv(j1,1))*Yv(j1,1))/2._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*Conjg(lam(1))*vL(j1)*Yv(j1,1))/2._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd*vu*Conjg(lam(j1))*kap(1,1,j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd*vu*Conjg(lam(j1))*kap(1,j1,1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd*vu*Conjg(lam(j1))*kap(j1,1,1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vd*Conjg(Yv(j1,1))*vL(j1)*lam(1))/2._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd*vu*Conjg(kap(1,1,j1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd*vu*Conjg(kap(1,j1,1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd*vu*Conjg(kap(j1,1,1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vR(j1)*Tk(1,1,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vR(j1)*Tk(1,j1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vR(j1)*Tk(j1,1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vu*Conjg(kap(1,1,j1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vu*Conjg(kap(1,j1,1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vu*Conjg(kap(j1,1,1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,1,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,j1,1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,1,1))/6._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(1,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(1,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,j2,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(1,1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(1,j1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j1,1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,1,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(1,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(1,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j1,1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(1,1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(1,j1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(Conjg(kap(j1,1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
mat2a(3,3) = mat2a(3,3)+(vd**2*Conjg(lam(1))*lam(1))/2._dp
mat2a(3,3) = mat2a(3,3)+(vu**2*Conjg(lam(1))*lam(1))/2._dp
mat2a(3,4) = 0._dp 
mat2a(3,4) = mat2a(3,4)+mv2(1,2)/2._dp
mat2a(3,4) = mat2a(3,4)+mv2(2,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Yv(j2,2))*vL(j1)*vL(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Yv(j2,1))*vL(j1)*vL(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(Tk(1,2,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(Tk(1,j1,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(Tk(2,1,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(Tk(2,j1,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(Tk(j1,1,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(Tk(j1,2,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(Yv(j1,2))*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(lam(2))*vL(j1)*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(Yv(j1,1))*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(lam(1))*vL(j1)*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(lam(j1))*kap(1,2,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(lam(j1))*kap(1,j1,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(lam(j1))*kap(2,1,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(lam(j1))*kap(2,j1,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(lam(j1))*kap(j1,1,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(lam(j1))*kap(j1,2,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(Yv(j1,2))*vL(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vd*Conjg(Yv(j1,1))*vL(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(kap(1,2,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(kap(1,j1,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(kap(2,1,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(kap(2,j1,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(kap(j1,1,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd*vu*Conjg(kap(j1,2,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vR(j1)*Tk(1,2,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vR(j1)*Tk(1,j1,2))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vR(j1)*Tk(2,1,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vR(j1)*Tk(2,j1,1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vR(j1)*Tk(j1,1,2))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vR(j1)*Tk(j1,2,1))/(6._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(kap(1,2,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(kap(1,j1,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(kap(2,1,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(kap(2,j1,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(kap(j1,1,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(kap(j1,2,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,2,1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(1,2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(1,j1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(2,1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(2,j1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,2,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(1,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(1,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(2,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(2,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,2,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(1,2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(1,j1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(2,1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(2,j1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)-(Conjg(kap(j1,2,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
mat2a(3,4) = mat2a(3,4)+(vd**2*Conjg(lam(2))*lam(1))/4._dp
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(lam(2))*lam(1))/4._dp
mat2a(3,4) = mat2a(3,4)+(vd**2*Conjg(lam(1))*lam(2))/4._dp
mat2a(3,4) = mat2a(3,4)+(vu**2*Conjg(lam(1))*lam(2))/4._dp
mat2a(3,5) = 0._dp 
mat2a(3,5) = mat2a(3,5)+mv2(1,3)/2._dp
mat2a(3,5) = mat2a(3,5)+mv2(3,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Yv(j2,3))*vL(j1)*vL(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Yv(j2,1))*vL(j1)*vL(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(Tk(1,3,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(Tk(1,j1,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(Tk(3,1,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(Tk(3,j1,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(Tk(j1,1,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(Tk(j1,3,1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(Yv(j1,3))*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(lam(3))*vL(j1)*Yv(j1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(Yv(j1,1))*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(lam(1))*vL(j1)*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(lam(j1))*kap(1,3,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(lam(j1))*kap(1,j1,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(lam(j1))*kap(3,1,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(lam(j1))*kap(3,j1,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(lam(j1))*kap(j1,1,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(lam(j1))*kap(j1,3,1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(Yv(j1,3))*vL(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vd*Conjg(Yv(j1,1))*vL(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(kap(1,3,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(kap(1,j1,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(kap(3,1,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(kap(3,j1,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(kap(j1,1,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd*vu*Conjg(kap(j1,3,1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vR(j1)*Tk(1,3,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vR(j1)*Tk(1,j1,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vR(j1)*Tk(3,1,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vR(j1)*Tk(3,j1,1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vR(j1)*Tk(j1,1,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vR(j1)*Tk(j1,3,1))/(6._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(kap(1,3,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(kap(1,j1,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(kap(3,1,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(kap(3,j1,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(kap(j1,1,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(kap(j1,3,1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(1,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,j1,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,3,1))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(1,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(1,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(1,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,3,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,j2,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(1,3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(1,j1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(3,1,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(3,j1,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,3,1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,1,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(1,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j1,j3,1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(kap(j3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(1,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(1,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(3,1,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(3,j1,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,3,1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(1,3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(1,j1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(3,1,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(3,j1,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)-(Conjg(kap(j1,3,1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
mat2a(3,5) = mat2a(3,5)+(vd**2*Conjg(lam(3))*lam(1))/4._dp
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(lam(3))*lam(1))/4._dp
mat2a(3,5) = mat2a(3,5)+(vd**2*Conjg(lam(1))*lam(3))/4._dp
mat2a(3,5) = mat2a(3,5)+(vu**2*Conjg(lam(1))*lam(3))/4._dp
mat2a(3,6) = 0._dp 
mat2a(3,6) = mat2a(3,6)+(vu*Conjg(Tv(1,1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,6) = mat2a(3,6)-(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,6) = mat2a(3,6)-(Conjg(Yv(1,j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(lam(j1))*vR(j1)*Yv(1,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vd*Conjg(lam(1))*vR(j1)*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(vd*Conjg(Yv(1,j1))*vR(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vd*Conjg(Yv(1,1))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(1,1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(kap(1,j1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(kap(1,j2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(kap(j1,1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(kap(j1,j2,1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(kap(j2,1,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(kap(j2,j1,1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)+(Conjg(Yv(1,1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
mat2a(3,6) = mat2a(3,6)+(vu*Tv(1,1))/(2._dp*sqrt(2._dp))
mat2a(3,7) = 0._dp 
mat2a(3,7) = mat2a(3,7)+(vu*Conjg(Tv(2,1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,7) = mat2a(3,7)-(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,7) = mat2a(3,7)-(Conjg(Yv(2,j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vd*Conjg(lam(j1))*vR(j1)*Yv(2,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vd*Conjg(lam(1))*vR(j1)*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(vd*Conjg(Yv(2,j1))*vR(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vd*Conjg(Yv(2,1))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(2,1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(kap(1,j1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(kap(1,j2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(kap(j1,1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(kap(j1,j2,1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(kap(j2,1,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(kap(j2,j1,1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)+(Conjg(Yv(2,1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
mat2a(3,7) = mat2a(3,7)+(vu*Tv(2,1))/(2._dp*sqrt(2._dp))
mat2a(3,8) = 0._dp 
mat2a(3,8) = mat2a(3,8)+(vu*Conjg(Tv(3,1)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,8) = mat2a(3,8)-(Conjg(Yv(j2,1))*vL(j2)*vR(j1)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,8) = mat2a(3,8)-(Conjg(Yv(3,j2))*vL(j1)*vR(j2)*Yv(j1,1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vd*Conjg(lam(j1))*vR(j1)*Yv(3,1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vd*Conjg(lam(1))*vR(j1)*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(vd*Conjg(Yv(3,j1))*vR(j1)*lam(1))/4._dp
End Do 
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vd*Conjg(Yv(3,1))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(3,1))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(kap(1,j1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(kap(1,j2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(kap(j1,1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(kap(j1,j2,1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(kap(j2,1,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(kap(j2,j1,1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)+(Conjg(Yv(3,1))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(1,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(1,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,j2,1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,j1,1))/12._dp
End Do 
End Do 
mat2a(3,8) = mat2a(3,8)+(vu*Tv(3,1))/(2._dp*sqrt(2._dp))
mat2a(4,4) = 0._dp 
mat2a(4,4) = mat2a(4,4)+mv2(2,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(Yv(j2,2))*vL(j1)*vL(j2)*Yv(j1,2))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(Tk(2,2,j1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(Tk(2,j1,2))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(Tk(j1,2,2))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vu**2*Conjg(Yv(j1,2))*Yv(j1,2))/2._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*Conjg(lam(2))*vL(j1)*Yv(j1,2))/2._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd*vu*Conjg(lam(j1))*kap(2,2,j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd*vu*Conjg(lam(j1))*kap(2,j1,2))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd*vu*Conjg(lam(j1))*kap(j1,2,2))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vd*Conjg(Yv(j1,2))*vL(j1)*lam(2))/2._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd*vu*Conjg(kap(2,2,j1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd*vu*Conjg(kap(2,j1,2))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd*vu*Conjg(kap(j1,2,2))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vR(j1)*Tk(2,2,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vR(j1)*Tk(2,j1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vR(j1)*Tk(j1,2,2))/(3._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vu*Conjg(kap(2,2,j1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vu*Conjg(kap(2,j1,2))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vu*Conjg(kap(j1,2,2))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,j1,2))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,2,2))/6._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(2,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(2,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,2,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,j2,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(2,2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(2,j1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j1,2,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(2,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(2,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j1,2,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(2,2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(2,j1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(Conjg(kap(j1,2,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
mat2a(4,4) = mat2a(4,4)+(vd**2*Conjg(lam(2))*lam(2))/2._dp
mat2a(4,4) = mat2a(4,4)+(vu**2*Conjg(lam(2))*lam(2))/2._dp
mat2a(4,5) = 0._dp 
mat2a(4,5) = mat2a(4,5)+mv2(2,3)/2._dp
mat2a(4,5) = mat2a(4,5)+mv2(3,2)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Yv(j2,3))*vL(j1)*vL(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Yv(j2,2))*vL(j1)*vL(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(Tk(2,3,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(Tk(2,j1,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(Tk(3,2,j1))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(Tk(3,j1,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(Tk(j1,2,3))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(Tk(j1,3,2))*vR(j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(Yv(j1,3))*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(lam(3))*vL(j1)*Yv(j1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(Yv(j1,2))*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(lam(2))*vL(j1)*Yv(j1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(lam(j1))*kap(2,3,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(lam(j1))*kap(2,j1,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(lam(j1))*kap(3,2,j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(lam(j1))*kap(3,j1,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(lam(j1))*kap(j1,2,3))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(lam(j1))*kap(j1,3,2))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(Yv(j1,3))*vL(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vd*Conjg(Yv(j1,2))*vL(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(kap(2,3,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(kap(2,j1,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(kap(3,2,j1))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(kap(3,j1,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(kap(j1,2,3))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd*vu*Conjg(kap(j1,3,2))*lam(j1))/12._dp
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vR(j1)*Tk(2,3,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vR(j1)*Tk(2,j1,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vR(j1)*Tk(3,2,j1))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vR(j1)*Tk(3,j1,2))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vR(j1)*Tk(j1,2,3))/(6._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vR(j1)*Tk(j1,3,2))/(6._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(kap(2,3,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(kap(2,j1,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(kap(3,2,j1))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(kap(3,j1,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(kap(j1,2,3))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(kap(j1,3,2))*vL(j2)*Yv(j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(2,j1,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,j1,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,3,2))/12._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(2,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(2,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(3,j1,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(3,j2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,2,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,3,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,j2,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j1,j2,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(2,3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(2,j1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(3,2,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(3,j1,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,2,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,3,2))*vR(j2)*vR(j3)*kap(j1,j3,j2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,2,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,2))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(2,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,2,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j1,j3,2))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(kap(j3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(2,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(2,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(3,2,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(3,j1,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,2,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,3,2))*vR(j2)*vR(j3)*kap(j2,j1,j3))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(2,3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(2,j1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(3,2,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(3,j1,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,2,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)-(Conjg(kap(j1,3,2))*vR(j2)*vR(j3)*kap(j2,j3,j1))/36._dp
End Do 
End Do 
End Do 
mat2a(4,5) = mat2a(4,5)+(vd**2*Conjg(lam(3))*lam(2))/4._dp
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(lam(3))*lam(2))/4._dp
mat2a(4,5) = mat2a(4,5)+(vd**2*Conjg(lam(2))*lam(3))/4._dp
mat2a(4,5) = mat2a(4,5)+(vu**2*Conjg(lam(2))*lam(3))/4._dp
mat2a(4,6) = 0._dp 
mat2a(4,6) = mat2a(4,6)+(vu*Conjg(Tv(1,2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,6) = mat2a(4,6)-(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,6) = mat2a(4,6)-(Conjg(Yv(1,j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vd*Conjg(lam(j1))*vR(j1)*Yv(1,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vd*Conjg(lam(2))*vR(j1)*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(vd*Conjg(Yv(1,j1))*vR(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vd*Conjg(Yv(1,2))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(1,2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(kap(2,j1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(kap(2,j2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(kap(j1,2,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(kap(j1,j2,2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(kap(j2,2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(kap(j2,j1,2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)+(Conjg(Yv(1,2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
mat2a(4,6) = mat2a(4,6)+(vu*Tv(1,2))/(2._dp*sqrt(2._dp))
mat2a(4,7) = 0._dp 
mat2a(4,7) = mat2a(4,7)+(vu*Conjg(Tv(2,2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,7) = mat2a(4,7)-(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,7) = mat2a(4,7)-(Conjg(Yv(2,j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vd*Conjg(lam(j1))*vR(j1)*Yv(2,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vd*Conjg(lam(2))*vR(j1)*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(vd*Conjg(Yv(2,j1))*vR(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vd*Conjg(Yv(2,2))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(2,2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(kap(2,j1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(kap(2,j2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(kap(j1,2,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(kap(j1,j2,2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(kap(j2,2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(kap(j2,j1,2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)+(Conjg(Yv(2,2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
mat2a(4,7) = mat2a(4,7)+(vu*Tv(2,2))/(2._dp*sqrt(2._dp))
mat2a(4,8) = 0._dp 
mat2a(4,8) = mat2a(4,8)+(vu*Conjg(Tv(3,2)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,8) = mat2a(4,8)-(Conjg(Yv(j2,2))*vL(j2)*vR(j1)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,8) = mat2a(4,8)-(Conjg(Yv(3,j2))*vL(j1)*vR(j2)*Yv(j1,2))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vd*Conjg(lam(j1))*vR(j1)*Yv(3,2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vd*Conjg(lam(2))*vR(j1)*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(vd*Conjg(Yv(3,j1))*vR(j1)*lam(2))/4._dp
End Do 
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vd*Conjg(Yv(3,2))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(3,2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(kap(2,j1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(kap(2,j2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(kap(j1,2,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(kap(j1,j2,2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(kap(j2,2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(kap(j2,j1,2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)+(Conjg(Yv(3,2))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(2,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(2,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,2,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,j2,2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,j1,2))/12._dp
End Do 
End Do 
mat2a(4,8) = mat2a(4,8)+(vu*Tv(3,2))/(2._dp*sqrt(2._dp))
mat2a(5,5) = 0._dp 
mat2a(5,5) = mat2a(5,5)+mv2(3,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(Yv(j2,3))*vL(j1)*vL(j2)*Yv(j1,3))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(Tk(3,3,j1))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(Tk(3,j1,3))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(Tk(j1,3,3))*vR(j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vu**2*Conjg(Yv(j1,3))*Yv(j1,3))/2._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*Conjg(lam(3))*vL(j1)*Yv(j1,3))/2._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd*vu*Conjg(lam(j1))*kap(3,3,j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd*vu*Conjg(lam(j1))*kap(3,j1,3))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd*vu*Conjg(lam(j1))*kap(j1,3,3))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vd*Conjg(Yv(j1,3))*vL(j1)*lam(3))/2._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd*vu*Conjg(kap(3,3,j1))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd*vu*Conjg(kap(3,j1,3))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd*vu*Conjg(kap(j1,3,3))*lam(j1))/6._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vR(j1)*Tk(3,3,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vR(j1)*Tk(3,j1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vR(j1)*Tk(j1,3,3))/(3._dp*sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vu*Conjg(kap(3,3,j1))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vu*Conjg(kap(3,j1,3))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vu*Conjg(kap(j1,3,3))*vL(j2)*Yv(j2,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,3,j1))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(3,j1,3))/6._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*kap(j1,3,3))/6._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(3,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(3,j1,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(3,j2,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*kap(j1,3,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j1,j2,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(3,3,j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(3,j1,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j1,3,3))*vR(j2)*vR(j3)*kap(j1,j3,j2))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j1,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(3,j3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,3,j3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j1,j3,3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(kap(j3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(3,3,j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(3,j1,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j1,3,3))*vR(j2)*vR(j3)*kap(j2,j1,j3))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(3,3,j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(3,j1,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(Conjg(kap(j1,3,3))*vR(j2)*vR(j3)*kap(j2,j3,j1))/18._dp
End Do 
End Do 
End Do 
mat2a(5,5) = mat2a(5,5)+(vd**2*Conjg(lam(3))*lam(3))/2._dp
mat2a(5,5) = mat2a(5,5)+(vu**2*Conjg(lam(3))*lam(3))/2._dp
mat2a(5,6) = 0._dp 
mat2a(5,6) = mat2a(5,6)+(vu*Conjg(Tv(1,3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,6) = mat2a(5,6)-(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,6) = mat2a(5,6)-(Conjg(Yv(1,j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vd*Conjg(lam(j1))*vR(j1)*Yv(1,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vd*Conjg(lam(3))*vR(j1)*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(vd*Conjg(Yv(1,j1))*vR(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vd*Conjg(Yv(1,3))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(1,3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(kap(3,j1,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(kap(3,j2,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(kap(j1,3,j2))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(kap(j1,j2,3))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(kap(j2,3,j1))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(kap(j2,j1,3))*vR(j2)*Yv(1,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)+(Conjg(Yv(1,3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Yv(1,j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
mat2a(5,6) = mat2a(5,6)+(vu*Tv(1,3))/(2._dp*sqrt(2._dp))
mat2a(5,7) = 0._dp 
mat2a(5,7) = mat2a(5,7)+(vu*Conjg(Tv(2,3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,7) = mat2a(5,7)-(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,7) = mat2a(5,7)-(Conjg(Yv(2,j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vd*Conjg(lam(j1))*vR(j1)*Yv(2,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vd*Conjg(lam(3))*vR(j1)*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(vd*Conjg(Yv(2,j1))*vR(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vd*Conjg(Yv(2,3))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(2,3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(kap(3,j1,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(kap(3,j2,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(kap(j1,3,j2))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(kap(j1,j2,3))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(kap(j2,3,j1))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(kap(j2,j1,3))*vR(j2)*Yv(2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)+(Conjg(Yv(2,3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Yv(2,j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
mat2a(5,7) = mat2a(5,7)+(vu*Tv(2,3))/(2._dp*sqrt(2._dp))
mat2a(5,8) = 0._dp 
mat2a(5,8) = mat2a(5,8)+(vu*Conjg(Tv(3,3)))/(2._dp*sqrt(2._dp))
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,8) = mat2a(5,8)-(Conjg(Yv(j2,3))*vL(j2)*vR(j1)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,8) = mat2a(5,8)-(Conjg(Yv(3,j2))*vL(j1)*vR(j2)*Yv(j1,3))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vd*Conjg(lam(j1))*vR(j1)*Yv(3,3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vd*Conjg(lam(3))*vR(j1)*Yv(3,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(vd*Conjg(Yv(3,j1))*vR(j1)*lam(3))/4._dp
End Do 
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vd*Conjg(Yv(3,3))*vR(j1)*lam(j1))/4._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(Conjg(Yv(j1,j2))*vL(j1)*vR(j2)*Yv(3,3))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(kap(3,j1,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(kap(3,j2,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(kap(j1,3,j2))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(kap(j1,j2,3))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(kap(j2,3,j1))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(kap(j2,j1,3))*vR(j2)*Yv(3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)+(Conjg(Yv(3,3))*vL(j1)*vR(j2)*Yv(j1,j2))/4._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(3,j1,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(3,j2,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,3,j2))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j1,j2,3))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,3,j1))/12._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Yv(3,j1))*vR(j2)*kap(j2,j1,3))/12._dp
End Do 
End Do 
mat2a(5,8) = mat2a(5,8)+(vu*Tv(3,3))/(2._dp*sqrt(2._dp))
mat2a(6,6) = 0._dp 
mat2a(6,6) = mat2a(6,6)+(g1**2*vd**2)/8._dp
mat2a(6,6) = mat2a(6,6)+(g2**2*vd**2)/8._dp
mat2a(6,6) = mat2a(6,6)-(g1**2*vu**2)/8._dp
mat2a(6,6) = mat2a(6,6)-(g2**2*vu**2)/8._dp
mat2a(6,6) = mat2a(6,6)+ml2(1,1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,6) = mat2a(6,6)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(1,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(vu**2*Conjg(Yv(1,j1))*Yv(1,j1))/2._dp
End Do 
mat2a(6,6) = mat2a(6,6)+(g2**2*Cos(TW)**2*RXiZ*vL(1)**2)/4._dp
mat2a(6,6) = mat2a(6,6)+(g1*g2*Cos(TW)*RXiZ*Sin(TW)*vL(1)**2)/2._dp
mat2a(6,6) = mat2a(6,6)+(g1**2*RXiZ*Sin(TW)**2*vL(1)**2)/4._dp
mat2a(6,7) = 0._dp 
mat2a(6,7) = mat2a(6,7)+ml2(1,2)/2._dp
mat2a(6,7) = mat2a(6,7)+ml2(2,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,7) = mat2a(6,7)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,7) = mat2a(6,7)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,7) = mat2a(6,7)+(vu**2*Conjg(Yv(2,j1))*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(6,7) = mat2a(6,7)+(vu**2*Conjg(Yv(1,j1))*Yv(2,j1))/4._dp
End Do 
mat2a(6,8) = 0._dp 
mat2a(6,8) = mat2a(6,8)+ml2(1,3)/2._dp
mat2a(6,8) = mat2a(6,8)+ml2(3,1)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,8) = mat2a(6,8)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(1,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,8) = mat2a(6,8)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,8) = mat2a(6,8)+(vu**2*Conjg(Yv(3,j1))*Yv(1,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(6,8) = mat2a(6,8)+(vu**2*Conjg(Yv(1,j1))*Yv(3,j1))/4._dp
End Do 
mat2a(7,7) = 0._dp 
mat2a(7,7) = mat2a(7,7)+(g1**2*vd**2)/8._dp
mat2a(7,7) = mat2a(7,7)+(g2**2*vd**2)/8._dp
mat2a(7,7) = mat2a(7,7)-(g1**2*vu**2)/8._dp
mat2a(7,7) = mat2a(7,7)-(g2**2*vu**2)/8._dp
mat2a(7,7) = mat2a(7,7)+ml2(2,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,7) = mat2a(7,7)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(vu**2*Conjg(Yv(2,j1))*Yv(2,j1))/2._dp
End Do 
mat2a(7,7) = mat2a(7,7)+(g2**2*Cos(TW)**2*RXiZ*vL(2)**2)/4._dp
mat2a(7,7) = mat2a(7,7)+(g1*g2*Cos(TW)*RXiZ*Sin(TW)*vL(2)**2)/2._dp
mat2a(7,7) = mat2a(7,7)+(g1**2*RXiZ*Sin(TW)**2*vL(2)**2)/4._dp
mat2a(7,8) = 0._dp 
mat2a(7,8) = mat2a(7,8)+ml2(2,3)/2._dp
mat2a(7,8) = mat2a(7,8)+ml2(3,2)/2._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,8) = mat2a(7,8)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(2,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,8) = mat2a(7,8)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(3,j1))/4._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(7,8) = mat2a(7,8)+(vu**2*Conjg(Yv(3,j1))*Yv(2,j1))/4._dp
End Do 
Do j1 = 1,3
mat2a(7,8) = mat2a(7,8)+(vu**2*Conjg(Yv(2,j1))*Yv(3,j1))/4._dp
End Do 
mat2a(8,8) = 0._dp 
mat2a(8,8) = mat2a(8,8)+(g1**2*vd**2)/8._dp
mat2a(8,8) = mat2a(8,8)+(g2**2*vd**2)/8._dp
mat2a(8,8) = mat2a(8,8)-(g1**2*vu**2)/8._dp
mat2a(8,8) = mat2a(8,8)-(g2**2*vu**2)/8._dp
mat2a(8,8) = mat2a(8,8)+ml2(3,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(8,8) = mat2a(8,8)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(vu**2*Conjg(Yv(3,j1))*Yv(3,j1))/2._dp
End Do 
mat2a(8,8) = mat2a(8,8)+(g2**2*Cos(TW)**2*RXiZ*vL(3)**2)/4._dp
mat2a(8,8) = mat2a(8,8)+(g1*g2*Cos(TW)*RXiZ*Sin(TW)*vL(3)**2)/2._dp
mat2a(8,8) = mat2a(8,8)+(g1**2*RXiZ*Sin(TW)**2*vL(3)**2)/4._dp

 
 Do i1=2,8
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = (mat2a(i2,i1)) 
  End do 
End do 

 
Do i1=1,8
PiSf(i1,:,:) = ZeroC 
p2 = MAh2(i1)
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,               & 
& MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplUAhAhAh,cplUAhAhhh,           & 
& cplcChaChaUAhL,cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,cplcFdFdUAhR,   & 
& cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,cplUAhhhVZ,         & 
& cplUAhHpmcHpm,cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,cplUAhUAhhhhh,       & 
& cplUAhUAhHpmcHpm,cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,         & 
& kont,PiSf(i1,:,:))

End Do 
Do i1=8,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - PiP2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram
    return 
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
  Call TerminateProgram 
   return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,8
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
   If ((i1.Gt.1).or.(Abs(mass2(i1)).gt.MaxVal(Abs(mass2)))) Then 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses' 
     !Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram
   return 
   End If 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,8
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,               & 
& MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplUAhAhAh,cplUAhAhhh,           & 
& cplcChaChaUAhL,cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,cplcFdFdUAhR,   & 
& cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,cplUAhhhVZ,         & 
& cplUAhHpmcHpm,cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,cplUAhUAhhhhh,       & 
& cplUAhUAhHpmcHpm,cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,         & 
& kont,PiSf(i1,:,:))

End Do 
Do i1=8,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,8
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     !Write(*,*) 'generation: ',i1 
     !Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopAh
 
 
Subroutine Pi1LoopAh(p2,MAh,MAh2,Mhh,Mhh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,             & 
& MFu,MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplUAhAhAh,cplUAhAhhh,       & 
& cplcChaChaUAhL,cplcChaChaUAhR,cplChiChiUAhL,cplChiChiUAhR,cplcFdFdUAhL,cplcFdFdUAhR,   & 
& cplcFuFuUAhL,cplcFuFuUAhR,cplcgWmgWmUAh,cplcgWpCgWpCUAh,cplUAhhhhh,cplUAhhhVZ,         & 
& cplUAhHpmcHpm,cplUAhHpmcVWm,cplUAhSdcSd,cplUAhSucSu,cplUAhUAhAhAh,cplUAhUAhhhhh,       & 
& cplUAhUAhHpmcHpm,cplUAhUAhSdcSd,cplUAhUAhSucSu,cplUAhUAhcVWmVWm,cplUAhUAhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh(8),MAh2(8),Mhh(8),Mhh2(8),MCha(5),MCha2(5),MChi(10),MChi2(10),MFd(3),             & 
& MFd2(3),MFu(3),MFu2(3),MVZ,MVZ2,MHpm(8),MHpm2(8),MVWm,MVWm2,MSd(6),MSd2(6),            & 
& MSu(6),MSu2(6)

Complex(dp), Intent(in) :: cplUAhAhAh(8,8,8),cplUAhAhhh(8,8,8),cplcChaChaUAhL(5,5,8),cplcChaChaUAhR(5,5,8),      & 
& cplChiChiUAhL(10,10,8),cplChiChiUAhR(10,10,8),cplcFdFdUAhL(3,3,8),cplcFdFdUAhR(3,3,8), & 
& cplcFuFuUAhL(3,3,8),cplcFuFuUAhR(3,3,8),cplcgWmgWmUAh(8),cplcgWpCgWpCUAh(8),           & 
& cplUAhhhhh(8,8,8),cplUAhhhVZ(8,8),cplUAhHpmcHpm(8,8,8),cplUAhHpmcVWm(8,8),             & 
& cplUAhSdcSd(8,6,6),cplUAhSucSu(8,6,6),cplUAhUAhAhAh(8,8,8,8),cplUAhUAhhhhh(8,8,8,8),   & 
& cplUAhUAhHpmcHpm(8,8,8,8),cplUAhUAhSdcSd(8,8,6,6),cplUAhUAhSucSu(8,8,6,6),             & 
& cplUAhUAhcVWmVWm(8,8),cplUAhUAhVZVZ(8,8)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(8,8) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(8,8) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,MAh2(i1),MAh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhAhAh(gO1,i1,i2)
coup2 = Conjg(cplUAhAhAh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,Mhh2(i1),MAh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhAhhh(gO1,i2,i1)
coup2 = Conjg(cplUAhAhhh(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Cha], Cha 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 5
       Do i2 = 1, 5
 G0m2 = Gloop(p2,MCha2(i1),MCha2(i2)) 
B0m2 = -2._dp*MCha(i1)*MCha(i2)*B0(p2,MCha2(i1),MCha2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcChaChaUAhL(i1,i2,gO1)
coupR1 = cplcChaChaUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplcChaChaUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplcChaChaUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Chi, Chi 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 10
       Do i2 = 1, 10
 G0m2 = Gloop(p2,MChi2(i1),MChi2(i2)) 
B0m2 = -2._dp*MChi(i1)*MChi(i2)*B0(p2,MChi2(i1),MChi2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplChiChiUAhL(i1,i2,gO1)
coupR1 = cplChiChiUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplChiChiUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplChiChiUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Gloop(p2,MFd2(i1),MFd2(i2)) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*B0(p2,MFd2(i1),MFd2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcFdFdUAhL(i1,i2,gO1)
coupR1 = cplcFdFdUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFdFdUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFdFdUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Gloop(p2,MFu2(i1),MFu2(i2)) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*B0(p2,MFu2(i1),MFu2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcFuFuUAhL(i1,i2,gO1)
coupR1 = cplcFuFuUAhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFuUAhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFuUAhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgWmgWmUAh(gO1)
coup2 =  cplcgWmgWmUAh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVWm2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgWpCgWpCUAh(gO1)
coup2 =  cplcgWpCgWpCUAh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,Mhh2(i1),Mhh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhhhhh(gO1,i1,i2)
coup2 = Conjg(cplUAhhhhh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,Mhh2(i2),MVZ2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhhhVZ(gO1,i2)
coup2 =  Conjg(cplUAhhhVZ(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,MHpm2(i1),MHpm2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhHpmcHpm(gO1,i2,i1)
coup2 = Conjg(cplUAhHpmcHpm(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,MHpm2(i2),MVWm2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhHpmcVWm(gO1,i2)
coup2 =  Conjg(cplUAhHpmcVWm(gO2,i2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B0m2 = B0(p2,MSd2(i1),MSd2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhSdcSd(gO1,i2,i1)
coup2 = Conjg(cplUAhSdcSd(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B0m2 = B0(p2,MSu2(i1),MSu2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhSucSu(gO1,i2,i1)
coup2 = Conjg(cplUAhSucSu(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MAh2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhAhAh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(Mhh2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhhhhh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MHpm2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhHpmcHpm(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSd2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhSdcSd(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSu2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhSucSu(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVWm2) + 0.25_dp*RXi*A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhcVWmVWm(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVZ2) + 0.25_dp*RXi*A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUAhUAhVZVZ(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 8
  Do gO1 = gO2+1, 8
     res(gO1,gO2) = (res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopAh 
 
Subroutine OneLoopHpm(g1,g2,Ye,Te,lam,Tlam,Yv,Tv,kap,ml2,mlHd2,mHd2,mHu2,             & 
& me2,vd,vu,vL,vR,MHpm,MHpm2,MAh,MAh2,MVWm,MVWm2,MChi,MChi2,MCha,MCha2,MFu,              & 
& MFu2,MFd,MFd2,Mhh,Mhh2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcUHpm,cplAhcUHpmVWm,         & 
& cplChiChacUHpmL,cplChiChacUHpmR,cplcFuFdcUHpmL,cplcFuFdcUHpmR,cplcgZgWmcUHpm,          & 
& cplcgWmgZUHpm,cplcgWpCgZcUHpm,cplcgZgWpCUHpm,cplhhHpmcUHpm,cplhhcUHpmVWm,              & 
& cplHpmcUHpmVP,cplHpmcUHpmVZ,cplSdcUHpmcSu,cplcUHpmVPVWm,cplcUHpmVWmVZ,cplAhAhUHpmcUHpm,& 
& cplhhhhUHpmcUHpm,cplUHpmHpmcUHpmcHpm,cplUHpmSdcUHpmcSd,cplUHpmSucUHpmcSu,              & 
& cplUHpmcUHpmVPVP,cplUHpmcUHpmcVWmVWm,cplUHpmcUHpmVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MHpm(8),MHpm2(8),MAh(8),MAh2(8),MVWm,MVWm2,MChi(10),MChi2(10),MCha(5),MCha2(5),       & 
& MFu(3),MFu2(3),MFd(3),MFd2(3),Mhh(8),Mhh2(8),MVZ,MVZ2,MSu(6),MSu2(6),MSd(6),MSd2(6)

Real(dp), Intent(in) :: g1,g2,mlHd2(3),mHd2,mHu2,vd,vu,vL(3),vR(3)

Complex(dp), Intent(in) :: Ye(3,3),Te(3,3),lam(3),Tlam(3),Yv(3,3),Tv(3,3),kap(3,3,3),ml2(3,3),me2(3,3)

Complex(dp), Intent(in) :: cplAhHpmcUHpm(8,8,8),cplAhcUHpmVWm(8,8),cplChiChacUHpmL(10,5,8),cplChiChacUHpmR(10,5,8),& 
& cplcFuFdcUHpmL(3,3,8),cplcFuFdcUHpmR(3,3,8),cplcgZgWmcUHpm(8),cplcgWmgZUHpm(8),        & 
& cplcgWpCgZcUHpm(8),cplcgZgWpCUHpm(8),cplhhHpmcUHpm(8,8,8),cplhhcUHpmVWm(8,8),          & 
& cplHpmcUHpmVP(8,8),cplHpmcUHpmVZ(8,8),cplSdcUHpmcSu(6,8,6),cplcUHpmVPVWm(8),           & 
& cplcUHpmVWmVZ(8),cplAhAhUHpmcUHpm(8,8,8,8),cplhhhhUHpmcUHpm(8,8,8,8),cplUHpmHpmcUHpmcHpm(8,8,8,8),& 
& cplUHpmSdcUHpmcSd(8,6,8,6),cplUHpmSucUHpmcSu(8,6,8,6),cplUHpmcUHpmVPVP(8,8),           & 
& cplUHpmcUHpmcVWmVWm(8,8),cplUHpmcUHpmVZVZ(8,8)

Complex(dp) :: mat2a(8,8), mat2(8,8),  PiSf(8,8,8)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(8), test_m2(8),p2, test(8) 
Real(dp), Intent(out) :: mass(8), mass2(8) 
Complex(dp), Intent(out) ::  RS(8,8) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopHpm'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+mHd2
mat2a(1,1) = mat2a(1,1)+(g1**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vd**2)/8._dp
mat2a(1,1) = mat2a(1,1)-(g1**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vu**2)/8._dp
mat2a(1,1) = mat2a(1,1)+(g2**2*vd**2*RXiWm)/4._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,1) = mat2a(1,1)+(Conjg(lam(j2))*vR(j1)*vR(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,1) = mat2a(1,1)+(Conjg(Ye(j1,j3))*vL(j2)*vL(j3)*Ye(j1,j2))/2._dp
End Do 
End Do 
End Do 
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+(g2**2*vd*vu)/4._dp
mat2a(1,2) = mat2a(1,2)-(g2**2*vd*vu*RXiWm)/4._dp
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(Tlam(j1))*vR(j1))/sqrt(2._dp)
End Do 
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)-(vd*vu*Conjg(lam(j1))*lam(j1))/2._dp
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(vu*Conjg(lam(j1))*vL(j2)*Yv(j2,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j1,j3,j2))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j2,j1,j3))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,2) = mat2a(1,2)+(Conjg(lam(j1))*vR(j2)*vR(j3)*kap(j2,j3,j1))/6._dp
End Do 
End Do 
End Do 
mat2a(1,3) = 0._dp 
mat2a(1,3) = mat2a(1,3)+mlHd2(1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,3) = mat2a(1,3)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(1,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,3) = mat2a(1,3)-(vd*Conjg(Ye(j1,j2))*vL(j2)*Ye(j1,1))/2._dp
End Do 
End Do 
mat2a(1,3) = mat2a(1,3)+(g2**2*vd*vL(1))/4._dp
mat2a(1,3) = mat2a(1,3)+(g2**2*vd*RXiWm*vL(1))/4._dp
mat2a(1,4) = 0._dp 
mat2a(1,4) = mat2a(1,4)+mlHd2(2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,4) = mat2a(1,4)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,4) = mat2a(1,4)-(vd*Conjg(Ye(j1,j2))*vL(j2)*Ye(j1,2))/2._dp
End Do 
End Do 
mat2a(1,4) = mat2a(1,4)+(g2**2*vd*vL(2))/4._dp
mat2a(1,4) = mat2a(1,4)+(g2**2*vd*RXiWm*vL(2))/4._dp
mat2a(1,5) = 0._dp 
mat2a(1,5) = mat2a(1,5)+mlHd2(3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(1,5) = mat2a(1,5)-(Conjg(lam(j2))*vR(j1)*vR(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,5) = mat2a(1,5)-(vd*Conjg(Ye(j1,j2))*vL(j2)*Ye(j1,3))/2._dp
End Do 
End Do 
mat2a(1,5) = mat2a(1,5)+(g2**2*vd*vL(3))/4._dp
mat2a(1,5) = mat2a(1,5)+(g2**2*vd*RXiWm*vL(3))/4._dp
mat2a(1,6) = 0._dp 
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-((Conjg(Te(1,j1))*vL(j1))/sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,6) = mat2a(1,6)-(vu*Conjg(Ye(1,j1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(1,7) = 0._dp 
Do j1 = 1,3
mat2a(1,7) = mat2a(1,7)-((Conjg(Te(2,j1))*vL(j1))/sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,7) = mat2a(1,7)-(vu*Conjg(Ye(2,j1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(1,8) = 0._dp 
Do j1 = 1,3
mat2a(1,8) = mat2a(1,8)-((Conjg(Te(3,j1))*vL(j1))/sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(1,8) = mat2a(1,8)-(vu*Conjg(Ye(3,j1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+mHu2
mat2a(2,2) = mat2a(2,2)-(g1**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vd**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g1**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vu**2)/8._dp
mat2a(2,2) = mat2a(2,2)+(g2**2*vu**2*RXiWm)/4._dp
Do j1 = 1,3
Do j2 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(lam(j2))*vR(j1)*vR(j2)*lam(j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)-(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(g2**2*vL(j1)**2)/8._dp
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,2) = mat2a(2,2)+(Conjg(Yv(j1,j3))*vR(j2)*vR(j3)*Yv(j1,j2))/2._dp
End Do 
End Do 
End Do 
mat2a(2,3) = 0._dp 
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)+(vd*vu*Conjg(lam(j1))*Yv(1,j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-((vR(j1)*Tv(1,j1))/sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(vu*Conjg(Yv(j2,j1))*vL(j2)*Yv(1,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(1,j1))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(1,j1))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,3) = mat2a(2,3)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(1,j1))/6._dp
End Do 
End Do 
End Do 
mat2a(2,3) = mat2a(2,3)+(g2**2*vu*vL(1))/4._dp
mat2a(2,3) = mat2a(2,3)-(g2**2*vu*RXiWm*vL(1))/4._dp
mat2a(2,4) = 0._dp 
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)+(vd*vu*Conjg(lam(j1))*Yv(2,j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-((vR(j1)*Tv(2,j1))/sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(vu*Conjg(Yv(j2,j1))*vL(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(2,j1))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(2,j1))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,4) = mat2a(2,4)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(2,j1))/6._dp
End Do 
End Do 
End Do 
mat2a(2,4) = mat2a(2,4)+(g2**2*vu*vL(2))/4._dp
mat2a(2,4) = mat2a(2,4)-(g2**2*vu*RXiWm*vL(2))/4._dp
mat2a(2,5) = 0._dp 
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)+(vd*vu*Conjg(lam(j1))*Yv(3,j1))/2._dp
End Do 
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-((vR(j1)*Tv(3,j1))/sqrt(2._dp))
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(vu*Conjg(Yv(j2,j1))*vL(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j1,j3,j2))*vR(j2)*vR(j3)*Yv(3,j1))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j2,j1,j3))*vR(j2)*vR(j3)*Yv(3,j1))/6._dp
End Do 
End Do 
End Do 
Do j3 = 1,3
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,5) = mat2a(2,5)-(Conjg(kap(j2,j3,j1))*vR(j2)*vR(j3)*Yv(3,j1))/6._dp
End Do 
End Do 
End Do 
mat2a(2,5) = mat2a(2,5)+(g2**2*vu*vL(3))/4._dp
mat2a(2,5) = mat2a(2,5)-(g2**2*vu*RXiWm*vL(3))/4._dp
mat2a(2,6) = 0._dp 
Do j1 = 1,3
Do j2 = 1,3
mat2a(2,6) = mat2a(2,6)-(Conjg(Ye(1,j2))*vL(j2)*vR(j1)*lam(j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,6) = mat2a(2,6)-(vd*Conjg(Ye(1,j1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(2,7) = 0._dp 
Do j1 = 1,3
Do j2 = 1,3
mat2a(2,7) = mat2a(2,7)-(Conjg(Ye(2,j2))*vL(j2)*vR(j1)*lam(j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,7) = mat2a(2,7)-(vd*Conjg(Ye(2,j1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(2,8) = 0._dp 
Do j1 = 1,3
Do j2 = 1,3
mat2a(2,8) = mat2a(2,8)-(Conjg(Ye(3,j2))*vL(j2)*vR(j1)*lam(j1))/2._dp
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
mat2a(2,8) = mat2a(2,8)-(vd*Conjg(Ye(3,j1))*vR(j2)*Yv(j1,j2))/2._dp
End Do 
End Do 
mat2a(3,3) = 0._dp 
mat2a(3,3) = mat2a(3,3)+(g1**2*vd**2)/8._dp
mat2a(3,3) = mat2a(3,3)-(g2**2*vd**2)/8._dp
mat2a(3,3) = mat2a(3,3)-(g1**2*vu**2)/8._dp
mat2a(3,3) = mat2a(3,3)+(g2**2*vu**2)/8._dp
mat2a(3,3) = mat2a(3,3)+ml2(1,1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,3) = mat2a(3,3)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(1,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(3,3) = mat2a(3,3)+(vd**2*Conjg(Ye(j1,1))*Ye(j1,1))/2._dp
End Do 
mat2a(3,3) = mat2a(3,3)+(g2**2*vL(1)**2)/4._dp
mat2a(3,3) = mat2a(3,3)+(g2**2*RXiWm*vL(1)**2)/4._dp
mat2a(3,4) = 0._dp 
mat2a(3,4) = mat2a(3,4)+ml2(1,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,4) = mat2a(3,4)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,4) = mat2a(3,4)+(vd**2*Conjg(Ye(j1,1))*Ye(j1,2))/2._dp
End Do 
mat2a(3,4) = mat2a(3,4)+(g2**2*vL(1)*vL(2))/4._dp
mat2a(3,5) = 0._dp 
mat2a(3,5) = mat2a(3,5)+ml2(1,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(3,5) = mat2a(3,5)+(Conjg(Yv(1,j2))*vR(j1)*vR(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(3,5) = mat2a(3,5)+(vd**2*Conjg(Ye(j1,1))*Ye(j1,3))/2._dp
End Do 
mat2a(3,5) = mat2a(3,5)+(g2**2*vL(1)*vL(3))/4._dp
mat2a(3,6) = 0._dp 
mat2a(3,6) = mat2a(3,6)+(vd*Conjg(Te(1,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,6) = mat2a(3,6)-(vu*Conjg(Ye(1,1))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(3,7) = 0._dp 
mat2a(3,7) = mat2a(3,7)+(vd*Conjg(Te(2,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,7) = mat2a(3,7)-(vu*Conjg(Ye(2,1))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(3,8) = 0._dp 
mat2a(3,8) = mat2a(3,8)+(vd*Conjg(Te(3,1)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(3,8) = mat2a(3,8)-(vu*Conjg(Ye(3,1))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(4,4) = 0._dp 
mat2a(4,4) = mat2a(4,4)+(g1**2*vd**2)/8._dp
mat2a(4,4) = mat2a(4,4)-(g2**2*vd**2)/8._dp
mat2a(4,4) = mat2a(4,4)-(g1**2*vu**2)/8._dp
mat2a(4,4) = mat2a(4,4)+(g2**2*vu**2)/8._dp
mat2a(4,4) = mat2a(4,4)+ml2(2,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,4) = mat2a(4,4)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(2,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(4,4) = mat2a(4,4)+(vd**2*Conjg(Ye(j1,2))*Ye(j1,2))/2._dp
End Do 
mat2a(4,4) = mat2a(4,4)+(g2**2*vL(2)**2)/4._dp
mat2a(4,4) = mat2a(4,4)+(g2**2*RXiWm*vL(2)**2)/4._dp
mat2a(4,5) = 0._dp 
mat2a(4,5) = mat2a(4,5)+ml2(2,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(4,5) = mat2a(4,5)+(Conjg(Yv(2,j2))*vR(j1)*vR(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(4,5) = mat2a(4,5)+(vd**2*Conjg(Ye(j1,2))*Ye(j1,3))/2._dp
End Do 
mat2a(4,5) = mat2a(4,5)+(g2**2*vL(2)*vL(3))/4._dp
mat2a(4,6) = 0._dp 
mat2a(4,6) = mat2a(4,6)+(vd*Conjg(Te(1,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(4,6) = mat2a(4,6)-(vu*Conjg(Ye(1,2))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(4,7) = 0._dp 
mat2a(4,7) = mat2a(4,7)+(vd*Conjg(Te(2,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(4,7) = mat2a(4,7)-(vu*Conjg(Ye(2,2))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(4,8) = 0._dp 
mat2a(4,8) = mat2a(4,8)+(vd*Conjg(Te(3,2)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(4,8) = mat2a(4,8)-(vu*Conjg(Ye(3,2))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(5,5) = 0._dp 
mat2a(5,5) = mat2a(5,5)+(g1**2*vd**2)/8._dp
mat2a(5,5) = mat2a(5,5)-(g2**2*vd**2)/8._dp
mat2a(5,5) = mat2a(5,5)-(g1**2*vu**2)/8._dp
mat2a(5,5) = mat2a(5,5)+(g2**2*vu**2)/8._dp
mat2a(5,5) = mat2a(5,5)+ml2(3,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(5,5) = mat2a(5,5)+(Conjg(Yv(3,j2))*vR(j1)*vR(j2)*Yv(3,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(g1**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)-(g2**2*vL(j1)**2)/8._dp
End Do 
Do j1 = 1,3
mat2a(5,5) = mat2a(5,5)+(vd**2*Conjg(Ye(j1,3))*Ye(j1,3))/2._dp
End Do 
mat2a(5,5) = mat2a(5,5)+(g2**2*vL(3)**2)/4._dp
mat2a(5,5) = mat2a(5,5)+(g2**2*RXiWm*vL(3)**2)/4._dp
mat2a(5,6) = 0._dp 
mat2a(5,6) = mat2a(5,6)+(vd*Conjg(Te(1,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(5,6) = mat2a(5,6)-(vu*Conjg(Ye(1,3))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(5,7) = 0._dp 
mat2a(5,7) = mat2a(5,7)+(vd*Conjg(Te(2,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(5,7) = mat2a(5,7)-(vu*Conjg(Ye(2,3))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(5,8) = 0._dp 
mat2a(5,8) = mat2a(5,8)+(vd*Conjg(Te(3,3)))/sqrt(2._dp)
Do j1 = 1,3
mat2a(5,8) = mat2a(5,8)-(vu*Conjg(Ye(3,3))*vR(j1)*lam(j1))/2._dp
End Do 
mat2a(6,6) = 0._dp 
mat2a(6,6) = mat2a(6,6)-(g1**2*vd**2)/4._dp
mat2a(6,6) = mat2a(6,6)+(g1**2*vu**2)/4._dp
mat2a(6,6) = mat2a(6,6)+me2(1,1)
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,6) = mat2a(6,6)+(Conjg(Ye(1,j2))*vL(j1)*vL(j2)*Ye(1,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)-(g1**2*vL(j1)**2)/4._dp
End Do 
Do j1 = 1,3
mat2a(6,6) = mat2a(6,6)+(vd**2*Conjg(Ye(1,j1))*Ye(1,j1))/2._dp
End Do 
mat2a(6,7) = 0._dp 
mat2a(6,7) = mat2a(6,7)+me2(1,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,7) = mat2a(6,7)+(Conjg(Ye(2,j2))*vL(j1)*vL(j2)*Ye(1,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,7) = mat2a(6,7)+(vd**2*Conjg(Ye(2,j1))*Ye(1,j1))/2._dp
End Do 
mat2a(6,8) = 0._dp 
mat2a(6,8) = mat2a(6,8)+me2(1,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(6,8) = mat2a(6,8)+(Conjg(Ye(3,j2))*vL(j1)*vL(j2)*Ye(1,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(6,8) = mat2a(6,8)+(vd**2*Conjg(Ye(3,j1))*Ye(1,j1))/2._dp
End Do 
mat2a(7,7) = 0._dp 
mat2a(7,7) = mat2a(7,7)-(g1**2*vd**2)/4._dp
mat2a(7,7) = mat2a(7,7)+(g1**2*vu**2)/4._dp
mat2a(7,7) = mat2a(7,7)+me2(2,2)
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,7) = mat2a(7,7)+(Conjg(Ye(2,j2))*vL(j1)*vL(j2)*Ye(2,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)-(g1**2*vL(j1)**2)/4._dp
End Do 
Do j1 = 1,3
mat2a(7,7) = mat2a(7,7)+(vd**2*Conjg(Ye(2,j1))*Ye(2,j1))/2._dp
End Do 
mat2a(7,8) = 0._dp 
mat2a(7,8) = mat2a(7,8)+me2(2,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(7,8) = mat2a(7,8)+(Conjg(Ye(3,j2))*vL(j1)*vL(j2)*Ye(2,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(7,8) = mat2a(7,8)+(vd**2*Conjg(Ye(3,j1))*Ye(2,j1))/2._dp
End Do 
mat2a(8,8) = 0._dp 
mat2a(8,8) = mat2a(8,8)-(g1**2*vd**2)/4._dp
mat2a(8,8) = mat2a(8,8)+(g1**2*vu**2)/4._dp
mat2a(8,8) = mat2a(8,8)+me2(3,3)
Do j1 = 1,3
Do j2 = 1,3
mat2a(8,8) = mat2a(8,8)+(Conjg(Ye(3,j2))*vL(j1)*vL(j2)*Ye(3,j1))/2._dp
End Do 
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)-(g1**2*vL(j1)**2)/4._dp
End Do 
Do j1 = 1,3
mat2a(8,8) = mat2a(8,8)+(vd**2*Conjg(Ye(3,j1))*Ye(3,j1))/2._dp
End Do 

 
 Do i1=2,8
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = Conjg(mat2a(i2,i1)) 
  End do 
End do 

 
Do i1=1,8
PiSf(i1,:,:) = ZeroC 
p2 = MHpm2(i1)
If (i1.eq.1) p2 = 0._dp 
Call Pi1LoopHpm(p2,MHpm,MHpm2,MAh,MAh2,MVWm,MVWm2,MChi,MChi2,MCha,MCha2,              & 
& MFu,MFu2,MFd,MFd2,Mhh,Mhh2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcUHpm,cplAhcUHpmVWm,     & 
& cplChiChacUHpmL,cplChiChacUHpmR,cplcFuFdcUHpmL,cplcFuFdcUHpmR,cplcgZgWmcUHpm,          & 
& cplcgWmgZUHpm,cplcgWpCgZcUHpm,cplcgZgWpCUHpm,cplhhHpmcUHpm,cplhhcUHpmVWm,              & 
& cplHpmcUHpmVP,cplHpmcUHpmVZ,cplSdcUHpmcSu,cplcUHpmVPVWm,cplcUHpmVWmVZ,cplAhAhUHpmcUHpm,& 
& cplhhhhUHpmcUHpm,cplUHpmHpmcUHpmcHpm,cplUHpmSdcUHpmcSd,cplUHpmSucUHpmcSu,              & 
& cplUHpmcUHpmVPVP,cplUHpmcUHpmcVWmVWm,cplUHpmcUHpmVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=8,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
   Call TerminateProgram 
   return
   endif
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,8
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
   If ((i1.Gt.1).or.(Abs(mass2(i1)).gt.MaxVal(Abs(mass2)))) Then 
    ! Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
    ! Write(*,*) 'in the calculation of the masses' 
    ! Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram
   return 
   End If 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,8
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1LoopHpm(p2,MHpm,MHpm2,MAh,MAh2,MVWm,MVWm2,MChi,MChi2,MCha,MCha2,              & 
& MFu,MFu2,MFd,MFd2,Mhh,Mhh2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcUHpm,cplAhcUHpmVWm,     & 
& cplChiChacUHpmL,cplChiChacUHpmR,cplcFuFdcUHpmL,cplcFuFdcUHpmR,cplcgZgWmcUHpm,          & 
& cplcgWmgZUHpm,cplcgWpCgZcUHpm,cplcgZgWpCUHpm,cplhhHpmcUHpm,cplhhcUHpmVWm,              & 
& cplHpmcUHpmVP,cplHpmcUHpmVZ,cplSdcUHpmcSu,cplcUHpmVPVWm,cplcUHpmVWmVZ,cplAhAhUHpmcUHpm,& 
& cplhhhhUHpmcUHpm,cplUHpmHpmcUHpmcHpm,cplUHpmSdcUHpmcSd,cplUHpmSucUHpmcSu,              & 
& cplUHpmcUHpmVPVP,cplUHpmcUHpmcVWmVWm,cplUHpmcUHpmVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=8,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,8
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     !Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     !Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     !Write(*,*) 'generation: ',i1 
     !Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  !Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopHpm
 
 
Subroutine Pi1LoopHpm(p2,MHpm,MHpm2,MAh,MAh2,MVWm,MVWm2,MChi,MChi2,MCha,              & 
& MCha2,MFu,MFu2,MFd,MFd2,Mhh,Mhh2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcUHpm,             & 
& cplAhcUHpmVWm,cplChiChacUHpmL,cplChiChacUHpmR,cplcFuFdcUHpmL,cplcFuFdcUHpmR,           & 
& cplcgZgWmcUHpm,cplcgWmgZUHpm,cplcgWpCgZcUHpm,cplcgZgWpCUHpm,cplhhHpmcUHpm,             & 
& cplhhcUHpmVWm,cplHpmcUHpmVP,cplHpmcUHpmVZ,cplSdcUHpmcSu,cplcUHpmVPVWm,cplcUHpmVWmVZ,   & 
& cplAhAhUHpmcUHpm,cplhhhhUHpmcUHpm,cplUHpmHpmcUHpmcHpm,cplUHpmSdcUHpmcSd,               & 
& cplUHpmSucUHpmcSu,cplUHpmcUHpmVPVP,cplUHpmcUHpmcVWmVWm,cplUHpmcUHpmVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHpm(8),MHpm2(8),MAh(8),MAh2(8),MVWm,MVWm2,MChi(10),MChi2(10),MCha(5),MCha2(5),       & 
& MFu(3),MFu2(3),MFd(3),MFd2(3),Mhh(8),Mhh2(8),MVZ,MVZ2,MSu(6),MSu2(6),MSd(6),MSd2(6)

Complex(dp), Intent(in) :: cplAhHpmcUHpm(8,8,8),cplAhcUHpmVWm(8,8),cplChiChacUHpmL(10,5,8),cplChiChacUHpmR(10,5,8),& 
& cplcFuFdcUHpmL(3,3,8),cplcFuFdcUHpmR(3,3,8),cplcgZgWmcUHpm(8),cplcgWmgZUHpm(8),        & 
& cplcgWpCgZcUHpm(8),cplcgZgWpCUHpm(8),cplhhHpmcUHpm(8,8,8),cplhhcUHpmVWm(8,8),          & 
& cplHpmcUHpmVP(8,8),cplHpmcUHpmVZ(8,8),cplSdcUHpmcSu(6,8,6),cplcUHpmVPVWm(8),           & 
& cplcUHpmVWmVZ(8),cplAhAhUHpmcUHpm(8,8,8,8),cplhhhhUHpmcUHpm(8,8,8,8),cplUHpmHpmcUHpmcHpm(8,8,8,8),& 
& cplUHpmSdcUHpmcSd(8,6,8,6),cplUHpmSucUHpmcSu(8,6,8,6),cplUHpmcUHpmVPVP(8,8),           & 
& cplUHpmcUHpmcVWmVWm(8,8),cplUHpmcUHpmVZVZ(8,8)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(8,8) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(8,8) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Hpm, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,MHpm2(i1),MAh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhHpmcUHpm(i2,i1,gO1)
coup2 = Conjg(cplAhHpmcUHpm(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, Ah 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,MAh2(i2),MVWm2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhcUHpmVWm(i2,gO1)
coup2 =  Conjg(cplAhcUHpmVWm(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! Chi, Cha 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 10
       Do i2 = 1, 5
 G0m2 = Gloop(p2,MChi2(i1),MCha2(i2)) 
B0m2 = -2._dp*MChi(i1)*MCha(i2)*B0(p2,MChi2(i1),MCha2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplChiChacUHpmL(i1,i2,gO1)
coupR1 = cplChiChacUHpmR(i1,i2,gO1)
coupL2 =  Conjg(cplChiChacUHpmL(i1,i2,gO2))
coupR2 =  Conjg(cplChiChacUHpmR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Gloop(p2,MFu2(i1),MFd2(i2)) 
B0m2 = -2._dp*MFu(i1)*MFd(i2)*B0(p2,MFu2(i1),MFd2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coupL1 = cplcFuFdcUHpmL(i1,i2,gO1)
coupR1 = cplcFuFdcUHpmR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFdcUHpmL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFdcUHpmR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gZ], gWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVWm2*RXi,MVZ2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgZgWmcUHpm(gO1)
coup2 =  cplcgWmgZUHpm(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gZ 
!------------------------ 
sumI = 0._dp 
 
F0m2 = -Real(B0(p2,MVZ2*RXi,MVWm2*RXi),dp) 
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcgWpCgZcUHpm(gO1)
coup2 =  cplcgZgWpCUHpm(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! Hpm, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B0m2 = B0(p2,MHpm2(i1),Mhh2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplhhHpmcUHpm(i2,i1,gO1)
coup2 = Conjg(cplhhHpmcUHpm(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,Mhh2(i2),MVWm2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplhhcUHpmVWm(i2,gO1)
coup2 =  Conjg(cplhhcUHpmVWm(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VP, Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,MHpm2(i2),0._dp) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplHpmcUHpmVP(i2,gO1)
coup2 =  Conjg(cplHpmcUHpmVP(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 F0m2 = FloopRXi(p2,MHpm2(i2),MVZ2) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplHpmcUHpmVZ(i2,gO1)
coup2 =  Conjg(cplHpmcUHpmVZ(i2,gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Su], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B0m2 = B0(p2,MSu2(i1),MSd2(i2)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplSdcUHpmcSu(i2,gO1,i1)
coup2 = Conjg(cplSdcUHpmcSu(i2,gO2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, VP 
!------------------------ 
sumI = 0._dp 
 
F0m2 = SVVloop(p2,0._dp,MVWm2)   
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcUHpmVPVWm(gO1)
coup2 =  Conjg(cplcUHpmVPVWm(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VWm 
!------------------------ 
sumI = 0._dp 
 
F0m2 = SVVloop(p2,MVWm2,MVZ2)   
 Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplcUHpmVWmVZ(gO1)
coup2 =  Conjg(cplcUHpmVWmVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MAh2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplAhAhUHpmcUHpm(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(Mhh2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplhhhhUHpmcUHpm(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
 A0m2 = A0(MHpm2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUHpmHpmcUHpmcHpm(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSd2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUHpmSdcUHpmcSd(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
 A0m2 = A0(MSu2(i1)) 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUHpmSucUHpmcSu(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 !------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
!------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVWm2) + 0.25_dp*RXi*A0(MVWm2*RXi) - 0.5_dp*MVWm2*rMS 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUHpmcUHpmcVWmVWm(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
A0m2 = 0.75_dp*A0(MVZ2) + 0.25_dp*RXi*A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 8
  Do gO2 = gO1, 8
coup1 = cplUHpmcUHpmVZVZ(gO2,gO1)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  


Do gO2 = 1, 8
  Do gO1 = gO2+1, 8
     res(gO1,gO2) = Conjg(res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopHpm 
 
Subroutine OneLoopChi(g1,g2,lam,Yv,kap,M1,M2,vd,vu,vL,vR,MChi,MChi2,MAh,              & 
& MAh2,MHpm,MHpm2,MCha,MCha2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MSd,MSd2,MFd,MFd2,             & 
& MSu,MSu2,MFu,MFu2,cplUChiChiAhL,cplUChiChiAhR,cplUChiChacHpmL,cplUChiChacHpmR,         & 
& cplUChiChacVWmL,cplUChiChacVWmR,cplUChiChihhL,cplUChiChihhR,cplUChiChiVZL,             & 
& cplUChiChiVZR,cplUChiFdcSdL,cplUChiFdcSdR,cplUChiFucSuL,cplUChiFucSuR,delta,           & 
& MChi_1L,MChi2_1L,ZN_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MChi(10),MChi2(10),MAh(8),MAh2(8),MHpm(8),MHpm2(8),MCha(5),MCha2(5),MVWm,             & 
& MVWm2,Mhh(8),Mhh2(8),MVZ,MVZ2,MSd(6),MSd2(6),MFd(3),MFd2(3),MSu(6),MSu2(6),            & 
& MFu(3),MFu2(3)

Real(dp), Intent(in) :: g1,g2,vd,vu,vL(3),vR(3)

Complex(dp), Intent(in) :: lam(3),Yv(3,3),kap(3,3,3),M1,M2

Complex(dp), Intent(in) :: cplUChiChiAhL(10,10,8),cplUChiChiAhR(10,10,8),cplUChiChacHpmL(10,5,8),cplUChiChacHpmR(10,5,8),& 
& cplUChiChacVWmL(10,5),cplUChiChacVWmR(10,5),cplUChiChihhL(10,10,8),cplUChiChihhR(10,10,8),& 
& cplUChiChiVZL(10,10),cplUChiChiVZR(10,10),cplUChiFdcSdL(10,3,6),cplUChiFdcSdR(10,3,6), & 
& cplUChiFucSuL(10,3,6),cplUChiFucSuR(10,3,6)

Complex(dp) :: mat1a(10,10), mat1(10,10), mat2(10,10) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1, j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(10), test_m2(10),p2 
Real(dp), Intent(out) :: MChi_1L(10),MChi2_1L(10) 
Complex(dp), Intent(out) ::  ZN_1L(10,10) 
Real(dp) :: MChi_t(10),MChi2_t(10) 
Complex(dp) ::  ZN_t(10,10) 
Complex(dp) ::  phaseM, E10(10), sigL(10,10), sigR(10,10), sigS(10,10) 
Real(dp) :: ZNa(10,10), test(2), eig(10) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMChi'
 
mat1a(1,1) = 0._dp 
mat1a(1,2) = 0._dp 
mat1a(1,3) = 0._dp 
mat1a(1,4) = 0._dp 
mat1a(1,4) = mat1a(1,4)-(g1*vL(1))/2._dp
mat1a(1,5) = 0._dp 
mat1a(1,5) = mat1a(1,5)+(g2*vL(1))/2._dp
mat1a(1,6) = 0._dp 
mat1a(1,7) = 0._dp 
Do j1 = 1,3
mat1a(1,7) = mat1a(1,7)+(vR(j1)*Yv(1,j1))/sqrt(2._dp)
End Do 
mat1a(1,8) = 0._dp 
mat1a(1,8) = mat1a(1,8)+(vu*Yv(1,1))/sqrt(2._dp)
mat1a(1,9) = 0._dp 
mat1a(1,9) = mat1a(1,9)+(vu*Yv(1,2))/sqrt(2._dp)
mat1a(1,10) = 0._dp 
mat1a(1,10) = mat1a(1,10)+(vu*Yv(1,3))/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,2) = 0._dp 
mat1a(2,3) = 0._dp 
mat1a(2,4) = 0._dp 
mat1a(2,4) = mat1a(2,4)-(g1*vL(2))/2._dp
mat1a(2,5) = 0._dp 
mat1a(2,5) = mat1a(2,5)+(g2*vL(2))/2._dp
mat1a(2,6) = 0._dp 
mat1a(2,7) = 0._dp 
Do j1 = 1,3
mat1a(2,7) = mat1a(2,7)+(vR(j1)*Yv(2,j1))/sqrt(2._dp)
End Do 
mat1a(2,8) = 0._dp 
mat1a(2,8) = mat1a(2,8)+(vu*Yv(2,1))/sqrt(2._dp)
mat1a(2,9) = 0._dp 
mat1a(2,9) = mat1a(2,9)+(vu*Yv(2,2))/sqrt(2._dp)
mat1a(2,10) = 0._dp 
mat1a(2,10) = mat1a(2,10)+(vu*Yv(2,3))/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,2) = 0._dp 
mat1a(3,3) = 0._dp 
mat1a(3,4) = 0._dp 
mat1a(3,4) = mat1a(3,4)-(g1*vL(3))/2._dp
mat1a(3,5) = 0._dp 
mat1a(3,5) = mat1a(3,5)+(g2*vL(3))/2._dp
mat1a(3,6) = 0._dp 
mat1a(3,7) = 0._dp 
Do j1 = 1,3
mat1a(3,7) = mat1a(3,7)+(vR(j1)*Yv(3,j1))/sqrt(2._dp)
End Do 
mat1a(3,8) = 0._dp 
mat1a(3,8) = mat1a(3,8)+(vu*Yv(3,1))/sqrt(2._dp)
mat1a(3,9) = 0._dp 
mat1a(3,9) = mat1a(3,9)+(vu*Yv(3,2))/sqrt(2._dp)
mat1a(3,10) = 0._dp 
mat1a(3,10) = mat1a(3,10)+(vu*Yv(3,3))/sqrt(2._dp)
mat1a(4,1) = 0._dp 
mat1a(4,1) = mat1a(4,1)-(g1*vL(1))/2._dp
mat1a(4,2) = 0._dp 
mat1a(4,2) = mat1a(4,2)-(g1*vL(2))/2._dp
mat1a(4,3) = 0._dp 
mat1a(4,3) = mat1a(4,3)-(g1*vL(3))/2._dp
mat1a(4,4) = 0._dp 
mat1a(4,4) = mat1a(4,4)+M1
mat1a(4,5) = 0._dp 
mat1a(4,6) = 0._dp 
mat1a(4,6) = mat1a(4,6)-(g1*vd)/2._dp
mat1a(4,7) = 0._dp 
mat1a(4,7) = mat1a(4,7)+(g1*vu)/2._dp
mat1a(4,8) = 0._dp 
mat1a(4,9) = 0._dp 
mat1a(4,10) = 0._dp 
mat1a(5,1) = 0._dp 
mat1a(5,1) = mat1a(5,1)+(g2*vL(1))/2._dp
mat1a(5,2) = 0._dp 
mat1a(5,2) = mat1a(5,2)+(g2*vL(2))/2._dp
mat1a(5,3) = 0._dp 
mat1a(5,3) = mat1a(5,3)+(g2*vL(3))/2._dp
mat1a(5,4) = 0._dp 
mat1a(5,5) = 0._dp 
mat1a(5,5) = mat1a(5,5)+M2
mat1a(5,6) = 0._dp 
mat1a(5,6) = mat1a(5,6)+(g2*vd)/2._dp
mat1a(5,7) = 0._dp 
mat1a(5,7) = mat1a(5,7)-(g2*vu)/2._dp
mat1a(5,8) = 0._dp 
mat1a(5,9) = 0._dp 
mat1a(5,10) = 0._dp 
mat1a(6,1) = 0._dp 
mat1a(6,2) = 0._dp 
mat1a(6,3) = 0._dp 
mat1a(6,4) = 0._dp 
mat1a(6,4) = mat1a(6,4)-(g1*vd)/2._dp
mat1a(6,5) = 0._dp 
mat1a(6,5) = mat1a(6,5)+(g2*vd)/2._dp
mat1a(6,6) = 0._dp 
mat1a(6,7) = 0._dp 
Do j1 = 1,3
mat1a(6,7) = mat1a(6,7)-((vR(j1)*lam(j1))/sqrt(2._dp))
End Do 
mat1a(6,8) = 0._dp 
mat1a(6,8) = mat1a(6,8)-((vu*lam(1))/sqrt(2._dp))
mat1a(6,9) = 0._dp 
mat1a(6,9) = mat1a(6,9)-((vu*lam(2))/sqrt(2._dp))
mat1a(6,10) = 0._dp 
mat1a(6,10) = mat1a(6,10)-((vu*lam(3))/sqrt(2._dp))
mat1a(7,1) = 0._dp 
Do j1 = 1,3
mat1a(7,1) = mat1a(7,1)+(vR(j1)*Yv(1,j1))/sqrt(2._dp)
End Do 
mat1a(7,2) = 0._dp 
Do j1 = 1,3
mat1a(7,2) = mat1a(7,2)+(vR(j1)*Yv(2,j1))/sqrt(2._dp)
End Do 
mat1a(7,3) = 0._dp 
Do j1 = 1,3
mat1a(7,3) = mat1a(7,3)+(vR(j1)*Yv(3,j1))/sqrt(2._dp)
End Do 
mat1a(7,4) = 0._dp 
mat1a(7,4) = mat1a(7,4)+(g1*vu)/2._dp
mat1a(7,5) = 0._dp 
mat1a(7,5) = mat1a(7,5)-(g2*vu)/2._dp
mat1a(7,6) = 0._dp 
Do j1 = 1,3
mat1a(7,6) = mat1a(7,6)-((vR(j1)*lam(j1))/sqrt(2._dp))
End Do 
mat1a(7,7) = 0._dp 
mat1a(7,8) = 0._dp 
Do j1 = 1,3
mat1a(7,8) = mat1a(7,8)+(vL(j1)*Yv(j1,1))/sqrt(2._dp)
End Do 
mat1a(7,8) = mat1a(7,8)-((vd*lam(1))/sqrt(2._dp))
mat1a(7,9) = 0._dp 
Do j1 = 1,3
mat1a(7,9) = mat1a(7,9)+(vL(j1)*Yv(j1,2))/sqrt(2._dp)
End Do 
mat1a(7,9) = mat1a(7,9)-((vd*lam(2))/sqrt(2._dp))
mat1a(7,10) = 0._dp 
Do j1 = 1,3
mat1a(7,10) = mat1a(7,10)+(vL(j1)*Yv(j1,3))/sqrt(2._dp)
End Do 
mat1a(7,10) = mat1a(7,10)-((vd*lam(3))/sqrt(2._dp))
mat1a(8,1) = 0._dp 
mat1a(8,1) = mat1a(8,1)+(vu*Yv(1,1))/sqrt(2._dp)
mat1a(8,2) = 0._dp 
mat1a(8,2) = mat1a(8,2)+(vu*Yv(2,1))/sqrt(2._dp)
mat1a(8,3) = 0._dp 
mat1a(8,3) = mat1a(8,3)+(vu*Yv(3,1))/sqrt(2._dp)
mat1a(8,4) = 0._dp 
mat1a(8,5) = 0._dp 
mat1a(8,6) = 0._dp 
mat1a(8,6) = mat1a(8,6)-((vu*lam(1))/sqrt(2._dp))
mat1a(8,7) = 0._dp 
Do j1 = 1,3
mat1a(8,7) = mat1a(8,7)+(vL(j1)*Yv(j1,1))/sqrt(2._dp)
End Do 
mat1a(8,7) = mat1a(8,7)-((vd*lam(1))/sqrt(2._dp))
mat1a(8,8) = 0._dp 
Do j1 = 1,3
mat1a(8,8) = mat1a(8,8)+(sqrt(2._dp)*vR(j1)*kap(1,1,j1))/3._dp
End Do 
Do j1 = 1,3
mat1a(8,8) = mat1a(8,8)+(sqrt(2._dp)*vR(j1)*kap(1,j1,1))/3._dp
End Do 
Do j1 = 1,3
mat1a(8,8) = mat1a(8,8)+(sqrt(2._dp)*vR(j1)*kap(j1,1,1))/3._dp
End Do 
mat1a(8,9) = 0._dp 
Do j1 = 1,3
mat1a(8,9) = mat1a(8,9)+(vR(j1)*kap(1,2,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,9) = mat1a(8,9)+(vR(j1)*kap(1,j1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,9) = mat1a(8,9)+(vR(j1)*kap(2,1,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,9) = mat1a(8,9)+(vR(j1)*kap(2,j1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,9) = mat1a(8,9)+(vR(j1)*kap(j1,1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,9) = mat1a(8,9)+(vR(j1)*kap(j1,2,1))/(3._dp*sqrt(2._dp))
End Do 
mat1a(8,10) = 0._dp 
Do j1 = 1,3
mat1a(8,10) = mat1a(8,10)+(vR(j1)*kap(1,3,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,10) = mat1a(8,10)+(vR(j1)*kap(1,j1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,10) = mat1a(8,10)+(vR(j1)*kap(3,1,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,10) = mat1a(8,10)+(vR(j1)*kap(3,j1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,10) = mat1a(8,10)+(vR(j1)*kap(j1,1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(8,10) = mat1a(8,10)+(vR(j1)*kap(j1,3,1))/(3._dp*sqrt(2._dp))
End Do 
mat1a(9,1) = 0._dp 
mat1a(9,1) = mat1a(9,1)+(vu*Yv(1,2))/sqrt(2._dp)
mat1a(9,2) = 0._dp 
mat1a(9,2) = mat1a(9,2)+(vu*Yv(2,2))/sqrt(2._dp)
mat1a(9,3) = 0._dp 
mat1a(9,3) = mat1a(9,3)+(vu*Yv(3,2))/sqrt(2._dp)
mat1a(9,4) = 0._dp 
mat1a(9,5) = 0._dp 
mat1a(9,6) = 0._dp 
mat1a(9,6) = mat1a(9,6)-((vu*lam(2))/sqrt(2._dp))
mat1a(9,7) = 0._dp 
Do j1 = 1,3
mat1a(9,7) = mat1a(9,7)+(vL(j1)*Yv(j1,2))/sqrt(2._dp)
End Do 
mat1a(9,7) = mat1a(9,7)-((vd*lam(2))/sqrt(2._dp))
mat1a(9,8) = 0._dp 
Do j1 = 1,3
mat1a(9,8) = mat1a(9,8)+(vR(j1)*kap(1,2,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,8) = mat1a(9,8)+(vR(j1)*kap(1,j1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,8) = mat1a(9,8)+(vR(j1)*kap(2,1,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,8) = mat1a(9,8)+(vR(j1)*kap(2,j1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,8) = mat1a(9,8)+(vR(j1)*kap(j1,1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,8) = mat1a(9,8)+(vR(j1)*kap(j1,2,1))/(3._dp*sqrt(2._dp))
End Do 
mat1a(9,9) = 0._dp 
Do j1 = 1,3
mat1a(9,9) = mat1a(9,9)+(sqrt(2._dp)*vR(j1)*kap(2,2,j1))/3._dp
End Do 
Do j1 = 1,3
mat1a(9,9) = mat1a(9,9)+(sqrt(2._dp)*vR(j1)*kap(2,j1,2))/3._dp
End Do 
Do j1 = 1,3
mat1a(9,9) = mat1a(9,9)+(sqrt(2._dp)*vR(j1)*kap(j1,2,2))/3._dp
End Do 
mat1a(9,10) = 0._dp 
Do j1 = 1,3
mat1a(9,10) = mat1a(9,10)+(vR(j1)*kap(2,3,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,10) = mat1a(9,10)+(vR(j1)*kap(2,j1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,10) = mat1a(9,10)+(vR(j1)*kap(3,2,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,10) = mat1a(9,10)+(vR(j1)*kap(3,j1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,10) = mat1a(9,10)+(vR(j1)*kap(j1,2,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(9,10) = mat1a(9,10)+(vR(j1)*kap(j1,3,2))/(3._dp*sqrt(2._dp))
End Do 
mat1a(10,1) = 0._dp 
mat1a(10,1) = mat1a(10,1)+(vu*Yv(1,3))/sqrt(2._dp)
mat1a(10,2) = 0._dp 
mat1a(10,2) = mat1a(10,2)+(vu*Yv(2,3))/sqrt(2._dp)
mat1a(10,3) = 0._dp 
mat1a(10,3) = mat1a(10,3)+(vu*Yv(3,3))/sqrt(2._dp)
mat1a(10,4) = 0._dp 
mat1a(10,5) = 0._dp 
mat1a(10,6) = 0._dp 
mat1a(10,6) = mat1a(10,6)-((vu*lam(3))/sqrt(2._dp))
mat1a(10,7) = 0._dp 
Do j1 = 1,3
mat1a(10,7) = mat1a(10,7)+(vL(j1)*Yv(j1,3))/sqrt(2._dp)
End Do 
mat1a(10,7) = mat1a(10,7)-((vd*lam(3))/sqrt(2._dp))
mat1a(10,8) = 0._dp 
Do j1 = 1,3
mat1a(10,8) = mat1a(10,8)+(vR(j1)*kap(1,3,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,8) = mat1a(10,8)+(vR(j1)*kap(1,j1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,8) = mat1a(10,8)+(vR(j1)*kap(3,1,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,8) = mat1a(10,8)+(vR(j1)*kap(3,j1,1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,8) = mat1a(10,8)+(vR(j1)*kap(j1,1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,8) = mat1a(10,8)+(vR(j1)*kap(j1,3,1))/(3._dp*sqrt(2._dp))
End Do 
mat1a(10,9) = 0._dp 
Do j1 = 1,3
mat1a(10,9) = mat1a(10,9)+(vR(j1)*kap(2,3,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,9) = mat1a(10,9)+(vR(j1)*kap(2,j1,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,9) = mat1a(10,9)+(vR(j1)*kap(3,2,j1))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,9) = mat1a(10,9)+(vR(j1)*kap(3,j1,2))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,9) = mat1a(10,9)+(vR(j1)*kap(j1,2,3))/(3._dp*sqrt(2._dp))
End Do 
Do j1 = 1,3
mat1a(10,9) = mat1a(10,9)+(vR(j1)*kap(j1,3,2))/(3._dp*sqrt(2._dp))
End Do 
mat1a(10,10) = 0._dp 
Do j1 = 1,3
mat1a(10,10) = mat1a(10,10)+(sqrt(2._dp)*vR(j1)*kap(3,3,j1))/3._dp
End Do 
Do j1 = 1,3
mat1a(10,10) = mat1a(10,10)+(sqrt(2._dp)*vR(j1)*kap(3,j1,3))/3._dp
End Do 
Do j1 = 1,3
mat1a(10,10) = mat1a(10,10)+(sqrt(2._dp)*vR(j1)*kap(j1,3,3))/3._dp
End Do 

 
 Do il=10,1,-1
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MChi2(il)
Call Sigma1LoopChi(p2,MChi,MChi2,MAh,MAh2,MHpm,MHpm2,MCha,MCha2,MVWm,MVWm2,           & 
& Mhh,Mhh2,MVZ,MVZ2,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,cplUChiChiAhL,cplUChiChiAhR,     & 
& cplUChiChacHpmL,cplUChiChacHpmR,cplUChiChacVWmL,cplUChiChacVWmR,cplUChiChihhL,         & 
& cplUChiChihhR,cplUChiChiVZL,cplUChiChiVZR,cplUChiFdcSdL,cplUChiFdcSdR,cplUChiFucSuL,   & 
& cplUChiFucSuR,sigL,sigR,sigS)

mat1 = mat1a - 0.5_dp*(SigS + Transpose(SigS) + & 
      & MatMul(Transpose(SigL),mat1a) + MatMul(SigR,mat1a) + & 
      & MatMul(mat1a,Transpose(SigR)) + MatMul(mat1a,SigL)) 

!rruiz 
call chop(mat1)
If (ForceRealMatrices) mat1 = Real(mat1,dp) 
If (Maxval(Abs(Aimag(mat1))).Eq.0._dp) Then 
Call EigenSystemQP(Real(mat1,dp),Eig,ZNa,ierr,test) 
 
   Do i1=1,10
   If (Eig(i1).Lt.0._dp) Then 
    MChi_t(i1) = - Eig(i1) 
    ZN_1L(i1,:) = (0._dp,1._dp)*ZNa(i1,:) 
   Else 
    MChi_t(i1) = Eig(i1) 
    ZN_1L(i1,:) = ZNa(i1,:)
    End If 
   End Do 
 
Do i1=1,9
  Do i2=i1+1,10
    If (Abs(MChi_t(i1)).Gt.Abs(MChi_t(i2))) Then 
      Eig(1) = MChi_t(i1) 
      MChi_t(i1) = MChi_t(i2) 
      MChi_t(i2) = Eig(1) 
      E10 = ZN_1L(i1,:) 
      ZN_1L(i1,:) = ZN_1L(i2,:) 
      ZN_1L(i2,:) = E10
    End If 
   End Do 
End Do 
 
MChi_1L(iL) = MChi_t(iL) 
MChi2_1L(iL) = MChi_t(iL)**2 
Else 
 
mat2 = Matmul( Transpose(Conjg( mat1) ), mat1 ) 
Call EigensystemQP(mat2, Eig, ZN_1L, ierr, test) 
mat2 = Matmul( Conjg(ZN_1L), Matmul( mat1, Transpose( Conjg(ZN_1L)))) 
Do i1=1,10
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
  phaseM = Sqrt( mat2(i1,i1) / Abs(mat2(i1,i1))) 
  ZN_1L(i1,:)= phaseM * ZN_1L(i1,:) 
End if 
  If (Eig(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      !Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      !Write(*,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      Call TerminateProgram
   return 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,Eig(i1) 
  Eig(i1) = 1._dp 
   SignOfMassChanged = .True. 
 End if 
End Do 
MChi_1L = Sqrt( Eig ) 
 
MChi2_1L = Eig 
 
End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  !Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MChi2_1L(iL)
Call Sigma1LoopChi(p2,MChi,MChi2,MAh,MAh2,MHpm,MHpm2,MCha,MCha2,MVWm,MVWm2,           & 
& Mhh,Mhh2,MVZ,MVZ2,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,cplUChiChiAhL,cplUChiChiAhR,     & 
& cplUChiChacHpmL,cplUChiChacHpmR,cplUChiChacVWmL,cplUChiChacVWmR,cplUChiChihhL,         & 
& cplUChiChihhR,cplUChiChiVZL,cplUChiChiVZR,cplUChiFdcSdL,cplUChiFdcSdR,cplUChiFucSuL,   & 
& cplUChiFucSuR,sigL,sigR,sigS)

mat1 = mat1a - 0.5_dp*(SigS + Transpose(SigS) + & 
      & MatMul(Transpose(SigL),mat1a) + MatMul(SigR,mat1a) + & 
      & MatMul(mat1a,Transpose(SigR)) + MatMul(mat1a,SigL)) 
!rruiz
call chop(mat1) 
If (ForceRealMatrices) mat1 = Real(mat1,dp) 
If (Maxval(Abs(Aimag(mat1))).Eq.0._dp) Then 
Call EigenSystemQP(Real(mat1,dp),Eig,ZNa,ierr,test) 
 
   Do i1=1,10
   If (Eig(i1).Lt.0._dp) Then 
    MChi_t(i1) = - Eig(i1) 
    ZN_1L(i1,:) = (0._dp,1._dp)*ZNa(i1,:) 
   Else 
    MChi_t(i1) = Eig(i1) 
    ZN_1L(i1,:) = ZNa(i1,:)
    End If 
   End Do 
 
Do i1=1,9
  Do i2=i1+1,10
    If (Abs(MChi_t(i1)).Gt.Abs(MChi_t(i2))) Then 
      Eig(1) = MChi_t(i1) 
      MChi_t(i1) = MChi_t(i2) 
      MChi_t(i2) = Eig(1) 
      E10 = ZN_1L(i1,:) 
      ZN_1L(i1,:) = ZN_1L(i2,:) 
      ZN_1L(i2,:) = E10
    End If 
   End Do 
End Do 
 
MChi_1L(iL) = MChi_t(iL) 
MChi2_1L(iL) = MChi_t(iL)**2 
Else 
 
mat2 = Matmul( Transpose(Conjg( mat1) ), mat1 ) 
Call EigensystemQP(mat2, Eig, ZN_1L, ierr, test) 
mat2 = Matmul( Conjg(ZN_1L), Matmul( mat1, Transpose( Conjg(ZN_1L)))) 
Do i1=1,10
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
  phaseM = Sqrt( mat2(i1,i1) / Abs(mat2(i1,i1))) 
  ZN_1L(i1,:)= phaseM * ZN_1L(i1,:) 
End if 
  If (Eig(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      !Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      !Write(*,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      Call TerminateProgram
   return 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,Eig(i1) 
  Eig(i1) = 1._dp 
   SignOfMassChanged = .True. 
 End if 
End Do 
MChi_1L = Sqrt( Eig ) 
 
MChi2_1L = Eig 
 
End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  !Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram
   return 
  endif
  ierr = 0 
End If 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MChi2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MChi2_1L(il))
End If 
If (Abs(MChi2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
End Do 
 
Iname = Iname -1 
End Subroutine OneLoopChi
 
 
Subroutine Sigma1LoopChi(p2,MChi,MChi2,MAh,MAh2,MHpm,MHpm2,MCha,MCha2,MVWm,           & 
& MVWm2,Mhh,Mhh2,MVZ,MVZ2,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,cplUChiChiAhL,             & 
& cplUChiChiAhR,cplUChiChacHpmL,cplUChiChacHpmR,cplUChiChacVWmL,cplUChiChacVWmR,         & 
& cplUChiChihhL,cplUChiChihhR,cplUChiChiVZL,cplUChiChiVZR,cplUChiFdcSdL,cplUChiFdcSdR,   & 
& cplUChiFucSuL,cplUChiFucSuR,sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MChi(10),MChi2(10),MAh(8),MAh2(8),MHpm(8),MHpm2(8),MCha(5),MCha2(5),MVWm,             & 
& MVWm2,Mhh(8),Mhh2(8),MVZ,MVZ2,MSd(6),MSd2(6),MFd(3),MFd2(3),MSu(6),MSu2(6),            & 
& MFu(3),MFu2(3)

Complex(dp), Intent(in) :: cplUChiChiAhL(10,10,8),cplUChiChiAhR(10,10,8),cplUChiChacHpmL(10,5,8),cplUChiChacHpmR(10,5,8),& 
& cplUChiChacVWmL(10,5),cplUChiChacVWmR(10,5),cplUChiChihhL(10,10,8),cplUChiChihhR(10,10,8),& 
& cplUChiChiVZL(10,10),cplUChiChiVZR(10,10),cplUChiFdcSdL(10,3,6),cplUChiFdcSdR(10,3,6), & 
& cplUChiFucSuL(10,3,6),cplUChiFucSuR(10,3,6)

Complex(dp), Intent(out) :: SigL(10,10),SigR(10,10), SigS(10,10) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(10,10), sumR(10,10), sumS(10,10) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Chi, Ah 
!------------------------ 
    Do i1 = 1, 10
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -1._dp*B1(p2,MChi2(i1),MAh2(i2)) 
B0m2 = 2._dp*MChi(i1)*B0(p2,MChi2(i1),MAh2(i2)) 
coupL1 = cplUChiChiAhL(gO1,i1,i2)
coupR1 = cplUChiChiAhR(gO1,i1,i2)
coupL2 =  Conjg(cplUChiChiAhL(gO2,i1,i2))
coupR2 =  Conjg(cplUChiChiAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp/2._dp* sumL
SigR = SigR +1._dp/2._dp* sumR 
SigS = SigS +1._dp/2._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[Hpm], Cha 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -1._dp*B1(p2,MCha2(i2),MHpm2(i1)) 
B0m2 = 2._dp*MCha(i2)*B0(p2,MCha2(i2),MHpm2(i1)) 
coupL1 = cplUChiChacHpmL(gO1,i2,i1)
coupR1 = cplUChiChacHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplUChiChacHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplUChiChacHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Cha 
!------------------------ 
      Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -2._dp*B1(p2,MCha2(i2),MVWm2) 
B0m2 = -8._dp*MCha(i2)*(B0(p2,MCha2(i2),MVWm2) - 0.5_dp*rMS) 
coupL1 = cplUChiChacVWmL(gO1,i2)
coupR1 = cplUChiChacVWmR(gO1,i2)
coupL2 =  Conjg(cplUChiChacVWmL(gO2,i2))
coupR2 =  Conjg(cplUChiChacVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! hh, Chi 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -1._dp*B1(p2,MChi2(i2),Mhh2(i1)) 
B0m2 = 2._dp*MChi(i2)*B0(p2,MChi2(i2),Mhh2(i1)) 
coupL1 = cplUChiChihhL(gO1,i2,i1)
coupR1 = cplUChiChihhR(gO1,i2,i1)
coupL2 =  Conjg(cplUChiChihhL(gO2,i2,i1))
coupR2 =  Conjg(cplUChiChihhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp/2._dp* sumL
SigR = SigR +1._dp/2._dp* sumR 
SigS = SigS +1._dp/2._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VZ, Chi 
!------------------------ 
      Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -2._dp*B1(p2,MChi2(i2),MVZ2) 
B0m2 = -8._dp*MChi(i2)*(B0(p2,MChi2(i2),MVZ2) - 0.5_dp*rMS) 
coupL1 = cplUChiChiVZL(gO1,i2)
coupR1 = cplUChiChiVZR(gO1,i2)
coupL2 =  Conjg(cplUChiChiVZL(gO2,i2))
coupR2 =  Conjg(cplUChiChiVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp/2._dp* sumL
SigR = SigR +1._dp/2._dp* sumR 
SigS = SigS +1._dp/2._dp* sumS 
    End Do 
 !------------------------ 
! conj[Sd], Fd 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -1._dp*B1(p2,MFd2(i2),MSd2(i1)) 
B0m2 = 2._dp*MFd(i2)*B0(p2,MFd2(i2),MSd2(i1)) 
coupL1 = cplUChiFdcSdL(gO1,i2,i1)
coupR1 = cplUChiFdcSdR(gO1,i2,i1)
coupL2 =  Conjg(cplUChiFdcSdL(gO2,i2,i1))
coupR2 =  Conjg(cplUChiFdcSdR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +3._dp* sumL
SigR = SigR +3._dp* sumR 
SigS = SigS +3._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[Su], Fu 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 10
  Do gO2 = 1, 10
B1m2 = -1._dp*B1(p2,MFu2(i2),MSu2(i1)) 
B0m2 = 2._dp*MFu(i2)*B0(p2,MFu2(i2),MSu2(i1)) 
coupL1 = cplUChiFucSuL(gO1,i2,i1)
coupR1 = cplUChiFucSuR(gO1,i2,i1)
coupL2 =  Conjg(cplUChiFucSuL(gO2,i2,i1))
coupR2 =  Conjg(cplUChiFucSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +3._dp* sumL
SigR = SigR +3._dp* sumR 
SigS = SigS +3._dp* sumS 
      End Do 
     End Do 
 

SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopChi 
 
Subroutine OneLoopCha(g2,Ye,lam,Yv,M2,vd,vu,vL,vR,MCha,MCha2,MAh,MAh2,Mhh,            & 
& Mhh2,MVZ,MVZ2,MHpm,MHpm2,MChi,MChi2,MVWm,MVWm2,MSu,MSu2,MFd,MFd2,MFu,MFu2,             & 
& MSd,MSd2,cplcUChaChaAhL,cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,   & 
& cplcUChaChaVPR,cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,          & 
& cplcUChaChiVWmL,cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,          & 
& cplcUChacFuSdR,delta,MCha_1L,MCha2_1L,ZER_1L,ZEL_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MCha(5),MCha2(5),MAh(8),MAh2(8),Mhh(8),Mhh2(8),MVZ,MVZ2,MHpm(8),MHpm2(8),             & 
& MChi(10),MChi2(10),MVWm,MVWm2,MSu(6),MSu2(6),MFd(3),MFd2(3),MFu(3),MFu2(3),            & 
& MSd(6),MSd2(6)

Real(dp), Intent(in) :: g2,vd,vu,vL(3),vR(3)

Complex(dp), Intent(in) :: Ye(3,3),lam(3),Yv(3,3),M2

Complex(dp), Intent(in) :: cplcUChaChaAhL(5,5,8),cplcUChaChaAhR(5,5,8),cplcUChaChahhL(5,5,8),cplcUChaChahhR(5,5,8),& 
& cplcUChaChaVPL(5,5),cplcUChaChaVPR(5,5),cplcUChaChaVZL(5,5),cplcUChaChaVZR(5,5),       & 
& cplcUChaChiHpmL(5,10,8),cplcUChaChiHpmR(5,10,8),cplcUChaChiVWmL(5,10),cplcUChaChiVWmR(5,10),& 
& cplcUChaFdcSuL(5,3,6),cplcUChaFdcSuR(5,3,6),cplcUChacFuSdL(5,3,6),cplcUChacFuSdR(5,3,6)

Complex(dp) :: mat1a(5,5), mat1(5,5) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(5), test_m2(5), p2 
Real(dp), Intent(out) :: MCha_1L(5),MCha2_1L(5) 
 Complex(dp), Intent(out) :: ZER_1L(5,5), ZEL_1L(5,5) 
 
 Real(dp) :: MCha_t(5),MCha2_t(5) 
 Complex(dp) :: ZER_t(5,5), ZEL_t(5,5), sigL(5,5), sigR(5,5), sigS(5,5) 
 
 Complex(dp) :: mat(5,5)=0._dp, mat2(5,5)=0._dp, phaseM 

Complex(dp) :: ZER2(5,5), ZEL2(5,5) 
 
 Real(dp) :: ZER1(5,5), ZEL1(5,5), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMCha'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(vd*Ye(1,1))/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vd*Ye(2,1))/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(vd*Ye(3,1))/sqrt(2._dp)
mat1a(1,4) = 0._dp 
mat1a(1,4) = mat1a(1,4)+(g2*vL(1))/sqrt(2._dp)
mat1a(1,5) = 0._dp 
Do j1 = 1,3
mat1a(1,5) = mat1a(1,5)-((vR(j1)*Yv(1,j1))/sqrt(2._dp))
End Do 
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(vd*Ye(1,2))/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vd*Ye(2,2))/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(vd*Ye(3,2))/sqrt(2._dp)
mat1a(2,4) = 0._dp 
mat1a(2,4) = mat1a(2,4)+(g2*vL(2))/sqrt(2._dp)
mat1a(2,5) = 0._dp 
Do j1 = 1,3
mat1a(2,5) = mat1a(2,5)-((vR(j1)*Yv(2,j1))/sqrt(2._dp))
End Do 
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(vd*Ye(1,3))/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(vd*Ye(2,3))/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(vd*Ye(3,3))/sqrt(2._dp)
mat1a(3,4) = 0._dp 
mat1a(3,4) = mat1a(3,4)+(g2*vL(3))/sqrt(2._dp)
mat1a(3,5) = 0._dp 
Do j1 = 1,3
mat1a(3,5) = mat1a(3,5)-((vR(j1)*Yv(3,j1))/sqrt(2._dp))
End Do 
mat1a(4,1) = 0._dp 
mat1a(4,2) = 0._dp 
mat1a(4,3) = 0._dp 
mat1a(4,4) = 0._dp 
mat1a(4,4) = mat1a(4,4)+M2
mat1a(4,5) = 0._dp 
mat1a(4,5) = mat1a(4,5)+(g2*vu)/sqrt(2._dp)
mat1a(5,1) = 0._dp 
Do j1 = 1,3
mat1a(5,1) = mat1a(5,1)-((vL(j1)*Ye(1,j1))/sqrt(2._dp))
End Do 
mat1a(5,2) = 0._dp 
Do j1 = 1,3
mat1a(5,2) = mat1a(5,2)-((vL(j1)*Ye(2,j1))/sqrt(2._dp))
End Do 
mat1a(5,3) = 0._dp 
Do j1 = 1,3
mat1a(5,3) = mat1a(5,3)-((vL(j1)*Ye(3,j1))/sqrt(2._dp))
End Do 
mat1a(5,4) = 0._dp 
mat1a(5,4) = mat1a(5,4)+(g2*vd)/sqrt(2._dp)
mat1a(5,5) = 0._dp 
Do j1 = 1,3
mat1a(5,5) = mat1a(5,5)+(vR(j1)*lam(j1))/sqrt(2._dp)
End Do 

 
 Do il=5,1,-1
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MCha2(il) 
Call Sigma1LoopCha(p2,MCha,MCha2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,               & 
& MChi,MChi2,MVWm,MVWm2,MSu,MSu2,MFd,MFd2,MFu,MFu2,MSd,MSd2,cplcUChaChaAhL,              & 
& cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,cplcUChaChaVPR,            & 
& cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,cplcUChaChiVWmL,         & 
& cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,cplcUChacFuSdR,           & 
& sigL,sigR,sigS)

mat1 = mat1a - SigS - MatMul(SigR,mat1a) - MatMul(mat1a,SigL) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MCha2_t,ZEL1,ierr,test) 
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MCha2_t,ZEL2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MCha2_t(iL)
Call Sigma1LoopCha(p2,MCha,MCha2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,               & 
& MChi,MChi2,MVWm,MVWm2,MSu,MSu2,MFd,MFd2,MFu,MFu2,MSd,MSd2,cplcUChaChaAhL,              & 
& cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,cplcUChaChaVPR,            & 
& cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,cplcUChaChiVWmL,         & 
& cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,cplcUChacFuSdR,           & 
& sigL,sigR,sigS)

mat1 = mat1a - SigS - MatMul(SigR,mat1a) - MatMul(mat1a,SigL) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MCha2_t,ZEL1,ierr,test) 
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MCha2_t,ZEL2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
MCha2_1L(il) = MCha2_t(il) 
MCha_1L(il) = Sqrt(MCha2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MCha2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MCha2_1L(il))
End If 
If (Abs(MCha2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MCha2_t,ZER1,ierr,test) 
 
 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MCha2_t,ZER2,ierr,test) 
 
 
End If 
ZER2 = Conjg(ZER2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZER2),mat1),Transpose( Conjg(ZEL2))) 
Do i1=1,5
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZEL2(i1,:) = phaseM *ZEL2(i1,:) 
 End if 
End Do 
 
ZER_1L = ZER2 
 ZEL_1L = ZEL2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopCha
 
 
Subroutine Sigma1LoopCha(p2,MCha,MCha2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHpm,               & 
& MHpm2,MChi,MChi2,MVWm,MVWm2,MSu,MSu2,MFd,MFd2,MFu,MFu2,MSd,MSd2,cplcUChaChaAhL,        & 
& cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,cplcUChaChaVPR,            & 
& cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,cplcUChaChiVWmL,         & 
& cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,cplcUChacFuSdR,           & 
& sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MCha(5),MCha2(5),MAh(8),MAh2(8),Mhh(8),Mhh2(8),MVZ,MVZ2,MHpm(8),MHpm2(8),             & 
& MChi(10),MChi2(10),MVWm,MVWm2,MSu(6),MSu2(6),MFd(3),MFd2(3),MFu(3),MFu2(3),            & 
& MSd(6),MSd2(6)

Complex(dp), Intent(in) :: cplcUChaChaAhL(5,5,8),cplcUChaChaAhR(5,5,8),cplcUChaChahhL(5,5,8),cplcUChaChahhR(5,5,8),& 
& cplcUChaChaVPL(5,5),cplcUChaChaVPR(5,5),cplcUChaChaVZL(5,5),cplcUChaChaVZR(5,5),       & 
& cplcUChaChiHpmL(5,10,8),cplcUChaChiHpmR(5,10,8),cplcUChaChiVWmL(5,10),cplcUChaChiVWmR(5,10),& 
& cplcUChaFdcSuL(5,3,6),cplcUChaFdcSuR(5,3,6),cplcUChacFuSdL(5,3,6),cplcUChacFuSdR(5,3,6)

Complex(dp), Intent(out) :: SigL(5,5),SigR(5,5), SigS(5,5) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(5,5), sumR(5,5), sumS(5,5) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Cha, Ah 
!------------------------ 
    Do i1 = 1, 5
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i1),MAh2(i2)),dp) 
B0m2 = MCha(i1)*Real(B0(p2,MCha2(i1),MAh2(i2)),dp) 
coupL1 = cplcUChaChaAhL(gO1,i1,i2)
coupR1 = cplcUChaChaAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUChaChaAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUChaChaAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! hh, Cha 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i2),Mhh2(i1)),dp) 
B0m2 = MCha(i2)*Real(B0(p2,MCha2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUChaChahhL(gO1,i2,i1)
coupR1 = cplcUChaChahhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUChaChahhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUChaChahhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VP, Cha 
!------------------------ 
      Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = - Real(B1(p2,MCha2(i2),0._dp),dp) 
B0m2 = -4._dp*MCha(i2)*Real(B0(p2,MCha2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUChaChaVPL(gO1,i2)
coupR1 = cplcUChaChaVPR(gO1,i2)
coupL2 =  Conjg(cplcUChaChaVPL(gO2,i2))
coupR2 =  Conjg(cplcUChaChaVPR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! VZ, Cha 
!------------------------ 
      Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = - Real(B1(p2,MCha2(i2),MVZ2),dp) 
B0m2 = -4._dp*MCha(i2)*Real(B0(p2,MCha2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUChaChaVZL(gO1,i2)
coupR1 = cplcUChaChaVZR(gO1,i2)
coupL2 =  Conjg(cplcUChaChaVZL(gO2,i2))
coupR2 =  Conjg(cplcUChaChaVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Hpm, Chi 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = -0.5_dp*Real(B1(p2,MChi2(i2),MHpm2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(p2,MChi2(i2),MHpm2(i1)),dp) 
coupL1 = cplcUChaChiHpmL(gO1,i2,i1)
coupR1 = cplcUChaChiHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUChaChiHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUChaChiHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VWm, Chi 
!------------------------ 
      Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = - Real(B1(p2,MChi2(i2),MVWm2),dp) 
B0m2 = -4._dp*MChi(i2)*Real(B0(p2,MChi2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUChaChiVWmL(gO1,i2)
coupR1 = cplcUChaChiVWmR(gO1,i2)
coupL2 =  Conjg(cplcUChaChiVWmL(gO2,i2))
coupR2 =  Conjg(cplcUChaChiVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! conj[Su], Fd 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i2),MSu2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(p2,MFd2(i2),MSu2(i1)),dp) 
coupL1 = cplcUChaFdcSuL(gO1,i2,i1)
coupR1 = cplcUChaFdcSuR(gO1,i2,i1)
coupL2 =  Conjg(cplcUChaFdcSuL(gO2,i2,i1))
coupR2 =  Conjg(cplcUChaFdcSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +3._dp* sumL
SigR = SigR +3._dp* sumR 
SigS = SigS +3._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Sd 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i1),MSd2(i2)),dp) 
B0m2 = MFu(i1)*Real(B0(p2,MFu2(i1),MSd2(i2)),dp) 
coupL1 = cplcUChacFuSdL(gO1,i1,i2)
coupR1 = cplcUChacFuSdR(gO1,i1,i2)
coupL2 =  Conjg(cplcUChacFuSdL(gO2,i1,i2))
coupR2 =  Conjg(cplcUChacFuSdR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +3._dp* sumL
SigR = SigR +3._dp* sumR 
SigS = SigS +3._dp* sumS 
      End Do 
     End Do 
 SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopCha 
 
Subroutine OneLoopFd(Yd,vd,MFd,MFd2,MAh,MAh2,MSu,MSu2,MCha,MCha2,MSd,MSd2,            & 
& MChi,MChi2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,MFu,MFu2,MVWm,MVWm2,MGlu,MGlu2,cplcUFdFdAhL,   & 
& cplcUFdFdAhR,cplcUFdChaSuL,cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,cplcUFdFdhhL,     & 
& cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,         & 
& cplcUFdFdVZR,cplcUFdFuHpmL,cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFdGluSdL,    & 
& cplcUFdGluSdR,delta,MFd_1L,MFd2_1L,ZDL_1L,ZDR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh(8),MAh2(8),MSu(6),MSu2(6),MCha(5),MCha2(5),MSd(6),MSd2(6),         & 
& MChi(10),MChi2(10),Mhh(8),Mhh2(8),MVZ,MVZ2,MHpm(8),MHpm2(8),MFu(3),MFu2(3),            & 
& MVWm,MVWm2,MGlu,MGlu2

Real(dp), Intent(in) :: vd

Complex(dp), Intent(in) :: Yd(3,3)

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3,8),cplcUFdFdAhR(3,3,8),cplcUFdChaSuL(3,5,6),cplcUFdChaSuR(3,5,6),    & 
& cplcUFdChiSdL(3,10,6),cplcUFdChiSdR(3,10,6),cplcUFdFdhhL(3,3,8),cplcUFdFdhhR(3,3,8),   & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHpmL(3,3,8),cplcUFdFuHpmR(3,3,8),         & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFdGluSdL(3,6),cplcUFdGluSdR(3,6)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFd_1L(3),MFd2_1L(3) 
 Complex(dp), Intent(out) :: ZDL_1L(3,3), ZDR_1L(3,3) 
 
 Real(dp) :: MFd_t(3),MFd2_t(3) 
 Complex(dp) :: ZDL_t(3,3), ZDR_t(3,3), sigL(3,3), sigR(3,3), sigS(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFd'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(vd*Yd(1,1))/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vd*Yd(2,1))/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(vd*Yd(3,1))/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(vd*Yd(1,2))/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vd*Yd(2,2))/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(vd*Yd(3,2))/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(vd*Yd(1,3))/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(vd*Yd(2,3))/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(vd*Yd(3,3))/sqrt(2._dp)

 
 Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MFd2(il) 
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,MSu,MSu2,MCha,MCha2,MSd,MSd2,MChi,             & 
& MChi2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,MFu,MFu2,MVWm,MVWm2,MGlu,MGlu2,cplcUFdFdAhL,        & 
& cplcUFdFdAhR,cplcUFdChaSuL,cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,cplcUFdFdhhL,     & 
& cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,         & 
& cplcUFdFdVZR,cplcUFdFuHpmL,cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFdGluSdL,    & 
& cplcUFdGluSdR,sigL,sigR,sigS)

mat1 = mat1a - SigS - MatMul(SigR,mat1a) - MatMul(mat1a,SigL) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MFd2_t(iL)
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,MSu,MSu2,MCha,MCha2,MSd,MSd2,MChi,             & 
& MChi2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,MFu,MFu2,MVWm,MVWm2,MGlu,MGlu2,cplcUFdFdAhL,        & 
& cplcUFdFdAhR,cplcUFdChaSuL,cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,cplcUFdFdhhL,     & 
& cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,         & 
& cplcUFdFdVZR,cplcUFdFuHpmL,cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFdGluSdL,    & 
& cplcUFdGluSdR,sigL,sigR,sigS)

mat1 = mat1a - SigS - MatMul(SigR,mat1a) - MatMul(mat1a,SigL) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
     Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
MFd2_1L(il) = MFd2_t(il) 
MFd_1L(il) = Sqrt(MFd2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFd2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFd2_1L(il))
End If 
If (Abs(MFd2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDL1,ierr,test) 
 
 
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then 
   Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat1),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
ZDL_1L = ZDL2 
 ZDR_1L = ZDR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFd
 
 
Subroutine Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,MSu,MSu2,MCha,MCha2,MSd,MSd2,            & 
& MChi,MChi2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,MFu,MFu2,MVWm,MVWm2,MGlu,MGlu2,cplcUFdFdAhL,   & 
& cplcUFdFdAhR,cplcUFdChaSuL,cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,cplcUFdFdhhL,     & 
& cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,         & 
& cplcUFdFdVZR,cplcUFdFuHpmL,cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFdGluSdL,    & 
& cplcUFdGluSdR,sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh(8),MAh2(8),MSu(6),MSu2(6),MCha(5),MCha2(5),MSd(6),MSd2(6),         & 
& MChi(10),MChi2(10),Mhh(8),Mhh2(8),MVZ,MVZ2,MHpm(8),MHpm2(8),MFu(3),MFu2(3),            & 
& MVWm,MVWm2,MGlu,MGlu2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3,8),cplcUFdFdAhR(3,3,8),cplcUFdChaSuL(3,5,6),cplcUFdChaSuR(3,5,6),    & 
& cplcUFdChiSdL(3,10,6),cplcUFdChiSdR(3,10,6),cplcUFdFdhhL(3,3,8),cplcUFdFdhhR(3,3,8),   & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHpmL(3,3,8),cplcUFdFuHpmR(3,3,8),         & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFdGluSdL(3,6),cplcUFdGluSdR(3,6)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigS(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumS(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i1),MAh2(i2)),dp) 
B0m2 = MFd(i1)*Real(B0(p2,MFd2(i1),MAh2(i2)),dp) 
coupL1 = cplcUFdFdAhL(gO1,i1,i2)
coupR1 = cplcUFdFdAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! Su, Cha 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i2),MSu2(i1)),dp) 
B0m2 = MCha(i2)*Real(B0(p2,MCha2(i2),MSu2(i1)),dp) 
coupL1 = cplcUFdChaSuL(gO1,i2,i1)
coupR1 = cplcUFdChaSuR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdChaSuL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdChaSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! Sd, Chi 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MChi2(i2),MSd2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(p2,MChi2(i2),MSd2(i1)),dp) 
coupL1 = cplcUFdChiSdL(gO1,i2,i1)
coupR1 = cplcUFdChiSdR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdChiSdL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdChiSdR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! hh, Fd 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(p2,MFd2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VG, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFd2(i2),0._dp),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(p2,MFd2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVGL(gO1,i2)
coupR1 = cplcUFdFdVGR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVGL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVGR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigS = SigS +4._dp/3._dp* sumS 
    End Do 
 !------------------------ 
! VP, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFd2(i2),0._dp),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(p2,MFd2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVPL(gO1,i2)
coupR1 = cplcUFdFdVPR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVPL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVPR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! VZ, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFd2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(p2,MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Hpm, Fu 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i2),MHpm2(i1)),dp) 
B0m2 = MFu(i2)*Real(B0(p2,MFu2(i2),MHpm2(i1)),dp) 
coupL1 = cplcUFdFuHpmL(gO1,i2,i1)
coupR1 = cplcUFdFuHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFuHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFuHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VWm, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFu2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(p2,MFu2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFuVWmL(gO1,i2)
coupR1 = cplcUFdFuVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFdFuVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFdFuVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Sd, Glu 
!------------------------ 
    Do i1 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MGlu2,MSd2(i1)),dp) 
B0m2 = MGlu*Real(B0(p2,MGlu2,MSd2(i1)),dp) 
coupL1 = cplcUFdGluSdL(gO1,i1)
coupR1 = cplcUFdGluSdR(gO1,i1)
coupL2 =  Conjg(cplcUFdGluSdL(gO2,i1))
coupR2 =  Conjg(cplcUFdGluSdR(gO2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigS = SigS +4._dp/3._dp* sumS 
      End Do 
 SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopFd 
 
Subroutine OneLoopFu(Yu,vu,MFu,MFu2,MAh,MAh2,MSu,MSu2,MChi,MChi2,MHpm,MHpm2,          & 
& MFd,MFd2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MGlu,MGlu2,MCha,MCha2,MSd,MSd2,cplcUFuFuAhL,     & 
& cplcUFuFuAhR,cplcUFuChiSuL,cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL, & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,     & 
& cplcChacUFuSdR,delta,MFu_1L,MFu2_1L,ZUL_1L,ZUR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh(8),MAh2(8),MSu(6),MSu2(6),MChi(10),MChi2(10),MHpm(8),              & 
& MHpm2(8),MFd(3),MFd2(3),MVWm,MVWm2,Mhh(8),Mhh2(8),MVZ,MVZ2,MGlu,MGlu2,MCha(5),         & 
& MCha2(5),MSd(6),MSd2(6)

Real(dp), Intent(in) :: vu

Complex(dp), Intent(in) :: Yu(3,3)

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3,8),cplcUFuFuAhR(3,3,8),cplcUFuChiSuL(3,10,6),cplcUFuChiSuR(3,10,6),  & 
& cplcUFuFdcHpmL(3,3,8),cplcUFuFdcHpmR(3,3,8),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),   & 
& cplcUFuFuhhL(3,3,8),cplcUFuFuhhR(3,3,8),cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),           & 
& cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),               & 
& cplcUFuGluSuL(3,6),cplcUFuGluSuR(3,6),cplcChacUFuSdL(5,3,6),cplcChacUFuSdR(5,3,6)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFu_1L(3),MFu2_1L(3) 
 Complex(dp), Intent(out) :: ZUL_1L(3,3), ZUR_1L(3,3) 
 
 Real(dp) :: MFu_t(3),MFu2_t(3) 
 Complex(dp) :: ZUL_t(3,3), ZUR_t(3,3), sigL(3,3), sigR(3,3), sigS(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFu'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(vu*Yu(1,1))/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vu*Yu(2,1))/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(vu*Yu(3,1))/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(vu*Yu(1,2))/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vu*Yu(2,2))/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(vu*Yu(3,2))/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(vu*Yu(1,3))/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(vu*Yu(2,3))/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(vu*Yu(3,3))/sqrt(2._dp)

 
 Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MFu2(il) 
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MSu,MSu2,MChi,MChi2,MHpm,MHpm2,MFd,            & 
& MFd2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MGlu,MGlu2,MCha,MCha2,MSd,MSd2,cplcUFuFuAhL,         & 
& cplcUFuFuAhR,cplcUFuChiSuL,cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL, & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,     & 
& cplcChacUFuSdR,sigL,sigR,sigS)

mat1 = mat1a - SigS - MatMul(SigR,mat1a) - MatMul(mat1a,SigL) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigS=0._dp 
p2 = MFu2_t(iL)
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MSu,MSu2,MChi,MChi2,MHpm,MHpm2,MFd,            & 
& MFd2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MGlu,MGlu2,MCha,MCha2,MSd,MSd2,cplcUFuFuAhL,         & 
& cplcUFuFuAhR,cplcUFuChiSuL,cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL, & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,     & 
& cplcChacUFuSdR,sigL,sigR,sigS)

mat1 = mat1a - SigS - MatMul(SigR,mat1a) - MatMul(mat1a,SigL) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram 
   return
  endif
  ierr = 0 
End If 
 
MFu2_1L(il) = MFu2_t(il) 
MFu_1L(il) = Sqrt(MFu2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFu2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFu2_1L(il))
End If 
If (Abs(MFu2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
!rruiz
call chop(mat2)
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUL1,ierr,test) 
 
 
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) then
    Call TerminateProgram
   return 
  endif
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat1),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
ZUL_1L = ZUL2 
 ZUR_1L = ZUR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFu
 
 
Subroutine Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MSu,MSu2,MChi,MChi2,MHpm,MHpm2,          & 
& MFd,MFd2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MGlu,MGlu2,MCha,MCha2,MSd,MSd2,cplcUFuFuAhL,     & 
& cplcUFuFuAhR,cplcUFuChiSuL,cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,cplcUFuFdcVWmL, & 
& cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,       & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,cplcChacUFuSdL,     & 
& cplcChacUFuSdR,sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh(8),MAh2(8),MSu(6),MSu2(6),MChi(10),MChi2(10),MHpm(8),              & 
& MHpm2(8),MFd(3),MFd2(3),MVWm,MVWm2,Mhh(8),Mhh2(8),MVZ,MVZ2,MGlu,MGlu2,MCha(5),         & 
& MCha2(5),MSd(6),MSd2(6)

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3,8),cplcUFuFuAhR(3,3,8),cplcUFuChiSuL(3,10,6),cplcUFuChiSuR(3,10,6),  & 
& cplcUFuFdcHpmL(3,3,8),cplcUFuFdcHpmR(3,3,8),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),   & 
& cplcUFuFuhhL(3,3,8),cplcUFuFuhhR(3,3,8),cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),           & 
& cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),               & 
& cplcUFuGluSuL(3,6),cplcUFuGluSuR(3,6),cplcChacUFuSdL(5,3,6),cplcChacUFuSdR(5,3,6)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigS(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumS(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i1),MAh2(i2)),dp) 
B0m2 = MFu(i1)*Real(B0(p2,MFu2(i1),MAh2(i2)),dp) 
coupL1 = cplcUFuFuAhL(gO1,i1,i2)
coupR1 = cplcUFuFuAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! Su, Chi 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MChi2(i2),MSu2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(p2,MChi2(i2),MSu2(i1)),dp) 
coupL1 = cplcUFuChiSuL(gO1,i2,i1)
coupR1 = cplcUFuChiSuR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuChiSuL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuChiSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[Hpm], Fd 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i2),MHpm2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(p2,MFd2(i2),MHpm2(i1)),dp) 
coupL1 = cplcUFuFdcHpmL(gO1,i2,i1)
coupR1 = cplcUFuFdcHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFdcHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFdcHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fd 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFd2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(p2,MFd2(i2),MVWm2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFdcVWmL(gO1,i2)
coupR1 = cplcUFuFdcVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdcVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdcVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! hh, Fu 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(B0(p2,MFu2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VG, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFu2(i2),0._dp),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(p2,MFu2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVGL(gO1,i2)
coupR1 = cplcUFuFuVGR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVGL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVGR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigS = SigS +4._dp/3._dp* sumS 
    End Do 
 !------------------------ 
! VP, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFu2(i2),0._dp),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(p2,MFu2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVPL(gO1,i2)
coupR1 = cplcUFuFuVPR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVPL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVPR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! VZ, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = - Real(B1(p2,MFu2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(p2,MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Su, Glu 
!------------------------ 
    Do i1 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MGlu2,MSu2(i1)),dp) 
B0m2 = MGlu*Real(B0(p2,MGlu2,MSu2(i1)),dp) 
coupL1 = cplcUFuGluSuL(gO1,i1)
coupR1 = cplcUFuGluSuR(gO1,i1)
coupL2 =  Conjg(cplcUFuGluSuL(gO2,i1))
coupR2 =  Conjg(cplcUFuGluSuR(gO2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigS = SigS +4._dp/3._dp* sumS 
      End Do 
 !------------------------ 
! bar[Cha], Sd 
!------------------------ 
    Do i1 = 1, 5
       Do i2 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i1),MSd2(i2)),dp) 
B0m2 = MCha(i1)*Real(B0(p2,MCha2(i1),MSd2(i2)),dp) 
coupL1 = cplcChacUFuSdL(i1,gO1,i2)
coupR1 = cplcChacUFuSdR(i1,gO1,i2)
coupL2 =  Conjg(cplcChacUFuSdL(i1,gO2,i2))
coupR2 =  Conjg(cplcChacUFuSdR(i1,gO2,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopFu 
 
Subroutine OneLoopGlu(M3,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,MGlu,MGlu2,              & 
& cplGluFdcSdL,cplGluFdcSdR,cplGluFucSuL,cplGluFucSuR,cplGluGluVGL,cplGluGluVGR,         & 
& delta,mass,mass2,kont)

Real(dp), Intent(in) :: MSd(6),MSd2(6),MFd(3),MFd2(3),MSu(6),MSu2(6),MFu(3),MFu2(3),MGlu,MGlu2

Complex(dp), Intent(in) :: M3

Complex(dp), Intent(in) :: cplGluFdcSdL(3,6),cplGluFdcSdR(3,6),cplGluFucSuL(3,6),cplGluFucSuR(3,6),              & 
& cplGluGluVGL,cplGluGluVGR

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, sig 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopGlu'
 
mi = MGlu 

 
p2 = MGlu2
sig = ZeroC 
Call Sigma1LoopGlu(p2,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,MGlu,MGlu2,cplGluFdcSdL,    & 
& cplGluFdcSdR,cplGluFucSuL,cplGluFucSuR,cplGluGluVGL,cplGluGluVGR,sig)

mass = mi - sig 
mass2= mass**2 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
sig = ZeroC 
Call Sigma1LoopGlu(p2,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,MGlu,MGlu2,cplGluFdcSdL,    & 
& cplGluFdcSdR,cplGluFucSuL,cplGluFucSuR,cplGluGluVGL,cplGluGluVGR,sig)

mass = mi - sig 
mass2= mass**2 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
    mass = sqrt(mass2) 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  !Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopGlu
 
 
Subroutine Sigma1LoopGlu(p2,MSd,MSd2,MFd,MFd2,MSu,MSu2,MFu,MFu2,MGlu,MGlu2,           & 
& cplGluFdcSdL,cplGluFdcSdR,cplGluFucSuL,cplGluFucSuR,cplGluGluVGL,cplGluGluVGR,sig)

Implicit None 
Real(dp), Intent(in) :: MSd(6),MSd2(6),MFd(3),MFd2(3),MSu(6),MSu2(6),MFu(3),MFu2(3),MGlu,MGlu2

Complex(dp), Intent(in) :: cplGluFdcSdL(3,6),cplGluFdcSdR(3,6),cplGluFucSuL(3,6),cplGluFucSuR(3,6),              & 
& cplGluGluVGL,cplGluGluVGR

Complex(dp), Intent(out) :: Sig 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumS,sumR,sumL 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Sig = Cmplx(0._dp,0._dp,dp) 
!------------------------ 
! conj[Sd], Fd 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 3
 SumR = 0._dp 
SumL = 0._dp 
SumS = 0._dp 
B1m2 = -1._dp*B1(p2,MFd2(i2),MSd2(i1)) 
B0m2 = -2._dp*MFd(i2)*B0(p2,MFd2(i2),MSd2(i1)) 
coupL1 = cplGluFdcSdL(i2,i1)
coupR1 = cplGluFdcSdR(i2,i1)
coupL2 =  Conjg(cplGluFdcSdL(i2,i1))
coupR2 =  Conjg(cplGluFdcSdR(i2,i1))
SumS = coupR1*coupL2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
Sig = Sig +1._dp/2._dp*(sumS + MGlu*(sumL+sumR))
      End Do 
     End Do 
 !------------------------ 
! conj[Su], Fu 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 3
 SumR = 0._dp 
SumL = 0._dp 
SumS = 0._dp 
B1m2 = -1._dp*B1(p2,MFu2(i2),MSu2(i1)) 
B0m2 = -2._dp*MFu(i2)*B0(p2,MFu2(i2),MSu2(i1)) 
coupL1 = cplGluFucSuL(i2,i1)
coupR1 = cplGluFucSuR(i2,i1)
coupL2 =  Conjg(cplGluFucSuL(i2,i1))
coupR2 =  Conjg(cplGluFucSuR(i2,i1))
SumS = coupR1*coupL2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
Sig = Sig +1._dp/2._dp*(sumS + MGlu*(sumL+sumR))
      End Do 
     End Do 
 !------------------------ 
! VG, Glu 
!------------------------ 
SumR = 0._dp 
SumL = 0._dp 
SumS = 0._dp 
B1m2 = -2._dp*B1(p2,MGlu2,0._dp) 
B0m2 = -8._dp*MGlu*(B0(p2,MGlu2,0._dp) - 0.5_dp*rMS) 
coupL1 = cplGluGluVGL
coupR1 = cplGluGluVGR
coupL2 =  Conjg(cplGluGluVGL)
coupR2 =  Conjg(cplGluGluVGR)
SumS = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
Sig = Sig +3._dp/2._dp*(sumS + MGlu*(sumL+sumR))


Sig = oo16pi2*Sig 
 
End Subroutine Sigma1LoopGlu 
 
Subroutine OneLoopVZ(g1,g2,vd,vu,vL,TW,Mhh,Mhh2,MAh,MAh2,MCha,MCha2,MChi,             & 
& MChi2,MFd,MFd2,MFu,MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,              & 
& cplAhhhVZ,cplcChaChaVZL,cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,           & 
& cplcFdFdVZR,cplcFuFuVZL,cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ,             & 
& cplHpmcHpmVZ,cplHpmcVWmVZ,cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,              & 
& cplhhhhVZVZ,cplHpmcHpmVZVZ,cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,  & 
& cplcVWmVWmVZVZ3,delta,mass,mass2,kont)

Real(dp), Intent(in) :: Mhh(8),Mhh2(8),MAh(8),MAh2(8),MCha(5),MCha2(5),MChi(10),MChi2(10),MFd(3),             & 
& MFd2(3),MFu(3),MFu2(3),MVZ,MVZ2,MHpm(8),MHpm2(8),MVWm,MVWm2,MSd(6),MSd2(6),            & 
& MSu(6),MSu2(6)

Real(dp), Intent(in) :: g1,g2,vd,vu,vL(3),TW

Complex(dp), Intent(in) :: cplAhhhVZ(8,8),cplcChaChaVZL(5,5),cplcChaChaVZR(5,5),cplChiChiVZL(10,10),             & 
& cplChiChiVZR(10,10),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),& 
& cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ(8),cplHpmcHpmVZ(8,8),cplHpmcVWmVZ(8),            & 
& cplSdcSdVZ(6,6),cplSucSuVZ(6,6),cplcVWmVWmVZ,cplAhAhVZVZ(8,8),cplhhhhVZVZ(8,8),        & 
& cplHpmcHpmVZVZ(8,8),cplSdcSdVZVZ(6,6),cplSucSuVZVZ(6,6),cplcVWmVWmVZVZ1,               & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, sig 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopVZ'
 
mi2 = MVZ2 

 
p2 = MVZ2
PiSf = ZeroC 
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,               & 
& MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhhhVZ,cplcChaChaVZL,         & 
& cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVZL,           & 
& cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,           & 
& cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHpmcHpmVZVZ,             & 
& cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,             & 
& kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,MFu,               & 
& MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhhhVZ,cplcChaChaVZL,         & 
& cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVZL,           & 
& cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,           & 
& cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHpmcHpmVZVZ,             & 
& cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,             & 
& kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
    mass = sqrt(mass2) 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
    ! Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
    ! Write(ErrCan,*) 'in the calculation of the masses' 
    ! Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  !Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopVZ
 
 
Subroutine Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MCha,MCha2,MChi,MChi2,MFd,MFd2,             & 
& MFu,MFu2,MVZ,MVZ2,MHpm,MHpm2,MVWm,MVWm2,MSd,MSd2,MSu,MSu2,cplAhhhVZ,cplcChaChaVZL,     & 
& cplcChaChaVZR,cplChiChiVZL,cplChiChiVZR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVZL,           & 
& cplcFuFuVZR,cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ,cplHpmcHpmVZ,cplHpmcVWmVZ,           & 
& cplSdcSdVZ,cplSucSuVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHpmcHpmVZVZ,             & 
& cplSdcSdVZVZ,cplSucSuVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(8),Mhh2(8),MAh(8),MAh2(8),MCha(5),MCha2(5),MChi(10),MChi2(10),MFd(3),             & 
& MFd2(3),MFu(3),MFu2(3),MVZ,MVZ2,MHpm(8),MHpm2(8),MVWm,MVWm2,MSd(6),MSd2(6),            & 
& MSu(6),MSu2(6)

Complex(dp), Intent(in) :: cplAhhhVZ(8,8),cplcChaChaVZL(5,5),cplcChaChaVZR(5,5),cplChiChiVZL(10,10),             & 
& cplChiChiVZR(10,10),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),& 
& cplcgWmgWmVZ,cplcgWpCgWpCVZ,cplhhVZVZ(8),cplHpmcHpmVZ(8,8),cplHpmcVWmVZ(8),            & 
& cplSdcSdVZ(6,6),cplSucSuVZ(6,6),cplcVWmVWmVZ,cplAhAhVZVZ(8,8),cplhhhhVZVZ(8,8),        & 
& cplHpmcHpmVZVZ(8,8),cplSdcSdVZVZ(6,6),cplSucSuVZVZ(6,6),cplcVWmVWmVZVZ1,               & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B22m2 = -4._dp*B00(p2,MAh2(i2),Mhh2(i1))  
coup1 = cplAhhhVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Cha], Cha 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 5
       Do i2 = 1, 5
 H0m2 = Hloop(p2,MCha2(i1),MCha2(i2)) 
B0m2 = 4._dp*MCha(i1)*MCha(i2)*B0(p2,MCha2(i1),MCha2(i2)) 
coupL1 = cplcChaChaVZL(i1,i2)
coupR1 = cplcChaChaVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Chi, Chi 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 10
       Do i2 = 1, 10
 H0m2 = Hloop(p2,MChi2(i1),MChi2(i2)) 
B0m2 = 4._dp*MChi(i1)*MChi(i2)*B0(p2,MChi2(i1),MChi2(i2)) 
coupL1 = cplChiChiVZL(i1,i2)
coupR1 = cplChiChiVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Hloop(p2,MFd2(i1),MFd2(i2)) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*B0(p2,MFd2(i1),MFd2(i2)) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Hloop(p2,MFu2(i1),MFu2(i2)) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*B0(p2,MFu2(i1),MFu2(i2)) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWm], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = B00(p2,MVWm2,MVWm2)
coup1 = cplcgWmgWmVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gWmC 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = B00(p2,MVWm2,MVWm2)
coup1 = cplcgWpCgWpCVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 B0m2 = B0(p2,MVZ2,Mhh2(i2)) 
coup1 = cplhhVZVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Hpm], Hpm 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B22m2 = -4._dp*B00(p2,MHpm2(i2),MHpm2(i1))  
coup1 = cplHpmcHpmVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 B0m2 = B0(p2,MVWm2,MHpm2(i2)) 
coup1 = cplHpmcVWmVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
    End Do 
 !------------------------ 
! conj[Sd], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B22m2 = -4._dp*B00(p2,MSd2(i2),MSd2(i1))  
coup1 = cplSdcSdVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[Su], Su 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B22m2 = -4._dp*B00(p2,MSu2(i2),MSu2(i1))  
coup1 = cplSucSuVZ(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], VWm 
!------------------------ 
sumI = 0._dp 
 
B0m2 = 10._dp*B00(p2,MVWm2,MVWm2)+(MVWm2+MVWm2+4._dp*p2)*B0(p2,MVWm2,MVWm2)
A0m12 = A0(MVWm2) 
A0m22 =A0(MVWm2) 
coup1 = cplcVWmVWmVZ
coup2 = Conjg(coup1) 
    SumI = -2._dp*rMS*(MVWm2+MVWm2-p2/3._dp)+A0m12+A0m22+B0m2  
    SumI = -SumI*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 8
 SumI = 0._dp 
 A0m2 = A0(MAh2(i1))
 coup1 = cplAhAhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 8
 SumI = 0._dp 
 A0m2 = A0(Mhh2(i1))
 coup1 = cplhhhhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm] 
!------------------------ 
    Do i1 = 1, 8
 SumI = 0._dp 
 A0m2 = A0(MHpm2(i1))
 coup1 = cplHpmcHpmVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! conj[Sd] 
!------------------------ 
    Do i1 = 1, 6
 SumI = 0._dp 
 A0m2 = A0(MSd2(i1))
 coup1 = cplSdcSdVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +3* SumI  
      End Do 
 !------------------------ 
! conj[Su] 
!------------------------ 
    Do i1 = 1, 6
 SumI = 0._dp 
 A0m2 = A0(MSu2(i1))
 coup1 = cplSucSuVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +3* SumI  
      End Do 
 !------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = A0(MVWm2) 
coup1 = cplcVWmVWmVZVZ1
coup2 = cplcVWmVWmVZVZ2
coup3 = cplcVWmVWmVZVZ3
SumI = (2._dp*rMS*coup1*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVZ 
 
Subroutine OneLoopVWm(g2,vd,vu,vL,MHpm,MHpm2,MAh,MAh2,MChi,MChi2,MCha,MCha2,          & 
& MFu,MFu2,MFd,MFd2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcVWm,         & 
& cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcgWpCgAcVWm,              & 
& cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,     & 
& cplHpmcVWmVZ,cplSdcSucVWm,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,     & 
& cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSucSucVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,     & 
& cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,delta,mass,mass2,kont)

Real(dp), Intent(in) :: MHpm(8),MHpm2(8),MAh(8),MAh2(8),MChi(10),MChi2(10),MCha(5),MCha2(5),MFu(3),           & 
& MFu2(3),MFd(3),MFd2(3),Mhh(8),Mhh2(8),MVWm,MVWm2,MVZ,MVZ2,MSu(6),MSu2(6),              & 
& MSd(6),MSd2(6)

Real(dp), Intent(in) :: g2,vd,vu,vL(3)

Complex(dp), Intent(in) :: cplAhHpmcVWm(8,8),cplChiChacVWmL(10,5),cplChiChacVWmR(10,5),cplcFuFdcVWmL(3,3),       & 
& cplcFuFdcVWmR(3,3),cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,          & 
& cplhhHpmcVWm(8,8),cplhhcVWmVWm(8),cplHpmcVWmVP(8),cplHpmcVWmVZ(8),cplSdcSucVWm(6,6),   & 
& cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm(8,8),cplhhhhcVWmVWm(8,8),cplHpmcHpmcVWmVWm(8,8),& 
& cplSdcSdcVWmVWm(6,6),cplSucSucVWmVWm(6,6),cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,             & 
& cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, sig 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopVWm'
 
mi2 = MVWm2 

 
p2 = MVWm2
PiSf = ZeroC 
Call Pi1LoopVWm(p2,MHpm,MHpm2,MAh,MAh2,MChi,MChi2,MCha,MCha2,MFu,MFu2,MFd,            & 
& MFd2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcVWm,cplChiChacVWmL,       & 
& cplChiChacVWmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,               & 
& cplcgZgWmcVWm,cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplHpmcVWmVZ,      & 
& cplSdcSucVWm,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,& 
& cplSdcSdcVWmVWm,cplSucSucVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,       & 
& cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,              & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopVWm(p2,MHpm,MHpm2,MAh,MAh2,MChi,MChi2,MCha,MCha2,MFu,MFu2,MFd,            & 
& MFd2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcVWm,cplChiChacVWmL,       & 
& cplChiChacVWmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcgWpCgAcVWm,cplcgAgWmcVWm,               & 
& cplcgZgWmcVWm,cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,cplHpmcVWmVZ,      & 
& cplSdcSucVWm,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,cplHpmcHpmcVWmVWm,& 
& cplSdcSdcVWmVWm,cplSucSucVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,       & 
& cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,cplcVWmVWmVZVZ1,              & 
& cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
    mass = sqrt(mass2) 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     !Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     !Write(ErrCan,*) 'in the calculation of the masses' 
     !Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  !Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopVWm
 
 
Subroutine Pi1LoopVWm(p2,MHpm,MHpm2,MAh,MAh2,MChi,MChi2,MCha,MCha2,MFu,               & 
& MFu2,MFd,MFd2,Mhh,Mhh2,MVWm,MVWm2,MVZ,MVZ2,MSu,MSu2,MSd,MSd2,cplAhHpmcVWm,             & 
& cplChiChacVWmL,cplChiChacVWmR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplcgWpCgAcVWm,              & 
& cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,cplhhHpmcVWm,cplhhcVWmVWm,cplHpmcVWmVP,     & 
& cplHpmcVWmVZ,cplSdcSucVWm,cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm,cplhhhhcVWmVWm,     & 
& cplHpmcHpmcVWmVWm,cplSdcSdcVWmVWm,cplSucSucVWmVWm,cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,     & 
& cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHpm(8),MHpm2(8),MAh(8),MAh2(8),MChi(10),MChi2(10),MCha(5),MCha2(5),MFu(3),           & 
& MFu2(3),MFd(3),MFd2(3),Mhh(8),Mhh2(8),MVWm,MVWm2,MVZ,MVZ2,MSu(6),MSu2(6),              & 
& MSd(6),MSd2(6)

Complex(dp), Intent(in) :: cplAhHpmcVWm(8,8),cplChiChacVWmL(10,5),cplChiChacVWmR(10,5),cplcFuFdcVWmL(3,3),       & 
& cplcFuFdcVWmR(3,3),cplcgWpCgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpCgZcVWm,          & 
& cplhhHpmcVWm(8,8),cplhhcVWmVWm(8),cplHpmcVWmVP(8),cplHpmcVWmVZ(8),cplSdcSucVWm(6,6),   & 
& cplcVWmVPVWm,cplcVWmVWmVZ,cplAhAhcVWmVWm(8,8),cplhhhhcVWmVWm(8,8),cplHpmcHpmcVWmVWm(8,8),& 
& cplSdcSdcVWmVWm(6,6),cplSucSucVWmVWm(6,6),cplcVWmVPVPVWm3,cplcVWmVPVPVWm1,             & 
& cplcVWmVPVPVWm2,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,cplcVWmcVWmVWmVWm1,              & 
& cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Hpm, Ah 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B22m2 = -4._dp*B00(p2,MAh2(i2),MHpm2(i1))  
coup1 = cplAhHpmcVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! Chi, Cha 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 10
       Do i2 = 1, 5
 H0m2 = Hloop(p2,MChi2(i1),MCha2(i2)) 
B0m2 = 4._dp*MChi(i1)*MCha(i2)*B0(p2,MChi2(i1),MCha2(i2)) 
coupL1 = cplChiChacVWmL(i1,i2)
coupR1 = cplChiChacVWmR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Fd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Hloop(p2,MFu2(i1),MFd2(i2)) 
B0m2 = 4._dp*MFu(i1)*MFd(i2)*B0(p2,MFu2(i1),MFd2(i2)) 
coupL1 = cplcFuFdcVWmL(i1,i2)
coupR1 = cplcFuFdcVWmR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! bar[gWmC], gP 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = B00(p2,0._dp,MVWm2)
coup1 = cplcgWpCgAcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gP], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = B00(p2,MVWm2,0._dp)
coup1 = cplcgAgWmcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gZ], gWm 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = B00(p2,MVWm2,MVZ2)
coup1 = cplcgZgWmcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! bar[gWmC], gZ 
!------------------------ 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = B00(p2,MVZ2,MVWm2)
coup1 = cplcgWpCgZcVWm
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
!------------------------ 
! Hpm, hh 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 8
       Do i2 = 1, 8
 B22m2 = -4._dp*B00(p2,Mhh2(i2),MHpm2(i1))  
coup1 = cplhhHpmcVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, hh 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 B0m2 = B0(p2,MVWm2,Mhh2(i2)) 
coup1 = cplhhcVWmVWm(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VP, Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 B0m2 = B0(p2,0._dp,MHpm2(i2)) 
coup1 = cplHpmcVWmVP(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! VZ, Hpm 
!------------------------ 
sumI = 0._dp 
 
      Do i2 = 1, 8
 B0m2 = B0(p2,MVZ2,MHpm2(i2)) 
coup1 = cplHpmcVWmVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 !------------------------ 
! conj[Su], Sd 
!------------------------ 
sumI = 0._dp 
 
    Do i1 = 1, 6
       Do i2 = 1, 6
 B22m2 = -4._dp*B00(p2,MSd2(i2),MSu2(i1))  
coup1 = cplSdcSucVWm(i2,i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 !------------------------ 
! VWm, VP 
!------------------------ 
sumI = 0._dp 
 
B0m2 = 10._dp*B00(p2,MVWm2,0._dp)+(MVWm2+0._dp+4._dp*p2)*B0(p2,MVWm2,0._dp)
A0m12 = A0(MVWm2) 
A0m22 =A0(0._dp) 
coup1 = cplcVWmVPVWm
coup2 = Conjg(coup1) 
    SumI = -2._dp*rMS*(MVWm2+0._dp-p2/3._dp)+A0m12+A0m22+B0m2  
    SumI = -SumI*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! VZ, VWm 
!------------------------ 
sumI = 0._dp 
 
B0m2 = 10._dp*B00(p2,MVZ2,MVWm2)+(MVZ2+MVWm2+4._dp*p2)*B0(p2,MVZ2,MVWm2)
A0m12 = A0(MVZ2) 
A0m22 =A0(MVWm2) 
coup1 = cplcVWmVWmVZ
coup2 = Conjg(coup1) 
    SumI = -2._dp*rMS*(MVZ2+MVWm2-p2/3._dp)+A0m12+A0m22+B0m2  
    SumI = -SumI*coup1*coup2 
res = res +1._dp* SumI  
!------------------------ 
! Ah 
!------------------------ 
    Do i1 = 1, 8
 SumI = 0._dp 
 A0m2 = A0(MAh2(i1))
 coup1 = cplAhAhcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! hh 
!------------------------ 
    Do i1 = 1, 8
 SumI = 0._dp 
 A0m2 = A0(Mhh2(i1))
 coup1 = cplhhhhcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 !------------------------ 
! conj[Hpm] 
!------------------------ 
    Do i1 = 1, 8
 SumI = 0._dp 
 A0m2 = A0(MHpm2(i1))
 coup1 = cplHpmcHpmcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +1* SumI  
      End Do 
 !------------------------ 
! conj[Sd] 
!------------------------ 
    Do i1 = 1, 6
 SumI = 0._dp 
 A0m2 = A0(MSd2(i1))
 coup1 = cplSdcSdcVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +3* SumI  
      End Do 
 !------------------------ 
! conj[Su] 
!------------------------ 
    Do i1 = 1, 6
 SumI = 0._dp 
 A0m2 = A0(MSu2(i1))
 coup1 = cplSucSucVWmVWm(i1,i1)
 SumI = coup1*A0m2 
res = res +3* SumI  
      End Do 
 !------------------------ 
! VP 
!------------------------ 
SumI = 0._dp 
A0m2 = A0(0._dp) 
coup1 = cplcVWmVPVPVWm3
coup2 = cplcVWmVPVPVWm1
coup3 = cplcVWmVPVPVWm2
SumI = (2._dp*rMS*coup1*0._dp-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
!------------------------ 
! conj[VWm] 
!------------------------ 
SumI = 0._dp 
A0m2 = A0(MVWm2) 
coup1 = cplcVWmcVWmVWmVWm2
coup2 = cplcVWmcVWmVWmVWm3
coup3 = cplcVWmcVWmVWmVWm1
SumI = (2._dp*rMS*coup1*MVWm2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
!------------------------ 
! VZ 
!------------------------ 
SumI = 0._dp 
A0m2 = A0(MVZ2) 
coup1 = cplcVWmVWmVZVZ1
coup2 = cplcVWmVWmVZVZ2
coup3 = cplcVWmVWmVZVZ3
SumI = (2._dp*rMS*coup1*MVZ2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
res = oo16pi2*res 
 
End Subroutine Pi1LoopVWm 
 
Subroutine Sigma1LoopChaMZ(p2,MCha,MCha2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHpm,             & 
& MHpm2,MChi,MChi2,MVWm,MVWm2,MSu,MSu2,MFd,MFd2,MFu,MFu2,MSd,MSd2,cplcUChaChaAhL,        & 
& cplcUChaChaAhR,cplcUChaChahhL,cplcUChaChahhR,cplcUChaChaVPL,cplcUChaChaVPR,            & 
& cplcUChaChaVZL,cplcUChaChaVZR,cplcUChaChiHpmL,cplcUChaChiHpmR,cplcUChaChiVWmL,         & 
& cplcUChaChiVWmR,cplcUChaFdcSuL,cplcUChaFdcSuR,cplcUChacFuSdL,cplcUChacFuSdR,           & 
& sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MCha(5),MCha2(5),MAh(8),MAh2(8),Mhh(8),Mhh2(8),MVZ,MVZ2,MHpm(8),MHpm2(8),             & 
& MChi(10),MChi2(10),MVWm,MVWm2,MSu(6),MSu2(6),MFd(3),MFd2(3),MFu(3),MFu2(3),            & 
& MSd(6),MSd2(6)

Complex(dp), Intent(in) :: cplcUChaChaAhL(5,5,8),cplcUChaChaAhR(5,5,8),cplcUChaChahhL(5,5,8),cplcUChaChahhR(5,5,8),& 
& cplcUChaChaVPL(5,5),cplcUChaChaVPR(5,5),cplcUChaChaVZL(5,5),cplcUChaChaVZR(5,5),       & 
& cplcUChaChiHpmL(5,10,8),cplcUChaChiHpmR(5,10,8),cplcUChaChiVWmL(5,10),cplcUChaChiVWmR(5,10),& 
& cplcUChaFdcSuL(5,3,6),cplcUChaFdcSuR(5,3,6),cplcUChacFuSdL(5,3,6),cplcUChacFuSdR(5,3,6)

Complex(dp), Intent(out) :: SigL(5,5),SigR(5,5), SigS(5,5) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(5,5), sumR(5,5), sumS(5,5) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Cha, Ah 
!------------------------ 
    Do i1 = 1, 5
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MCha2(gO1),MCha2(i1),MAh2(i2)),dp) 
B0m2 = MCha(i1)*Real(B0(MCha2(gO1),MCha2(i1),MAh2(i2)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i1),MAh2(i2)),dp) 
B0m2 = MCha(i1)*Real(B0(p2,MCha2(i1),MAh2(i2)),dp) 
End If 
coupL1 = cplcUChaChaAhL(gO1,i1,i2)
coupR1 = cplcUChaChaAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUChaChaAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUChaChaAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! hh, Cha 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MCha2(gO1),MCha2(i2),Mhh2(i1)),dp) 
B0m2 = MCha(i2)*Real(B0(MCha2(gO1),MCha2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i2),Mhh2(i1)),dp) 
B0m2 = MCha(i2)*Real(B0(p2,MCha2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUChaChahhL(gO1,i2,i1)
coupR1 = cplcUChaChahhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUChaChahhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUChaChahhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VZ, Cha 
!------------------------ 
      Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = - Real(B1(MCha2(gO1),MCha2(i2),MVZ2),dp) 
B0m2 = -4._dp*MCha(i2)*Real(B0(MCha2(gO1),MCha2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = - Real(B1(p2,MCha2(i2),MVZ2),dp) 
B0m2 = -4._dp*MCha(i2)*Real(B0(p2,MCha2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUChaChaVZL(gO1,i2)
coupR1 = cplcUChaChaVZR(gO1,i2)
coupL2 =  Conjg(cplcUChaChaVZL(gO2,i2))
coupR2 =  Conjg(cplcUChaChaVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Hpm, Chi 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MCha2(gO1),MChi2(i2),MHpm2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(MCha2(gO1),MChi2(i2),MHpm2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MChi2(i2),MHpm2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(p2,MChi2(i2),MHpm2(i1)),dp) 
End If 
coupL1 = cplcUChaChiHpmL(gO1,i2,i1)
coupR1 = cplcUChaChiHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUChaChiHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUChaChiHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VWm, Chi 
!------------------------ 
      Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = - Real(B1(MCha2(gO1),MChi2(i2),MVWm2),dp) 
B0m2 = -4._dp*MChi(i2)*Real(B0(MCha2(gO1),MChi2(i2),MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = - Real(B1(p2,MChi2(i2),MVWm2),dp) 
B0m2 = -4._dp*MChi(i2)*Real(B0(p2,MChi2(i2),MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUChaChiVWmL(gO1,i2)
coupR1 = cplcUChaChiVWmR(gO1,i2)
coupL2 =  Conjg(cplcUChaChiVWmL(gO2,i2))
coupR2 =  Conjg(cplcUChaChiVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! conj[Su], Fd 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MCha2(gO1),MFd2(i2),MSu2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(MCha2(gO1),MFd2(i2),MSu2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i2),MSu2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(p2,MFd2(i2),MSu2(i1)),dp) 
End If 
coupL1 = cplcUChaFdcSuL(gO1,i2,i1)
coupR1 = cplcUChaFdcSuR(gO1,i2,i1)
coupL2 =  Conjg(cplcUChaFdcSuL(gO2,i2,i1))
coupR2 =  Conjg(cplcUChaFdcSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +3._dp* sumL
SigR = SigR +3._dp* sumR 
SigS = SigS +3._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! bar[Fu], Sd 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 5
  Do gO2 = 1, 5
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MCha2(gO1),MFu2(i1),MSd2(i2)),dp) 
B0m2 = MFu(i1)*Real(B0(MCha2(gO1),MFu2(i1),MSd2(i2)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i1),MSd2(i2)),dp) 
B0m2 = MFu(i1)*Real(B0(p2,MFu2(i1),MSd2(i2)),dp) 
End If 
coupL1 = cplcUChacFuSdL(gO1,i1,i2)
coupR1 = cplcUChacFuSdR(gO1,i1,i2)
coupL2 =  Conjg(cplcUChacFuSdL(gO2,i1,i2))
coupR2 =  Conjg(cplcUChacFuSdR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +3._dp* sumL
SigR = SigR +3._dp* sumR 
SigS = SigS +3._dp* sumS 
      End Do 
     End Do 
 SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopChaMZ 
 
Subroutine Sigma1LoopFdMZ(p2,MFd,MFd2,MAh,MAh2,MSu,MSu2,MCha,MCha2,MSd,               & 
& MSd2,MChi,MChi2,Mhh,Mhh2,MVZ,MVZ2,MHpm,MHpm2,MFu,MFu2,MVWm,MVWm2,MGlu,MGlu2,           & 
& cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdChaSuL,cplcUFdChaSuR,cplcUFdChiSdL,cplcUFdChiSdR,     & 
& cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,         & 
& cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHpmL,cplcUFdFuHpmR,cplcUFdFuVWmL,cplcUFdFuVWmR,     & 
& cplcUFdGluSdL,cplcUFdGluSdR,sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh(8),MAh2(8),MSu(6),MSu2(6),MCha(5),MCha2(5),MSd(6),MSd2(6),         & 
& MChi(10),MChi2(10),Mhh(8),Mhh2(8),MVZ,MVZ2,MHpm(8),MHpm2(8),MFu(3),MFu2(3),            & 
& MVWm,MVWm2,MGlu,MGlu2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3,8),cplcUFdFdAhR(3,3,8),cplcUFdChaSuL(3,5,6),cplcUFdChaSuR(3,5,6),    & 
& cplcUFdChiSdL(3,10,6),cplcUFdChiSdR(3,10,6),cplcUFdFdhhL(3,3,8),cplcUFdFdhhR(3,3,8),   & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHpmL(3,3,8),cplcUFdFuHpmR(3,3,8),         & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFdGluSdL(3,6),cplcUFdGluSdR(3,6)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigS(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumS(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFd2(gO1),MFd2(i1),MAh2(i2)),dp) 
B0m2 = MFd(i1)*Real(B0(MFd2(gO1),MFd2(i1),MAh2(i2)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i1),MAh2(i2)),dp) 
B0m2 = MFd(i1)*Real(B0(p2,MFd2(i1),MAh2(i2)),dp) 
End If 
coupL1 = cplcUFdFdAhL(gO1,i1,i2)
coupR1 = cplcUFdFdAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! Su, Cha 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 5
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFd2(gO1),MCha2(i2),MSu2(i1)),dp) 
B0m2 = MCha(i2)*Real(B0(MFd2(gO1),MCha2(i2),MSu2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i2),MSu2(i1)),dp) 
B0m2 = MCha(i2)*Real(B0(p2,MCha2(i2),MSu2(i1)),dp) 
End If 
coupL1 = cplcUFdChaSuL(gO1,i2,i1)
coupR1 = cplcUFdChaSuR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdChaSuL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdChaSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! Sd, Chi 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFd2(gO1),MChi2(i2),MSd2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(MFd2(gO1),MChi2(i2),MSd2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MChi2(i2),MSd2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(p2,MChi2(i2),MSd2(i1)),dp) 
End If 
coupL1 = cplcUFdChiSdL(gO1,i2,i1)
coupR1 = cplcUFdChiSdR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdChiSdL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdChiSdR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! hh, Fd 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFd2(gO1),MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(MFd2(gO1),MFd2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(p2,MFd2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VZ, Fd 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = - Real(B1(MFd2(gO1),MFd2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(MFd2(gO1),MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = - Real(B1(p2,MFd2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(p2,MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Hpm, Fu 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFd2(gO1),MFu2(i2),MHpm2(i1)),dp) 
B0m2 = MFu(i2)*Real(B0(MFd2(gO1),MFu2(i2),MHpm2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i2),MHpm2(i1)),dp) 
B0m2 = MFu(i2)*Real(B0(p2,MFu2(i2),MHpm2(i1)),dp) 
End If 
coupL1 = cplcUFdFuHpmL(gO1,i2,i1)
coupR1 = cplcUFdFuHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFuHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFuHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VWm, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = - Real(B1(MFd2(gO1),MFu2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(MFd2(gO1),MFu2(i2),MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = - Real(B1(p2,MFu2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(p2,MFu2(i2),MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFdFuVWmL(gO1,i2)
coupR1 = cplcUFdFuVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFdFuVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFdFuVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Sd, Glu 
!------------------------ 
    Do i1 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFd2(gO1),MGlu2,MSd2(i1)),dp) 
B0m2 = MGlu*Real(B0(MFd2(gO1),MGlu2,MSd2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MGlu2,MSd2(i1)),dp) 
B0m2 = MGlu*Real(B0(p2,MGlu2,MSd2(i1)),dp) 
End If 
coupL1 = cplcUFdGluSdL(gO1,i1)
coupR1 = cplcUFdGluSdR(gO1,i1)
coupL2 =  Conjg(cplcUFdGluSdL(gO2,i1))
coupR2 =  Conjg(cplcUFdGluSdR(gO2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigS = SigS +4._dp/3._dp* sumS 
      End Do 
 SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopFdMZ 
 
Subroutine Sigma1LoopFuMZ(p2,MFu,MFu2,MAh,MAh2,MSu,MSu2,MChi,MChi2,MHpm,              & 
& MHpm2,MFd,MFd2,MVWm,MVWm2,Mhh,Mhh2,MVZ,MVZ2,MGlu,MGlu2,MCha,MCha2,MSd,MSd2,            & 
& cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuChiSuL,cplcUFuChiSuR,cplcUFuFdcHpmL,cplcUFuFdcHpmR,   & 
& cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,     & 
& cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFuGluSuL,cplcUFuGluSuR,       & 
& cplcChacUFuSdL,cplcChacUFuSdR,sigL,sigR,sigS)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh(8),MAh2(8),MSu(6),MSu2(6),MChi(10),MChi2(10),MHpm(8),              & 
& MHpm2(8),MFd(3),MFd2(3),MVWm,MVWm2,Mhh(8),Mhh2(8),MVZ,MVZ2,MGlu,MGlu2,MCha(5),         & 
& MCha2(5),MSd(6),MSd2(6)

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3,8),cplcUFuFuAhR(3,3,8),cplcUFuChiSuL(3,10,6),cplcUFuChiSuR(3,10,6),  & 
& cplcUFuFdcHpmL(3,3,8),cplcUFuFdcHpmR(3,3,8),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),   & 
& cplcUFuFuhhL(3,3,8),cplcUFuFuhhR(3,3,8),cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),           & 
& cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),               & 
& cplcUFuGluSuL(3,6),cplcUFuGluSuR(3,6),cplcChacUFuSdL(5,3,6),cplcChacUFuSdR(5,3,6)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigS(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumS(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigS = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
    Do i1 = 1, 3
       Do i2 = 1, 8
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFu2(gO1),MFu2(i1),MAh2(i2)),dp) 
B0m2 = MFu(i1)*Real(B0(MFu2(gO1),MFu2(i1),MAh2(i2)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i1),MAh2(i2)),dp) 
B0m2 = MFu(i1)*Real(B0(p2,MFu2(i1),MAh2(i2)),dp) 
End If 
coupL1 = cplcUFuFuAhL(gO1,i1,i2)
coupR1 = cplcUFuFuAhR(gO1,i1,i2)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1,i2))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! Su, Chi 
!------------------------ 
    Do i1 = 1, 6
       Do i2 = 1, 10
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFu2(gO1),MChi2(i2),MSu2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(MFu2(gO1),MChi2(i2),MSu2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MChi2(i2),MSu2(i1)),dp) 
B0m2 = MChi(i2)*Real(B0(p2,MChi2(i2),MSu2(i1)),dp) 
End If 
coupL1 = cplcUFuChiSuL(gO1,i2,i1)
coupR1 = cplcUFuChiSuR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuChiSuL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuChiSuR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[Hpm], Fd 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFu2(gO1),MFd2(i2),MHpm2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(MFu2(gO1),MFd2(i2),MHpm2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFd2(i2),MHpm2(i1)),dp) 
B0m2 = MFd(i2)*Real(B0(p2,MFd2(i2),MHpm2(i1)),dp) 
End If 
coupL1 = cplcUFuFdcHpmL(gO1,i2,i1)
coupR1 = cplcUFuFdcHpmR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFdcHpmL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFdcHpmR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! conj[VWm], Fd 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = - Real(B1(MFu2(gO1),MFd2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(MFu2(gO1),MFd2(i2),MVWm2)-0.5_dp*rMS,dp) 
Else 
B1m2 = - Real(B1(p2,MFd2(i2),MVWm2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(B0(p2,MFd2(i2),MVWm2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFuFdcVWmL(gO1,i2)
coupR1 = cplcUFuFdcVWmR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdcVWmL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdcVWmR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! hh, Fu 
!------------------------ 
    Do i1 = 1, 8
       Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFu2(gO1),MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(B0(MFu2(gO1),MFu2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(B0(p2,MFu2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 !------------------------ 
! VZ, Fu 
!------------------------ 
      Do i2 = 1, 3
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = - Real(B1(MFu2(gO1),MFu2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(MFu2(gO1),MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = - Real(B1(p2,MFu2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(B0(p2,MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumS(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
    End Do 
 !------------------------ 
! Su, Glu 
!------------------------ 
    Do i1 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFu2(gO1),MGlu2,MSu2(i1)),dp) 
B0m2 = MGlu*Real(B0(MFu2(gO1),MGlu2,MSu2(i1)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MGlu2,MSu2(i1)),dp) 
B0m2 = MGlu*Real(B0(p2,MGlu2,MSu2(i1)),dp) 
End If 
coupL1 = cplcUFuGluSuL(gO1,i1)
coupR1 = cplcUFuGluSuR(gO1,i1)
coupL2 =  Conjg(cplcUFuGluSuL(gO2,i1))
coupR2 =  Conjg(cplcUFuGluSuR(gO2,i1))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigS = SigS +4._dp/3._dp* sumS 
      End Do 
 !------------------------ 
! bar[Cha], Sd 
!------------------------ 
    Do i1 = 1, 5
       Do i2 = 1, 6
 SumS = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -0.5_dp*Real(B1(MFu2(gO1),MCha2(i1),MSd2(i2)),dp) 
B0m2 = MCha(i1)*Real(B0(MFu2(gO1),MCha2(i1),MSd2(i2)),dp) 
Else 
B1m2 = -0.5_dp*Real(B1(p2,MCha2(i1),MSd2(i2)),dp) 
B0m2 = MCha(i1)*Real(B0(p2,MCha2(i1),MSd2(i2)),dp) 
End If 
coupL1 = cplcChacUFuSdL(i1,gO1,i2)
coupR1 = cplcChacUFuSdR(i1,gO1,i2)
coupL2 =  Conjg(cplcChacUFuSdL(i1,gO2,i2))
coupR2 =  Conjg(cplcChacUFuSdR(i1,gO2,i2))
SumS(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigS = SigS +1._dp* sumS 
      End Do 
     End Do 
 SigL = oo16pi2*SigL 
 
SigR = oo16pi2*SigR 
 
SigS = oo16pi2*SigS 
 
End Subroutine Sigma1LoopFuMZ 
 
Complex(dp) Function FloopRXi(p2,m12,m22) 
Implicit None 

Real(dp),Intent(in)::p2,m12,m22 

If (RXi.eq.1._dp) Then 
  FloopRXi=Floop(p2,m12,m22)
Else
  If ((m12.gt.0.1).and.(m22.gt.0.1)) Then 
      FloopRXi=A0(m12)-A0(m22)+((m12-p2)*A0(m22))/m22-&
      & ((m12-p2+m22*RXi)*A0(m22*RXi))/m22+(-m12+m22+p2)*b0(p2,m12,m22)-&
      & (m12-(m12-p2)**2/m22+3._dp*p2)*b0(p2,m12,m22)-((m12-p2)**2*b0(p2,m12,m22*RXi))/m22
  Else
   If ((p2.gt.0.1_dp).or.(m12.gt.0.1_dp)) Then
    FloopRXi=A0(m12)-2._dp*(m12+p2)*B0(p2,0._dp,m12)
   Else
    FloopRXi=0.0_dp
   End if
  End if
End if
End Function FloopRXi


Complex(dp) Function SVVloop(p2,m12,m22) 
Implicit None 

Real(dp),Intent(in)::p2,m12,m22 

If (RXi.eq.1._dp) Then 
  SVVloop=4._dp*Real(B0(p2,m12,m22)-0.5_dp*rMS,dp)  
Else
  If ((m12.gt.0).and.(m22.gt.0)) Then 
      SVVloop=rMS-A0(m12)/(8._dp*m12)+(RXi*A0(m12))/(8._dp*m12)-A0(m22)/(8._dp*m22)+(RXi*A0(m22))/(8._dp*m22)+A0(m12*RXi)/(8._dp*m12) &
       & -(RXi*A0(m12*RXi))/(8._dp*m12)+A0(m22*RXi)/(8._dp*m22)-&
       & (RXi*A0(m22*RXi))/(8._dp*m22)-(5._dp*B0(p2,m12,m22))/4._dp-(m12*B0(p2,m12,m22))/(8._dp*m22) &
       & -(m22*B0(p2,m12,m22))/(8._dp*m12)+(p2*B0(p2,m12,m22))/(4._dp*m12)+(p2*B0(p2,m12,m22))/(4._dp*m22)-&
       & (p2**2*B0(p2,m12,m22))/(8._dp*m12*m22)+(m12*B0(p2,m12,m22*RXi))/(8._dp*m22)-(p2*B0(p2,m12,m22*RXi))/(4._dp*m22) &
       &+(p2**2*B0(p2,m12,m22*RXi))/(8._dp*m12*m22)-(RXi*B0(p2,m12,m22*RXi))/4._dp-&
       & (p2*RXi*B0(p2,m12,m22*RXi))/(4._dp*m12)+(m22*RXi**2*B0(p2,m12,m22*RXi))/(8._dp*m12)&
       &+(m22*B0(p2,m22,m12*RXi))/(8._dp*m12)-(p2*B0(p2,m22,m12*RXi))/(4._dp*m12)+&
       & (p2**2*B0(p2,m22,m12*RXi))/(8._dp*m12*m22)-(RXi*B0(p2,m22,m12*RXi))/4._dp-(p2*RXi*B0(p2,m22,m12*RXi))/(4._dp*m22)&
       & +(m12*RXi**2*B0(p2,m22,m12*RXi))/(8._dp*m22)-&
       & (p2**2*B0(p2,m12*RXi,m22*RXi))/(8._dp*m12*m22)+(p2*RXi*B0(p2,m12*RXi,m22*RXi))/(4._dp*m12) & 
       & +(p2*RXi*B0(p2,m12*RXi,m22*RXi))/(4._dp*m22)-(RXi**2*B0(p2,m12*RXi,m22*RXi))/4._dp-&
       & (m12*RXi**2*B0(p2,m12*RXi,m22*RXi))/(8._dp*m22)-(m22*RXi**2*B0(p2,m12*RXi,m22*RXi))/(8._dp*m12)
  Else
     SVVloop=4._dp*Real(B0(p2,m12,m22)-0.5_dp*rMS,dp)  
   End if
End if
End Function SVVloop


End Module LoopMasses_munuSSM3G 
