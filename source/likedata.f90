module likedata
  use ParamDef

  implicit none


  Type(LikeDatum) :: Mtop, Mbottom, Melectron, Mmuon, Mtau, &
                     alpha_em_inv, alpha_S
  Type(LikeDatum) :: MHiggs, Sol_NH, Sol_IH, Atm_NH, Atm_IH, &
                     Sinth12_NH, Sinth12_IH, Sinth13_NH, Sinth13_IH, &
                     Sinth23_NH, Sinth23_IH
  Type(LikeDatum) :: Bsgamma, RDeltaMBs, Delta0, RBtaunu, &
                     RBDtaunu_BDenu, Rl23, Dstaunu, Dsmunu, &
                     Bsmumu, Bdmumu, Dmunu, RKmunu_pimunu, &
                     AFBksmunu_low, BDtaunu, AnomalousMu

contains

subroutine InitializeDataSets

  character(200) :: file_grid_mass
  character(200) :: file_grid_sigma
  character(200) :: file_grid_lnlike
  character(4000) :: grid_inline


if (use_data .eq. current_data) then

   Mtop%datum_type = Gaussian
   Mtop%tau = 0.0
   Mtop%tau_percent = .false.
   Mtop%mu = 173.2
   Mtop%sigma = 0.9

   Mbottom%datum_type = Gaussian
   Mbottom%tau = 0.0
   Mbottom%tau_percent = .false.
   Mbottom%mu = 4.20
   Mbottom%sigma = 0.07

   Melectron%datum_type = Gaussian
   Melectron%tau = 0.0
   Melectron%tau_percent = .false.
   Melectron%mu = 0.510998928 !MeV
   Melectron%sigma = 0.000000011

   Mmuon%datum_type = Gaussian
   Mmuon%tau = 0.0
   Mmuon%tau_percent = .false.
   Mmuon%mu = 105.6583715 !MeV
   Mmuon%sigma = 0.0000035

   Mtau%datum_type = Gaussian
   Mtau%tau = 0.0
   Mtau%tau_percent = .false.
   Mtau%mu = 1.77682 !GeV
   Mtau%sigma = 1.6d-4

   alpha_em_inv%datum_type = Gaussian
   alpha_em_inv%tau = 0.0
   alpha_em_inv%tau_percent = .false.
   !hep-ph/0611102, Updated Nov 2006
   alpha_em_inv%mu = 127.955
   alpha_em_inv%sigma = 0.030

   alpha_S%datum_type = Gaussian
   alpha_S%tau = 0.0
   alpha_S%tau_percent = .false.
   !Updated Nov 2006
   alpha_S%mu = 0.1176
   alpha_S%sigma = 0.002

   Sol_NH%datum_type = Gaussian
   Sol_NH%tau = 0.0
   Sol_NH%tau_percent = .false.
   Sol_NH%mu = 7.39
   Sol_NH%sigma = 0.20

   Sol_IH%datum_type = Gaussian
   Sol_IH%tau = 0.0
   Sol_IH%tau_percent = .false.
   Sol_IH%mu = 7.39
   Sol_IH%sigma = 0.20

   Atm_NH%datum_type = Gaussian
   Atm_NH%tau = 0.0
   Atm_NH%tau_percent = .false.
   Atm_NH%mu = 2.525
   Atm_NH%sigma = 0.032

   Atm_IH%datum_type = Gaussian
   Atm_IH%tau = 0.0
   Atm_IH%tau_percent = .false.
   Atm_IH%mu = 2.499
   Atm_IH%sigma = 0.048

   Sinth12_NH%datum_type = Gaussian !aca seteo todos los constraints 
   Sinth12_NH%tau = 0.0
   Sinth12_NH%tau_percent = .false.
   Sinth12_NH%mu = 0.310
   Sinth12_NH%sigma = 0.012

   Sinth12_IH%datum_type = Gaussian
   Sinth12_IH%tau = 0.0
   Sinth12_IH%tau_percent = .false.
   Sinth12_IH%mu = 0.310
   Sinth12_IH%sigma = 0.013

   !Symmetrized errors: sigma^+-sigma^-
   Sinth23_NH%datum_type = Gaussian
   Sinth23_NH%tau = 0.0
   Sinth23_NH%tau_percent = .false.
   Sinth23_NH%mu = 0.580
   Sinth23_NH%sigma = 0.017

   !Symmetrized errors: sigma^+-sigma^-
   Sinth23_IH%datum_type = Gaussian
   Sinth23_IH%tau = 0.0
   Sinth23_IH%tau_percent = .false.
   Sinth23_IH%mu = 0.584
   Sinth23_IH%sigma = 0.016

   Sinth13_NH%datum_type = Gaussian
   Sinth13_NH%tau = 0.0
   Sinth13_NH%tau_percent = .false.
   !Updated Nov 2006
   Sinth13_NH%mu = 0.0224
   Sinth13_NH%sigma = 0.00065

   Sinth13_IH%datum_type = Gaussian
   Sinth13_IH%tau = 0.0
   Sinth13_IH%tau_percent = .false.
   !Updated Nov 2006
   Sinth13_IH%mu = 0.02264
   Sinth13_IH%sigma = 0.00066

   AnomalousMu%datum_type = Gaussian
   !all numbers in units of 10^10
   AnomalousMu%tau = 2.0
   AnomalousMu%tau_percent = .false.
   !Updated from arXiv:1010.4180
   AnomalousMu%mu = 28.7
   AnomalousMu%sigma = 8.0

   Bsgamma%datum_type = Gaussian
   !all numbers in units of 10^4
   Bsgamma%tau = 0.30
   Bsgamma%tau_percent = .false.
   !Updated from HFAG 2012
   Bsgamma%mu = 3.55
   Bsgamma%sigma = 0.26

   RDeltaMBs%datum_type = Gaussian
   RDeltaMBs%tau = 0.
   RDeltaMBs%tau_percent = .false.
   !Updated July 12
   RDeltaMBs%mu = 1.04
   RDeltaMBs%sigma = 0.11

   RBtaunu%datum_type = 1
   !no SUSY error
   RBtaunu%tau = 0.
   RBtaunu%tau_percent = .false.
   !Updated from HFAG 2012
   RBtaunu%mu = 1.63
   RBtaunu%sigma = 0.54

   Delta0%datum_type = 1
   !no SUSY error
   !all numbers in units of 10^2
   Delta0%tau = 1.75
   Delta0%tau_percent = .false.
   !Superiso manual: being conservative
   Delta0%mu = 3.1
   Delta0%sigma = 2.3

   RBDtaunu_BDenu%datum_type = 1
   !all numbers in units of 10^2
   RBDtaunu_BDenu%tau = 3.5
   RBDtaunu_BDenu%tau_percent = .false.
   !Updated March 09 from arXiv:0808.3551
   RBDtaunu_BDenu%mu = 41.6
   RBDtaunu_BDenu%sigma = 12.8

   BDtaunu%datum_type = 1
   !all numbers in units of 10^3
   BDtaunu%tau = 0.8
   BDtaunu%tau_percent = .false.
   !from arXiv:0709.1698
   BDtaunu%mu = 8.6
   BDtaunu%sigma = 2.71

   Dsmunu%datum_type = 1
   !all numbers in units of 10^3
   Dsmunu%tau = 0.2
   Dsmunu%tau_percent = .false.
   !Updated from HFAG 2010
   Dsmunu%mu = 5.81
   Dsmunu%sigma = 0.43

   Dmunu%datum_type = 1
   !all numbers in units of 10^4
   Dmunu%tau = 0.2
   Dmunu%tau_percent = .false.
   !Updated from PHYSICAL REVIEW D 78, 052003 (2008)
   Dmunu%mu = 3.82
   Dmunu%sigma = 0.33

   Bsmumu%datum_type = 1
   Bsmumu%tau = 0.00038
   Bsmumu%tau_percent = .false.
   !updated cds.cern.ch/record/1564324
   Bsmumu%mu = 0.029
   Bsmumu%sigma = 0.007

   Bdmumu%datum_type = 1
   Bdmumu%tau = 0.00038
   Bdmumu%tau_percent = .false.
   !updated cds.cern.ch/record/1564324
   Bdmumu%mu = 0.0036
   Bdmumu%sigma = 0.0015

   !Higgs detection
   MHiggs%datum_type = Gaussian
   MHiggs%tau = 2.0
   MHiggs%tau_percent = .false.
   !CMS (July 2012)
   MHiggs%mu = 125.3
   MHiggs%sigma = 0.6

else if (use_data .eq. synthetic_data) then


   !imaginary Higgs detection
   MHiggs%datum_type = Gaussian
   MHiggs%tau =  1.5 !factor of 2
   MHiggs%tau_percent = .false.
   MHiggs%mu =  106.97
   MHiggs%sigma =  2.0 !made up

end if

return


end subroutine InitializeDataSets


end module likedata
