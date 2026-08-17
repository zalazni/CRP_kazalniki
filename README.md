# CRP_kazalniki
Podatki in skripte za CRP projekt Podnebne projekcije agroklimatskih kazalnikov

V kodi **kazalniki_izracuni_in_izrisi.R** se opravijo vsi izračuni in izrisi rezultatov, ki vključujejo naslednje izhodne datoteke:

- izračuni kazalnikov za koruzo (**kazalniki_OPSI_historical_GS.rds**) in pšenico (**kazalniki_OPSI_historical_psenica+koruza.rds**) za referenčno obdobje 1981-2010,
- podnebne projekcije kazalnikov za koruzo (**kazalniki_41-70_rcp45.rds**, **kazalniki_41-70_rcp85.rds**, **kazalniki_71-00_rcp45.rds**, **kazalniki_71-00_rcp85.rds**) in pšenico (**psenica_kazalniki_11-40_rcp45.rds**, **psenica_kazalniki_11-40_rcp85.rds**, **psenica_kazalniki_41-70_rcp45.rds**, **psenica_kazalniki_41-70_rcp85.rds**, **psenica_kazalniki_71-00_rcp45.rds**, **psenica_kazalniki_71-00_rcp85.rds**),
- vrednosti pragov (percentilov) za izbrane kazalnike v referenčnem obdobju 1981-2010 (**tn90p_referencno_obdobje_psenica+koruza.rds**, **tx99p_referencno_obdobje_psenica+koruza.rds**, **tx90p_referencno_obdobje_psenica+koruza.rds**, **t75p_referencno_obdobje_psenica+koruza.rds**, **rr75p_referencno_obdobje_psenica+koruza.rds**, **tn10p_referencno_obdobje_psenica+koruza.rds**),
- karte kazalnikov za koruzo in pšenico za referenčno obdobje 1981-2010 ter scenarija RCP4.5 in RCP8.5 za obdobji 2041-2070, 2071-2100 (slike v mapi **koruza** in **pšenica**)
- grafi časovnih potekov z ovojnicami za pšenico (slike v mapi **pšenica/kazalniki_ovojnice**)

ENG: 	Analysis of a set of agroclimatic indicators for maize and wheat

The code in kazalniki_izracuni_in_izrisi.R performs all calculations and plots the results, which include the following output files:

- calculations of indicators for maize (kazalniki_OPSI_historical_GS.rds) and wheat (kazalniki_OPSI_historical_psenica+koruza.rds) for the reference period 1981–2010,
- climate projections of indicators for maize (kazalniki_41-70_rcp45.rds, kazalniki_41-70_rcp85.rds, kazalniki_71-00_rcp45.rds, kazalniki_71-00_rcp85.rds) and wheat (psenica_kazalniki_11-40_rcp45.rds, psenica_kazalniki_11-40_rcp85.rds, psenica_kazalniki_41-70_rcp45.rds, psenica_kazalniki_41-70_rcp85.rds, psenica_kazalniki_71-00_rcp45.rds, psenica_kazalniki_71-00_rcp85.rds),
- threshold values (percentiles) for selected indicators in the 1981–2010 reference period (tn90p_referencno_obdobje_psenica+koruza.rds, tx99p_referencno_obdobje_psenica+koruza.rds, tx90p_referencno_obdobje_psenica+koruza.rds, t75p_referencno_obdobje_psenica+koruza.rds, rr75p_referencno_obdobje_psenica+koruza.rds, tn10p_referencno_obdobje_psenica+koruza.rds),
- indicator maps for maize and wheat for the reference period 1981–2010 and the RCP4.5 and RCP8.5 scenarios for the periods 2041–2070 and 2071–2100 (images in the koruza and pšenica folders)
- time-series graphs with envelopes for wheat (images in the pšenica/kazalniki_ovojnice folder)
