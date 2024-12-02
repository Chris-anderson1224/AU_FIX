@AbapCatalog.sqlViewName: '/ILSIEDU/BF'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Fix Items'
define view /ILSIEDU/BKG_FIX
  as select from /ILSIEDU/bkg_fix_calc
{
  //Org
  OObjid,
  OShort,
  OStext,
  //Module
  SMObjid,
  SMShort,
  SMStext,
  //Student
  STObjid,
  StuNum,
  StuName,
  //Booking
  peryr,
  perid,
  smrating,
  smratingt,
  smstatus,
  smstatust,
  //Module Credits
  cpmin,
  cpopt,
  cpmax,
  cpunit,
  //Booking Credits
  //Attempted
  cpattempfu,
  calc_cpattempfu,
  bad_attempt,
  //Earned
  cpearnedfu,
  calc_cpearnedfu,
  bad_earned,
  //Graded
  cpgradedfu,
  calc_cpgradedfu,
  bad_graded,
  //Units
  cpunitfu,
  //Scales
  scaleid,
  gradescale,
  bad_scaleid,
  //Templates
  templ_id,
  zztempl_id as bkgtempl_id,
  bad_templid,
  bkgid
}
where
     bad_attempt = 'X'
  or bad_earned  = 'X'
  or bad_graded  = 'X'
  or bad_scaleid = 'X'
  or bad_templid = 'X'
