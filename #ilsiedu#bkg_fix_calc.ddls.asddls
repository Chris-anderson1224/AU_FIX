@AbapCatalog.sqlViewName: '/ILSIEDU/BF_CALC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Fix Calculations'
define view /ILSIEDU/bkg_fix_calc
  as select from /ILSIEDU/BKG_FIX_ISSUES
{
  OObjid,
  OShort,
  OStext,
  SMObjid,
  SMShort,
  SMStext,
  STObjid,
  StuNum,
  StuName,
  peryr,
  perid,
  smrating,
  smratingt,
  smstatus,
  smstatust,
  cpmin,
  cpopt,
  cpmax,
  cpunit,
  cpattempfu,
  calc_cpattempfu,
  //Flag if invalid attempted credits
  cast( case
    when cpattempfu != calc_cpattempfu then 'X'
    else ' '
  end as xfeld ) as bad_attempt,
  cpearnedfu,
  calc_cpearnedfu,
  //Flag if invalid earned credits
  cast( case
    when cpearnedfu != calc_cpearnedfu then 'X'
    else ' '
  end as xfeld ) as bad_earned,
  cpgradedfu,
  calc_cpgradedfu,
  //Flag if invalid graded credits
  cast( case
    when cpgradedfu != calc_cpgradedfu then 'X'
    else ' '
  end as xfeld ) as bad_graded,
  cpunitfu,
  scaleid,
  gradescale,
  //flag if new scale is needed
  cast( case
    when scaleid = '' then ' '
    when scaleid != gradescale then 'X'
    else ' '
  end as xfeld ) as bad_scaleid,
  templ_id,
  zztempl_id,
  //flag if new template is needed
  cast( case
    when templ_id != zztempl_id then 'X'
    else ' '
  end as xfeld ) as bad_templid,
  bkgid
}

/*

Calculations for what is incorrect:
    Credits:
    All
        Attemtped should be between CPMIN and CPMAX
        Flag to check if credits should be the optimum?
        Rating of AUD should have 0 credits in all fields
    Module status = 2
        Earned/Graded should equal attempted
        If earned graded are 0, leave at 0
    Module status NE 2
        Earned/grade should be 0

    Templates:
    If sm templid is populated, zztemplid should be that
    if not populated, it should be the default (from t7piqswitchval)- Handled in previous view level

    Scale:
    If scale is populated, set to that
    If not populated, don't flag

*/

//  //If rating is AUD, make sure all credit values are 0
//  cast( case
//    when smrating = 'AUD' and ( cpattempfu > 0 OR cpearnedfu > 0 OR cpgradedfu > 0 ) then 'X'
//    else ' '
//  end as xfeld ) as rating_error,
//  //Check credits can be earned based on status
//  cast( case
//    when smstatus = '02' and ( ( cpearnedfu != 0 and cpearnedfu != cpattempfu ) or ( cpgradedfu != 0 and cpgradedfu != cpattempfu ) ) then 'X'
//    when smstatus != '02' and cpearnedfu > 0 then 'X'
//    else ' '
//  end as xfeld ) as status_error,
//  //Check attempted between min and max
//  cast( case
//    when cpattempfu not between cpmin and cpmax then 'X'
//    else ' '
//  end as xfeld ) as attemp_error,
