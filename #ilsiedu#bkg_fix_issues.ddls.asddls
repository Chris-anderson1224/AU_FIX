@AbapCatalog.sqlViewName: '/ILSIEDU/BF_ISS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Fix Issues'
define view /ILSIEDU/BKG_FIX_ISSUES
  //Modules and their orgs
  as select from    /ILSIEDU/MODULE_ORGS as smo
  //Organization Data
    inner join      hrp1000              as org  on  smo.OObjid = org.objid
                                                 and org.otype  = 'O'
                                                 and org.begda  <= $session.system_date
                                                 and org.endda  >= $session.system_date
  //Students for Modules - no date limitations
    inner join      hrp1001              as stsm on  smo.SMObjid = stsm.sobid
                                                 and stsm.sclas  = 'SM'
  //Students - Only information as of today
    inner join      hrp1000              as st   on  stsm.otjid = st.otjid
                                                 and st.otype   = 'ST'
                                                 and st.begda   <= $session.system_date
                                                 and st.endda   >= $session.system_date
  //Booking Information - no date limitations
    inner join      hrpad506             as mbkg on stsm.adatanr = mbkg.adatanr
  //Appraisal top - assignment to booking
    inner join      piqdbagr_assignm     as agra on mbkg.id = agra.modreg_id
  //Appraisal credits - credits earned/graded/attemtped
    inner join      piqdbagr_foll_up     as agrf on agra.agrid = agrf.agrid
  //Appraisal general info (grade received)
    inner join      piqdbagr_gen         as agrg on agra.agrid = agrg.agrid
  //Default Appraisal template
    inner join      t7piqswitchvalue     as tmpl on  tmpl.grpid = 'APPR'
                                                 and tmpl.valid = 'TEMPL'
  //Module Credits - for comaprison on begda of booking
    left outer join hrp1741              as cred on  smo.SMObjid =  cred.objid
                                                 and cred.otype  =  'SM'
                                                 and cred.begda  <= stsm.begda
                                                 and cred.endda  >= stsm.endda
  //Module grading details
    left outer join hrp1710              as dets on  smo.SMObjid =  dets.objid
                                                 and dets.otype  =  'SM'
                                                 and dets.begda  <= stsm.begda
                                                 and dets.endda  >= stsm.endda
  //Module Rating text
    left outer join t7piqsmratingt       as rate on  rate.smrating = mbkg.smrating
                                                 and rate.spras    = $session.system_language
  //Module Status Text
    left outer join t7piqsmstatt         as stat on  stat.smstatus = mbkg.smstatus
                                                 and stat.spras    = $session.system_language

{
  //Org Data
  smo.OObjid,
  org.short                            as OShort,
  org.stext                            as OStext,
  //Module Data
  smo.SMObjid,
  smo.Short                            as SMShort,
  smo.Stext                            as SMStext,
  //Student Data
  st.objid                             as STObjid,
  st.short                             as StuNum,
  st.stext                             as StuName,
  //Booking Data
  mbkg.peryr,
  mbkg.perid,
  mbkg.smstatus,
  stat.smstatust,
  mbkg.smrating,
  rate.smratingt,
  //Module Credits
  cred.cpmin,
  cred.cpopt,
  cred.cpmax,
  cred.cpunit,
  //Booking Credits
  agrf.cpattempfu,
  //Calc proper attempted
  @Semantics.quantity.unitOfMeasure: 'CPUNITFU'
  cast( case
    when mbkg.smrating = 'AUD' then 0
    when mbkg.smstatus = '04' then 0
    when agrf.cpattempfu between cred.cpmin and cred.cpmax then agrf.cpattempfu
    else cred.cpopt
  end as piqcpattemp preserving type ) as calc_cpattempfu,
  agrf.cpearnedfu,
  //Calc proper earned
  @Semantics.quantity.unitOfMeasure: 'CPUNITFU'
  cast( case
    when mbkg.smrating = 'AUD' or agrf.cpearnedfu = 0 then 0
    when mbkg.smstatus = '02' and agrf.cpattempfu between cred.cpmin and cred.cpmax then agrf.cpattempfu
    when mbkg.smstatus = '02' then cred.cpopt
    else 0
  end as piqcpearned preserving type ) as calc_cpearnedfu,
  agrf.cpgradedfu,
  //Calc proper graded
  @Semantics.quantity.unitOfMeasure: 'CPUNITFU'
  cast( case
    when mbkg.smrating = 'AUD' or agrf.cpgradedfu = 0 then 0
    when ( mbkg.smstatus = '02' or mbkg.smstatus = '03' ) and agrf.cpattempfu between cred.cpmin and cred.cpmax then agrf.cpattempfu
    when mbkg.smstatus = '02' or mbkg.smstatus = '03' then cred.cpopt
    else 0
  end as piqcpgraded preserving type ) as calc_cpgradedfu,
  case
    when agrf.cpunitfu = '' then cred.cpunit
    else agrf.cpunitfu
  end as cpunitfu,
  //Module Scale
  dets.scaleid,
  //Booking Scale
  agrg.gradescale,
  //Module Template
  case
    when dets.templ_id = '0000' then cast( tmpl.value as piqagrtemplid )
    else dets.templ_id
  end                                  as templ_id,
  //Booking Template
  mbkg.zztempl_id,
  tmpl.value,
  mbkg.id as bkgid
}
where
      org.plvar  = '01'
  and stsm.plvar = '01'
  and st.plvar   = '01'
  and cred.plvar = '01'
  and dets.plvar = '01'
