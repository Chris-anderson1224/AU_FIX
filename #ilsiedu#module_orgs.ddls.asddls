@AbapCatalog.sqlViewName: '/ILSIEDU/SMO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Module Organization Assignments'
define view /ILSIEDU/MODULE_ORGS as select from hrp1000 as sm
inner join hrp1001 as o
on sm.otjid = o.varyf
and o.subty = 'A501'
and o.otype = 'O'
and o.sclas = 'SM'
{
    key sm.objid as SMObjid,
    key o.objid as OObjid,
    sm.short as Short,
    sm.stext as Stext
}
where
sm.begda <= $session.system_date
and sm.endda >= $session.system_date
and o.begda <= $session.system_date
and o.endda >= $session.system_date
and sm.plvar = '01'
and o.plvar = '01'
