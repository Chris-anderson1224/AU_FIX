@AbapCatalog.sqlViewName: '/ILSIEDU/DMRCNT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Migration Review Counts'
define view /ILSIEDU/dmr_cnts
  as select from /ilsiedu/dmrdata
{
  key runid                                            as Runid,
      count( * )                                       as total,
      sum( case corrected when 'X' then 1 else 0 end ) as corrected
}
group by
  runid
