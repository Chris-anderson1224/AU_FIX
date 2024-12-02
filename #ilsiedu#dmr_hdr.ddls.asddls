@AbapCatalog.sqlViewName: '/ILSIEDU/DMRH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Data Migration Reveiw Upload Header'
define view /ILSIEDU/DMR_HDR
  as select from /ilsiedu/dmrhdr as hdr
    inner join   /ilsiedu/dmrmt  as rmt on  hdr.runmode = rmt.runmode
                                        and rmt.spras   = $session.system_language
{
  key hdr.runid    as Runid,
      hdr.runmode  as Runmode,
      rmt.text     as RunmodeTxt,
      hdr.uname    as Uname,
      hdr.upl_date as UplDate,
      hdr.upl_time as UplTime
}
