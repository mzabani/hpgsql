-- |
-- This file has is very similar to a similarly named file from the great postgresql-simple library, more specifically
-- to https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/src/Database.PostgreSQL.Simple.TypeInfo.Static.html
-- We started with that and later came up with our own, though.
-- We thank you for their great work.
module HPgsql.TypeInfo
  ( boolOid,
    byteaOid,
    charOid,
    nameOid,
    int8Oid,
    int2Oid,
    int4Oid,
    regprocOid,
    textOid,
    oidOid,
    tidOid,
    xidOid,
    cidOid,
    xmlOid,
    pointOid,
    lsegOid,
    pathOid,
    boxOid,
    polygonOid,
    lineOid,
    cidrOid,
    float4Oid,
    float8Oid,
    unknownOid,
    circleOid,
    moneyOid,
    macaddrOid,
    inetOid,
    bpcharOid,
    varcharOid,
    dateOid,
    timeOid,
    timestampOid,
    timestamptzOid,
    intervalOid,
    timetzOid,
    bitOid,
    varbitOid,
    numericOid,
    refcursorOid,
    recordOid,
    voidOid,
    regprocedureOid,
    regoperOid,
    regoperatorOid,
    regclassOid,
    regtypeOid,
    uuidOid,
    jsonOid,
    jsonbOid,
    int2vectorOid,
    oidvectorOid,
    int4rangeOid,
    _int4rangeOid,
    numrangeOid,
    _numrangeOid,
    tsrangeOid,
    _tsrangeOid,
    tstzrangeOid,
    _tstzrangeOid,
    daterangeOid,
    _daterangeOid,
    int8rangeOid,
    _int8rangeOid,
    Format (..),
    Oid (..),
    TypeInfo (..),
    builtinPgTypesMap,
  )
where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | The text format is not well supported because
-- if you have e.g. a custom type made of types with
-- mixed formats, it's hard to derive a FromPgField
-- for it. And also for FromPgField (Vector a), we would
-- have to implement two different parsers: one for binary
-- and one for Text, which isn't great.
-- We will likely remove support for the text format eventually.
data Format = BadlySupportedTextFmt | BinaryFmt
  deriving stock (Eq, Show)

newtype Oid = Oid Int32
  deriving stock (Show)
  deriving newtype (Eq, Real, Num, Ord, Integral, Enum)

newtype TypeInfo = TypeInfo
  { typeName :: Text
  }

-- The query to get all the code you find in this file is:
-- 1. psql -X -t -d postgres -c "select typname || E'Oid :: Oid\n' || typname || 'Oid = Oid ' || oid from pg_catalog.pg_type order by oid" | sed 's|\+||g'
-- 2. psql -X -t -d postgres -c "select '(' || typname || 'Oid, TypeInfo \"' || typname || '\"),' from pg_catalog.pg_type order by oid" for the Map

builtinPgTypesMap :: Map Oid TypeInfo
builtinPgTypesMap =
  Map.fromList
    [ (boolOid, TypeInfo "bool"),
      (byteaOid, TypeInfo "bytea"),
      (charOid, TypeInfo "char"),
      (nameOid, TypeInfo "name"),
      (int8Oid, TypeInfo "int8"),
      (int2Oid, TypeInfo "int2"),
      (int2vectorOid, TypeInfo "int2vector"),
      (int4Oid, TypeInfo "int4"),
      (regprocOid, TypeInfo "regproc"),
      (textOid, TypeInfo "text"),
      (oidOid, TypeInfo "oid"),
      (tidOid, TypeInfo "tid"),
      (xidOid, TypeInfo "xid"),
      (cidOid, TypeInfo "cid"),
      (oidvectorOid, TypeInfo "oidvector"),
      (pg_ddl_commandOid, TypeInfo "pg_ddl_command"),
      (pg_typeOid, TypeInfo "pg_type"),
      (pg_attributeOid, TypeInfo "pg_attribute"),
      (pg_procOid, TypeInfo "pg_proc"),
      (pg_classOid, TypeInfo "pg_class"),
      (jsonOid, TypeInfo "json"),
      (xmlOid, TypeInfo "xml"),
      (_xmlOid, TypeInfo "_xml"),
      (pg_node_treeOid, TypeInfo "pg_node_tree"),
      (_jsonOid, TypeInfo "_json"),
      (_pg_typeOid, TypeInfo "_pg_type"),
      (table_am_handlerOid, TypeInfo "table_am_handler"),
      (_pg_attributeOid, TypeInfo "_pg_attribute"),
      (_xid8Oid, TypeInfo "_xid8"),
      (_pg_procOid, TypeInfo "_pg_proc"),
      (_pg_classOid, TypeInfo "_pg_class"),
      (index_am_handlerOid, TypeInfo "index_am_handler"),
      (pointOid, TypeInfo "point"),
      (lsegOid, TypeInfo "lseg"),
      (pathOid, TypeInfo "path"),
      (boxOid, TypeInfo "box"),
      (polygonOid, TypeInfo "polygon"),
      (lineOid, TypeInfo "line"),
      (_lineOid, TypeInfo "_line"),
      (cidrOid, TypeInfo "cidr"),
      (_cidrOid, TypeInfo "_cidr"),
      (float4Oid, TypeInfo "float4"),
      (float8Oid, TypeInfo "float8"),
      (unknownOid, TypeInfo "unknown"),
      (circleOid, TypeInfo "circle"),
      (_circleOid, TypeInfo "_circle"),
      (macaddr8Oid, TypeInfo "macaddr8"),
      (_macaddr8Oid, TypeInfo "_macaddr8"),
      (moneyOid, TypeInfo "money"),
      (_moneyOid, TypeInfo "_money"),
      (macaddrOid, TypeInfo "macaddr"),
      (inetOid, TypeInfo "inet"),
      (_boolOid, TypeInfo "_bool"),
      (_byteaOid, TypeInfo "_bytea"),
      (_charOid, TypeInfo "_char"),
      (_nameOid, TypeInfo "_name"),
      (_int2Oid, TypeInfo "_int2"),
      (_int2vectorOid, TypeInfo "_int2vector"),
      (_int4Oid, TypeInfo "_int4"),
      (_regprocOid, TypeInfo "_regproc"),
      (_textOid, TypeInfo "_text"),
      (_tidOid, TypeInfo "_tid"),
      (_xidOid, TypeInfo "_xid"),
      (_cidOid, TypeInfo "_cid"),
      (_oidvectorOid, TypeInfo "_oidvector"),
      (_bpcharOid, TypeInfo "_bpchar"),
      (_varcharOid, TypeInfo "_varchar"),
      (_int8Oid, TypeInfo "_int8"),
      (_pointOid, TypeInfo "_point"),
      (_lsegOid, TypeInfo "_lseg"),
      (_pathOid, TypeInfo "_path"),
      (_boxOid, TypeInfo "_box"),
      (_float4Oid, TypeInfo "_float4"),
      (_float8Oid, TypeInfo "_float8"),
      (_polygonOid, TypeInfo "_polygon"),
      (_oidOid, TypeInfo "_oid"),
      (aclitemOid, TypeInfo "aclitem"),
      (_aclitemOid, TypeInfo "_aclitem"),
      (_macaddrOid, TypeInfo "_macaddr"),
      (_inetOid, TypeInfo "_inet"),
      (bpcharOid, TypeInfo "bpchar"),
      (varcharOid, TypeInfo "varchar"),
      (dateOid, TypeInfo "date"),
      (timeOid, TypeInfo "time"),
      (timestampOid, TypeInfo "timestamp"),
      (_timestampOid, TypeInfo "_timestamp"),
      (_dateOid, TypeInfo "_date"),
      (_timeOid, TypeInfo "_time"),
      (timestamptzOid, TypeInfo "timestamptz"),
      (_timestamptzOid, TypeInfo "_timestamptz"),
      (intervalOid, TypeInfo "interval"),
      (_intervalOid, TypeInfo "_interval"),
      (_numericOid, TypeInfo "_numeric"),
      (pg_databaseOid, TypeInfo "pg_database"),
      (_cstringOid, TypeInfo "_cstring"),
      (timetzOid, TypeInfo "timetz"),
      (_timetzOid, TypeInfo "_timetz"),
      (bitOid, TypeInfo "bit"),
      (_bitOid, TypeInfo "_bit"),
      (varbitOid, TypeInfo "varbit"),
      (_varbitOid, TypeInfo "_varbit"),
      (numericOid, TypeInfo "numeric"),
      (refcursorOid, TypeInfo "refcursor"),
      (_refcursorOid, TypeInfo "_refcursor"),
      (regprocedureOid, TypeInfo "regprocedure"),
      (regoperOid, TypeInfo "regoper"),
      (regoperatorOid, TypeInfo "regoperator"),
      (regclassOid, TypeInfo "regclass"),
      (regtypeOid, TypeInfo "regtype"),
      (_regprocedureOid, TypeInfo "_regprocedure"),
      (_regoperOid, TypeInfo "_regoper"),
      (_regoperatorOid, TypeInfo "_regoperator"),
      (_regclassOid, TypeInfo "_regclass"),
      (_regtypeOid, TypeInfo "_regtype"),
      (recordOid, TypeInfo "record"),
      (cstringOid, TypeInfo "cstring"),
      (anyOid, TypeInfo "any"),
      (anyarrayOid, TypeInfo "anyarray"),
      (voidOid, TypeInfo "void"),
      (triggerOid, TypeInfo "trigger"),
      (language_handlerOid, TypeInfo "language_handler"),
      (internalOid, TypeInfo "internal"),
      (anyelementOid, TypeInfo "anyelement"),
      (_recordOid, TypeInfo "_record"),
      (anynonarrayOid, TypeInfo "anynonarray"),
      (pg_authidOid, TypeInfo "pg_authid"),
      (pg_auth_membersOid, TypeInfo "pg_auth_members"),
      (_txid_snapshotOid, TypeInfo "_txid_snapshot"),
      (uuidOid, TypeInfo "uuid"),
      (_uuidOid, TypeInfo "_uuid"),
      (txid_snapshotOid, TypeInfo "txid_snapshot"),
      (fdw_handlerOid, TypeInfo "fdw_handler"),
      (pg_lsnOid, TypeInfo "pg_lsn"),
      (_pg_lsnOid, TypeInfo "_pg_lsn"),
      (tsm_handlerOid, TypeInfo "tsm_handler"),
      (pg_ndistinctOid, TypeInfo "pg_ndistinct"),
      (pg_dependenciesOid, TypeInfo "pg_dependencies"),
      (anyenumOid, TypeInfo "anyenum"),
      (tsvectorOid, TypeInfo "tsvector"),
      (tsqueryOid, TypeInfo "tsquery"),
      (gtsvectorOid, TypeInfo "gtsvector"),
      (_tsvectorOid, TypeInfo "_tsvector"),
      (_gtsvectorOid, TypeInfo "_gtsvector"),
      (_tsqueryOid, TypeInfo "_tsquery"),
      (regconfigOid, TypeInfo "regconfig"),
      (_regconfigOid, TypeInfo "_regconfig"),
      (regdictionaryOid, TypeInfo "regdictionary"),
      (_regdictionaryOid, TypeInfo "_regdictionary"),
      (jsonbOid, TypeInfo "jsonb"),
      (_jsonbOid, TypeInfo "_jsonb"),
      (anyrangeOid, TypeInfo "anyrange"),
      (event_triggerOid, TypeInfo "event_trigger"),
      (int4rangeOid, TypeInfo "int4range"),
      (_int4rangeOid, TypeInfo "_int4range"),
      (numrangeOid, TypeInfo "numrange"),
      (_numrangeOid, TypeInfo "_numrange"),
      (tsrangeOid, TypeInfo "tsrange"),
      (_tsrangeOid, TypeInfo "_tsrange"),
      (tstzrangeOid, TypeInfo "tstzrange"),
      (_tstzrangeOid, TypeInfo "_tstzrange"),
      (daterangeOid, TypeInfo "daterange"),
      (_daterangeOid, TypeInfo "_daterange"),
      (int8rangeOid, TypeInfo "int8range"),
      (_int8rangeOid, TypeInfo "_int8range"),
      (pg_shseclabelOid, TypeInfo "pg_shseclabel"),
      (jsonpathOid, TypeInfo "jsonpath"),
      (_jsonpathOid, TypeInfo "_jsonpath"),
      (regnamespaceOid, TypeInfo "regnamespace"),
      (_regnamespaceOid, TypeInfo "_regnamespace"),
      (regroleOid, TypeInfo "regrole"),
      (_regroleOid, TypeInfo "_regrole"),
      (regcollationOid, TypeInfo "regcollation"),
      (_regcollationOid, TypeInfo "_regcollation"),
      (int4multirangeOid, TypeInfo "int4multirange"),
      (nummultirangeOid, TypeInfo "nummultirange"),
      (tsmultirangeOid, TypeInfo "tsmultirange"),
      (tstzmultirangeOid, TypeInfo "tstzmultirange"),
      (datemultirangeOid, TypeInfo "datemultirange"),
      (int8multirangeOid, TypeInfo "int8multirange"),
      (anymultirangeOid, TypeInfo "anymultirange"),
      (anycompatiblemultirangeOid, TypeInfo "anycompatiblemultirange"),
      (pg_brin_bloom_summaryOid, TypeInfo "pg_brin_bloom_summary"),
      (pg_brin_minmax_multi_summaryOid, TypeInfo "pg_brin_minmax_multi_summary"),
      (pg_mcv_listOid, TypeInfo "pg_mcv_list"),
      (pg_snapshotOid, TypeInfo "pg_snapshot"),
      (_pg_snapshotOid, TypeInfo "_pg_snapshot"),
      (xid8Oid, TypeInfo "xid8"),
      (anycompatibleOid, TypeInfo "anycompatible"),
      (anycompatiblearrayOid, TypeInfo "anycompatiblearray"),
      (anycompatiblenonarrayOid, TypeInfo "anycompatiblenonarray"),
      (anycompatiblerangeOid, TypeInfo "anycompatiblerange"),
      (pg_subscriptionOid, TypeInfo "pg_subscription"),
      (_int4multirangeOid, TypeInfo "_int4multirange"),
      (_nummultirangeOid, TypeInfo "_nummultirange"),
      (_tsmultirangeOid, TypeInfo "_tsmultirange"),
      (_tstzmultirangeOid, TypeInfo "_tstzmultirange"),
      (_datemultirangeOid, TypeInfo "_datemultirange"),
      (_int8multirangeOid, TypeInfo "_int8multirange"),
      (_pg_attrdefOid, TypeInfo "_pg_attrdef"),
      (pg_attrdefOid, TypeInfo "pg_attrdef"),
      (_pg_constraintOid, TypeInfo "_pg_constraint"),
      (pg_constraintOid, TypeInfo "pg_constraint"),
      (_pg_inheritsOid, TypeInfo "_pg_inherits"),
      (pg_inheritsOid, TypeInfo "pg_inherits"),
      (_pg_indexOid, TypeInfo "_pg_index"),
      (pg_indexOid, TypeInfo "pg_index"),
      (_pg_operatorOid, TypeInfo "_pg_operator"),
      (pg_operatorOid, TypeInfo "pg_operator"),
      (_pg_opfamilyOid, TypeInfo "_pg_opfamily"),
      (pg_opfamilyOid, TypeInfo "pg_opfamily"),
      (_pg_opclassOid, TypeInfo "_pg_opclass"),
      (pg_opclassOid, TypeInfo "pg_opclass"),
      (_pg_amOid, TypeInfo "_pg_am"),
      (pg_amOid, TypeInfo "pg_am"),
      (_pg_amopOid, TypeInfo "_pg_amop"),
      (pg_amopOid, TypeInfo "pg_amop"),
      (_pg_amprocOid, TypeInfo "_pg_amproc"),
      (pg_amprocOid, TypeInfo "pg_amproc"),
      (_pg_languageOid, TypeInfo "_pg_language"),
      (pg_languageOid, TypeInfo "pg_language"),
      (_pg_largeobject_metadataOid, TypeInfo "_pg_largeobject_metadata"),
      (pg_largeobject_metadataOid, TypeInfo "pg_largeobject_metadata"),
      (_pg_largeobjectOid, TypeInfo "_pg_largeobject"),
      (pg_largeobjectOid, TypeInfo "pg_largeobject"),
      (_pg_aggregateOid, TypeInfo "_pg_aggregate"),
      (pg_aggregateOid, TypeInfo "pg_aggregate"),
      (_pg_statisticOid, TypeInfo "_pg_statistic"),
      (pg_statisticOid, TypeInfo "pg_statistic"),
      (_pg_statistic_extOid, TypeInfo "_pg_statistic_ext"),
      (pg_statistic_extOid, TypeInfo "pg_statistic_ext"),
      (_pg_statistic_ext_dataOid, TypeInfo "_pg_statistic_ext_data"),
      (pg_statistic_ext_dataOid, TypeInfo "pg_statistic_ext_data"),
      (_pg_rewriteOid, TypeInfo "_pg_rewrite"),
      (pg_rewriteOid, TypeInfo "pg_rewrite"),
      (_pg_triggerOid, TypeInfo "_pg_trigger"),
      (pg_triggerOid, TypeInfo "pg_trigger"),
      (_pg_event_triggerOid, TypeInfo "_pg_event_trigger"),
      (pg_event_triggerOid, TypeInfo "pg_event_trigger"),
      (_pg_descriptionOid, TypeInfo "_pg_description"),
      (pg_descriptionOid, TypeInfo "pg_description"),
      (_pg_castOid, TypeInfo "_pg_cast"),
      (pg_castOid, TypeInfo "pg_cast"),
      (_pg_enumOid, TypeInfo "_pg_enum"),
      (pg_enumOid, TypeInfo "pg_enum"),
      (_pg_namespaceOid, TypeInfo "_pg_namespace"),
      (pg_namespaceOid, TypeInfo "pg_namespace"),
      (_pg_conversionOid, TypeInfo "_pg_conversion"),
      (pg_conversionOid, TypeInfo "pg_conversion"),
      (_pg_dependOid, TypeInfo "_pg_depend"),
      (pg_dependOid, TypeInfo "pg_depend"),
      (_pg_databaseOid, TypeInfo "_pg_database"),
      (_pg_db_role_settingOid, TypeInfo "_pg_db_role_setting"),
      (pg_db_role_settingOid, TypeInfo "pg_db_role_setting"),
      (_pg_tablespaceOid, TypeInfo "_pg_tablespace"),
      (pg_tablespaceOid, TypeInfo "pg_tablespace"),
      (_pg_authidOid, TypeInfo "_pg_authid"),
      (_pg_auth_membersOid, TypeInfo "_pg_auth_members"),
      (_pg_shdependOid, TypeInfo "_pg_shdepend"),
      (pg_shdependOid, TypeInfo "pg_shdepend"),
      (_pg_shdescriptionOid, TypeInfo "_pg_shdescription"),
      (pg_shdescriptionOid, TypeInfo "pg_shdescription"),
      (_pg_ts_configOid, TypeInfo "_pg_ts_config"),
      (pg_ts_configOid, TypeInfo "pg_ts_config"),
      (_pg_ts_config_mapOid, TypeInfo "_pg_ts_config_map"),
      (pg_ts_config_mapOid, TypeInfo "pg_ts_config_map"),
      (_pg_ts_dictOid, TypeInfo "_pg_ts_dict"),
      (pg_ts_dictOid, TypeInfo "pg_ts_dict"),
      (_pg_ts_parserOid, TypeInfo "_pg_ts_parser"),
      (pg_ts_parserOid, TypeInfo "pg_ts_parser"),
      (_pg_ts_templateOid, TypeInfo "_pg_ts_template"),
      (pg_ts_templateOid, TypeInfo "pg_ts_template"),
      (_pg_extensionOid, TypeInfo "_pg_extension"),
      (pg_extensionOid, TypeInfo "pg_extension"),
      (_pg_foreign_data_wrapperOid, TypeInfo "_pg_foreign_data_wrapper"),
      (pg_foreign_data_wrapperOid, TypeInfo "pg_foreign_data_wrapper"),
      (_pg_foreign_serverOid, TypeInfo "_pg_foreign_server"),
      (pg_foreign_serverOid, TypeInfo "pg_foreign_server"),
      (_pg_user_mappingOid, TypeInfo "_pg_user_mapping"),
      (pg_user_mappingOid, TypeInfo "pg_user_mapping"),
      (_pg_foreign_tableOid, TypeInfo "_pg_foreign_table"),
      (pg_foreign_tableOid, TypeInfo "pg_foreign_table"),
      (_pg_policyOid, TypeInfo "_pg_policy"),
      (pg_policyOid, TypeInfo "pg_policy"),
      (_pg_replication_originOid, TypeInfo "_pg_replication_origin"),
      (pg_replication_originOid, TypeInfo "pg_replication_origin"),
      (_pg_default_aclOid, TypeInfo "_pg_default_acl"),
      (pg_default_aclOid, TypeInfo "pg_default_acl"),
      (_pg_init_privsOid, TypeInfo "_pg_init_privs"),
      (pg_init_privsOid, TypeInfo "pg_init_privs"),
      (_pg_seclabelOid, TypeInfo "_pg_seclabel"),
      (pg_seclabelOid, TypeInfo "pg_seclabel"),
      (_pg_shseclabelOid, TypeInfo "_pg_shseclabel"),
      (_pg_collationOid, TypeInfo "_pg_collation"),
      (pg_collationOid, TypeInfo "pg_collation"),
      (_pg_parameter_aclOid, TypeInfo "_pg_parameter_acl"),
      (pg_parameter_aclOid, TypeInfo "pg_parameter_acl"),
      (_pg_partitioned_tableOid, TypeInfo "_pg_partitioned_table"),
      (pg_partitioned_tableOid, TypeInfo "pg_partitioned_table"),
      (_pg_rangeOid, TypeInfo "_pg_range"),
      (pg_rangeOid, TypeInfo "pg_range"),
      (_pg_transformOid, TypeInfo "_pg_transform"),
      (pg_transformOid, TypeInfo "pg_transform"),
      (_pg_sequenceOid, TypeInfo "_pg_sequence"),
      (pg_sequenceOid, TypeInfo "pg_sequence"),
      (_pg_publicationOid, TypeInfo "_pg_publication"),
      (pg_publicationOid, TypeInfo "pg_publication"),
      (_pg_publication_namespaceOid, TypeInfo "_pg_publication_namespace"),
      (pg_publication_namespaceOid, TypeInfo "pg_publication_namespace"),
      (_pg_publication_relOid, TypeInfo "_pg_publication_rel"),
      (pg_publication_relOid, TypeInfo "pg_publication_rel"),
      (_pg_subscriptionOid, TypeInfo "_pg_subscription"),
      (_pg_subscription_relOid, TypeInfo "_pg_subscription_rel"),
      (pg_subscription_relOid, TypeInfo "pg_subscription_rel"),
      (_pg_rolesOid, TypeInfo "_pg_roles"),
      (pg_rolesOid, TypeInfo "pg_roles"),
      (_pg_shadowOid, TypeInfo "_pg_shadow"),
      (pg_shadowOid, TypeInfo "pg_shadow"),
      (_pg_groupOid, TypeInfo "_pg_group"),
      (pg_groupOid, TypeInfo "pg_group"),
      (_pg_userOid, TypeInfo "_pg_user"),
      (pg_userOid, TypeInfo "pg_user"),
      (_pg_policiesOid, TypeInfo "_pg_policies"),
      (pg_policiesOid, TypeInfo "pg_policies"),
      (_pg_rulesOid, TypeInfo "_pg_rules"),
      (pg_rulesOid, TypeInfo "pg_rules"),
      (_pg_viewsOid, TypeInfo "_pg_views"),
      (pg_viewsOid, TypeInfo "pg_views"),
      (_pg_tablesOid, TypeInfo "_pg_tables"),
      (pg_tablesOid, TypeInfo "pg_tables"),
      (_pg_matviewsOid, TypeInfo "_pg_matviews"),
      (pg_matviewsOid, TypeInfo "pg_matviews"),
      (_pg_indexesOid, TypeInfo "_pg_indexes"),
      (pg_indexesOid, TypeInfo "pg_indexes"),
      (_pg_sequencesOid, TypeInfo "_pg_sequences"),
      (pg_sequencesOid, TypeInfo "pg_sequences"),
      (_pg_statsOid, TypeInfo "_pg_stats"),
      (pg_statsOid, TypeInfo "pg_stats"),
      (_pg_stats_extOid, TypeInfo "_pg_stats_ext"),
      (pg_stats_extOid, TypeInfo "pg_stats_ext"),
      (_pg_stats_ext_exprsOid, TypeInfo "_pg_stats_ext_exprs"),
      (pg_stats_ext_exprsOid, TypeInfo "pg_stats_ext_exprs"),
      (_pg_publication_tablesOid, TypeInfo "_pg_publication_tables"),
      (pg_publication_tablesOid, TypeInfo "pg_publication_tables"),
      (_pg_locksOid, TypeInfo "_pg_locks"),
      (pg_locksOid, TypeInfo "pg_locks"),
      (_pg_cursorsOid, TypeInfo "_pg_cursors"),
      (pg_cursorsOid, TypeInfo "pg_cursors"),
      (_pg_available_extensionsOid, TypeInfo "_pg_available_extensions"),
      (pg_available_extensionsOid, TypeInfo "pg_available_extensions"),
      (_pg_available_extension_versionsOid, TypeInfo "_pg_available_extension_versions"),
      (pg_available_extension_versionsOid, TypeInfo "pg_available_extension_versions"),
      (_pg_prepared_xactsOid, TypeInfo "_pg_prepared_xacts"),
      (pg_prepared_xactsOid, TypeInfo "pg_prepared_xacts"),
      (_pg_prepared_statementsOid, TypeInfo "_pg_prepared_statements"),
      (pg_prepared_statementsOid, TypeInfo "pg_prepared_statements"),
      (_pg_seclabelsOid, TypeInfo "_pg_seclabels"),
      (pg_seclabelsOid, TypeInfo "pg_seclabels"),
      (_pg_settingsOid, TypeInfo "_pg_settings"),
      (pg_settingsOid, TypeInfo "pg_settings"),
      (_pg_file_settingsOid, TypeInfo "_pg_file_settings"),
      (pg_file_settingsOid, TypeInfo "pg_file_settings"),
      (_pg_hba_file_rulesOid, TypeInfo "_pg_hba_file_rules"),
      (pg_hba_file_rulesOid, TypeInfo "pg_hba_file_rules"),
      (_pg_ident_file_mappingsOid, TypeInfo "_pg_ident_file_mappings"),
      (pg_ident_file_mappingsOid, TypeInfo "pg_ident_file_mappings"),
      (_pg_timezone_abbrevsOid, TypeInfo "_pg_timezone_abbrevs"),
      (pg_timezone_abbrevsOid, TypeInfo "pg_timezone_abbrevs"),
      (_pg_timezone_namesOid, TypeInfo "_pg_timezone_names"),
      (pg_timezone_namesOid, TypeInfo "pg_timezone_names"),
      (_pg_configOid, TypeInfo "_pg_config"),
      (pg_configOid, TypeInfo "pg_config"),
      (_pg_shmem_allocationsOid, TypeInfo "_pg_shmem_allocations"),
      (pg_shmem_allocationsOid, TypeInfo "pg_shmem_allocations"),
      (_pg_backend_memory_contextsOid, TypeInfo "_pg_backend_memory_contexts"),
      (pg_backend_memory_contextsOid, TypeInfo "pg_backend_memory_contexts"),
      (_pg_stat_all_tablesOid, TypeInfo "_pg_stat_all_tables"),
      (pg_stat_all_tablesOid, TypeInfo "pg_stat_all_tables"),
      (_pg_stat_xact_all_tablesOid, TypeInfo "_pg_stat_xact_all_tables"),
      (pg_stat_xact_all_tablesOid, TypeInfo "pg_stat_xact_all_tables"),
      (_pg_stat_sys_tablesOid, TypeInfo "_pg_stat_sys_tables"),
      (pg_stat_sys_tablesOid, TypeInfo "pg_stat_sys_tables"),
      (_pg_stat_xact_sys_tablesOid, TypeInfo "_pg_stat_xact_sys_tables"),
      (pg_stat_xact_sys_tablesOid, TypeInfo "pg_stat_xact_sys_tables"),
      (_pg_stat_user_tablesOid, TypeInfo "_pg_stat_user_tables"),
      (pg_stat_user_tablesOid, TypeInfo "pg_stat_user_tables"),
      (_pg_stat_xact_user_tablesOid, TypeInfo "_pg_stat_xact_user_tables"),
      (pg_stat_xact_user_tablesOid, TypeInfo "pg_stat_xact_user_tables"),
      (_pg_statio_all_tablesOid, TypeInfo "_pg_statio_all_tables"),
      (pg_statio_all_tablesOid, TypeInfo "pg_statio_all_tables"),
      (_pg_statio_sys_tablesOid, TypeInfo "_pg_statio_sys_tables"),
      (pg_statio_sys_tablesOid, TypeInfo "pg_statio_sys_tables"),
      (_pg_statio_user_tablesOid, TypeInfo "_pg_statio_user_tables"),
      (pg_statio_user_tablesOid, TypeInfo "pg_statio_user_tables"),
      (_pg_stat_all_indexesOid, TypeInfo "_pg_stat_all_indexes"),
      (pg_stat_all_indexesOid, TypeInfo "pg_stat_all_indexes"),
      (_pg_stat_sys_indexesOid, TypeInfo "_pg_stat_sys_indexes"),
      (pg_stat_sys_indexesOid, TypeInfo "pg_stat_sys_indexes"),
      (_pg_stat_user_indexesOid, TypeInfo "_pg_stat_user_indexes"),
      (pg_stat_user_indexesOid, TypeInfo "pg_stat_user_indexes"),
      (_pg_statio_all_indexesOid, TypeInfo "_pg_statio_all_indexes"),
      (pg_statio_all_indexesOid, TypeInfo "pg_statio_all_indexes"),
      (_pg_statio_sys_indexesOid, TypeInfo "_pg_statio_sys_indexes"),
      (pg_statio_sys_indexesOid, TypeInfo "pg_statio_sys_indexes"),
      (_pg_statio_user_indexesOid, TypeInfo "_pg_statio_user_indexes"),
      (pg_statio_user_indexesOid, TypeInfo "pg_statio_user_indexes"),
      (_pg_statio_all_sequencesOid, TypeInfo "_pg_statio_all_sequences"),
      (pg_statio_all_sequencesOid, TypeInfo "pg_statio_all_sequences"),
      (_pg_statio_sys_sequencesOid, TypeInfo "_pg_statio_sys_sequences"),
      (pg_statio_sys_sequencesOid, TypeInfo "pg_statio_sys_sequences"),
      (_pg_statio_user_sequencesOid, TypeInfo "_pg_statio_user_sequences"),
      (pg_statio_user_sequencesOid, TypeInfo "pg_statio_user_sequences"),
      (_pg_stat_activityOid, TypeInfo "_pg_stat_activity"),
      (pg_stat_activityOid, TypeInfo "pg_stat_activity"),
      (_pg_stat_replicationOid, TypeInfo "_pg_stat_replication"),
      (pg_stat_replicationOid, TypeInfo "pg_stat_replication"),
      (_pg_stat_slruOid, TypeInfo "_pg_stat_slru"),
      (pg_stat_slruOid, TypeInfo "pg_stat_slru"),
      (_pg_stat_wal_receiverOid, TypeInfo "_pg_stat_wal_receiver"),
      (pg_stat_wal_receiverOid, TypeInfo "pg_stat_wal_receiver"),
      (_pg_stat_recovery_prefetchOid, TypeInfo "_pg_stat_recovery_prefetch"),
      (pg_stat_recovery_prefetchOid, TypeInfo "pg_stat_recovery_prefetch"),
      (_pg_stat_subscriptionOid, TypeInfo "_pg_stat_subscription"),
      (pg_stat_subscriptionOid, TypeInfo "pg_stat_subscription"),
      (_pg_stat_sslOid, TypeInfo "_pg_stat_ssl"),
      (pg_stat_sslOid, TypeInfo "pg_stat_ssl"),
      (_pg_stat_gssapiOid, TypeInfo "_pg_stat_gssapi"),
      (pg_stat_gssapiOid, TypeInfo "pg_stat_gssapi"),
      (_pg_replication_slotsOid, TypeInfo "_pg_replication_slots"),
      (pg_replication_slotsOid, TypeInfo "pg_replication_slots"),
      (_pg_stat_replication_slotsOid, TypeInfo "_pg_stat_replication_slots"),
      (pg_stat_replication_slotsOid, TypeInfo "pg_stat_replication_slots"),
      (_pg_stat_databaseOid, TypeInfo "_pg_stat_database"),
      (pg_stat_databaseOid, TypeInfo "pg_stat_database"),
      (_pg_stat_database_conflictsOid, TypeInfo "_pg_stat_database_conflicts"),
      (pg_stat_database_conflictsOid, TypeInfo "pg_stat_database_conflicts"),
      (_pg_stat_user_functionsOid, TypeInfo "_pg_stat_user_functions"),
      (pg_stat_user_functionsOid, TypeInfo "pg_stat_user_functions"),
      (_pg_stat_xact_user_functionsOid, TypeInfo "_pg_stat_xact_user_functions"),
      (pg_stat_xact_user_functionsOid, TypeInfo "pg_stat_xact_user_functions"),
      (_pg_stat_archiverOid, TypeInfo "_pg_stat_archiver"),
      (pg_stat_archiverOid, TypeInfo "pg_stat_archiver"),
      (_pg_stat_bgwriterOid, TypeInfo "_pg_stat_bgwriter"),
      (pg_stat_bgwriterOid, TypeInfo "pg_stat_bgwriter"),
      (_pg_stat_ioOid, TypeInfo "_pg_stat_io"),
      (pg_stat_ioOid, TypeInfo "pg_stat_io"),
      (_pg_stat_walOid, TypeInfo "_pg_stat_wal"),
      (pg_stat_walOid, TypeInfo "pg_stat_wal"),
      (_pg_stat_progress_analyzeOid, TypeInfo "_pg_stat_progress_analyze"),
      (pg_stat_progress_analyzeOid, TypeInfo "pg_stat_progress_analyze"),
      (_pg_stat_progress_vacuumOid, TypeInfo "_pg_stat_progress_vacuum"),
      (pg_stat_progress_vacuumOid, TypeInfo "pg_stat_progress_vacuum"),
      (_pg_stat_progress_clusterOid, TypeInfo "_pg_stat_progress_cluster"),
      (pg_stat_progress_clusterOid, TypeInfo "pg_stat_progress_cluster"),
      (_pg_stat_progress_create_indexOid, TypeInfo "_pg_stat_progress_create_index"),
      (pg_stat_progress_create_indexOid, TypeInfo "pg_stat_progress_create_index"),
      (_pg_stat_progress_basebackupOid, TypeInfo "_pg_stat_progress_basebackup"),
      (pg_stat_progress_basebackupOid, TypeInfo "pg_stat_progress_basebackup"),
      (_pg_stat_progress_copyOid, TypeInfo "_pg_stat_progress_copy"),
      (pg_stat_progress_copyOid, TypeInfo "pg_stat_progress_copy"),
      (_pg_user_mappingsOid, TypeInfo "_pg_user_mappings"),
      (pg_user_mappingsOid, TypeInfo "pg_user_mappings"),
      (_pg_replication_origin_statusOid, TypeInfo "_pg_replication_origin_status"),
      (pg_replication_origin_statusOid, TypeInfo "pg_replication_origin_status"),
      (_pg_stat_subscription_statsOid, TypeInfo "_pg_stat_subscription_stats"),
      (pg_stat_subscription_statsOid, TypeInfo "pg_stat_subscription_stats"),
      (_cardinal_numberOid, TypeInfo "_cardinal_number"),
      (cardinal_numberOid, TypeInfo "cardinal_number"),
      (_character_dataOid, TypeInfo "_character_data"),
      (character_dataOid, TypeInfo "character_data"),
      (_sql_identifierOid, TypeInfo "_sql_identifier"),
      (sql_identifierOid, TypeInfo "sql_identifier"),
      (_information_schema_catalog_nameOid, TypeInfo "_information_schema_catalog_name"),
      (information_schema_catalog_nameOid, TypeInfo "information_schema_catalog_name"),
      (_time_stampOid, TypeInfo "_time_stamp"),
      (time_stampOid, TypeInfo "time_stamp"),
      (_yes_or_noOid, TypeInfo "_yes_or_no"),
      (yes_or_noOid, TypeInfo "yes_or_no"),
      (_applicable_rolesOid, TypeInfo "_applicable_roles"),
      (applicable_rolesOid, TypeInfo "applicable_roles"),
      (_administrable_role_authorizationsOid, TypeInfo "_administrable_role_authorizations"),
      (administrable_role_authorizationsOid, TypeInfo "administrable_role_authorizations"),
      (_attributesOid, TypeInfo "_attributes"),
      (attributesOid, TypeInfo "attributes"),
      (_character_setsOid, TypeInfo "_character_sets"),
      (character_setsOid, TypeInfo "character_sets"),
      (_check_constraint_routine_usageOid, TypeInfo "_check_constraint_routine_usage"),
      (check_constraint_routine_usageOid, TypeInfo "check_constraint_routine_usage"),
      (_check_constraintsOid, TypeInfo "_check_constraints"),
      (check_constraintsOid, TypeInfo "check_constraints"),
      (_collationsOid, TypeInfo "_collations"),
      (collationsOid, TypeInfo "collations"),
      (_collation_character_set_applicabilityOid, TypeInfo "_collation_character_set_applicability"),
      (collation_character_set_applicabilityOid, TypeInfo "collation_character_set_applicability"),
      (_column_column_usageOid, TypeInfo "_column_column_usage"),
      (column_column_usageOid, TypeInfo "column_column_usage"),
      (_column_domain_usageOid, TypeInfo "_column_domain_usage"),
      (column_domain_usageOid, TypeInfo "column_domain_usage"),
      (_column_privilegesOid, TypeInfo "_column_privileges"),
      (column_privilegesOid, TypeInfo "column_privileges"),
      (_column_udt_usageOid, TypeInfo "_column_udt_usage"),
      (column_udt_usageOid, TypeInfo "column_udt_usage"),
      (_columnsOid, TypeInfo "_columns"),
      (columnsOid, TypeInfo "columns"),
      (_constraint_column_usageOid, TypeInfo "_constraint_column_usage"),
      (constraint_column_usageOid, TypeInfo "constraint_column_usage"),
      (_constraint_table_usageOid, TypeInfo "_constraint_table_usage"),
      (constraint_table_usageOid, TypeInfo "constraint_table_usage"),
      (_domain_constraintsOid, TypeInfo "_domain_constraints"),
      (domain_constraintsOid, TypeInfo "domain_constraints"),
      (_domain_udt_usageOid, TypeInfo "_domain_udt_usage"),
      (domain_udt_usageOid, TypeInfo "domain_udt_usage"),
      (_domainsOid, TypeInfo "_domains"),
      (domainsOid, TypeInfo "domains"),
      (_enabled_rolesOid, TypeInfo "_enabled_roles"),
      (enabled_rolesOid, TypeInfo "enabled_roles"),
      (_key_column_usageOid, TypeInfo "_key_column_usage"),
      (key_column_usageOid, TypeInfo "key_column_usage"),
      (_parametersOid, TypeInfo "_parameters"),
      (parametersOid, TypeInfo "parameters"),
      (_referential_constraintsOid, TypeInfo "_referential_constraints"),
      (referential_constraintsOid, TypeInfo "referential_constraints"),
      (_role_column_grantsOid, TypeInfo "_role_column_grants"),
      (role_column_grantsOid, TypeInfo "role_column_grants"),
      (_routine_column_usageOid, TypeInfo "_routine_column_usage"),
      (routine_column_usageOid, TypeInfo "routine_column_usage"),
      (_routine_privilegesOid, TypeInfo "_routine_privileges"),
      (routine_privilegesOid, TypeInfo "routine_privileges"),
      (_role_routine_grantsOid, TypeInfo "_role_routine_grants"),
      (role_routine_grantsOid, TypeInfo "role_routine_grants"),
      (_routine_routine_usageOid, TypeInfo "_routine_routine_usage"),
      (routine_routine_usageOid, TypeInfo "routine_routine_usage"),
      (_routine_sequence_usageOid, TypeInfo "_routine_sequence_usage"),
      (routine_sequence_usageOid, TypeInfo "routine_sequence_usage"),
      (_routine_table_usageOid, TypeInfo "_routine_table_usage"),
      (routine_table_usageOid, TypeInfo "routine_table_usage"),
      (_routinesOid, TypeInfo "_routines"),
      (routinesOid, TypeInfo "routines"),
      (_schemataOid, TypeInfo "_schemata"),
      (schemataOid, TypeInfo "schemata"),
      (_sequencesOid, TypeInfo "_sequences"),
      (sequencesOid, TypeInfo "sequences"),
      (_sql_featuresOid, TypeInfo "_sql_features"),
      (sql_featuresOid, TypeInfo "sql_features"),
      (_sql_implementation_infoOid, TypeInfo "_sql_implementation_info"),
      (sql_implementation_infoOid, TypeInfo "sql_implementation_info"),
      (_sql_partsOid, TypeInfo "_sql_parts"),
      (sql_partsOid, TypeInfo "sql_parts"),
      (_sql_sizingOid, TypeInfo "_sql_sizing"),
      (sql_sizingOid, TypeInfo "sql_sizing"),
      (_table_constraintsOid, TypeInfo "_table_constraints"),
      (table_constraintsOid, TypeInfo "table_constraints"),
      (_table_privilegesOid, TypeInfo "_table_privileges"),
      (table_privilegesOid, TypeInfo "table_privileges"),
      (_role_table_grantsOid, TypeInfo "_role_table_grants"),
      (role_table_grantsOid, TypeInfo "role_table_grants"),
      (_tablesOid, TypeInfo "_tables"),
      (tablesOid, TypeInfo "tables"),
      (_transformsOid, TypeInfo "_transforms"),
      (transformsOid, TypeInfo "transforms"),
      (_triggered_update_columnsOid, TypeInfo "_triggered_update_columns"),
      (triggered_update_columnsOid, TypeInfo "triggered_update_columns"),
      (_triggersOid, TypeInfo "_triggers"),
      (triggersOid, TypeInfo "triggers"),
      (_udt_privilegesOid, TypeInfo "_udt_privileges"),
      (udt_privilegesOid, TypeInfo "udt_privileges"),
      (_role_udt_grantsOid, TypeInfo "_role_udt_grants"),
      (role_udt_grantsOid, TypeInfo "role_udt_grants"),
      (_usage_privilegesOid, TypeInfo "_usage_privileges"),
      (usage_privilegesOid, TypeInfo "usage_privileges"),
      (_role_usage_grantsOid, TypeInfo "_role_usage_grants"),
      (role_usage_grantsOid, TypeInfo "role_usage_grants"),
      (_user_defined_typesOid, TypeInfo "_user_defined_types"),
      (user_defined_typesOid, TypeInfo "user_defined_types"),
      (_view_column_usageOid, TypeInfo "_view_column_usage"),
      (view_column_usageOid, TypeInfo "view_column_usage"),
      (_view_routine_usageOid, TypeInfo "_view_routine_usage"),
      (view_routine_usageOid, TypeInfo "view_routine_usage"),
      (_view_table_usageOid, TypeInfo "_view_table_usage"),
      (view_table_usageOid, TypeInfo "view_table_usage"),
      (_viewsOid, TypeInfo "_views"),
      (viewsOid, TypeInfo "views"),
      (_data_type_privilegesOid, TypeInfo "_data_type_privileges"),
      (data_type_privilegesOid, TypeInfo "data_type_privileges"),
      (_element_typesOid, TypeInfo "_element_types"),
      (element_typesOid, TypeInfo "element_types"),
      (__pg_foreign_table_columnsOid, TypeInfo "__pg_foreign_table_columns"),
      (_pg_foreign_table_columnsOid, TypeInfo "_pg_foreign_table_columns"),
      (_column_optionsOid, TypeInfo "_column_options"),
      (column_optionsOid, TypeInfo "column_options"),
      (__pg_foreign_data_wrappersOid, TypeInfo "__pg_foreign_data_wrappers"),
      (_pg_foreign_data_wrappersOid, TypeInfo "_pg_foreign_data_wrappers"),
      (_foreign_data_wrapper_optionsOid, TypeInfo "_foreign_data_wrapper_options"),
      (foreign_data_wrapper_optionsOid, TypeInfo "foreign_data_wrapper_options"),
      (_foreign_data_wrappersOid, TypeInfo "_foreign_data_wrappers"),
      (foreign_data_wrappersOid, TypeInfo "foreign_data_wrappers"),
      (__pg_foreign_serversOid, TypeInfo "__pg_foreign_servers"),
      (_pg_foreign_serversOid, TypeInfo "_pg_foreign_servers"),
      (_foreign_server_optionsOid, TypeInfo "_foreign_server_options"),
      (foreign_server_optionsOid, TypeInfo "foreign_server_options"),
      (_foreign_serversOid, TypeInfo "_foreign_servers"),
      (foreign_serversOid, TypeInfo "foreign_servers"),
      (__pg_foreign_tablesOid, TypeInfo "__pg_foreign_tables"),
      (_pg_foreign_tablesOid, TypeInfo "_pg_foreign_tables"),
      (_foreign_table_optionsOid, TypeInfo "_foreign_table_options"),
      (foreign_table_optionsOid, TypeInfo "foreign_table_options"),
      (_foreign_tablesOid, TypeInfo "_foreign_tables"),
      (foreign_tablesOid, TypeInfo "foreign_tables"),
      (__pg_user_mappingsOid, TypeInfo "__pg_user_mappings"),
      (_pg_user_mappingsOid, TypeInfo "_pg_user_mappings"),
      (_user_mapping_optionsOid, TypeInfo "_user_mapping_options"),
      (user_mapping_optionsOid, TypeInfo "user_mapping_options"),
      (_user_mappingsOid, TypeInfo "_user_mappings"),
      (user_mappingsOid, TypeInfo "user_mappings")
    ]

boolOid :: Oid
boolOid = Oid 16

byteaOid :: Oid
byteaOid = Oid 17

charOid :: Oid
charOid = Oid 18

nameOid :: Oid
nameOid = Oid 19

int8Oid :: Oid
int8Oid = Oid 20

int2Oid :: Oid
int2Oid = Oid 21

int2vectorOid :: Oid
int2vectorOid = Oid 22

int4Oid :: Oid
int4Oid = Oid 23

regprocOid :: Oid
regprocOid = Oid 24

textOid :: Oid
textOid = Oid 25

oidOid :: Oid
oidOid = Oid 26

tidOid :: Oid
tidOid = Oid 27

xidOid :: Oid
xidOid = Oid 28

cidOid :: Oid
cidOid = Oid 29

oidvectorOid :: Oid
oidvectorOid = Oid 30

pg_ddl_commandOid :: Oid
pg_ddl_commandOid = Oid 32

pg_typeOid :: Oid
pg_typeOid = Oid 71

pg_attributeOid :: Oid
pg_attributeOid = Oid 75

pg_procOid :: Oid
pg_procOid = Oid 81

pg_classOid :: Oid
pg_classOid = Oid 83

jsonOid :: Oid
jsonOid = Oid 114

xmlOid :: Oid
xmlOid = Oid 142

_xmlOid :: Oid
_xmlOid = Oid 143

pg_node_treeOid :: Oid
pg_node_treeOid = Oid 194

_jsonOid :: Oid
_jsonOid = Oid 199

_pg_typeOid :: Oid
_pg_typeOid = Oid 210

table_am_handlerOid :: Oid
table_am_handlerOid = Oid 269

_pg_attributeOid :: Oid
_pg_attributeOid = Oid 270

_xid8Oid :: Oid
_xid8Oid = Oid 271

_pg_procOid :: Oid
_pg_procOid = Oid 272

_pg_classOid :: Oid
_pg_classOid = Oid 273

index_am_handlerOid :: Oid
index_am_handlerOid = Oid 325

pointOid :: Oid
pointOid = Oid 600

lsegOid :: Oid
lsegOid = Oid 601

pathOid :: Oid
pathOid = Oid 602

boxOid :: Oid
boxOid = Oid 603

polygonOid :: Oid
polygonOid = Oid 604

lineOid :: Oid
lineOid = Oid 628

_lineOid :: Oid
_lineOid = Oid 629

cidrOid :: Oid
cidrOid = Oid 650

_cidrOid :: Oid
_cidrOid = Oid 651

float4Oid :: Oid
float4Oid = Oid 700

float8Oid :: Oid
float8Oid = Oid 701

unknownOid :: Oid
unknownOid = Oid 705

circleOid :: Oid
circleOid = Oid 718

_circleOid :: Oid
_circleOid = Oid 719

macaddr8Oid :: Oid
macaddr8Oid = Oid 774

_macaddr8Oid :: Oid
_macaddr8Oid = Oid 775

moneyOid :: Oid
moneyOid = Oid 790

_moneyOid :: Oid
_moneyOid = Oid 791

macaddrOid :: Oid
macaddrOid = Oid 829

inetOid :: Oid
inetOid = Oid 869

_boolOid :: Oid
_boolOid = Oid 1000

_byteaOid :: Oid
_byteaOid = Oid 1001

_charOid :: Oid
_charOid = Oid 1002

_nameOid :: Oid
_nameOid = Oid 1003

_int2Oid :: Oid
_int2Oid = Oid 1005

_int2vectorOid :: Oid
_int2vectorOid = Oid 1006

_int4Oid :: Oid
_int4Oid = Oid 1007

_regprocOid :: Oid
_regprocOid = Oid 1008

_textOid :: Oid
_textOid = Oid 1009

_tidOid :: Oid
_tidOid = Oid 1010

_xidOid :: Oid
_xidOid = Oid 1011

_cidOid :: Oid
_cidOid = Oid 1012

_oidvectorOid :: Oid
_oidvectorOid = Oid 1013

_bpcharOid :: Oid
_bpcharOid = Oid 1014

_varcharOid :: Oid
_varcharOid = Oid 1015

_int8Oid :: Oid
_int8Oid = Oid 1016

_pointOid :: Oid
_pointOid = Oid 1017

_lsegOid :: Oid
_lsegOid = Oid 1018

_pathOid :: Oid
_pathOid = Oid 1019

_boxOid :: Oid
_boxOid = Oid 1020

_float4Oid :: Oid
_float4Oid = Oid 1021

_float8Oid :: Oid
_float8Oid = Oid 1022

_polygonOid :: Oid
_polygonOid = Oid 1027

_oidOid :: Oid
_oidOid = Oid 1028

aclitemOid :: Oid
aclitemOid = Oid 1033

_aclitemOid :: Oid
_aclitemOid = Oid 1034

_macaddrOid :: Oid
_macaddrOid = Oid 1040

_inetOid :: Oid
_inetOid = Oid 1041

bpcharOid :: Oid
bpcharOid = Oid 1042

varcharOid :: Oid
varcharOid = Oid 1043

dateOid :: Oid
dateOid = Oid 1082

timeOid :: Oid
timeOid = Oid 1083

timestampOid :: Oid
timestampOid = Oid 1114

_timestampOid :: Oid
_timestampOid = Oid 1115

_dateOid :: Oid
_dateOid = Oid 1182

_timeOid :: Oid
_timeOid = Oid 1183

timestamptzOid :: Oid
timestamptzOid = Oid 1184

_timestamptzOid :: Oid
_timestamptzOid = Oid 1185

intervalOid :: Oid
intervalOid = Oid 1186

_intervalOid :: Oid
_intervalOid = Oid 1187

_numericOid :: Oid
_numericOid = Oid 1231

pg_databaseOid :: Oid
pg_databaseOid = Oid 1248

_cstringOid :: Oid
_cstringOid = Oid 1263

timetzOid :: Oid
timetzOid = Oid 1266

_timetzOid :: Oid
_timetzOid = Oid 1270

bitOid :: Oid
bitOid = Oid 1560

_bitOid :: Oid
_bitOid = Oid 1561

varbitOid :: Oid
varbitOid = Oid 1562

_varbitOid :: Oid
_varbitOid = Oid 1563

numericOid :: Oid
numericOid = Oid 1700

refcursorOid :: Oid
refcursorOid = Oid 1790

_refcursorOid :: Oid
_refcursorOid = Oid 2201

regprocedureOid :: Oid
regprocedureOid = Oid 2202

regoperOid :: Oid
regoperOid = Oid 2203

regoperatorOid :: Oid
regoperatorOid = Oid 2204

regclassOid :: Oid
regclassOid = Oid 2205

regtypeOid :: Oid
regtypeOid = Oid 2206

_regprocedureOid :: Oid
_regprocedureOid = Oid 2207

_regoperOid :: Oid
_regoperOid = Oid 2208

_regoperatorOid :: Oid
_regoperatorOid = Oid 2209

_regclassOid :: Oid
_regclassOid = Oid 2210

_regtypeOid :: Oid
_regtypeOid = Oid 2211

recordOid :: Oid
recordOid = Oid 2249

cstringOid :: Oid
cstringOid = Oid 2275

anyOid :: Oid
anyOid = Oid 2276

anyarrayOid :: Oid
anyarrayOid = Oid 2277

voidOid :: Oid
voidOid = Oid 2278

triggerOid :: Oid
triggerOid = Oid 2279

language_handlerOid :: Oid
language_handlerOid = Oid 2280

internalOid :: Oid
internalOid = Oid 2281

anyelementOid :: Oid
anyelementOid = Oid 2283

_recordOid :: Oid
_recordOid = Oid 2287

anynonarrayOid :: Oid
anynonarrayOid = Oid 2776

pg_authidOid :: Oid
pg_authidOid = Oid 2842

pg_auth_membersOid :: Oid
pg_auth_membersOid = Oid 2843

_txid_snapshotOid :: Oid
_txid_snapshotOid = Oid 2949

uuidOid :: Oid
uuidOid = Oid 2950

_uuidOid :: Oid
_uuidOid = Oid 2951

txid_snapshotOid :: Oid
txid_snapshotOid = Oid 2970

fdw_handlerOid :: Oid
fdw_handlerOid = Oid 3115

pg_lsnOid :: Oid
pg_lsnOid = Oid 3220

_pg_lsnOid :: Oid
_pg_lsnOid = Oid 3221

tsm_handlerOid :: Oid
tsm_handlerOid = Oid 3310

pg_ndistinctOid :: Oid
pg_ndistinctOid = Oid 3361

pg_dependenciesOid :: Oid
pg_dependenciesOid = Oid 3402

anyenumOid :: Oid
anyenumOid = Oid 3500

tsvectorOid :: Oid
tsvectorOid = Oid 3614

tsqueryOid :: Oid
tsqueryOid = Oid 3615

gtsvectorOid :: Oid
gtsvectorOid = Oid 3642

_tsvectorOid :: Oid
_tsvectorOid = Oid 3643

_gtsvectorOid :: Oid
_gtsvectorOid = Oid 3644

_tsqueryOid :: Oid
_tsqueryOid = Oid 3645

regconfigOid :: Oid
regconfigOid = Oid 3734

_regconfigOid :: Oid
_regconfigOid = Oid 3735

regdictionaryOid :: Oid
regdictionaryOid = Oid 3769

_regdictionaryOid :: Oid
_regdictionaryOid = Oid 3770

jsonbOid :: Oid
jsonbOid = Oid 3802

_jsonbOid :: Oid
_jsonbOid = Oid 3807

anyrangeOid :: Oid
anyrangeOid = Oid 3831

event_triggerOid :: Oid
event_triggerOid = Oid 3838

int4rangeOid :: Oid
int4rangeOid = Oid 3904

_int4rangeOid :: Oid
_int4rangeOid = Oid 3905

numrangeOid :: Oid
numrangeOid = Oid 3906

_numrangeOid :: Oid
_numrangeOid = Oid 3907

tsrangeOid :: Oid
tsrangeOid = Oid 3908

_tsrangeOid :: Oid
_tsrangeOid = Oid 3909

tstzrangeOid :: Oid
tstzrangeOid = Oid 3910

_tstzrangeOid :: Oid
_tstzrangeOid = Oid 3911

daterangeOid :: Oid
daterangeOid = Oid 3912

_daterangeOid :: Oid
_daterangeOid = Oid 3913

int8rangeOid :: Oid
int8rangeOid = Oid 3926

_int8rangeOid :: Oid
_int8rangeOid = Oid 3927

pg_shseclabelOid :: Oid
pg_shseclabelOid = Oid 4066

jsonpathOid :: Oid
jsonpathOid = Oid 4072

_jsonpathOid :: Oid
_jsonpathOid = Oid 4073

regnamespaceOid :: Oid
regnamespaceOid = Oid 4089

_regnamespaceOid :: Oid
_regnamespaceOid = Oid 4090

regroleOid :: Oid
regroleOid = Oid 4096

_regroleOid :: Oid
_regroleOid = Oid 4097

regcollationOid :: Oid
regcollationOid = Oid 4191

_regcollationOid :: Oid
_regcollationOid = Oid 4192

int4multirangeOid :: Oid
int4multirangeOid = Oid 4451

nummultirangeOid :: Oid
nummultirangeOid = Oid 4532

tsmultirangeOid :: Oid
tsmultirangeOid = Oid 4533

tstzmultirangeOid :: Oid
tstzmultirangeOid = Oid 4534

datemultirangeOid :: Oid
datemultirangeOid = Oid 4535

int8multirangeOid :: Oid
int8multirangeOid = Oid 4536

anymultirangeOid :: Oid
anymultirangeOid = Oid 4537

anycompatiblemultirangeOid :: Oid
anycompatiblemultirangeOid = Oid 4538

pg_brin_bloom_summaryOid :: Oid
pg_brin_bloom_summaryOid = Oid 4600

pg_brin_minmax_multi_summaryOid :: Oid
pg_brin_minmax_multi_summaryOid = Oid 4601

pg_mcv_listOid :: Oid
pg_mcv_listOid = Oid 5017

pg_snapshotOid :: Oid
pg_snapshotOid = Oid 5038

_pg_snapshotOid :: Oid
_pg_snapshotOid = Oid 5039

xid8Oid :: Oid
xid8Oid = Oid 5069

anycompatibleOid :: Oid
anycompatibleOid = Oid 5077

anycompatiblearrayOid :: Oid
anycompatiblearrayOid = Oid 5078

anycompatiblenonarrayOid :: Oid
anycompatiblenonarrayOid = Oid 5079

anycompatiblerangeOid :: Oid
anycompatiblerangeOid = Oid 5080

pg_subscriptionOid :: Oid
pg_subscriptionOid = Oid 6101

_int4multirangeOid :: Oid
_int4multirangeOid = Oid 6150

_nummultirangeOid :: Oid
_nummultirangeOid = Oid 6151

_tsmultirangeOid :: Oid
_tsmultirangeOid = Oid 6152

_tstzmultirangeOid :: Oid
_tstzmultirangeOid = Oid 6153

_datemultirangeOid :: Oid
_datemultirangeOid = Oid 6155

_int8multirangeOid :: Oid
_int8multirangeOid = Oid 6157

_pg_attrdefOid :: Oid
_pg_attrdefOid = Oid 10000

pg_attrdefOid :: Oid
pg_attrdefOid = Oid 10001

_pg_constraintOid :: Oid
_pg_constraintOid = Oid 10002

pg_constraintOid :: Oid
pg_constraintOid = Oid 10003

_pg_inheritsOid :: Oid
_pg_inheritsOid = Oid 10004

pg_inheritsOid :: Oid
pg_inheritsOid = Oid 10005

_pg_indexOid :: Oid
_pg_indexOid = Oid 10006

pg_indexOid :: Oid
pg_indexOid = Oid 10007

_pg_operatorOid :: Oid
_pg_operatorOid = Oid 10008

pg_operatorOid :: Oid
pg_operatorOid = Oid 10009

_pg_opfamilyOid :: Oid
_pg_opfamilyOid = Oid 10010

pg_opfamilyOid :: Oid
pg_opfamilyOid = Oid 10011

_pg_opclassOid :: Oid
_pg_opclassOid = Oid 10012

pg_opclassOid :: Oid
pg_opclassOid = Oid 10013

_pg_amOid :: Oid
_pg_amOid = Oid 10014

pg_amOid :: Oid
pg_amOid = Oid 10015

_pg_amopOid :: Oid
_pg_amopOid = Oid 10016

pg_amopOid :: Oid
pg_amopOid = Oid 10017

_pg_amprocOid :: Oid
_pg_amprocOid = Oid 10018

pg_amprocOid :: Oid
pg_amprocOid = Oid 10019

_pg_languageOid :: Oid
_pg_languageOid = Oid 10020

pg_languageOid :: Oid
pg_languageOid = Oid 10021

_pg_largeobject_metadataOid :: Oid
_pg_largeobject_metadataOid = Oid 10022

pg_largeobject_metadataOid :: Oid
pg_largeobject_metadataOid = Oid 10023

_pg_largeobjectOid :: Oid
_pg_largeobjectOid = Oid 10024

pg_largeobjectOid :: Oid
pg_largeobjectOid = Oid 10025

_pg_aggregateOid :: Oid
_pg_aggregateOid = Oid 10026

pg_aggregateOid :: Oid
pg_aggregateOid = Oid 10027

_pg_statisticOid :: Oid
_pg_statisticOid = Oid 10028

pg_statisticOid :: Oid
pg_statisticOid = Oid 10029

_pg_statistic_extOid :: Oid
_pg_statistic_extOid = Oid 10030

pg_statistic_extOid :: Oid
pg_statistic_extOid = Oid 10031

_pg_statistic_ext_dataOid :: Oid
_pg_statistic_ext_dataOid = Oid 10032

pg_statistic_ext_dataOid :: Oid
pg_statistic_ext_dataOid = Oid 10033

_pg_rewriteOid :: Oid
_pg_rewriteOid = Oid 10034

pg_rewriteOid :: Oid
pg_rewriteOid = Oid 10035

_pg_triggerOid :: Oid
_pg_triggerOid = Oid 10036

pg_triggerOid :: Oid
pg_triggerOid = Oid 10037

_pg_event_triggerOid :: Oid
_pg_event_triggerOid = Oid 10038

pg_event_triggerOid :: Oid
pg_event_triggerOid = Oid 10039

_pg_descriptionOid :: Oid
_pg_descriptionOid = Oid 10040

pg_descriptionOid :: Oid
pg_descriptionOid = Oid 10041

_pg_castOid :: Oid
_pg_castOid = Oid 10042

pg_castOid :: Oid
pg_castOid = Oid 10043

_pg_enumOid :: Oid
_pg_enumOid = Oid 10044

pg_enumOid :: Oid
pg_enumOid = Oid 10045

_pg_namespaceOid :: Oid
_pg_namespaceOid = Oid 10046

pg_namespaceOid :: Oid
pg_namespaceOid = Oid 10047

_pg_conversionOid :: Oid
_pg_conversionOid = Oid 10048

pg_conversionOid :: Oid
pg_conversionOid = Oid 10049

_pg_dependOid :: Oid
_pg_dependOid = Oid 10050

pg_dependOid :: Oid
pg_dependOid = Oid 10051

_pg_databaseOid :: Oid
_pg_databaseOid = Oid 10052

_pg_db_role_settingOid :: Oid
_pg_db_role_settingOid = Oid 10053

pg_db_role_settingOid :: Oid
pg_db_role_settingOid = Oid 10054

_pg_tablespaceOid :: Oid
_pg_tablespaceOid = Oid 10055

pg_tablespaceOid :: Oid
pg_tablespaceOid = Oid 10056

_pg_authidOid :: Oid
_pg_authidOid = Oid 10057

_pg_auth_membersOid :: Oid
_pg_auth_membersOid = Oid 10058

_pg_shdependOid :: Oid
_pg_shdependOid = Oid 10059

pg_shdependOid :: Oid
pg_shdependOid = Oid 10060

_pg_shdescriptionOid :: Oid
_pg_shdescriptionOid = Oid 10061

pg_shdescriptionOid :: Oid
pg_shdescriptionOid = Oid 10062

_pg_ts_configOid :: Oid
_pg_ts_configOid = Oid 10063

pg_ts_configOid :: Oid
pg_ts_configOid = Oid 10064

_pg_ts_config_mapOid :: Oid
_pg_ts_config_mapOid = Oid 10065

pg_ts_config_mapOid :: Oid
pg_ts_config_mapOid = Oid 10066

_pg_ts_dictOid :: Oid
_pg_ts_dictOid = Oid 10067

pg_ts_dictOid :: Oid
pg_ts_dictOid = Oid 10068

_pg_ts_parserOid :: Oid
_pg_ts_parserOid = Oid 10069

pg_ts_parserOid :: Oid
pg_ts_parserOid = Oid 10070

_pg_ts_templateOid :: Oid
_pg_ts_templateOid = Oid 10071

pg_ts_templateOid :: Oid
pg_ts_templateOid = Oid 10072

_pg_extensionOid :: Oid
_pg_extensionOid = Oid 10073

pg_extensionOid :: Oid
pg_extensionOid = Oid 10074

_pg_foreign_data_wrapperOid :: Oid
_pg_foreign_data_wrapperOid = Oid 10075

pg_foreign_data_wrapperOid :: Oid
pg_foreign_data_wrapperOid = Oid 10076

_pg_foreign_serverOid :: Oid
_pg_foreign_serverOid = Oid 10077

pg_foreign_serverOid :: Oid
pg_foreign_serverOid = Oid 10078

_pg_user_mappingOid :: Oid
_pg_user_mappingOid = Oid 10079

pg_user_mappingOid :: Oid
pg_user_mappingOid = Oid 10080

_pg_foreign_tableOid :: Oid
_pg_foreign_tableOid = Oid 10081

pg_foreign_tableOid :: Oid
pg_foreign_tableOid = Oid 10082

_pg_policyOid :: Oid
_pg_policyOid = Oid 10083

pg_policyOid :: Oid
pg_policyOid = Oid 10084

_pg_replication_originOid :: Oid
_pg_replication_originOid = Oid 10085

pg_replication_originOid :: Oid
pg_replication_originOid = Oid 10086

_pg_default_aclOid :: Oid
_pg_default_aclOid = Oid 10087

pg_default_aclOid :: Oid
pg_default_aclOid = Oid 10088

_pg_init_privsOid :: Oid
_pg_init_privsOid = Oid 10089

pg_init_privsOid :: Oid
pg_init_privsOid = Oid 10090

_pg_seclabelOid :: Oid
_pg_seclabelOid = Oid 10091

pg_seclabelOid :: Oid
pg_seclabelOid = Oid 10092

_pg_shseclabelOid :: Oid
_pg_shseclabelOid = Oid 10093

_pg_collationOid :: Oid
_pg_collationOid = Oid 10094

pg_collationOid :: Oid
pg_collationOid = Oid 10095

_pg_parameter_aclOid :: Oid
_pg_parameter_aclOid = Oid 10096

pg_parameter_aclOid :: Oid
pg_parameter_aclOid = Oid 10097

_pg_partitioned_tableOid :: Oid
_pg_partitioned_tableOid = Oid 10098

pg_partitioned_tableOid :: Oid
pg_partitioned_tableOid = Oid 10099

_pg_rangeOid :: Oid
_pg_rangeOid = Oid 10100

pg_rangeOid :: Oid
pg_rangeOid = Oid 10101

_pg_transformOid :: Oid
_pg_transformOid = Oid 10102

pg_transformOid :: Oid
pg_transformOid = Oid 10103

_pg_sequenceOid :: Oid
_pg_sequenceOid = Oid 10104

pg_sequenceOid :: Oid
pg_sequenceOid = Oid 10105

_pg_publicationOid :: Oid
_pg_publicationOid = Oid 10106

pg_publicationOid :: Oid
pg_publicationOid = Oid 10107

_pg_publication_namespaceOid :: Oid
_pg_publication_namespaceOid = Oid 10108

pg_publication_namespaceOid :: Oid
pg_publication_namespaceOid = Oid 10109

_pg_publication_relOid :: Oid
_pg_publication_relOid = Oid 10110

pg_publication_relOid :: Oid
pg_publication_relOid = Oid 10111

_pg_subscriptionOid :: Oid
_pg_subscriptionOid = Oid 10112

_pg_subscription_relOid :: Oid
_pg_subscription_relOid = Oid 10113

pg_subscription_relOid :: Oid
pg_subscription_relOid = Oid 10114

_pg_rolesOid :: Oid
_pg_rolesOid = Oid 12001

pg_rolesOid :: Oid
pg_rolesOid = Oid 12002

_pg_shadowOid :: Oid
_pg_shadowOid = Oid 12006

pg_shadowOid :: Oid
pg_shadowOid = Oid 12007

_pg_groupOid :: Oid
_pg_groupOid = Oid 12011

pg_groupOid :: Oid
pg_groupOid = Oid 12012

_pg_userOid :: Oid
_pg_userOid = Oid 12015

pg_userOid :: Oid
pg_userOid = Oid 12016

_pg_policiesOid :: Oid
_pg_policiesOid = Oid 12019

pg_policiesOid :: Oid
pg_policiesOid = Oid 12020

_pg_rulesOid :: Oid
_pg_rulesOid = Oid 12024

pg_rulesOid :: Oid
pg_rulesOid = Oid 12025

_pg_viewsOid :: Oid
_pg_viewsOid = Oid 12029

pg_viewsOid :: Oid
pg_viewsOid = Oid 12030

_pg_tablesOid :: Oid
_pg_tablesOid = Oid 12034

pg_tablesOid :: Oid
pg_tablesOid = Oid 12035

_pg_matviewsOid :: Oid
_pg_matviewsOid = Oid 12039

pg_matviewsOid :: Oid
pg_matviewsOid = Oid 12040

_pg_indexesOid :: Oid
_pg_indexesOid = Oid 12044

pg_indexesOid :: Oid
pg_indexesOid = Oid 12045

_pg_sequencesOid :: Oid
_pg_sequencesOid = Oid 12049

pg_sequencesOid :: Oid
pg_sequencesOid = Oid 12050

_pg_statsOid :: Oid
_pg_statsOid = Oid 12054

pg_statsOid :: Oid
pg_statsOid = Oid 12055

_pg_stats_extOid :: Oid
_pg_stats_extOid = Oid 12059

pg_stats_extOid :: Oid
pg_stats_extOid = Oid 12060

_pg_stats_ext_exprsOid :: Oid
_pg_stats_ext_exprsOid = Oid 12064

pg_stats_ext_exprsOid :: Oid
pg_stats_ext_exprsOid = Oid 12065

_pg_publication_tablesOid :: Oid
_pg_publication_tablesOid = Oid 12069

pg_publication_tablesOid :: Oid
pg_publication_tablesOid = Oid 12070

_pg_locksOid :: Oid
_pg_locksOid = Oid 12074

pg_locksOid :: Oid
pg_locksOid = Oid 12075

_pg_cursorsOid :: Oid
_pg_cursorsOid = Oid 12078

pg_cursorsOid :: Oid
pg_cursorsOid = Oid 12079

_pg_available_extensionsOid :: Oid
_pg_available_extensionsOid = Oid 12082

pg_available_extensionsOid :: Oid
pg_available_extensionsOid = Oid 12083

_pg_available_extension_versionsOid :: Oid
_pg_available_extension_versionsOid = Oid 12086

pg_available_extension_versionsOid :: Oid
pg_available_extension_versionsOid = Oid 12087

_pg_prepared_xactsOid :: Oid
_pg_prepared_xactsOid = Oid 12091

pg_prepared_xactsOid :: Oid
pg_prepared_xactsOid = Oid 12092

_pg_prepared_statementsOid :: Oid
_pg_prepared_statementsOid = Oid 12096

pg_prepared_statementsOid :: Oid
pg_prepared_statementsOid = Oid 12097

_pg_seclabelsOid :: Oid
_pg_seclabelsOid = Oid 12100

pg_seclabelsOid :: Oid
pg_seclabelsOid = Oid 12101

_pg_settingsOid :: Oid
_pg_settingsOid = Oid 12105

pg_settingsOid :: Oid
pg_settingsOid = Oid 12106

_pg_file_settingsOid :: Oid
_pg_file_settingsOid = Oid 12111

pg_file_settingsOid :: Oid
pg_file_settingsOid = Oid 12112

_pg_hba_file_rulesOid :: Oid
_pg_hba_file_rulesOid = Oid 12115

pg_hba_file_rulesOid :: Oid
pg_hba_file_rulesOid = Oid 12116

_pg_ident_file_mappingsOid :: Oid
_pg_ident_file_mappingsOid = Oid 12119

pg_ident_file_mappingsOid :: Oid
pg_ident_file_mappingsOid = Oid 12120

_pg_timezone_abbrevsOid :: Oid
_pg_timezone_abbrevsOid = Oid 12123

pg_timezone_abbrevsOid :: Oid
pg_timezone_abbrevsOid = Oid 12124

_pg_timezone_namesOid :: Oid
_pg_timezone_namesOid = Oid 12127

pg_timezone_namesOid :: Oid
pg_timezone_namesOid = Oid 12128

_pg_configOid :: Oid
_pg_configOid = Oid 12131

pg_configOid :: Oid
pg_configOid = Oid 12132

_pg_shmem_allocationsOid :: Oid
_pg_shmem_allocationsOid = Oid 12135

pg_shmem_allocationsOid :: Oid
pg_shmem_allocationsOid = Oid 12136

_pg_backend_memory_contextsOid :: Oid
_pg_backend_memory_contextsOid = Oid 12139

pg_backend_memory_contextsOid :: Oid
pg_backend_memory_contextsOid = Oid 12140

_pg_stat_all_tablesOid :: Oid
_pg_stat_all_tablesOid = Oid 12143

pg_stat_all_tablesOid :: Oid
pg_stat_all_tablesOid = Oid 12144

_pg_stat_xact_all_tablesOid :: Oid
_pg_stat_xact_all_tablesOid = Oid 12148

pg_stat_xact_all_tablesOid :: Oid
pg_stat_xact_all_tablesOid = Oid 12149

_pg_stat_sys_tablesOid :: Oid
_pg_stat_sys_tablesOid = Oid 12153

pg_stat_sys_tablesOid :: Oid
pg_stat_sys_tablesOid = Oid 12154

_pg_stat_xact_sys_tablesOid :: Oid
_pg_stat_xact_sys_tablesOid = Oid 12158

pg_stat_xact_sys_tablesOid :: Oid
pg_stat_xact_sys_tablesOid = Oid 12159

_pg_stat_user_tablesOid :: Oid
_pg_stat_user_tablesOid = Oid 12162

pg_stat_user_tablesOid :: Oid
pg_stat_user_tablesOid = Oid 12163

_pg_stat_xact_user_tablesOid :: Oid
_pg_stat_xact_user_tablesOid = Oid 12167

pg_stat_xact_user_tablesOid :: Oid
pg_stat_xact_user_tablesOid = Oid 12168

_pg_statio_all_tablesOid :: Oid
_pg_statio_all_tablesOid = Oid 12171

pg_statio_all_tablesOid :: Oid
pg_statio_all_tablesOid = Oid 12172

_pg_statio_sys_tablesOid :: Oid
_pg_statio_sys_tablesOid = Oid 12176

pg_statio_sys_tablesOid :: Oid
pg_statio_sys_tablesOid = Oid 12177

_pg_statio_user_tablesOid :: Oid
_pg_statio_user_tablesOid = Oid 12180

pg_statio_user_tablesOid :: Oid
pg_statio_user_tablesOid = Oid 12181

_pg_stat_all_indexesOid :: Oid
_pg_stat_all_indexesOid = Oid 12184

pg_stat_all_indexesOid :: Oid
pg_stat_all_indexesOid = Oid 12185

_pg_stat_sys_indexesOid :: Oid
_pg_stat_sys_indexesOid = Oid 12189

pg_stat_sys_indexesOid :: Oid
pg_stat_sys_indexesOid = Oid 12190

_pg_stat_user_indexesOid :: Oid
_pg_stat_user_indexesOid = Oid 12193

pg_stat_user_indexesOid :: Oid
pg_stat_user_indexesOid = Oid 12194

_pg_statio_all_indexesOid :: Oid
_pg_statio_all_indexesOid = Oid 12197

pg_statio_all_indexesOid :: Oid
pg_statio_all_indexesOid = Oid 12198

_pg_statio_sys_indexesOid :: Oid
_pg_statio_sys_indexesOid = Oid 12202

pg_statio_sys_indexesOid :: Oid
pg_statio_sys_indexesOid = Oid 12203

_pg_statio_user_indexesOid :: Oid
_pg_statio_user_indexesOid = Oid 12206

pg_statio_user_indexesOid :: Oid
pg_statio_user_indexesOid = Oid 12207

_pg_statio_all_sequencesOid :: Oid
_pg_statio_all_sequencesOid = Oid 12210

pg_statio_all_sequencesOid :: Oid
pg_statio_all_sequencesOid = Oid 12211

_pg_statio_sys_sequencesOid :: Oid
_pg_statio_sys_sequencesOid = Oid 12215

pg_statio_sys_sequencesOid :: Oid
pg_statio_sys_sequencesOid = Oid 12216

_pg_statio_user_sequencesOid :: Oid
_pg_statio_user_sequencesOid = Oid 12219

pg_statio_user_sequencesOid :: Oid
pg_statio_user_sequencesOid = Oid 12220

_pg_stat_activityOid :: Oid
_pg_stat_activityOid = Oid 12223

pg_stat_activityOid :: Oid
pg_stat_activityOid = Oid 12224

_pg_stat_replicationOid :: Oid
_pg_stat_replicationOid = Oid 12228

pg_stat_replicationOid :: Oid
pg_stat_replicationOid = Oid 12229

_pg_stat_slruOid :: Oid
_pg_stat_slruOid = Oid 12233

pg_stat_slruOid :: Oid
pg_stat_slruOid = Oid 12234

_pg_stat_wal_receiverOid :: Oid
_pg_stat_wal_receiverOid = Oid 12237

pg_stat_wal_receiverOid :: Oid
pg_stat_wal_receiverOid = Oid 12238

_pg_stat_recovery_prefetchOid :: Oid
_pg_stat_recovery_prefetchOid = Oid 12241

pg_stat_recovery_prefetchOid :: Oid
pg_stat_recovery_prefetchOid = Oid 12242

_pg_stat_subscriptionOid :: Oid
_pg_stat_subscriptionOid = Oid 12245

pg_stat_subscriptionOid :: Oid
pg_stat_subscriptionOid = Oid 12246

_pg_stat_sslOid :: Oid
_pg_stat_sslOid = Oid 12250

pg_stat_sslOid :: Oid
pg_stat_sslOid = Oid 12251

_pg_stat_gssapiOid :: Oid
_pg_stat_gssapiOid = Oid 12254

pg_stat_gssapiOid :: Oid
pg_stat_gssapiOid = Oid 12255

_pg_replication_slotsOid :: Oid
_pg_replication_slotsOid = Oid 12258

pg_replication_slotsOid :: Oid
pg_replication_slotsOid = Oid 12259

_pg_stat_replication_slotsOid :: Oid
_pg_stat_replication_slotsOid = Oid 12263

pg_stat_replication_slotsOid :: Oid
pg_stat_replication_slotsOid = Oid 12264

_pg_stat_databaseOid :: Oid
_pg_stat_databaseOid = Oid 12267

pg_stat_databaseOid :: Oid
pg_stat_databaseOid = Oid 12268

_pg_stat_database_conflictsOid :: Oid
_pg_stat_database_conflictsOid = Oid 12272

pg_stat_database_conflictsOid :: Oid
pg_stat_database_conflictsOid = Oid 12273

_pg_stat_user_functionsOid :: Oid
_pg_stat_user_functionsOid = Oid 12276

pg_stat_user_functionsOid :: Oid
pg_stat_user_functionsOid = Oid 12277

_pg_stat_xact_user_functionsOid :: Oid
_pg_stat_xact_user_functionsOid = Oid 12281

pg_stat_xact_user_functionsOid :: Oid
pg_stat_xact_user_functionsOid = Oid 12282

_pg_stat_archiverOid :: Oid
_pg_stat_archiverOid = Oid 12286

pg_stat_archiverOid :: Oid
pg_stat_archiverOid = Oid 12287

_pg_stat_bgwriterOid :: Oid
_pg_stat_bgwriterOid = Oid 12290

pg_stat_bgwriterOid :: Oid
pg_stat_bgwriterOid = Oid 12291

_pg_stat_ioOid :: Oid
_pg_stat_ioOid = Oid 12294

pg_stat_ioOid :: Oid
pg_stat_ioOid = Oid 12295

_pg_stat_walOid :: Oid
_pg_stat_walOid = Oid 12298

pg_stat_walOid :: Oid
pg_stat_walOid = Oid 12299

_pg_stat_progress_analyzeOid :: Oid
_pg_stat_progress_analyzeOid = Oid 12302

pg_stat_progress_analyzeOid :: Oid
pg_stat_progress_analyzeOid = Oid 12303

_pg_stat_progress_vacuumOid :: Oid
_pg_stat_progress_vacuumOid = Oid 12307

pg_stat_progress_vacuumOid :: Oid
pg_stat_progress_vacuumOid = Oid 12308

_pg_stat_progress_clusterOid :: Oid
_pg_stat_progress_clusterOid = Oid 12312

pg_stat_progress_clusterOid :: Oid
pg_stat_progress_clusterOid = Oid 12313

_pg_stat_progress_create_indexOid :: Oid
_pg_stat_progress_create_indexOid = Oid 12317

pg_stat_progress_create_indexOid :: Oid
pg_stat_progress_create_indexOid = Oid 12318

_pg_stat_progress_basebackupOid :: Oid
_pg_stat_progress_basebackupOid = Oid 12322

pg_stat_progress_basebackupOid :: Oid
pg_stat_progress_basebackupOid = Oid 12323

_pg_stat_progress_copyOid :: Oid
_pg_stat_progress_copyOid = Oid 12327

pg_stat_progress_copyOid :: Oid
pg_stat_progress_copyOid = Oid 12328

pg_user_mappingsOid :: Oid
pg_user_mappingsOid = Oid 12333

_pg_replication_origin_statusOid :: Oid
_pg_replication_origin_statusOid = Oid 12337

pg_replication_origin_statusOid :: Oid
pg_replication_origin_statusOid = Oid 12338

_pg_stat_subscription_statsOid :: Oid
_pg_stat_subscription_statsOid = Oid 12341

pg_stat_subscription_statsOid :: Oid
pg_stat_subscription_statsOid = Oid 12342

_cardinal_numberOid :: Oid
_cardinal_numberOid = Oid 13291

cardinal_numberOid :: Oid
cardinal_numberOid = Oid 13292

_character_dataOid :: Oid
_character_dataOid = Oid 13294

character_dataOid :: Oid
character_dataOid = Oid 13295

_sql_identifierOid :: Oid
_sql_identifierOid = Oid 13296

sql_identifierOid :: Oid
sql_identifierOid = Oid 13297

_information_schema_catalog_nameOid :: Oid
_information_schema_catalog_nameOid = Oid 13299

information_schema_catalog_nameOid :: Oid
information_schema_catalog_nameOid = Oid 13300

_time_stampOid :: Oid
_time_stampOid = Oid 13302

time_stampOid :: Oid
time_stampOid = Oid 13303

_yes_or_noOid :: Oid
_yes_or_noOid = Oid 13304

yes_or_noOid :: Oid
yes_or_noOid = Oid 13305

_applicable_rolesOid :: Oid
_applicable_rolesOid = Oid 13308

applicable_rolesOid :: Oid
applicable_rolesOid = Oid 13309

_administrable_role_authorizationsOid :: Oid
_administrable_role_authorizationsOid = Oid 13313

administrable_role_authorizationsOid :: Oid
administrable_role_authorizationsOid = Oid 13314

_attributesOid :: Oid
_attributesOid = Oid 13317

attributesOid :: Oid
attributesOid = Oid 13318

_character_setsOid :: Oid
_character_setsOid = Oid 13322

character_setsOid :: Oid
character_setsOid = Oid 13323

_check_constraint_routine_usageOid :: Oid
_check_constraint_routine_usageOid = Oid 13327

check_constraint_routine_usageOid :: Oid
check_constraint_routine_usageOid = Oid 13328

_check_constraintsOid :: Oid
_check_constraintsOid = Oid 13332

check_constraintsOid :: Oid
check_constraintsOid = Oid 13333

_collationsOid :: Oid
_collationsOid = Oid 13337

collationsOid :: Oid
collationsOid = Oid 13338

_collation_character_set_applicabilityOid :: Oid
_collation_character_set_applicabilityOid = Oid 13342

collation_character_set_applicabilityOid :: Oid
collation_character_set_applicabilityOid = Oid 13343

_column_column_usageOid :: Oid
_column_column_usageOid = Oid 13347

column_column_usageOid :: Oid
column_column_usageOid = Oid 13348

_column_domain_usageOid :: Oid
_column_domain_usageOid = Oid 13352

column_domain_usageOid :: Oid
column_domain_usageOid = Oid 13353

_column_privilegesOid :: Oid
_column_privilegesOid = Oid 13357

column_privilegesOid :: Oid
column_privilegesOid = Oid 13358

_column_udt_usageOid :: Oid
_column_udt_usageOid = Oid 13362

column_udt_usageOid :: Oid
column_udt_usageOid = Oid 13363

_columnsOid :: Oid
_columnsOid = Oid 13367

columnsOid :: Oid
columnsOid = Oid 13368

_constraint_column_usageOid :: Oid
_constraint_column_usageOid = Oid 13372

constraint_column_usageOid :: Oid
constraint_column_usageOid = Oid 13373

_constraint_table_usageOid :: Oid
_constraint_table_usageOid = Oid 13377

constraint_table_usageOid :: Oid
constraint_table_usageOid = Oid 13378

_domain_constraintsOid :: Oid
_domain_constraintsOid = Oid 13382

domain_constraintsOid :: Oid
domain_constraintsOid = Oid 13383

_domain_udt_usageOid :: Oid
_domain_udt_usageOid = Oid 13387

domain_udt_usageOid :: Oid
domain_udt_usageOid = Oid 13388

_domainsOid :: Oid
_domainsOid = Oid 13392

domainsOid :: Oid
domainsOid = Oid 13393

_enabled_rolesOid :: Oid
_enabled_rolesOid = Oid 13397

enabled_rolesOid :: Oid
enabled_rolesOid = Oid 13398

_key_column_usageOid :: Oid
_key_column_usageOid = Oid 13401

key_column_usageOid :: Oid
key_column_usageOid = Oid 13402

_parametersOid :: Oid
_parametersOid = Oid 13406

parametersOid :: Oid
parametersOid = Oid 13407

_referential_constraintsOid :: Oid
_referential_constraintsOid = Oid 13411

referential_constraintsOid :: Oid
referential_constraintsOid = Oid 13412

_role_column_grantsOid :: Oid
_role_column_grantsOid = Oid 13416

role_column_grantsOid :: Oid
role_column_grantsOid = Oid 13417

_routine_column_usageOid :: Oid
_routine_column_usageOid = Oid 13420

routine_column_usageOid :: Oid
routine_column_usageOid = Oid 13421

_routine_privilegesOid :: Oid
_routine_privilegesOid = Oid 13425

routine_privilegesOid :: Oid
routine_privilegesOid = Oid 13426

_role_routine_grantsOid :: Oid
_role_routine_grantsOid = Oid 13430

role_routine_grantsOid :: Oid
role_routine_grantsOid = Oid 13431

_routine_routine_usageOid :: Oid
_routine_routine_usageOid = Oid 13434

routine_routine_usageOid :: Oid
routine_routine_usageOid = Oid 13435

_routine_sequence_usageOid :: Oid
_routine_sequence_usageOid = Oid 13439

routine_sequence_usageOid :: Oid
routine_sequence_usageOid = Oid 13440

_routine_table_usageOid :: Oid
_routine_table_usageOid = Oid 13444

routine_table_usageOid :: Oid
routine_table_usageOid = Oid 13445

_routinesOid :: Oid
_routinesOid = Oid 13449

routinesOid :: Oid
routinesOid = Oid 13450

_schemataOid :: Oid
_schemataOid = Oid 13454

schemataOid :: Oid
schemataOid = Oid 13455

_sequencesOid :: Oid
_sequencesOid = Oid 13458

sequencesOid :: Oid
sequencesOid = Oid 13459

_sql_featuresOid :: Oid
_sql_featuresOid = Oid 13463

sql_featuresOid :: Oid
sql_featuresOid = Oid 13464

_sql_implementation_infoOid :: Oid
_sql_implementation_infoOid = Oid 13468

sql_implementation_infoOid :: Oid
sql_implementation_infoOid = Oid 13469

_sql_partsOid :: Oid
_sql_partsOid = Oid 13473

sql_partsOid :: Oid
sql_partsOid = Oid 13474

_sql_sizingOid :: Oid
_sql_sizingOid = Oid 13478

sql_sizingOid :: Oid
sql_sizingOid = Oid 13479

_table_constraintsOid :: Oid
_table_constraintsOid = Oid 13483

table_constraintsOid :: Oid
table_constraintsOid = Oid 13484

_table_privilegesOid :: Oid
_table_privilegesOid = Oid 13488

table_privilegesOid :: Oid
table_privilegesOid = Oid 13489

_role_table_grantsOid :: Oid
_role_table_grantsOid = Oid 13493

role_table_grantsOid :: Oid
role_table_grantsOid = Oid 13494

_tablesOid :: Oid
_tablesOid = Oid 13497

tablesOid :: Oid
tablesOid = Oid 13498

_transformsOid :: Oid
_transformsOid = Oid 13502

transformsOid :: Oid
transformsOid = Oid 13503

_triggered_update_columnsOid :: Oid
_triggered_update_columnsOid = Oid 13507

triggered_update_columnsOid :: Oid
triggered_update_columnsOid = Oid 13508

_triggersOid :: Oid
_triggersOid = Oid 13512

triggersOid :: Oid
triggersOid = Oid 13513

_udt_privilegesOid :: Oid
_udt_privilegesOid = Oid 13517

udt_privilegesOid :: Oid
udt_privilegesOid = Oid 13518

_role_udt_grantsOid :: Oid
_role_udt_grantsOid = Oid 13522

role_udt_grantsOid :: Oid
role_udt_grantsOid = Oid 13523

_usage_privilegesOid :: Oid
_usage_privilegesOid = Oid 13526

usage_privilegesOid :: Oid
usage_privilegesOid = Oid 13527

_role_usage_grantsOid :: Oid
_role_usage_grantsOid = Oid 13531

role_usage_grantsOid :: Oid
role_usage_grantsOid = Oid 13532

_user_defined_typesOid :: Oid
_user_defined_typesOid = Oid 13535

user_defined_typesOid :: Oid
user_defined_typesOid = Oid 13536

_view_column_usageOid :: Oid
_view_column_usageOid = Oid 13540

view_column_usageOid :: Oid
view_column_usageOid = Oid 13541

_view_routine_usageOid :: Oid
_view_routine_usageOid = Oid 13545

view_routine_usageOid :: Oid
view_routine_usageOid = Oid 13546

_view_table_usageOid :: Oid
_view_table_usageOid = Oid 13550

view_table_usageOid :: Oid
view_table_usageOid = Oid 13551

_viewsOid :: Oid
_viewsOid = Oid 13555

viewsOid :: Oid
viewsOid = Oid 13556

_data_type_privilegesOid :: Oid
_data_type_privilegesOid = Oid 13560

data_type_privilegesOid :: Oid
data_type_privilegesOid = Oid 13561

_element_typesOid :: Oid
_element_typesOid = Oid 13565

element_typesOid :: Oid
element_typesOid = Oid 13566

__pg_foreign_table_columnsOid :: Oid
__pg_foreign_table_columnsOid = Oid 13570

_pg_foreign_table_columnsOid :: Oid
_pg_foreign_table_columnsOid = Oid 13571

_column_optionsOid :: Oid
_column_optionsOid = Oid 13575

column_optionsOid :: Oid
column_optionsOid = Oid 13576

__pg_foreign_data_wrappersOid :: Oid
__pg_foreign_data_wrappersOid = Oid 13579

_pg_foreign_data_wrappersOid :: Oid
_pg_foreign_data_wrappersOid = Oid 13580

_foreign_data_wrapper_optionsOid :: Oid
_foreign_data_wrapper_optionsOid = Oid 13583

foreign_data_wrapper_optionsOid :: Oid
foreign_data_wrapper_optionsOid = Oid 13584

_foreign_data_wrappersOid :: Oid
_foreign_data_wrappersOid = Oid 13587

foreign_data_wrappersOid :: Oid
foreign_data_wrappersOid = Oid 13588

__pg_foreign_serversOid :: Oid
__pg_foreign_serversOid = Oid 13591

_pg_foreign_serversOid :: Oid
_pg_foreign_serversOid = Oid 13592

_foreign_server_optionsOid :: Oid
_foreign_server_optionsOid = Oid 13596

foreign_server_optionsOid :: Oid
foreign_server_optionsOid = Oid 13597

_foreign_serversOid :: Oid
_foreign_serversOid = Oid 13600

foreign_serversOid :: Oid
foreign_serversOid = Oid 13601

__pg_foreign_tablesOid :: Oid
__pg_foreign_tablesOid = Oid 13604

_pg_foreign_tablesOid :: Oid
_pg_foreign_tablesOid = Oid 13605

_foreign_table_optionsOid :: Oid
_foreign_table_optionsOid = Oid 13609

foreign_table_optionsOid :: Oid
foreign_table_optionsOid = Oid 13610

_foreign_tablesOid :: Oid
_foreign_tablesOid = Oid 13613

foreign_tablesOid :: Oid
foreign_tablesOid = Oid 13614

__pg_user_mappingsOid :: Oid
__pg_user_mappingsOid = Oid 13617

_pg_user_mappingsOid :: Oid
_pg_user_mappingsOid = Oid 13618

_user_mapping_optionsOid :: Oid
_user_mapping_optionsOid = Oid 13622

user_mapping_optionsOid :: Oid
user_mapping_optionsOid = Oid 13623

_user_mappingsOid :: Oid
_user_mappingsOid = Oid 13627

user_mappingsOid :: Oid
user_mappingsOid = Oid 13628
