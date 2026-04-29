-- |
-- This file has is very similar to a similarly named file from the great postgresql-simple library, more specifically
-- to https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/src/Database.PostgreSQL.Simple.TypeInfo.Static.html
-- We started with that and later came up with our own from scratch, though.
-- We thank them for their great work.
module Hpgsql.TypeInfo
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
    _boolOid,
    _byteaOid,
    _charOid,
    _nameOid,
    _int2Oid,
    _int4Oid,
    _int8Oid,
    _textOid,
    _oidOid,
    _float4Oid,
    _float8Oid,
    _numericOid,
    _dateOid,
    _timestamptzOid,
    _intervalOid,
    _jsonOid,
    _jsonbOid,
    _varcharOid,
    EncodingContext (..),
    Oid (..),
    TransactionStatus (..),
    TypeInfo (..),
    builtinPgTypesMap,
    buildTypeInfoCache,
    lookupTypeByName,
    lookupTypeByOid,
  )
where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

data TransactionStatus
  = -- | A command is in progress
    TransActive
  | -- | Not inside a transaction
    TransIdle
  | -- | Inside a transaction, but no command running
    TransInTrans
  | TransInError
  deriving stock (Eq, Show)

newtype Oid = Oid Int32
  deriving stock (Show)
  deriving newtype (Eq, Real, Num, Ord, Integral, Enum)

data TypeInfo = TypeInfo
  { typeOid :: Oid,
    typeName :: Text,
    -- | Only a Nothing if this 'TypeInfo' already represents an array type, otherwise
    -- the Oid of the Array type with elements of this 'TypeInfo'.
    oidOfArrayType :: Maybe Oid
  }

data TypeInfoCache = TypeInfoCache !(Map Oid TypeInfo) !(Map Text TypeInfo)

instance Semigroup TypeInfoCache where
  TypeInfoCache ma1 ma2 <> TypeInfoCache mb1 mb2 = TypeInfoCache (ma1 <> mb1) (ma2 <> mb2)

lookupTypeByOid :: Oid -> TypeInfoCache -> Maybe TypeInfo
lookupTypeByOid oid (TypeInfoCache m _) = Map.lookup oid m

lookupTypeByName :: Text -> TypeInfoCache -> Maybe TypeInfo
lookupTypeByName n (TypeInfoCache _ m) = Map.lookup n m

buildTypeInfoCache :: [TypeInfo] -> TypeInfoCache
buildTypeInfoCache ts = TypeInfoCache tsByOid tsByName
  where
    tsByOid = Map.fromList $ map (\ti -> (ti.typeOid, ti)) ts
    tsByName = Map.fromList $ map (\ti -> (ti.typeName, ti)) ts

newtype EncodingContext = EncodingContext
  { -- | A map with all builtin PostgreSQL types plus
    -- user-defined types, unless you specify custom connection options.
    typeInfoCache :: TypeInfoCache
  }

-- The query to get all the code you find in this file starting from here is:
-- 1. psql -X -t -d postgres -c "select typname || E'Oid :: Oid\n' || typname || 'Oid = Oid ' || oid from pg_catalog.pg_type order by oid" | sed 's|\+||g'
--    for the many type bindings at the very bottom of the file
-- 2. psql -X -t -d postgres -c "select 'TypeInfo ' || typname || 'Oid ' || '\"' || typname || '\" ' || case when typarray=0 then 'Nothing' else '(Just ' || typarray || ')' end || ',' from pg_catalog.pg_type order by oid"
--    for the Map

-- | This contains every type that is builtin to PostgreSQL
-- alongside some info about them.
-- User defined types are not available here, but are accessible
-- in @EncodingContext@ in many places where it might be useful.
builtinPgTypesMap :: TypeInfoCache
builtinPgTypesMap =
  buildTypeInfoCache
    [ TypeInfo boolOid "bool" (Just 1000),
      TypeInfo byteaOid "bytea" (Just 1001),
      TypeInfo charOid "char" (Just 1002),
      TypeInfo nameOid "name" (Just 1003),
      TypeInfo int8Oid "int8" (Just 1016),
      TypeInfo int2Oid "int2" (Just 1005),
      TypeInfo int2vectorOid "int2vector" (Just 1006),
      TypeInfo int4Oid "int4" (Just 1007),
      TypeInfo regprocOid "regproc" (Just 1008),
      TypeInfo textOid "text" (Just 1009),
      TypeInfo oidOid "oid" (Just 1028),
      TypeInfo tidOid "tid" (Just 1010),
      TypeInfo xidOid "xid" (Just 1011),
      TypeInfo cidOid "cid" (Just 1012),
      TypeInfo oidvectorOid "oidvector" (Just 1013),
      TypeInfo pg_ddl_commandOid "pg_ddl_command" Nothing,
      TypeInfo pg_typeOid "pg_type" (Just 210),
      TypeInfo pg_attributeOid "pg_attribute" (Just 270),
      TypeInfo pg_procOid "pg_proc" (Just 272),
      TypeInfo pg_classOid "pg_class" (Just 273),
      TypeInfo jsonOid "json" (Just 199),
      TypeInfo xmlOid "xml" (Just 143),
      TypeInfo _xmlOid "_xml" Nothing,
      TypeInfo pg_node_treeOid "pg_node_tree" Nothing,
      TypeInfo _jsonOid "_json" Nothing,
      TypeInfo _pg_typeOid "_pg_type" Nothing,
      TypeInfo table_am_handlerOid "table_am_handler" Nothing,
      TypeInfo _pg_attributeOid "_pg_attribute" Nothing,
      TypeInfo _xid8Oid "_xid8" Nothing,
      TypeInfo _pg_procOid "_pg_proc" Nothing,
      TypeInfo _pg_classOid "_pg_class" Nothing,
      TypeInfo index_am_handlerOid "index_am_handler" Nothing,
      TypeInfo pointOid "point" (Just 1017),
      TypeInfo lsegOid "lseg" (Just 1018),
      TypeInfo pathOid "path" (Just 1019),
      TypeInfo boxOid "box" (Just 1020),
      TypeInfo polygonOid "polygon" (Just 1027),
      TypeInfo lineOid "line" (Just 629),
      TypeInfo _lineOid "_line" Nothing,
      TypeInfo cidrOid "cidr" (Just 651),
      TypeInfo _cidrOid "_cidr" Nothing,
      TypeInfo float4Oid "float4" (Just 1021),
      TypeInfo float8Oid "float8" (Just 1022),
      TypeInfo unknownOid "unknown" Nothing,
      TypeInfo circleOid "circle" (Just 719),
      TypeInfo _circleOid "_circle" Nothing,
      TypeInfo macaddr8Oid "macaddr8" (Just 775),
      TypeInfo _macaddr8Oid "_macaddr8" Nothing,
      TypeInfo moneyOid "money" (Just 791),
      TypeInfo _moneyOid "_money" Nothing,
      TypeInfo macaddrOid "macaddr" (Just 1040),
      TypeInfo inetOid "inet" (Just 1041),
      TypeInfo _boolOid "_bool" Nothing,
      TypeInfo _byteaOid "_bytea" Nothing,
      TypeInfo _charOid "_char" Nothing,
      TypeInfo _nameOid "_name" Nothing,
      TypeInfo _int2Oid "_int2" Nothing,
      TypeInfo _int2vectorOid "_int2vector" Nothing,
      TypeInfo _int4Oid "_int4" Nothing,
      TypeInfo _regprocOid "_regproc" Nothing,
      TypeInfo _textOid "_text" Nothing,
      TypeInfo _tidOid "_tid" Nothing,
      TypeInfo _xidOid "_xid" Nothing,
      TypeInfo _cidOid "_cid" Nothing,
      TypeInfo _oidvectorOid "_oidvector" Nothing,
      TypeInfo _bpcharOid "_bpchar" Nothing,
      TypeInfo _varcharOid "_varchar" Nothing,
      TypeInfo _int8Oid "_int8" Nothing,
      TypeInfo _pointOid "_point" Nothing,
      TypeInfo _lsegOid "_lseg" Nothing,
      TypeInfo _pathOid "_path" Nothing,
      TypeInfo _boxOid "_box" Nothing,
      TypeInfo _float4Oid "_float4" Nothing,
      TypeInfo _float8Oid "_float8" Nothing,
      TypeInfo _polygonOid "_polygon" Nothing,
      TypeInfo _oidOid "_oid" Nothing,
      TypeInfo aclitemOid "aclitem" (Just 1034),
      TypeInfo _aclitemOid "_aclitem" Nothing,
      TypeInfo _macaddrOid "_macaddr" Nothing,
      TypeInfo _inetOid "_inet" Nothing,
      TypeInfo bpcharOid "bpchar" (Just 1014),
      TypeInfo varcharOid "varchar" (Just 1015),
      TypeInfo dateOid "date" (Just 1182),
      TypeInfo timeOid "time" (Just 1183),
      TypeInfo timestampOid "timestamp" (Just 1115),
      TypeInfo _timestampOid "_timestamp" Nothing,
      TypeInfo _dateOid "_date" Nothing,
      TypeInfo _timeOid "_time" Nothing,
      TypeInfo timestamptzOid "timestamptz" (Just 1185),
      TypeInfo _timestamptzOid "_timestamptz" Nothing,
      TypeInfo intervalOid "interval" (Just 1187),
      TypeInfo _intervalOid "_interval" Nothing,
      TypeInfo _numericOid "_numeric" Nothing,
      TypeInfo pg_databaseOid "pg_database" (Just 10052),
      TypeInfo _cstringOid "_cstring" Nothing,
      TypeInfo timetzOid "timetz" (Just 1270),
      TypeInfo _timetzOid "_timetz" Nothing,
      TypeInfo bitOid "bit" (Just 1561),
      TypeInfo _bitOid "_bit" Nothing,
      TypeInfo varbitOid "varbit" (Just 1563),
      TypeInfo _varbitOid "_varbit" Nothing,
      TypeInfo numericOid "numeric" (Just 1231),
      TypeInfo refcursorOid "refcursor" (Just 2201),
      TypeInfo _refcursorOid "_refcursor" Nothing,
      TypeInfo regprocedureOid "regprocedure" (Just 2207),
      TypeInfo regoperOid "regoper" (Just 2208),
      TypeInfo regoperatorOid "regoperator" (Just 2209),
      TypeInfo regclassOid "regclass" (Just 2210),
      TypeInfo regtypeOid "regtype" (Just 2211),
      TypeInfo _regprocedureOid "_regprocedure" Nothing,
      TypeInfo _regoperOid "_regoper" Nothing,
      TypeInfo _regoperatorOid "_regoperator" Nothing,
      TypeInfo _regclassOid "_regclass" Nothing,
      TypeInfo _regtypeOid "_regtype" Nothing,
      TypeInfo recordOid "record" (Just 2287),
      TypeInfo cstringOid "cstring" (Just 1263),
      TypeInfo anyOid "any" Nothing,
      TypeInfo anyarrayOid "anyarray" Nothing,
      TypeInfo voidOid "void" Nothing,
      TypeInfo triggerOid "trigger" Nothing,
      TypeInfo language_handlerOid "language_handler" Nothing,
      TypeInfo internalOid "internal" Nothing,
      TypeInfo anyelementOid "anyelement" Nothing,
      TypeInfo _recordOid "_record" Nothing,
      TypeInfo anynonarrayOid "anynonarray" Nothing,
      TypeInfo pg_authidOid "pg_authid" (Just 10057),
      TypeInfo pg_auth_membersOid "pg_auth_members" (Just 10058),
      TypeInfo _txid_snapshotOid "_txid_snapshot" Nothing,
      TypeInfo uuidOid "uuid" (Just 2951),
      TypeInfo _uuidOid "_uuid" Nothing,
      TypeInfo txid_snapshotOid "txid_snapshot" (Just 2949),
      TypeInfo fdw_handlerOid "fdw_handler" Nothing,
      TypeInfo pg_lsnOid "pg_lsn" (Just 3221),
      TypeInfo _pg_lsnOid "_pg_lsn" Nothing,
      TypeInfo tsm_handlerOid "tsm_handler" Nothing,
      TypeInfo pg_ndistinctOid "pg_ndistinct" Nothing,
      TypeInfo pg_dependenciesOid "pg_dependencies" Nothing,
      TypeInfo anyenumOid "anyenum" Nothing,
      TypeInfo tsvectorOid "tsvector" (Just 3643),
      TypeInfo tsqueryOid "tsquery" (Just 3645),
      TypeInfo gtsvectorOid "gtsvector" (Just 3644),
      TypeInfo _tsvectorOid "_tsvector" Nothing,
      TypeInfo _gtsvectorOid "_gtsvector" Nothing,
      TypeInfo _tsqueryOid "_tsquery" Nothing,
      TypeInfo regconfigOid "regconfig" (Just 3735),
      TypeInfo _regconfigOid "_regconfig" Nothing,
      TypeInfo regdictionaryOid "regdictionary" (Just 3770),
      TypeInfo _regdictionaryOid "_regdictionary" Nothing,
      TypeInfo jsonbOid "jsonb" (Just 3807),
      TypeInfo _jsonbOid "_jsonb" Nothing,
      TypeInfo anyrangeOid "anyrange" Nothing,
      TypeInfo event_triggerOid "event_trigger" Nothing,
      TypeInfo int4rangeOid "int4range" (Just 3905),
      TypeInfo _int4rangeOid "_int4range" Nothing,
      TypeInfo numrangeOid "numrange" (Just 3907),
      TypeInfo _numrangeOid "_numrange" Nothing,
      TypeInfo tsrangeOid "tsrange" (Just 3909),
      TypeInfo _tsrangeOid "_tsrange" Nothing,
      TypeInfo tstzrangeOid "tstzrange" (Just 3911),
      TypeInfo _tstzrangeOid "_tstzrange" Nothing,
      TypeInfo daterangeOid "daterange" (Just 3913),
      TypeInfo _daterangeOid "_daterange" Nothing,
      TypeInfo int8rangeOid "int8range" (Just 3927),
      TypeInfo _int8rangeOid "_int8range" Nothing,
      TypeInfo pg_shseclabelOid "pg_shseclabel" (Just 10093),
      TypeInfo jsonpathOid "jsonpath" (Just 4073),
      TypeInfo _jsonpathOid "_jsonpath" Nothing,
      TypeInfo regnamespaceOid "regnamespace" (Just 4090),
      TypeInfo _regnamespaceOid "_regnamespace" Nothing,
      TypeInfo regroleOid "regrole" (Just 4097),
      TypeInfo _regroleOid "_regrole" Nothing,
      TypeInfo regcollationOid "regcollation" (Just 4192),
      TypeInfo _regcollationOid "_regcollation" Nothing,
      TypeInfo int4multirangeOid "int4multirange" (Just 6150),
      TypeInfo nummultirangeOid "nummultirange" (Just 6151),
      TypeInfo tsmultirangeOid "tsmultirange" (Just 6152),
      TypeInfo tstzmultirangeOid "tstzmultirange" (Just 6153),
      TypeInfo datemultirangeOid "datemultirange" (Just 6155),
      TypeInfo int8multirangeOid "int8multirange" (Just 6157),
      TypeInfo anymultirangeOid "anymultirange" Nothing,
      TypeInfo anycompatiblemultirangeOid "anycompatiblemultirange" Nothing,
      TypeInfo pg_brin_bloom_summaryOid "pg_brin_bloom_summary" Nothing,
      TypeInfo pg_brin_minmax_multi_summaryOid "pg_brin_minmax_multi_summary" Nothing,
      TypeInfo pg_mcv_listOid "pg_mcv_list" Nothing,
      TypeInfo pg_snapshotOid "pg_snapshot" (Just 5039),
      TypeInfo _pg_snapshotOid "_pg_snapshot" Nothing,
      TypeInfo xid8Oid "xid8" (Just 271),
      TypeInfo anycompatibleOid "anycompatible" Nothing,
      TypeInfo anycompatiblearrayOid "anycompatiblearray" Nothing,
      TypeInfo anycompatiblenonarrayOid "anycompatiblenonarray" Nothing,
      TypeInfo anycompatiblerangeOid "anycompatiblerange" Nothing,
      TypeInfo pg_subscriptionOid "pg_subscription" (Just 10112),
      TypeInfo _int4multirangeOid "_int4multirange" Nothing,
      TypeInfo _nummultirangeOid "_nummultirange" Nothing,
      TypeInfo _tsmultirangeOid "_tsmultirange" Nothing,
      TypeInfo _tstzmultirangeOid "_tstzmultirange" Nothing,
      TypeInfo _datemultirangeOid "_datemultirange" Nothing,
      TypeInfo _int8multirangeOid "_int8multirange" Nothing,
      TypeInfo _pg_attrdefOid "_pg_attrdef" Nothing,
      TypeInfo pg_attrdefOid "pg_attrdef" (Just 10000),
      TypeInfo _pg_constraintOid "_pg_constraint" Nothing,
      TypeInfo pg_constraintOid "pg_constraint" (Just 10002),
      TypeInfo _pg_inheritsOid "_pg_inherits" Nothing,
      TypeInfo pg_inheritsOid "pg_inherits" (Just 10004),
      TypeInfo _pg_indexOid "_pg_index" Nothing,
      TypeInfo pg_indexOid "pg_index" (Just 10006),
      TypeInfo _pg_operatorOid "_pg_operator" Nothing,
      TypeInfo pg_operatorOid "pg_operator" (Just 10008),
      TypeInfo _pg_opfamilyOid "_pg_opfamily" Nothing,
      TypeInfo pg_opfamilyOid "pg_opfamily" (Just 10010),
      TypeInfo _pg_opclassOid "_pg_opclass" Nothing,
      TypeInfo pg_opclassOid "pg_opclass" (Just 10012),
      TypeInfo _pg_amOid "_pg_am" Nothing,
      TypeInfo pg_amOid "pg_am" (Just 10014),
      TypeInfo _pg_amopOid "_pg_amop" Nothing,
      TypeInfo pg_amopOid "pg_amop" (Just 10016),
      TypeInfo _pg_amprocOid "_pg_amproc" Nothing,
      TypeInfo pg_amprocOid "pg_amproc" (Just 10018),
      TypeInfo _pg_languageOid "_pg_language" Nothing,
      TypeInfo pg_languageOid "pg_language" (Just 10020),
      TypeInfo _pg_largeobject_metadataOid "_pg_largeobject_metadata" Nothing,
      TypeInfo pg_largeobject_metadataOid "pg_largeobject_metadata" (Just 10022),
      TypeInfo _pg_largeobjectOid "_pg_largeobject" Nothing,
      TypeInfo pg_largeobjectOid "pg_largeobject" (Just 10024),
      TypeInfo _pg_aggregateOid "_pg_aggregate" Nothing,
      TypeInfo pg_aggregateOid "pg_aggregate" (Just 10026),
      TypeInfo _pg_statisticOid "_pg_statistic" Nothing,
      TypeInfo pg_statisticOid "pg_statistic" (Just 10028),
      TypeInfo _pg_statistic_extOid "_pg_statistic_ext" Nothing,
      TypeInfo pg_statistic_extOid "pg_statistic_ext" (Just 10030),
      TypeInfo _pg_statistic_ext_dataOid "_pg_statistic_ext_data" Nothing,
      TypeInfo pg_statistic_ext_dataOid "pg_statistic_ext_data" (Just 10032),
      TypeInfo _pg_rewriteOid "_pg_rewrite" Nothing,
      TypeInfo pg_rewriteOid "pg_rewrite" (Just 10034),
      TypeInfo _pg_triggerOid "_pg_trigger" Nothing,
      TypeInfo pg_triggerOid "pg_trigger" (Just 10036),
      TypeInfo _pg_event_triggerOid "_pg_event_trigger" Nothing,
      TypeInfo pg_event_triggerOid "pg_event_trigger" (Just 10038),
      TypeInfo _pg_descriptionOid "_pg_description" Nothing,
      TypeInfo pg_descriptionOid "pg_description" (Just 10040),
      TypeInfo _pg_castOid "_pg_cast" Nothing,
      TypeInfo pg_castOid "pg_cast" (Just 10042),
      TypeInfo _pg_enumOid "_pg_enum" Nothing,
      TypeInfo pg_enumOid "pg_enum" (Just 10044),
      TypeInfo _pg_namespaceOid "_pg_namespace" Nothing,
      TypeInfo pg_namespaceOid "pg_namespace" (Just 10046),
      TypeInfo _pg_conversionOid "_pg_conversion" Nothing,
      TypeInfo pg_conversionOid "pg_conversion" (Just 10048),
      TypeInfo _pg_dependOid "_pg_depend" Nothing,
      TypeInfo pg_dependOid "pg_depend" (Just 10050),
      TypeInfo _pg_databaseOid "_pg_database" Nothing,
      TypeInfo _pg_db_role_settingOid "_pg_db_role_setting" Nothing,
      TypeInfo pg_db_role_settingOid "pg_db_role_setting" (Just 10053),
      TypeInfo _pg_tablespaceOid "_pg_tablespace" Nothing,
      TypeInfo pg_tablespaceOid "pg_tablespace" (Just 10055),
      TypeInfo _pg_authidOid "_pg_authid" Nothing,
      TypeInfo _pg_auth_membersOid "_pg_auth_members" Nothing,
      TypeInfo _pg_shdependOid "_pg_shdepend" Nothing,
      TypeInfo pg_shdependOid "pg_shdepend" (Just 10059),
      TypeInfo _pg_shdescriptionOid "_pg_shdescription" Nothing,
      TypeInfo pg_shdescriptionOid "pg_shdescription" (Just 10061),
      TypeInfo _pg_ts_configOid "_pg_ts_config" Nothing,
      TypeInfo pg_ts_configOid "pg_ts_config" (Just 10063),
      TypeInfo _pg_ts_config_mapOid "_pg_ts_config_map" Nothing,
      TypeInfo pg_ts_config_mapOid "pg_ts_config_map" (Just 10065),
      TypeInfo _pg_ts_dictOid "_pg_ts_dict" Nothing,
      TypeInfo pg_ts_dictOid "pg_ts_dict" (Just 10067),
      TypeInfo _pg_ts_parserOid "_pg_ts_parser" Nothing,
      TypeInfo pg_ts_parserOid "pg_ts_parser" (Just 10069),
      TypeInfo _pg_ts_templateOid "_pg_ts_template" Nothing,
      TypeInfo pg_ts_templateOid "pg_ts_template" (Just 10071),
      TypeInfo _pg_extensionOid "_pg_extension" Nothing,
      TypeInfo pg_extensionOid "pg_extension" (Just 10073),
      TypeInfo _pg_foreign_data_wrapperOid "_pg_foreign_data_wrapper" Nothing,
      TypeInfo pg_foreign_data_wrapperOid "pg_foreign_data_wrapper" (Just 10075),
      TypeInfo _pg_foreign_serverOid "_pg_foreign_server" Nothing,
      TypeInfo pg_foreign_serverOid "pg_foreign_server" (Just 10077),
      TypeInfo _pg_user_mappingOid "_pg_user_mapping" Nothing,
      TypeInfo pg_user_mappingOid "pg_user_mapping" (Just 10079),
      TypeInfo _pg_foreign_tableOid "_pg_foreign_table" Nothing,
      TypeInfo pg_foreign_tableOid "pg_foreign_table" (Just 10081),
      TypeInfo _pg_policyOid "_pg_policy" Nothing,
      TypeInfo pg_policyOid "pg_policy" (Just 10083),
      TypeInfo _pg_replication_originOid "_pg_replication_origin" Nothing,
      TypeInfo pg_replication_originOid "pg_replication_origin" (Just 10085),
      TypeInfo _pg_default_aclOid "_pg_default_acl" Nothing,
      TypeInfo pg_default_aclOid "pg_default_acl" (Just 10087),
      TypeInfo _pg_init_privsOid "_pg_init_privs" Nothing,
      TypeInfo pg_init_privsOid "pg_init_privs" (Just 10089),
      TypeInfo _pg_seclabelOid "_pg_seclabel" Nothing,
      TypeInfo pg_seclabelOid "pg_seclabel" (Just 10091),
      TypeInfo _pg_shseclabelOid "_pg_shseclabel" Nothing,
      TypeInfo _pg_collationOid "_pg_collation" Nothing,
      TypeInfo pg_collationOid "pg_collation" (Just 10094),
      TypeInfo _pg_parameter_aclOid "_pg_parameter_acl" Nothing,
      TypeInfo pg_parameter_aclOid "pg_parameter_acl" (Just 10096),
      TypeInfo _pg_partitioned_tableOid "_pg_partitioned_table" Nothing,
      TypeInfo pg_partitioned_tableOid "pg_partitioned_table" (Just 10098),
      TypeInfo _pg_rangeOid "_pg_range" Nothing,
      TypeInfo pg_rangeOid "pg_range" (Just 10100),
      TypeInfo _pg_transformOid "_pg_transform" Nothing,
      TypeInfo pg_transformOid "pg_transform" (Just 10102),
      TypeInfo _pg_sequenceOid "_pg_sequence" Nothing,
      TypeInfo pg_sequenceOid "pg_sequence" (Just 10104),
      TypeInfo _pg_publicationOid "_pg_publication" Nothing,
      TypeInfo pg_publicationOid "pg_publication" (Just 10106),
      TypeInfo _pg_publication_namespaceOid "_pg_publication_namespace" Nothing,
      TypeInfo pg_publication_namespaceOid "pg_publication_namespace" (Just 10108),
      TypeInfo _pg_publication_relOid "_pg_publication_rel" Nothing,
      TypeInfo pg_publication_relOid "pg_publication_rel" (Just 10110),
      TypeInfo _pg_subscriptionOid "_pg_subscription" Nothing,
      TypeInfo _pg_subscription_relOid "_pg_subscription_rel" Nothing,
      TypeInfo pg_subscription_relOid "pg_subscription_rel" (Just 10113),
      TypeInfo _pg_rolesOid "_pg_roles" Nothing,
      TypeInfo pg_rolesOid "pg_roles" (Just 12001),
      TypeInfo _pg_shadowOid "_pg_shadow" Nothing,
      TypeInfo pg_shadowOid "pg_shadow" (Just 12006),
      TypeInfo _pg_groupOid "_pg_group" Nothing,
      TypeInfo pg_groupOid "pg_group" (Just 12011),
      TypeInfo _pg_userOid "_pg_user" Nothing,
      TypeInfo pg_userOid "pg_user" (Just 12015),
      TypeInfo _pg_policiesOid "_pg_policies" Nothing,
      TypeInfo pg_policiesOid "pg_policies" (Just 12019),
      TypeInfo _pg_rulesOid "_pg_rules" Nothing,
      TypeInfo pg_rulesOid "pg_rules" (Just 12024),
      TypeInfo _pg_viewsOid "_pg_views" Nothing,
      TypeInfo pg_viewsOid "pg_views" (Just 12029),
      TypeInfo _pg_tablesOid "_pg_tables" Nothing,
      TypeInfo pg_tablesOid "pg_tables" (Just 12034),
      TypeInfo _pg_matviewsOid "_pg_matviews" Nothing,
      TypeInfo pg_matviewsOid "pg_matviews" (Just 12039),
      TypeInfo _pg_indexesOid "_pg_indexes" Nothing,
      TypeInfo pg_indexesOid "pg_indexes" (Just 12044),
      TypeInfo _pg_sequencesOid "_pg_sequences" Nothing,
      TypeInfo pg_sequencesOid "pg_sequences" (Just 12049),
      TypeInfo _pg_statsOid "_pg_stats" Nothing,
      TypeInfo pg_statsOid "pg_stats" (Just 12054),
      TypeInfo _pg_stats_extOid "_pg_stats_ext" Nothing,
      TypeInfo pg_stats_extOid "pg_stats_ext" (Just 12059),
      TypeInfo _pg_stats_ext_exprsOid "_pg_stats_ext_exprs" Nothing,
      TypeInfo pg_stats_ext_exprsOid "pg_stats_ext_exprs" (Just 12064),
      TypeInfo _pg_publication_tablesOid "_pg_publication_tables" Nothing,
      TypeInfo pg_publication_tablesOid "pg_publication_tables" (Just 12069),
      TypeInfo _pg_locksOid "_pg_locks" Nothing,
      TypeInfo pg_locksOid "pg_locks" (Just 12074),
      TypeInfo _pg_cursorsOid "_pg_cursors" Nothing,
      TypeInfo pg_cursorsOid "pg_cursors" (Just 12078),
      TypeInfo _pg_available_extensionsOid "_pg_available_extensions" Nothing,
      TypeInfo pg_available_extensionsOid "pg_available_extensions" (Just 12082),
      TypeInfo _pg_available_extension_versionsOid "_pg_available_extension_versions" Nothing,
      TypeInfo pg_available_extension_versionsOid "pg_available_extension_versions" (Just 12086),
      TypeInfo _pg_prepared_xactsOid "_pg_prepared_xacts" Nothing,
      TypeInfo pg_prepared_xactsOid "pg_prepared_xacts" (Just 12091),
      TypeInfo _pg_prepared_statementsOid "_pg_prepared_statements" Nothing,
      TypeInfo pg_prepared_statementsOid "pg_prepared_statements" (Just 12096),
      TypeInfo _pg_seclabelsOid "_pg_seclabels" Nothing,
      TypeInfo pg_seclabelsOid "pg_seclabels" (Just 12100),
      TypeInfo _pg_settingsOid "_pg_settings" Nothing,
      TypeInfo pg_settingsOid "pg_settings" (Just 12105),
      TypeInfo _pg_file_settingsOid "_pg_file_settings" Nothing,
      TypeInfo pg_file_settingsOid "pg_file_settings" (Just 12111),
      TypeInfo _pg_hba_file_rulesOid "_pg_hba_file_rules" Nothing,
      TypeInfo pg_hba_file_rulesOid "pg_hba_file_rules" (Just 12115),
      TypeInfo _pg_ident_file_mappingsOid "_pg_ident_file_mappings" Nothing,
      TypeInfo pg_ident_file_mappingsOid "pg_ident_file_mappings" (Just 12119),
      TypeInfo _pg_timezone_abbrevsOid "_pg_timezone_abbrevs" Nothing,
      TypeInfo pg_timezone_abbrevsOid "pg_timezone_abbrevs" (Just 12123),
      TypeInfo _pg_timezone_namesOid "_pg_timezone_names" Nothing,
      TypeInfo pg_timezone_namesOid "pg_timezone_names" (Just 12127),
      TypeInfo _pg_configOid "_pg_config" Nothing,
      TypeInfo pg_configOid "pg_config" (Just 12131),
      TypeInfo _pg_shmem_allocationsOid "_pg_shmem_allocations" Nothing,
      TypeInfo pg_shmem_allocationsOid "pg_shmem_allocations" (Just 12135),
      TypeInfo _pg_backend_memory_contextsOid "_pg_backend_memory_contexts" Nothing,
      TypeInfo pg_backend_memory_contextsOid "pg_backend_memory_contexts" (Just 12139),
      TypeInfo _pg_stat_all_tablesOid "_pg_stat_all_tables" Nothing,
      TypeInfo pg_stat_all_tablesOid "pg_stat_all_tables" (Just 12143),
      TypeInfo _pg_stat_xact_all_tablesOid "_pg_stat_xact_all_tables" Nothing,
      TypeInfo pg_stat_xact_all_tablesOid "pg_stat_xact_all_tables" (Just 12148),
      TypeInfo _pg_stat_sys_tablesOid "_pg_stat_sys_tables" Nothing,
      TypeInfo pg_stat_sys_tablesOid "pg_stat_sys_tables" (Just 12153),
      TypeInfo _pg_stat_xact_sys_tablesOid "_pg_stat_xact_sys_tables" Nothing,
      TypeInfo pg_stat_xact_sys_tablesOid "pg_stat_xact_sys_tables" (Just 12158),
      TypeInfo _pg_stat_user_tablesOid "_pg_stat_user_tables" Nothing,
      TypeInfo pg_stat_user_tablesOid "pg_stat_user_tables" (Just 12162),
      TypeInfo _pg_stat_xact_user_tablesOid "_pg_stat_xact_user_tables" Nothing,
      TypeInfo pg_stat_xact_user_tablesOid "pg_stat_xact_user_tables" (Just 12167),
      TypeInfo _pg_statio_all_tablesOid "_pg_statio_all_tables" Nothing,
      TypeInfo pg_statio_all_tablesOid "pg_statio_all_tables" (Just 12171),
      TypeInfo _pg_statio_sys_tablesOid "_pg_statio_sys_tables" Nothing,
      TypeInfo pg_statio_sys_tablesOid "pg_statio_sys_tables" (Just 12176),
      TypeInfo _pg_statio_user_tablesOid "_pg_statio_user_tables" Nothing,
      TypeInfo pg_statio_user_tablesOid "pg_statio_user_tables" (Just 12180),
      TypeInfo _pg_stat_all_indexesOid "_pg_stat_all_indexes" Nothing,
      TypeInfo pg_stat_all_indexesOid "pg_stat_all_indexes" (Just 12184),
      TypeInfo _pg_stat_sys_indexesOid "_pg_stat_sys_indexes" Nothing,
      TypeInfo pg_stat_sys_indexesOid "pg_stat_sys_indexes" (Just 12189),
      TypeInfo _pg_stat_user_indexesOid "_pg_stat_user_indexes" Nothing,
      TypeInfo pg_stat_user_indexesOid "pg_stat_user_indexes" (Just 12193),
      TypeInfo _pg_statio_all_indexesOid "_pg_statio_all_indexes" Nothing,
      TypeInfo pg_statio_all_indexesOid "pg_statio_all_indexes" (Just 12197),
      TypeInfo _pg_statio_sys_indexesOid "_pg_statio_sys_indexes" Nothing,
      TypeInfo pg_statio_sys_indexesOid "pg_statio_sys_indexes" (Just 12202),
      TypeInfo _pg_statio_user_indexesOid "_pg_statio_user_indexes" Nothing,
      TypeInfo pg_statio_user_indexesOid "pg_statio_user_indexes" (Just 12206),
      TypeInfo _pg_statio_all_sequencesOid "_pg_statio_all_sequences" Nothing,
      TypeInfo pg_statio_all_sequencesOid "pg_statio_all_sequences" (Just 12210),
      TypeInfo _pg_statio_sys_sequencesOid "_pg_statio_sys_sequences" Nothing,
      TypeInfo pg_statio_sys_sequencesOid "pg_statio_sys_sequences" (Just 12215),
      TypeInfo _pg_statio_user_sequencesOid "_pg_statio_user_sequences" Nothing,
      TypeInfo pg_statio_user_sequencesOid "pg_statio_user_sequences" (Just 12219),
      TypeInfo _pg_stat_activityOid "_pg_stat_activity" Nothing,
      TypeInfo pg_stat_activityOid "pg_stat_activity" (Just 12223),
      TypeInfo _pg_stat_replicationOid "_pg_stat_replication" Nothing,
      TypeInfo pg_stat_replicationOid "pg_stat_replication" (Just 12228),
      TypeInfo _pg_stat_slruOid "_pg_stat_slru" Nothing,
      TypeInfo pg_stat_slruOid "pg_stat_slru" (Just 12233),
      TypeInfo _pg_stat_wal_receiverOid "_pg_stat_wal_receiver" Nothing,
      TypeInfo pg_stat_wal_receiverOid "pg_stat_wal_receiver" (Just 12237),
      TypeInfo _pg_stat_recovery_prefetchOid "_pg_stat_recovery_prefetch" Nothing,
      TypeInfo pg_stat_recovery_prefetchOid "pg_stat_recovery_prefetch" (Just 12241),
      TypeInfo _pg_stat_subscriptionOid "_pg_stat_subscription" Nothing,
      TypeInfo pg_stat_subscriptionOid "pg_stat_subscription" (Just 12245),
      TypeInfo _pg_stat_sslOid "_pg_stat_ssl" Nothing,
      TypeInfo pg_stat_sslOid "pg_stat_ssl" (Just 12250),
      TypeInfo _pg_stat_gssapiOid "_pg_stat_gssapi" Nothing,
      TypeInfo pg_stat_gssapiOid "pg_stat_gssapi" (Just 12254),
      TypeInfo _pg_replication_slotsOid "_pg_replication_slots" Nothing,
      TypeInfo pg_replication_slotsOid "pg_replication_slots" (Just 12258),
      TypeInfo _pg_stat_replication_slotsOid "_pg_stat_replication_slots" Nothing,
      TypeInfo pg_stat_replication_slotsOid "pg_stat_replication_slots" (Just 12263),
      TypeInfo _pg_stat_databaseOid "_pg_stat_database" Nothing,
      TypeInfo pg_stat_databaseOid "pg_stat_database" (Just 12267),
      TypeInfo _pg_stat_database_conflictsOid "_pg_stat_database_conflicts" Nothing,
      TypeInfo pg_stat_database_conflictsOid "pg_stat_database_conflicts" (Just 12272),
      TypeInfo _pg_stat_user_functionsOid "_pg_stat_user_functions" Nothing,
      TypeInfo pg_stat_user_functionsOid "pg_stat_user_functions" (Just 12276),
      TypeInfo _pg_stat_xact_user_functionsOid "_pg_stat_xact_user_functions" Nothing,
      TypeInfo pg_stat_xact_user_functionsOid "pg_stat_xact_user_functions" (Just 12281),
      TypeInfo _pg_stat_archiverOid "_pg_stat_archiver" Nothing,
      TypeInfo pg_stat_archiverOid "pg_stat_archiver" (Just 12286),
      TypeInfo _pg_stat_bgwriterOid "_pg_stat_bgwriter" Nothing,
      TypeInfo pg_stat_bgwriterOid "pg_stat_bgwriter" (Just 12290),
      TypeInfo _pg_stat_ioOid "_pg_stat_io" Nothing,
      TypeInfo pg_stat_ioOid "pg_stat_io" (Just 12294),
      TypeInfo _pg_stat_walOid "_pg_stat_wal" Nothing,
      TypeInfo pg_stat_walOid "pg_stat_wal" (Just 12298),
      TypeInfo _pg_stat_progress_analyzeOid "_pg_stat_progress_analyze" Nothing,
      TypeInfo pg_stat_progress_analyzeOid "pg_stat_progress_analyze" (Just 12302),
      TypeInfo _pg_stat_progress_vacuumOid "_pg_stat_progress_vacuum" Nothing,
      TypeInfo pg_stat_progress_vacuumOid "pg_stat_progress_vacuum" (Just 12307),
      TypeInfo _pg_stat_progress_clusterOid "_pg_stat_progress_cluster" Nothing,
      TypeInfo pg_stat_progress_clusterOid "pg_stat_progress_cluster" (Just 12312),
      TypeInfo _pg_stat_progress_create_indexOid "_pg_stat_progress_create_index" Nothing,
      TypeInfo pg_stat_progress_create_indexOid "pg_stat_progress_create_index" (Just 12317),
      TypeInfo _pg_stat_progress_basebackupOid "_pg_stat_progress_basebackup" Nothing,
      TypeInfo pg_stat_progress_basebackupOid "pg_stat_progress_basebackup" (Just 12322),
      TypeInfo _pg_stat_progress_copyOid "_pg_stat_progress_copy" Nothing,
      TypeInfo pg_stat_progress_copyOid "pg_stat_progress_copy" (Just 12327),
      TypeInfo _pg_user_mappingsOid "_pg_user_mappings" Nothing,
      TypeInfo pg_user_mappingsOid "pg_user_mappings" (Just 12332),
      TypeInfo _pg_replication_origin_statusOid "_pg_replication_origin_status" Nothing,
      TypeInfo pg_replication_origin_statusOid "pg_replication_origin_status" (Just 12337),
      TypeInfo _pg_stat_subscription_statsOid "_pg_stat_subscription_stats" Nothing,
      TypeInfo pg_stat_subscription_statsOid "pg_stat_subscription_stats" (Just 12341),
      TypeInfo _cardinal_numberOid "_cardinal_number" Nothing,
      TypeInfo cardinal_numberOid "cardinal_number" (Just 13291),
      TypeInfo _character_dataOid "_character_data" Nothing,
      TypeInfo character_dataOid "character_data" (Just 13294),
      TypeInfo _sql_identifierOid "_sql_identifier" Nothing,
      TypeInfo sql_identifierOid "sql_identifier" (Just 13296),
      TypeInfo _information_schema_catalog_nameOid "_information_schema_catalog_name" Nothing,
      TypeInfo information_schema_catalog_nameOid "information_schema_catalog_name" (Just 13299),
      TypeInfo _time_stampOid "_time_stamp" Nothing,
      TypeInfo time_stampOid "time_stamp" (Just 13302),
      TypeInfo _yes_or_noOid "_yes_or_no" Nothing,
      TypeInfo yes_or_noOid "yes_or_no" (Just 13304),
      TypeInfo _applicable_rolesOid "_applicable_roles" Nothing,
      TypeInfo applicable_rolesOid "applicable_roles" (Just 13308),
      TypeInfo _administrable_role_authorizationsOid "_administrable_role_authorizations" Nothing,
      TypeInfo administrable_role_authorizationsOid "administrable_role_authorizations" (Just 13313),
      TypeInfo _attributesOid "_attributes" Nothing,
      TypeInfo attributesOid "attributes" (Just 13317),
      TypeInfo _character_setsOid "_character_sets" Nothing,
      TypeInfo character_setsOid "character_sets" (Just 13322),
      TypeInfo _check_constraint_routine_usageOid "_check_constraint_routine_usage" Nothing,
      TypeInfo check_constraint_routine_usageOid "check_constraint_routine_usage" (Just 13327),
      TypeInfo _check_constraintsOid "_check_constraints" Nothing,
      TypeInfo check_constraintsOid "check_constraints" (Just 13332),
      TypeInfo _collationsOid "_collations" Nothing,
      TypeInfo collationsOid "collations" (Just 13337),
      TypeInfo _collation_character_set_applicabilityOid "_collation_character_set_applicability" Nothing,
      TypeInfo collation_character_set_applicabilityOid "collation_character_set_applicability" (Just 13342),
      TypeInfo _column_column_usageOid "_column_column_usage" Nothing,
      TypeInfo column_column_usageOid "column_column_usage" (Just 13347),
      TypeInfo _column_domain_usageOid "_column_domain_usage" Nothing,
      TypeInfo column_domain_usageOid "column_domain_usage" (Just 13352),
      TypeInfo _column_privilegesOid "_column_privileges" Nothing,
      TypeInfo column_privilegesOid "column_privileges" (Just 13357),
      TypeInfo _column_udt_usageOid "_column_udt_usage" Nothing,
      TypeInfo column_udt_usageOid "column_udt_usage" (Just 13362),
      TypeInfo _columnsOid "_columns" Nothing,
      TypeInfo columnsOid "columns" (Just 13367),
      TypeInfo _constraint_column_usageOid "_constraint_column_usage" Nothing,
      TypeInfo constraint_column_usageOid "constraint_column_usage" (Just 13372),
      TypeInfo _constraint_table_usageOid "_constraint_table_usage" Nothing,
      TypeInfo constraint_table_usageOid "constraint_table_usage" (Just 13377),
      TypeInfo _domain_constraintsOid "_domain_constraints" Nothing,
      TypeInfo domain_constraintsOid "domain_constraints" (Just 13382),
      TypeInfo _domain_udt_usageOid "_domain_udt_usage" Nothing,
      TypeInfo domain_udt_usageOid "domain_udt_usage" (Just 13387),
      TypeInfo _domainsOid "_domains" Nothing,
      TypeInfo domainsOid "domains" (Just 13392),
      TypeInfo _enabled_rolesOid "_enabled_roles" Nothing,
      TypeInfo enabled_rolesOid "enabled_roles" (Just 13397),
      TypeInfo _key_column_usageOid "_key_column_usage" Nothing,
      TypeInfo key_column_usageOid "key_column_usage" (Just 13401),
      TypeInfo _parametersOid "_parameters" Nothing,
      TypeInfo parametersOid "parameters" (Just 13406),
      TypeInfo _referential_constraintsOid "_referential_constraints" Nothing,
      TypeInfo referential_constraintsOid "referential_constraints" (Just 13411),
      TypeInfo _role_column_grantsOid "_role_column_grants" Nothing,
      TypeInfo role_column_grantsOid "role_column_grants" (Just 13416),
      TypeInfo _routine_column_usageOid "_routine_column_usage" Nothing,
      TypeInfo routine_column_usageOid "routine_column_usage" (Just 13420),
      TypeInfo _routine_privilegesOid "_routine_privileges" Nothing,
      TypeInfo routine_privilegesOid "routine_privileges" (Just 13425),
      TypeInfo _role_routine_grantsOid "_role_routine_grants" Nothing,
      TypeInfo role_routine_grantsOid "role_routine_grants" (Just 13430),
      TypeInfo _routine_routine_usageOid "_routine_routine_usage" Nothing,
      TypeInfo routine_routine_usageOid "routine_routine_usage" (Just 13434),
      TypeInfo _routine_sequence_usageOid "_routine_sequence_usage" Nothing,
      TypeInfo routine_sequence_usageOid "routine_sequence_usage" (Just 13439),
      TypeInfo _routine_table_usageOid "_routine_table_usage" Nothing,
      TypeInfo routine_table_usageOid "routine_table_usage" (Just 13444),
      TypeInfo _routinesOid "_routines" Nothing,
      TypeInfo routinesOid "routines" (Just 13449),
      TypeInfo _schemataOid "_schemata" Nothing,
      TypeInfo schemataOid "schemata" (Just 13454),
      TypeInfo _sequencesOid "_sequences" Nothing,
      TypeInfo sequencesOid "sequences" (Just 13458),
      TypeInfo _sql_featuresOid "_sql_features" Nothing,
      TypeInfo sql_featuresOid "sql_features" (Just 13463),
      TypeInfo _sql_implementation_infoOid "_sql_implementation_info" Nothing,
      TypeInfo sql_implementation_infoOid "sql_implementation_info" (Just 13468),
      TypeInfo _sql_partsOid "_sql_parts" Nothing,
      TypeInfo sql_partsOid "sql_parts" (Just 13473),
      TypeInfo _sql_sizingOid "_sql_sizing" Nothing,
      TypeInfo sql_sizingOid "sql_sizing" (Just 13478),
      TypeInfo _table_constraintsOid "_table_constraints" Nothing,
      TypeInfo table_constraintsOid "table_constraints" (Just 13483),
      TypeInfo _table_privilegesOid "_table_privileges" Nothing,
      TypeInfo table_privilegesOid "table_privileges" (Just 13488),
      TypeInfo _role_table_grantsOid "_role_table_grants" Nothing,
      TypeInfo role_table_grantsOid "role_table_grants" (Just 13493),
      TypeInfo _tablesOid "_tables" Nothing,
      TypeInfo tablesOid "tables" (Just 13497),
      TypeInfo _transformsOid "_transforms" Nothing,
      TypeInfo transformsOid "transforms" (Just 13502),
      TypeInfo _triggered_update_columnsOid "_triggered_update_columns" Nothing,
      TypeInfo triggered_update_columnsOid "triggered_update_columns" (Just 13507),
      TypeInfo _triggersOid "_triggers" Nothing,
      TypeInfo triggersOid "triggers" (Just 13512),
      TypeInfo _udt_privilegesOid "_udt_privileges" Nothing,
      TypeInfo udt_privilegesOid "udt_privileges" (Just 13517),
      TypeInfo _role_udt_grantsOid "_role_udt_grants" Nothing,
      TypeInfo role_udt_grantsOid "role_udt_grants" (Just 13522),
      TypeInfo _usage_privilegesOid "_usage_privileges" Nothing,
      TypeInfo usage_privilegesOid "usage_privileges" (Just 13526),
      TypeInfo _role_usage_grantsOid "_role_usage_grants" Nothing,
      TypeInfo role_usage_grantsOid "role_usage_grants" (Just 13531),
      TypeInfo _user_defined_typesOid "_user_defined_types" Nothing,
      TypeInfo user_defined_typesOid "user_defined_types" (Just 13535),
      TypeInfo _view_column_usageOid "_view_column_usage" Nothing,
      TypeInfo view_column_usageOid "view_column_usage" (Just 13540),
      TypeInfo _view_routine_usageOid "_view_routine_usage" Nothing,
      TypeInfo view_routine_usageOid "view_routine_usage" (Just 13545),
      TypeInfo _view_table_usageOid "_view_table_usage" Nothing,
      TypeInfo view_table_usageOid "view_table_usage" (Just 13550),
      TypeInfo _viewsOid "_views" Nothing,
      TypeInfo viewsOid "views" (Just 13555),
      TypeInfo _data_type_privilegesOid "_data_type_privileges" Nothing,
      TypeInfo data_type_privilegesOid "data_type_privileges" (Just 13560),
      TypeInfo _element_typesOid "_element_types" Nothing,
      TypeInfo element_typesOid "element_types" (Just 13565),
      TypeInfo __pg_foreign_table_columnsOid "__pg_foreign_table_columns" Nothing,
      TypeInfo _pg_foreign_table_columnsOid "_pg_foreign_table_columns" (Just 13570),
      TypeInfo _column_optionsOid "_column_options" Nothing,
      TypeInfo column_optionsOid "column_options" (Just 13575),
      TypeInfo __pg_foreign_data_wrappersOid "__pg_foreign_data_wrappers" Nothing,
      TypeInfo _pg_foreign_data_wrappersOid "_pg_foreign_data_wrappers" (Just 13579),
      TypeInfo _foreign_data_wrapper_optionsOid "_foreign_data_wrapper_options" Nothing,
      TypeInfo foreign_data_wrapper_optionsOid "foreign_data_wrapper_options" (Just 13583),
      TypeInfo _foreign_data_wrappersOid "_foreign_data_wrappers" Nothing,
      TypeInfo foreign_data_wrappersOid "foreign_data_wrappers" (Just 13587),
      TypeInfo __pg_foreign_serversOid "__pg_foreign_servers" Nothing,
      TypeInfo _pg_foreign_serversOid "_pg_foreign_servers" (Just 13591),
      TypeInfo _foreign_server_optionsOid "_foreign_server_options" Nothing,
      TypeInfo foreign_server_optionsOid "foreign_server_options" (Just 13596),
      TypeInfo _foreign_serversOid "_foreign_servers" Nothing,
      TypeInfo foreign_serversOid "foreign_servers" (Just 13600),
      TypeInfo __pg_foreign_tablesOid "__pg_foreign_tables" Nothing,
      TypeInfo _pg_foreign_tablesOid "_pg_foreign_tables" (Just 13604),
      TypeInfo _foreign_table_optionsOid "_foreign_table_options" Nothing,
      TypeInfo foreign_table_optionsOid "foreign_table_options" (Just 13609),
      TypeInfo _foreign_tablesOid "_foreign_tables" Nothing,
      TypeInfo foreign_tablesOid "foreign_tables" (Just 13613),
      TypeInfo __pg_user_mappingsOid "__pg_user_mappings" Nothing,
      TypeInfo _pg_user_mappingsOid "_pg_user_mappings" (Just 13617),
      TypeInfo _user_mapping_optionsOid "_user_mapping_options" Nothing,
      TypeInfo user_mapping_optionsOid "user_mapping_options" (Just 13622),
      TypeInfo _user_mappingsOid "_user_mappings" Nothing,
      TypeInfo user_mappingsOid "user_mappings" (Just 13627)
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
