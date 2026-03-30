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
    Format (..),
    Oid (..),
    TransactionStatus (..),
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

-- | This replicates the postgresql-libpq constructor, because why not?
data TransactionStatus = TransActive | TransIdle | TransInTrans | TransInError
  deriving stock (Eq, Show)

newtype Oid = Oid Int32
  deriving stock (Show)
  deriving newtype (Eq, Real, Num, Ord, Integral, Enum)

data TypeInfo = TypeInfo
  { typeName :: Text,
    -- | Only a Nothing if this is already an array type
    oidOfArrayType :: Maybe Oid
  }

newtype EncodingContext = EncodingContext
  { typeInfoCache :: Map Oid TypeInfo
  }

-- The query to get all the code you find in this file is:
-- 1. psql -X -t -d postgres -c "select typname || E'Oid :: Oid\n' || typname || 'Oid = Oid ' || oid from pg_catalog.pg_type order by oid" | sed 's|\+||g'
-- 2. psql -X -t -d postgres -c "select '(' || typname || 'Oid, TypeInfo \"' || typname || '\" ' || case when typarray=0 then 'Nothing' else '(Just ' || typarray || ')' end || '),' from pg_catalog.pg_type order by oid" for the Map

builtinPgTypesMap :: Map Oid TypeInfo
builtinPgTypesMap =
  Map.fromList
    [ (boolOid, TypeInfo "bool" (Just 1000)),
      (byteaOid, TypeInfo "bytea" (Just 1001)),
      (charOid, TypeInfo "char" (Just 1002)),
      (nameOid, TypeInfo "name" (Just 1003)),
      (int8Oid, TypeInfo "int8" (Just 1016)),
      (int2Oid, TypeInfo "int2" (Just 1005)),
      (int2vectorOid, TypeInfo "int2vector" (Just 1006)),
      (int4Oid, TypeInfo "int4" (Just 1007)),
      (regprocOid, TypeInfo "regproc" (Just 1008)),
      (textOid, TypeInfo "text" (Just 1009)),
      (oidOid, TypeInfo "oid" (Just 1028)),
      (tidOid, TypeInfo "tid" (Just 1010)),
      (xidOid, TypeInfo "xid" (Just 1011)),
      (cidOid, TypeInfo "cid" (Just 1012)),
      (oidvectorOid, TypeInfo "oidvector" (Just 1013)),
      (pg_ddl_commandOid, TypeInfo "pg_ddl_command" Nothing),
      (pg_typeOid, TypeInfo "pg_type" (Just 210)),
      (pg_attributeOid, TypeInfo "pg_attribute" (Just 270)),
      (pg_procOid, TypeInfo "pg_proc" (Just 272)),
      (pg_classOid, TypeInfo "pg_class" (Just 273)),
      (jsonOid, TypeInfo "json" (Just 199)),
      (xmlOid, TypeInfo "xml" (Just 143)),
      (_xmlOid, TypeInfo "_xml" Nothing),
      (pg_node_treeOid, TypeInfo "pg_node_tree" Nothing),
      (_jsonOid, TypeInfo "_json" Nothing),
      (_pg_typeOid, TypeInfo "_pg_type" Nothing),
      (table_am_handlerOid, TypeInfo "table_am_handler" Nothing),
      (_pg_attributeOid, TypeInfo "_pg_attribute" Nothing),
      (_xid8Oid, TypeInfo "_xid8" Nothing),
      (_pg_procOid, TypeInfo "_pg_proc" Nothing),
      (_pg_classOid, TypeInfo "_pg_class" Nothing),
      (index_am_handlerOid, TypeInfo "index_am_handler" Nothing),
      (pointOid, TypeInfo "point" (Just 1017)),
      (lsegOid, TypeInfo "lseg" (Just 1018)),
      (pathOid, TypeInfo "path" (Just 1019)),
      (boxOid, TypeInfo "box" (Just 1020)),
      (polygonOid, TypeInfo "polygon" (Just 1027)),
      (lineOid, TypeInfo "line" (Just 629)),
      (_lineOid, TypeInfo "_line" Nothing),
      (cidrOid, TypeInfo "cidr" (Just 651)),
      (_cidrOid, TypeInfo "_cidr" Nothing),
      (float4Oid, TypeInfo "float4" (Just 1021)),
      (float8Oid, TypeInfo "float8" (Just 1022)),
      (unknownOid, TypeInfo "unknown" Nothing),
      (circleOid, TypeInfo "circle" (Just 719)),
      (_circleOid, TypeInfo "_circle" Nothing),
      (macaddr8Oid, TypeInfo "macaddr8" (Just 775)),
      (_macaddr8Oid, TypeInfo "_macaddr8" Nothing),
      (moneyOid, TypeInfo "money" (Just 791)),
      (_moneyOid, TypeInfo "_money" Nothing),
      (macaddrOid, TypeInfo "macaddr" (Just 1040)),
      (inetOid, TypeInfo "inet" (Just 1041)),
      (_boolOid, TypeInfo "_bool" Nothing),
      (_byteaOid, TypeInfo "_bytea" Nothing),
      (_charOid, TypeInfo "_char" Nothing),
      (_nameOid, TypeInfo "_name" Nothing),
      (_int2Oid, TypeInfo "_int2" Nothing),
      (_int2vectorOid, TypeInfo "_int2vector" Nothing),
      (_int4Oid, TypeInfo "_int4" Nothing),
      (_regprocOid, TypeInfo "_regproc" Nothing),
      (_textOid, TypeInfo "_text" Nothing),
      (_tidOid, TypeInfo "_tid" Nothing),
      (_xidOid, TypeInfo "_xid" Nothing),
      (_cidOid, TypeInfo "_cid" Nothing),
      (_oidvectorOid, TypeInfo "_oidvector" Nothing),
      (_bpcharOid, TypeInfo "_bpchar" Nothing),
      (_varcharOid, TypeInfo "_varchar" Nothing),
      (_int8Oid, TypeInfo "_int8" Nothing),
      (_pointOid, TypeInfo "_point" Nothing),
      (_lsegOid, TypeInfo "_lseg" Nothing),
      (_pathOid, TypeInfo "_path" Nothing),
      (_boxOid, TypeInfo "_box" Nothing),
      (_float4Oid, TypeInfo "_float4" Nothing),
      (_float8Oid, TypeInfo "_float8" Nothing),
      (_polygonOid, TypeInfo "_polygon" Nothing),
      (_oidOid, TypeInfo "_oid" Nothing),
      (aclitemOid, TypeInfo "aclitem" (Just 1034)),
      (_aclitemOid, TypeInfo "_aclitem" Nothing),
      (_macaddrOid, TypeInfo "_macaddr" Nothing),
      (_inetOid, TypeInfo "_inet" Nothing),
      (bpcharOid, TypeInfo "bpchar" (Just 1014)),
      (varcharOid, TypeInfo "varchar" (Just 1015)),
      (dateOid, TypeInfo "date" (Just 1182)),
      (timeOid, TypeInfo "time" (Just 1183)),
      (timestampOid, TypeInfo "timestamp" (Just 1115)),
      (_timestampOid, TypeInfo "_timestamp" Nothing),
      (_dateOid, TypeInfo "_date" Nothing),
      (_timeOid, TypeInfo "_time" Nothing),
      (timestamptzOid, TypeInfo "timestamptz" (Just 1185)),
      (_timestamptzOid, TypeInfo "_timestamptz" Nothing),
      (intervalOid, TypeInfo "interval" (Just 1187)),
      (_intervalOid, TypeInfo "_interval" Nothing),
      (_numericOid, TypeInfo "_numeric" Nothing),
      (pg_databaseOid, TypeInfo "pg_database" (Just 10052)),
      (_cstringOid, TypeInfo "_cstring" Nothing),
      (timetzOid, TypeInfo "timetz" (Just 1270)),
      (_timetzOid, TypeInfo "_timetz" Nothing),
      (bitOid, TypeInfo "bit" (Just 1561)),
      (_bitOid, TypeInfo "_bit" Nothing),
      (varbitOid, TypeInfo "varbit" (Just 1563)),
      (_varbitOid, TypeInfo "_varbit" Nothing),
      (numericOid, TypeInfo "numeric" (Just 1231)),
      (refcursorOid, TypeInfo "refcursor" (Just 2201)),
      (_refcursorOid, TypeInfo "_refcursor" Nothing),
      (regprocedureOid, TypeInfo "regprocedure" (Just 2207)),
      (regoperOid, TypeInfo "regoper" (Just 2208)),
      (regoperatorOid, TypeInfo "regoperator" (Just 2209)),
      (regclassOid, TypeInfo "regclass" (Just 2210)),
      (regtypeOid, TypeInfo "regtype" (Just 2211)),
      (_regprocedureOid, TypeInfo "_regprocedure" Nothing),
      (_regoperOid, TypeInfo "_regoper" Nothing),
      (_regoperatorOid, TypeInfo "_regoperator" Nothing),
      (_regclassOid, TypeInfo "_regclass" Nothing),
      (_regtypeOid, TypeInfo "_regtype" Nothing),
      (recordOid, TypeInfo "record" (Just 2287)),
      (cstringOid, TypeInfo "cstring" (Just 1263)),
      (anyOid, TypeInfo "any" Nothing),
      (anyarrayOid, TypeInfo "anyarray" Nothing),
      (voidOid, TypeInfo "void" Nothing),
      (triggerOid, TypeInfo "trigger" Nothing),
      (language_handlerOid, TypeInfo "language_handler" Nothing),
      (internalOid, TypeInfo "internal" Nothing),
      (anyelementOid, TypeInfo "anyelement" Nothing),
      (_recordOid, TypeInfo "_record" Nothing),
      (anynonarrayOid, TypeInfo "anynonarray" Nothing),
      (pg_authidOid, TypeInfo "pg_authid" (Just 10057)),
      (pg_auth_membersOid, TypeInfo "pg_auth_members" (Just 10058)),
      (_txid_snapshotOid, TypeInfo "_txid_snapshot" Nothing),
      (uuidOid, TypeInfo "uuid" (Just 2951)),
      (_uuidOid, TypeInfo "_uuid" Nothing),
      (txid_snapshotOid, TypeInfo "txid_snapshot" (Just 2949)),
      (fdw_handlerOid, TypeInfo "fdw_handler" Nothing),
      (pg_lsnOid, TypeInfo "pg_lsn" (Just 3221)),
      (_pg_lsnOid, TypeInfo "_pg_lsn" Nothing),
      (tsm_handlerOid, TypeInfo "tsm_handler" Nothing),
      (pg_ndistinctOid, TypeInfo "pg_ndistinct" Nothing),
      (pg_dependenciesOid, TypeInfo "pg_dependencies" Nothing),
      (anyenumOid, TypeInfo "anyenum" Nothing),
      (tsvectorOid, TypeInfo "tsvector" (Just 3643)),
      (tsqueryOid, TypeInfo "tsquery" (Just 3645)),
      (gtsvectorOid, TypeInfo "gtsvector" (Just 3644)),
      (_tsvectorOid, TypeInfo "_tsvector" Nothing),
      (_gtsvectorOid, TypeInfo "_gtsvector" Nothing),
      (_tsqueryOid, TypeInfo "_tsquery" Nothing),
      (regconfigOid, TypeInfo "regconfig" (Just 3735)),
      (_regconfigOid, TypeInfo "_regconfig" Nothing),
      (regdictionaryOid, TypeInfo "regdictionary" (Just 3770)),
      (_regdictionaryOid, TypeInfo "_regdictionary" Nothing),
      (jsonbOid, TypeInfo "jsonb" (Just 3807)),
      (_jsonbOid, TypeInfo "_jsonb" Nothing),
      (anyrangeOid, TypeInfo "anyrange" Nothing),
      (event_triggerOid, TypeInfo "event_trigger" Nothing),
      (int4rangeOid, TypeInfo "int4range" (Just 3905)),
      (_int4rangeOid, TypeInfo "_int4range" Nothing),
      (numrangeOid, TypeInfo "numrange" (Just 3907)),
      (_numrangeOid, TypeInfo "_numrange" Nothing),
      (tsrangeOid, TypeInfo "tsrange" (Just 3909)),
      (_tsrangeOid, TypeInfo "_tsrange" Nothing),
      (tstzrangeOid, TypeInfo "tstzrange" (Just 3911)),
      (_tstzrangeOid, TypeInfo "_tstzrange" Nothing),
      (daterangeOid, TypeInfo "daterange" (Just 3913)),
      (_daterangeOid, TypeInfo "_daterange" Nothing),
      (int8rangeOid, TypeInfo "int8range" (Just 3927)),
      (_int8rangeOid, TypeInfo "_int8range" Nothing),
      (pg_shseclabelOid, TypeInfo "pg_shseclabel" (Just 10093)),
      (jsonpathOid, TypeInfo "jsonpath" (Just 4073)),
      (_jsonpathOid, TypeInfo "_jsonpath" Nothing),
      (regnamespaceOid, TypeInfo "regnamespace" (Just 4090)),
      (_regnamespaceOid, TypeInfo "_regnamespace" Nothing),
      (regroleOid, TypeInfo "regrole" (Just 4097)),
      (_regroleOid, TypeInfo "_regrole" Nothing),
      (regcollationOid, TypeInfo "regcollation" (Just 4192)),
      (_regcollationOid, TypeInfo "_regcollation" Nothing),
      (int4multirangeOid, TypeInfo "int4multirange" (Just 6150)),
      (nummultirangeOid, TypeInfo "nummultirange" (Just 6151)),
      (tsmultirangeOid, TypeInfo "tsmultirange" (Just 6152)),
      (tstzmultirangeOid, TypeInfo "tstzmultirange" (Just 6153)),
      (datemultirangeOid, TypeInfo "datemultirange" (Just 6155)),
      (int8multirangeOid, TypeInfo "int8multirange" (Just 6157)),
      (anymultirangeOid, TypeInfo "anymultirange" Nothing),
      (anycompatiblemultirangeOid, TypeInfo "anycompatiblemultirange" Nothing),
      (pg_brin_bloom_summaryOid, TypeInfo "pg_brin_bloom_summary" Nothing),
      (pg_brin_minmax_multi_summaryOid, TypeInfo "pg_brin_minmax_multi_summary" Nothing),
      (pg_mcv_listOid, TypeInfo "pg_mcv_list" Nothing),
      (pg_snapshotOid, TypeInfo "pg_snapshot" (Just 5039)),
      (_pg_snapshotOid, TypeInfo "_pg_snapshot" Nothing),
      (xid8Oid, TypeInfo "xid8" (Just 271)),
      (anycompatibleOid, TypeInfo "anycompatible" Nothing),
      (anycompatiblearrayOid, TypeInfo "anycompatiblearray" Nothing),
      (anycompatiblenonarrayOid, TypeInfo "anycompatiblenonarray" Nothing),
      (anycompatiblerangeOid, TypeInfo "anycompatiblerange" Nothing),
      (pg_subscriptionOid, TypeInfo "pg_subscription" (Just 10112)),
      (_int4multirangeOid, TypeInfo "_int4multirange" Nothing),
      (_nummultirangeOid, TypeInfo "_nummultirange" Nothing),
      (_tsmultirangeOid, TypeInfo "_tsmultirange" Nothing),
      (_tstzmultirangeOid, TypeInfo "_tstzmultirange" Nothing),
      (_datemultirangeOid, TypeInfo "_datemultirange" Nothing),
      (_int8multirangeOid, TypeInfo "_int8multirange" Nothing),
      (_pg_attrdefOid, TypeInfo "_pg_attrdef" Nothing),
      (pg_attrdefOid, TypeInfo "pg_attrdef" (Just 10000)),
      (_pg_constraintOid, TypeInfo "_pg_constraint" Nothing),
      (pg_constraintOid, TypeInfo "pg_constraint" (Just 10002)),
      (_pg_inheritsOid, TypeInfo "_pg_inherits" Nothing),
      (pg_inheritsOid, TypeInfo "pg_inherits" (Just 10004)),
      (_pg_indexOid, TypeInfo "_pg_index" Nothing),
      (pg_indexOid, TypeInfo "pg_index" (Just 10006)),
      (_pg_operatorOid, TypeInfo "_pg_operator" Nothing),
      (pg_operatorOid, TypeInfo "pg_operator" (Just 10008)),
      (_pg_opfamilyOid, TypeInfo "_pg_opfamily" Nothing),
      (pg_opfamilyOid, TypeInfo "pg_opfamily" (Just 10010)),
      (_pg_opclassOid, TypeInfo "_pg_opclass" Nothing),
      (pg_opclassOid, TypeInfo "pg_opclass" (Just 10012)),
      (_pg_amOid, TypeInfo "_pg_am" Nothing),
      (pg_amOid, TypeInfo "pg_am" (Just 10014)),
      (_pg_amopOid, TypeInfo "_pg_amop" Nothing),
      (pg_amopOid, TypeInfo "pg_amop" (Just 10016)),
      (_pg_amprocOid, TypeInfo "_pg_amproc" Nothing),
      (pg_amprocOid, TypeInfo "pg_amproc" (Just 10018)),
      (_pg_languageOid, TypeInfo "_pg_language" Nothing),
      (pg_languageOid, TypeInfo "pg_language" (Just 10020)),
      (_pg_largeobject_metadataOid, TypeInfo "_pg_largeobject_metadata" Nothing),
      (pg_largeobject_metadataOid, TypeInfo "pg_largeobject_metadata" (Just 10022)),
      (_pg_largeobjectOid, TypeInfo "_pg_largeobject" Nothing),
      (pg_largeobjectOid, TypeInfo "pg_largeobject" (Just 10024)),
      (_pg_aggregateOid, TypeInfo "_pg_aggregate" Nothing),
      (pg_aggregateOid, TypeInfo "pg_aggregate" (Just 10026)),
      (_pg_statisticOid, TypeInfo "_pg_statistic" Nothing),
      (pg_statisticOid, TypeInfo "pg_statistic" (Just 10028)),
      (_pg_statistic_extOid, TypeInfo "_pg_statistic_ext" Nothing),
      (pg_statistic_extOid, TypeInfo "pg_statistic_ext" (Just 10030)),
      (_pg_statistic_ext_dataOid, TypeInfo "_pg_statistic_ext_data" Nothing),
      (pg_statistic_ext_dataOid, TypeInfo "pg_statistic_ext_data" (Just 10032)),
      (_pg_rewriteOid, TypeInfo "_pg_rewrite" Nothing),
      (pg_rewriteOid, TypeInfo "pg_rewrite" (Just 10034)),
      (_pg_triggerOid, TypeInfo "_pg_trigger" Nothing),
      (pg_triggerOid, TypeInfo "pg_trigger" (Just 10036)),
      (_pg_event_triggerOid, TypeInfo "_pg_event_trigger" Nothing),
      (pg_event_triggerOid, TypeInfo "pg_event_trigger" (Just 10038)),
      (_pg_descriptionOid, TypeInfo "_pg_description" Nothing),
      (pg_descriptionOid, TypeInfo "pg_description" (Just 10040)),
      (_pg_castOid, TypeInfo "_pg_cast" Nothing),
      (pg_castOid, TypeInfo "pg_cast" (Just 10042)),
      (_pg_enumOid, TypeInfo "_pg_enum" Nothing),
      (pg_enumOid, TypeInfo "pg_enum" (Just 10044)),
      (_pg_namespaceOid, TypeInfo "_pg_namespace" Nothing),
      (pg_namespaceOid, TypeInfo "pg_namespace" (Just 10046)),
      (_pg_conversionOid, TypeInfo "_pg_conversion" Nothing),
      (pg_conversionOid, TypeInfo "pg_conversion" (Just 10048)),
      (_pg_dependOid, TypeInfo "_pg_depend" Nothing),
      (pg_dependOid, TypeInfo "pg_depend" (Just 10050)),
      (_pg_databaseOid, TypeInfo "_pg_database" Nothing),
      (_pg_db_role_settingOid, TypeInfo "_pg_db_role_setting" Nothing),
      (pg_db_role_settingOid, TypeInfo "pg_db_role_setting" (Just 10053)),
      (_pg_tablespaceOid, TypeInfo "_pg_tablespace" Nothing),
      (pg_tablespaceOid, TypeInfo "pg_tablespace" (Just 10055)),
      (_pg_authidOid, TypeInfo "_pg_authid" Nothing),
      (_pg_auth_membersOid, TypeInfo "_pg_auth_members" Nothing),
      (_pg_shdependOid, TypeInfo "_pg_shdepend" Nothing),
      (pg_shdependOid, TypeInfo "pg_shdepend" (Just 10059)),
      (_pg_shdescriptionOid, TypeInfo "_pg_shdescription" Nothing),
      (pg_shdescriptionOid, TypeInfo "pg_shdescription" (Just 10061)),
      (_pg_ts_configOid, TypeInfo "_pg_ts_config" Nothing),
      (pg_ts_configOid, TypeInfo "pg_ts_config" (Just 10063)),
      (_pg_ts_config_mapOid, TypeInfo "_pg_ts_config_map" Nothing),
      (pg_ts_config_mapOid, TypeInfo "pg_ts_config_map" (Just 10065)),
      (_pg_ts_dictOid, TypeInfo "_pg_ts_dict" Nothing),
      (pg_ts_dictOid, TypeInfo "pg_ts_dict" (Just 10067)),
      (_pg_ts_parserOid, TypeInfo "_pg_ts_parser" Nothing),
      (pg_ts_parserOid, TypeInfo "pg_ts_parser" (Just 10069)),
      (_pg_ts_templateOid, TypeInfo "_pg_ts_template" Nothing),
      (pg_ts_templateOid, TypeInfo "pg_ts_template" (Just 10071)),
      (_pg_extensionOid, TypeInfo "_pg_extension" Nothing),
      (pg_extensionOid, TypeInfo "pg_extension" (Just 10073)),
      (_pg_foreign_data_wrapperOid, TypeInfo "_pg_foreign_data_wrapper" Nothing),
      (pg_foreign_data_wrapperOid, TypeInfo "pg_foreign_data_wrapper" (Just 10075)),
      (_pg_foreign_serverOid, TypeInfo "_pg_foreign_server" Nothing),
      (pg_foreign_serverOid, TypeInfo "pg_foreign_server" (Just 10077)),
      (_pg_user_mappingOid, TypeInfo "_pg_user_mapping" Nothing),
      (pg_user_mappingOid, TypeInfo "pg_user_mapping" (Just 10079)),
      (_pg_foreign_tableOid, TypeInfo "_pg_foreign_table" Nothing),
      (pg_foreign_tableOid, TypeInfo "pg_foreign_table" (Just 10081)),
      (_pg_policyOid, TypeInfo "_pg_policy" Nothing),
      (pg_policyOid, TypeInfo "pg_policy" (Just 10083)),
      (_pg_replication_originOid, TypeInfo "_pg_replication_origin" Nothing),
      (pg_replication_originOid, TypeInfo "pg_replication_origin" (Just 10085)),
      (_pg_default_aclOid, TypeInfo "_pg_default_acl" Nothing),
      (pg_default_aclOid, TypeInfo "pg_default_acl" (Just 10087)),
      (_pg_init_privsOid, TypeInfo "_pg_init_privs" Nothing),
      (pg_init_privsOid, TypeInfo "pg_init_privs" (Just 10089)),
      (_pg_seclabelOid, TypeInfo "_pg_seclabel" Nothing),
      (pg_seclabelOid, TypeInfo "pg_seclabel" (Just 10091)),
      (_pg_shseclabelOid, TypeInfo "_pg_shseclabel" Nothing),
      (_pg_collationOid, TypeInfo "_pg_collation" Nothing),
      (pg_collationOid, TypeInfo "pg_collation" (Just 10094)),
      (_pg_parameter_aclOid, TypeInfo "_pg_parameter_acl" Nothing),
      (pg_parameter_aclOid, TypeInfo "pg_parameter_acl" (Just 10096)),
      (_pg_partitioned_tableOid, TypeInfo "_pg_partitioned_table" Nothing),
      (pg_partitioned_tableOid, TypeInfo "pg_partitioned_table" (Just 10098)),
      (_pg_rangeOid, TypeInfo "_pg_range" Nothing),
      (pg_rangeOid, TypeInfo "pg_range" (Just 10100)),
      (_pg_transformOid, TypeInfo "_pg_transform" Nothing),
      (pg_transformOid, TypeInfo "pg_transform" (Just 10102)),
      (_pg_sequenceOid, TypeInfo "_pg_sequence" Nothing),
      (pg_sequenceOid, TypeInfo "pg_sequence" (Just 10104)),
      (_pg_publicationOid, TypeInfo "_pg_publication" Nothing),
      (pg_publicationOid, TypeInfo "pg_publication" (Just 10106)),
      (_pg_publication_namespaceOid, TypeInfo "_pg_publication_namespace" Nothing),
      (pg_publication_namespaceOid, TypeInfo "pg_publication_namespace" (Just 10108)),
      (_pg_publication_relOid, TypeInfo "_pg_publication_rel" Nothing),
      (pg_publication_relOid, TypeInfo "pg_publication_rel" (Just 10110)),
      (_pg_subscriptionOid, TypeInfo "_pg_subscription" Nothing),
      (_pg_subscription_relOid, TypeInfo "_pg_subscription_rel" Nothing),
      (pg_subscription_relOid, TypeInfo "pg_subscription_rel" (Just 10113)),
      (_pg_rolesOid, TypeInfo "_pg_roles" Nothing),
      (pg_rolesOid, TypeInfo "pg_roles" (Just 12001)),
      (_pg_shadowOid, TypeInfo "_pg_shadow" Nothing),
      (pg_shadowOid, TypeInfo "pg_shadow" (Just 12006)),
      (_pg_groupOid, TypeInfo "_pg_group" Nothing),
      (pg_groupOid, TypeInfo "pg_group" (Just 12011)),
      (_pg_userOid, TypeInfo "_pg_user" Nothing),
      (pg_userOid, TypeInfo "pg_user" (Just 12015)),
      (_pg_policiesOid, TypeInfo "_pg_policies" Nothing),
      (pg_policiesOid, TypeInfo "pg_policies" (Just 12019)),
      (_pg_rulesOid, TypeInfo "_pg_rules" Nothing),
      (pg_rulesOid, TypeInfo "pg_rules" (Just 12024)),
      (_pg_viewsOid, TypeInfo "_pg_views" Nothing),
      (pg_viewsOid, TypeInfo "pg_views" (Just 12029)),
      (_pg_tablesOid, TypeInfo "_pg_tables" Nothing),
      (pg_tablesOid, TypeInfo "pg_tables" (Just 12034)),
      (_pg_matviewsOid, TypeInfo "_pg_matviews" Nothing),
      (pg_matviewsOid, TypeInfo "pg_matviews" (Just 12039)),
      (_pg_indexesOid, TypeInfo "_pg_indexes" Nothing),
      (pg_indexesOid, TypeInfo "pg_indexes" (Just 12044)),
      (_pg_sequencesOid, TypeInfo "_pg_sequences" Nothing),
      (pg_sequencesOid, TypeInfo "pg_sequences" (Just 12049)),
      (_pg_statsOid, TypeInfo "_pg_stats" Nothing),
      (pg_statsOid, TypeInfo "pg_stats" (Just 12054)),
      (_pg_stats_extOid, TypeInfo "_pg_stats_ext" Nothing),
      (pg_stats_extOid, TypeInfo "pg_stats_ext" (Just 12059)),
      (_pg_stats_ext_exprsOid, TypeInfo "_pg_stats_ext_exprs" Nothing),
      (pg_stats_ext_exprsOid, TypeInfo "pg_stats_ext_exprs" (Just 12064)),
      (_pg_publication_tablesOid, TypeInfo "_pg_publication_tables" Nothing),
      (pg_publication_tablesOid, TypeInfo "pg_publication_tables" (Just 12069)),
      (_pg_locksOid, TypeInfo "_pg_locks" Nothing),
      (pg_locksOid, TypeInfo "pg_locks" (Just 12074)),
      (_pg_cursorsOid, TypeInfo "_pg_cursors" Nothing),
      (pg_cursorsOid, TypeInfo "pg_cursors" (Just 12078)),
      (_pg_available_extensionsOid, TypeInfo "_pg_available_extensions" Nothing),
      (pg_available_extensionsOid, TypeInfo "pg_available_extensions" (Just 12082)),
      (_pg_available_extension_versionsOid, TypeInfo "_pg_available_extension_versions" Nothing),
      (pg_available_extension_versionsOid, TypeInfo "pg_available_extension_versions" (Just 12086)),
      (_pg_prepared_xactsOid, TypeInfo "_pg_prepared_xacts" Nothing),
      (pg_prepared_xactsOid, TypeInfo "pg_prepared_xacts" (Just 12091)),
      (_pg_prepared_statementsOid, TypeInfo "_pg_prepared_statements" Nothing),
      (pg_prepared_statementsOid, TypeInfo "pg_prepared_statements" (Just 12096)),
      (_pg_seclabelsOid, TypeInfo "_pg_seclabels" Nothing),
      (pg_seclabelsOid, TypeInfo "pg_seclabels" (Just 12100)),
      (_pg_settingsOid, TypeInfo "_pg_settings" Nothing),
      (pg_settingsOid, TypeInfo "pg_settings" (Just 12105)),
      (_pg_file_settingsOid, TypeInfo "_pg_file_settings" Nothing),
      (pg_file_settingsOid, TypeInfo "pg_file_settings" (Just 12111)),
      (_pg_hba_file_rulesOid, TypeInfo "_pg_hba_file_rules" Nothing),
      (pg_hba_file_rulesOid, TypeInfo "pg_hba_file_rules" (Just 12115)),
      (_pg_ident_file_mappingsOid, TypeInfo "_pg_ident_file_mappings" Nothing),
      (pg_ident_file_mappingsOid, TypeInfo "pg_ident_file_mappings" (Just 12119)),
      (_pg_timezone_abbrevsOid, TypeInfo "_pg_timezone_abbrevs" Nothing),
      (pg_timezone_abbrevsOid, TypeInfo "pg_timezone_abbrevs" (Just 12123)),
      (_pg_timezone_namesOid, TypeInfo "_pg_timezone_names" Nothing),
      (pg_timezone_namesOid, TypeInfo "pg_timezone_names" (Just 12127)),
      (_pg_configOid, TypeInfo "_pg_config" Nothing),
      (pg_configOid, TypeInfo "pg_config" (Just 12131)),
      (_pg_shmem_allocationsOid, TypeInfo "_pg_shmem_allocations" Nothing),
      (pg_shmem_allocationsOid, TypeInfo "pg_shmem_allocations" (Just 12135)),
      (_pg_backend_memory_contextsOid, TypeInfo "_pg_backend_memory_contexts" Nothing),
      (pg_backend_memory_contextsOid, TypeInfo "pg_backend_memory_contexts" (Just 12139)),
      (_pg_stat_all_tablesOid, TypeInfo "_pg_stat_all_tables" Nothing),
      (pg_stat_all_tablesOid, TypeInfo "pg_stat_all_tables" (Just 12143)),
      (_pg_stat_xact_all_tablesOid, TypeInfo "_pg_stat_xact_all_tables" Nothing),
      (pg_stat_xact_all_tablesOid, TypeInfo "pg_stat_xact_all_tables" (Just 12148)),
      (_pg_stat_sys_tablesOid, TypeInfo "_pg_stat_sys_tables" Nothing),
      (pg_stat_sys_tablesOid, TypeInfo "pg_stat_sys_tables" (Just 12153)),
      (_pg_stat_xact_sys_tablesOid, TypeInfo "_pg_stat_xact_sys_tables" Nothing),
      (pg_stat_xact_sys_tablesOid, TypeInfo "pg_stat_xact_sys_tables" (Just 12158)),
      (_pg_stat_user_tablesOid, TypeInfo "_pg_stat_user_tables" Nothing),
      (pg_stat_user_tablesOid, TypeInfo "pg_stat_user_tables" (Just 12162)),
      (_pg_stat_xact_user_tablesOid, TypeInfo "_pg_stat_xact_user_tables" Nothing),
      (pg_stat_xact_user_tablesOid, TypeInfo "pg_stat_xact_user_tables" (Just 12167)),
      (_pg_statio_all_tablesOid, TypeInfo "_pg_statio_all_tables" Nothing),
      (pg_statio_all_tablesOid, TypeInfo "pg_statio_all_tables" (Just 12171)),
      (_pg_statio_sys_tablesOid, TypeInfo "_pg_statio_sys_tables" Nothing),
      (pg_statio_sys_tablesOid, TypeInfo "pg_statio_sys_tables" (Just 12176)),
      (_pg_statio_user_tablesOid, TypeInfo "_pg_statio_user_tables" Nothing),
      (pg_statio_user_tablesOid, TypeInfo "pg_statio_user_tables" (Just 12180)),
      (_pg_stat_all_indexesOid, TypeInfo "_pg_stat_all_indexes" Nothing),
      (pg_stat_all_indexesOid, TypeInfo "pg_stat_all_indexes" (Just 12184)),
      (_pg_stat_sys_indexesOid, TypeInfo "_pg_stat_sys_indexes" Nothing),
      (pg_stat_sys_indexesOid, TypeInfo "pg_stat_sys_indexes" (Just 12189)),
      (_pg_stat_user_indexesOid, TypeInfo "_pg_stat_user_indexes" Nothing),
      (pg_stat_user_indexesOid, TypeInfo "pg_stat_user_indexes" (Just 12193)),
      (_pg_statio_all_indexesOid, TypeInfo "_pg_statio_all_indexes" Nothing),
      (pg_statio_all_indexesOid, TypeInfo "pg_statio_all_indexes" (Just 12197)),
      (_pg_statio_sys_indexesOid, TypeInfo "_pg_statio_sys_indexes" Nothing),
      (pg_statio_sys_indexesOid, TypeInfo "pg_statio_sys_indexes" (Just 12202)),
      (_pg_statio_user_indexesOid, TypeInfo "_pg_statio_user_indexes" Nothing),
      (pg_statio_user_indexesOid, TypeInfo "pg_statio_user_indexes" (Just 12206)),
      (_pg_statio_all_sequencesOid, TypeInfo "_pg_statio_all_sequences" Nothing),
      (pg_statio_all_sequencesOid, TypeInfo "pg_statio_all_sequences" (Just 12210)),
      (_pg_statio_sys_sequencesOid, TypeInfo "_pg_statio_sys_sequences" Nothing),
      (pg_statio_sys_sequencesOid, TypeInfo "pg_statio_sys_sequences" (Just 12215)),
      (_pg_statio_user_sequencesOid, TypeInfo "_pg_statio_user_sequences" Nothing),
      (pg_statio_user_sequencesOid, TypeInfo "pg_statio_user_sequences" (Just 12219)),
      (_pg_stat_activityOid, TypeInfo "_pg_stat_activity" Nothing),
      (pg_stat_activityOid, TypeInfo "pg_stat_activity" (Just 12223)),
      (_pg_stat_replicationOid, TypeInfo "_pg_stat_replication" Nothing),
      (pg_stat_replicationOid, TypeInfo "pg_stat_replication" (Just 12228)),
      (_pg_stat_slruOid, TypeInfo "_pg_stat_slru" Nothing),
      (pg_stat_slruOid, TypeInfo "pg_stat_slru" (Just 12233)),
      (_pg_stat_wal_receiverOid, TypeInfo "_pg_stat_wal_receiver" Nothing),
      (pg_stat_wal_receiverOid, TypeInfo "pg_stat_wal_receiver" (Just 12237)),
      (_pg_stat_recovery_prefetchOid, TypeInfo "_pg_stat_recovery_prefetch" Nothing),
      (pg_stat_recovery_prefetchOid, TypeInfo "pg_stat_recovery_prefetch" (Just 12241)),
      (_pg_stat_subscriptionOid, TypeInfo "_pg_stat_subscription" Nothing),
      (pg_stat_subscriptionOid, TypeInfo "pg_stat_subscription" (Just 12245)),
      (_pg_stat_sslOid, TypeInfo "_pg_stat_ssl" Nothing),
      (pg_stat_sslOid, TypeInfo "pg_stat_ssl" (Just 12250)),
      (_pg_stat_gssapiOid, TypeInfo "_pg_stat_gssapi" Nothing),
      (pg_stat_gssapiOid, TypeInfo "pg_stat_gssapi" (Just 12254)),
      (_pg_replication_slotsOid, TypeInfo "_pg_replication_slots" Nothing),
      (pg_replication_slotsOid, TypeInfo "pg_replication_slots" (Just 12258)),
      (_pg_stat_replication_slotsOid, TypeInfo "_pg_stat_replication_slots" Nothing),
      (pg_stat_replication_slotsOid, TypeInfo "pg_stat_replication_slots" (Just 12263)),
      (_pg_stat_databaseOid, TypeInfo "_pg_stat_database" Nothing),
      (pg_stat_databaseOid, TypeInfo "pg_stat_database" (Just 12267)),
      (_pg_stat_database_conflictsOid, TypeInfo "_pg_stat_database_conflicts" Nothing),
      (pg_stat_database_conflictsOid, TypeInfo "pg_stat_database_conflicts" (Just 12272)),
      (_pg_stat_user_functionsOid, TypeInfo "_pg_stat_user_functions" Nothing),
      (pg_stat_user_functionsOid, TypeInfo "pg_stat_user_functions" (Just 12276)),
      (_pg_stat_xact_user_functionsOid, TypeInfo "_pg_stat_xact_user_functions" Nothing),
      (pg_stat_xact_user_functionsOid, TypeInfo "pg_stat_xact_user_functions" (Just 12281)),
      (_pg_stat_archiverOid, TypeInfo "_pg_stat_archiver" Nothing),
      (pg_stat_archiverOid, TypeInfo "pg_stat_archiver" (Just 12286)),
      (_pg_stat_bgwriterOid, TypeInfo "_pg_stat_bgwriter" Nothing),
      (pg_stat_bgwriterOid, TypeInfo "pg_stat_bgwriter" (Just 12290)),
      (_pg_stat_ioOid, TypeInfo "_pg_stat_io" Nothing),
      (pg_stat_ioOid, TypeInfo "pg_stat_io" (Just 12294)),
      (_pg_stat_walOid, TypeInfo "_pg_stat_wal" Nothing),
      (pg_stat_walOid, TypeInfo "pg_stat_wal" (Just 12298)),
      (_pg_stat_progress_analyzeOid, TypeInfo "_pg_stat_progress_analyze" Nothing),
      (pg_stat_progress_analyzeOid, TypeInfo "pg_stat_progress_analyze" (Just 12302)),
      (_pg_stat_progress_vacuumOid, TypeInfo "_pg_stat_progress_vacuum" Nothing),
      (pg_stat_progress_vacuumOid, TypeInfo "pg_stat_progress_vacuum" (Just 12307)),
      (_pg_stat_progress_clusterOid, TypeInfo "_pg_stat_progress_cluster" Nothing),
      (pg_stat_progress_clusterOid, TypeInfo "pg_stat_progress_cluster" (Just 12312)),
      (_pg_stat_progress_create_indexOid, TypeInfo "_pg_stat_progress_create_index" Nothing),
      (pg_stat_progress_create_indexOid, TypeInfo "pg_stat_progress_create_index" (Just 12317)),
      (_pg_stat_progress_basebackupOid, TypeInfo "_pg_stat_progress_basebackup" Nothing),
      (pg_stat_progress_basebackupOid, TypeInfo "pg_stat_progress_basebackup" (Just 12322)),
      (_pg_stat_progress_copyOid, TypeInfo "_pg_stat_progress_copy" Nothing),
      (pg_stat_progress_copyOid, TypeInfo "pg_stat_progress_copy" (Just 12327)),
      (_pg_user_mappingsOid, TypeInfo "_pg_user_mappings" Nothing),
      (pg_user_mappingsOid, TypeInfo "pg_user_mappings" (Just 12332)),
      (_pg_replication_origin_statusOid, TypeInfo "_pg_replication_origin_status" Nothing),
      (pg_replication_origin_statusOid, TypeInfo "pg_replication_origin_status" (Just 12337)),
      (_pg_stat_subscription_statsOid, TypeInfo "_pg_stat_subscription_stats" Nothing),
      (pg_stat_subscription_statsOid, TypeInfo "pg_stat_subscription_stats" (Just 12341)),
      (_cardinal_numberOid, TypeInfo "_cardinal_number" Nothing),
      (cardinal_numberOid, TypeInfo "cardinal_number" (Just 13291)),
      (_character_dataOid, TypeInfo "_character_data" Nothing),
      (character_dataOid, TypeInfo "character_data" (Just 13294)),
      (_sql_identifierOid, TypeInfo "_sql_identifier" Nothing),
      (sql_identifierOid, TypeInfo "sql_identifier" (Just 13296)),
      (_information_schema_catalog_nameOid, TypeInfo "_information_schema_catalog_name" Nothing),
      (information_schema_catalog_nameOid, TypeInfo "information_schema_catalog_name" (Just 13299)),
      (_time_stampOid, TypeInfo "_time_stamp" Nothing),
      (time_stampOid, TypeInfo "time_stamp" (Just 13302)),
      (_yes_or_noOid, TypeInfo "_yes_or_no" Nothing),
      (yes_or_noOid, TypeInfo "yes_or_no" (Just 13304)),
      (_applicable_rolesOid, TypeInfo "_applicable_roles" Nothing),
      (applicable_rolesOid, TypeInfo "applicable_roles" (Just 13308)),
      (_administrable_role_authorizationsOid, TypeInfo "_administrable_role_authorizations" Nothing),
      (administrable_role_authorizationsOid, TypeInfo "administrable_role_authorizations" (Just 13313)),
      (_attributesOid, TypeInfo "_attributes" Nothing),
      (attributesOid, TypeInfo "attributes" (Just 13317)),
      (_character_setsOid, TypeInfo "_character_sets" Nothing),
      (character_setsOid, TypeInfo "character_sets" (Just 13322)),
      (_check_constraint_routine_usageOid, TypeInfo "_check_constraint_routine_usage" Nothing),
      (check_constraint_routine_usageOid, TypeInfo "check_constraint_routine_usage" (Just 13327)),
      (_check_constraintsOid, TypeInfo "_check_constraints" Nothing),
      (check_constraintsOid, TypeInfo "check_constraints" (Just 13332)),
      (_collationsOid, TypeInfo "_collations" Nothing),
      (collationsOid, TypeInfo "collations" (Just 13337)),
      (_collation_character_set_applicabilityOid, TypeInfo "_collation_character_set_applicability" Nothing),
      (collation_character_set_applicabilityOid, TypeInfo "collation_character_set_applicability" (Just 13342)),
      (_column_column_usageOid, TypeInfo "_column_column_usage" Nothing),
      (column_column_usageOid, TypeInfo "column_column_usage" (Just 13347)),
      (_column_domain_usageOid, TypeInfo "_column_domain_usage" Nothing),
      (column_domain_usageOid, TypeInfo "column_domain_usage" (Just 13352)),
      (_column_privilegesOid, TypeInfo "_column_privileges" Nothing),
      (column_privilegesOid, TypeInfo "column_privileges" (Just 13357)),
      (_column_udt_usageOid, TypeInfo "_column_udt_usage" Nothing),
      (column_udt_usageOid, TypeInfo "column_udt_usage" (Just 13362)),
      (_columnsOid, TypeInfo "_columns" Nothing),
      (columnsOid, TypeInfo "columns" (Just 13367)),
      (_constraint_column_usageOid, TypeInfo "_constraint_column_usage" Nothing),
      (constraint_column_usageOid, TypeInfo "constraint_column_usage" (Just 13372)),
      (_constraint_table_usageOid, TypeInfo "_constraint_table_usage" Nothing),
      (constraint_table_usageOid, TypeInfo "constraint_table_usage" (Just 13377)),
      (_domain_constraintsOid, TypeInfo "_domain_constraints" Nothing),
      (domain_constraintsOid, TypeInfo "domain_constraints" (Just 13382)),
      (_domain_udt_usageOid, TypeInfo "_domain_udt_usage" Nothing),
      (domain_udt_usageOid, TypeInfo "domain_udt_usage" (Just 13387)),
      (_domainsOid, TypeInfo "_domains" Nothing),
      (domainsOid, TypeInfo "domains" (Just 13392)),
      (_enabled_rolesOid, TypeInfo "_enabled_roles" Nothing),
      (enabled_rolesOid, TypeInfo "enabled_roles" (Just 13397)),
      (_key_column_usageOid, TypeInfo "_key_column_usage" Nothing),
      (key_column_usageOid, TypeInfo "key_column_usage" (Just 13401)),
      (_parametersOid, TypeInfo "_parameters" Nothing),
      (parametersOid, TypeInfo "parameters" (Just 13406)),
      (_referential_constraintsOid, TypeInfo "_referential_constraints" Nothing),
      (referential_constraintsOid, TypeInfo "referential_constraints" (Just 13411)),
      (_role_column_grantsOid, TypeInfo "_role_column_grants" Nothing),
      (role_column_grantsOid, TypeInfo "role_column_grants" (Just 13416)),
      (_routine_column_usageOid, TypeInfo "_routine_column_usage" Nothing),
      (routine_column_usageOid, TypeInfo "routine_column_usage" (Just 13420)),
      (_routine_privilegesOid, TypeInfo "_routine_privileges" Nothing),
      (routine_privilegesOid, TypeInfo "routine_privileges" (Just 13425)),
      (_role_routine_grantsOid, TypeInfo "_role_routine_grants" Nothing),
      (role_routine_grantsOid, TypeInfo "role_routine_grants" (Just 13430)),
      (_routine_routine_usageOid, TypeInfo "_routine_routine_usage" Nothing),
      (routine_routine_usageOid, TypeInfo "routine_routine_usage" (Just 13434)),
      (_routine_sequence_usageOid, TypeInfo "_routine_sequence_usage" Nothing),
      (routine_sequence_usageOid, TypeInfo "routine_sequence_usage" (Just 13439)),
      (_routine_table_usageOid, TypeInfo "_routine_table_usage" Nothing),
      (routine_table_usageOid, TypeInfo "routine_table_usage" (Just 13444)),
      (_routinesOid, TypeInfo "_routines" Nothing),
      (routinesOid, TypeInfo "routines" (Just 13449)),
      (_schemataOid, TypeInfo "_schemata" Nothing),
      (schemataOid, TypeInfo "schemata" (Just 13454)),
      (_sequencesOid, TypeInfo "_sequences" Nothing),
      (sequencesOid, TypeInfo "sequences" (Just 13458)),
      (_sql_featuresOid, TypeInfo "_sql_features" Nothing),
      (sql_featuresOid, TypeInfo "sql_features" (Just 13463)),
      (_sql_implementation_infoOid, TypeInfo "_sql_implementation_info" Nothing),
      (sql_implementation_infoOid, TypeInfo "sql_implementation_info" (Just 13468)),
      (_sql_partsOid, TypeInfo "_sql_parts" Nothing),
      (sql_partsOid, TypeInfo "sql_parts" (Just 13473)),
      (_sql_sizingOid, TypeInfo "_sql_sizing" Nothing),
      (sql_sizingOid, TypeInfo "sql_sizing" (Just 13478)),
      (_table_constraintsOid, TypeInfo "_table_constraints" Nothing),
      (table_constraintsOid, TypeInfo "table_constraints" (Just 13483)),
      (_table_privilegesOid, TypeInfo "_table_privileges" Nothing),
      (table_privilegesOid, TypeInfo "table_privileges" (Just 13488)),
      (_role_table_grantsOid, TypeInfo "_role_table_grants" Nothing),
      (role_table_grantsOid, TypeInfo "role_table_grants" (Just 13493)),
      (_tablesOid, TypeInfo "_tables" Nothing),
      (tablesOid, TypeInfo "tables" (Just 13497)),
      (_transformsOid, TypeInfo "_transforms" Nothing),
      (transformsOid, TypeInfo "transforms" (Just 13502)),
      (_triggered_update_columnsOid, TypeInfo "_triggered_update_columns" Nothing),
      (triggered_update_columnsOid, TypeInfo "triggered_update_columns" (Just 13507)),
      (_triggersOid, TypeInfo "_triggers" Nothing),
      (triggersOid, TypeInfo "triggers" (Just 13512)),
      (_udt_privilegesOid, TypeInfo "_udt_privileges" Nothing),
      (udt_privilegesOid, TypeInfo "udt_privileges" (Just 13517)),
      (_role_udt_grantsOid, TypeInfo "_role_udt_grants" Nothing),
      (role_udt_grantsOid, TypeInfo "role_udt_grants" (Just 13522)),
      (_usage_privilegesOid, TypeInfo "_usage_privileges" Nothing),
      (usage_privilegesOid, TypeInfo "usage_privileges" (Just 13526)),
      (_role_usage_grantsOid, TypeInfo "_role_usage_grants" Nothing),
      (role_usage_grantsOid, TypeInfo "role_usage_grants" (Just 13531)),
      (_user_defined_typesOid, TypeInfo "_user_defined_types" Nothing),
      (user_defined_typesOid, TypeInfo "user_defined_types" (Just 13535)),
      (_view_column_usageOid, TypeInfo "_view_column_usage" Nothing),
      (view_column_usageOid, TypeInfo "view_column_usage" (Just 13540)),
      (_view_routine_usageOid, TypeInfo "_view_routine_usage" Nothing),
      (view_routine_usageOid, TypeInfo "view_routine_usage" (Just 13545)),
      (_view_table_usageOid, TypeInfo "_view_table_usage" Nothing),
      (view_table_usageOid, TypeInfo "view_table_usage" (Just 13550)),
      (_viewsOid, TypeInfo "_views" Nothing),
      (viewsOid, TypeInfo "views" (Just 13555)),
      (_data_type_privilegesOid, TypeInfo "_data_type_privileges" Nothing),
      (data_type_privilegesOid, TypeInfo "data_type_privileges" (Just 13560)),
      (_element_typesOid, TypeInfo "_element_types" Nothing),
      (element_typesOid, TypeInfo "element_types" (Just 13565)),
      (__pg_foreign_table_columnsOid, TypeInfo "__pg_foreign_table_columns" Nothing),
      (_pg_foreign_table_columnsOid, TypeInfo "_pg_foreign_table_columns" (Just 13570)),
      (_column_optionsOid, TypeInfo "_column_options" Nothing),
      (column_optionsOid, TypeInfo "column_options" (Just 13575)),
      (__pg_foreign_data_wrappersOid, TypeInfo "__pg_foreign_data_wrappers" Nothing),
      (_pg_foreign_data_wrappersOid, TypeInfo "_pg_foreign_data_wrappers" (Just 13579)),
      (_foreign_data_wrapper_optionsOid, TypeInfo "_foreign_data_wrapper_options" Nothing),
      (foreign_data_wrapper_optionsOid, TypeInfo "foreign_data_wrapper_options" (Just 13583)),
      (_foreign_data_wrappersOid, TypeInfo "_foreign_data_wrappers" Nothing),
      (foreign_data_wrappersOid, TypeInfo "foreign_data_wrappers" (Just 13587)),
      (__pg_foreign_serversOid, TypeInfo "__pg_foreign_servers" Nothing),
      (_pg_foreign_serversOid, TypeInfo "_pg_foreign_servers" (Just 13591)),
      (_foreign_server_optionsOid, TypeInfo "_foreign_server_options" Nothing),
      (foreign_server_optionsOid, TypeInfo "foreign_server_options" (Just 13596)),
      (_foreign_serversOid, TypeInfo "_foreign_servers" Nothing),
      (foreign_serversOid, TypeInfo "foreign_servers" (Just 13600)),
      (__pg_foreign_tablesOid, TypeInfo "__pg_foreign_tables" Nothing),
      (_pg_foreign_tablesOid, TypeInfo "_pg_foreign_tables" (Just 13604)),
      (_foreign_table_optionsOid, TypeInfo "_foreign_table_options" Nothing),
      (foreign_table_optionsOid, TypeInfo "foreign_table_options" (Just 13609)),
      (_foreign_tablesOid, TypeInfo "_foreign_tables" Nothing),
      (foreign_tablesOid, TypeInfo "foreign_tables" (Just 13613)),
      (__pg_user_mappingsOid, TypeInfo "__pg_user_mappings" Nothing),
      (_pg_user_mappingsOid, TypeInfo "_pg_user_mappings" (Just 13617)),
      (_user_mapping_optionsOid, TypeInfo "_user_mapping_options" Nothing),
      (user_mapping_optionsOid, TypeInfo "user_mapping_options" (Just 13622)),
      (_user_mappingsOid, TypeInfo "_user_mappings" Nothing),
      (user_mappingsOid, TypeInfo "user_mappings" (Just 13627))
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
