------------------------------------------------------------------------------

-- |
-- This file has been copied from the great postgresql-simple library, more specifically
-- from https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/src/Database.PostgreSQL.Simple.TypeInfo.Static.html
-- We modified it to keep only the OIDs.
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
    array_recordOid,
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
    array_xmlOid,
    array_jsonOid,
    array_lineOid,
    array_cidrOid,
    array_circleOid,
    array_moneyOid,
    array_boolOid,
    array_byteaOid,
    array_charOid,
    array_nameOid,
    array_int2Oid,
    array_int2vectorOid,
    array_int4Oid,
    array_regprocOid,
    array_textOid,
    array_tidOid,
    array_xidOid,
    array_cidOid,
    array_oidvectorOid,
    array_bpcharOid,
    array_varcharOid,
    array_int8Oid,
    array_pointOid,
    array_lsegOid,
    array_pathOid,
    array_boxOid,
    array_float4Oid,
    array_float8Oid,
    array_polygonOid,
    array_oidOid,
    array_macaddrOid,
    array_inetOid,
    array_timestampOid,
    array_dateOid,
    array_timeOid,
    array_timestamptzOid,
    array_intervalOid,
    array_numericOid,
    array_timetzOid,
    array_bitOid,
    array_varbitOid,
    array_refcursorOid,
    array_regprocedureOid,
    array_regoperOid,
    array_regoperatorOid,
    array_regclassOid,
    array_regtypeOid,
    array_uuidOid,
    array_jsonbOid,
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
  )
where

import Data.Int (Int32)

data Format = TextFmt | BinaryFmt
  deriving stock (Eq, Show)

newtype Oid = Oid Int32
  deriving stock (Show)
  deriving newtype (Eq, Real, Num, Ord, Integral, Enum)

boolOid :: Oid
boolOid = Oid 16
{-# INLINE boolOid #-}

byteaOid :: Oid
byteaOid = Oid 17
{-# INLINE byteaOid #-}

charOid :: Oid
charOid = Oid 18
{-# INLINE charOid #-}

nameOid :: Oid
nameOid = Oid 19
{-# INLINE nameOid #-}

int8Oid :: Oid
int8Oid = Oid 20
{-# INLINE int8Oid #-}

int2Oid :: Oid
int2Oid = Oid 21
{-# INLINE int2Oid #-}

int4Oid :: Oid
int4Oid = Oid 23
{-# INLINE int4Oid #-}

regprocOid :: Oid
regprocOid = Oid 24
{-# INLINE regprocOid #-}

textOid :: Oid
textOid = Oid 25
{-# INLINE textOid #-}

oidOid :: Oid
oidOid = Oid 26
{-# INLINE oidOid #-}

tidOid :: Oid
tidOid = Oid 27
{-# INLINE tidOid #-}

xidOid :: Oid
xidOid = Oid 28
{-# INLINE xidOid #-}

cidOid :: Oid
cidOid = Oid 29
{-# INLINE cidOid #-}

xmlOid :: Oid
xmlOid = Oid 142
{-# INLINE xmlOid #-}

pointOid :: Oid
pointOid = Oid 600
{-# INLINE pointOid #-}

lsegOid :: Oid
lsegOid = Oid 601
{-# INLINE lsegOid #-}

pathOid :: Oid
pathOid = Oid 602
{-# INLINE pathOid #-}

boxOid :: Oid
boxOid = Oid 603
{-# INLINE boxOid #-}

polygonOid :: Oid
polygonOid = Oid 604
{-# INLINE polygonOid #-}

lineOid :: Oid
lineOid = Oid 628
{-# INLINE lineOid #-}

cidrOid :: Oid
cidrOid = Oid 650
{-# INLINE cidrOid #-}

float4Oid :: Oid
float4Oid = Oid 700
{-# INLINE float4Oid #-}

float8Oid :: Oid
float8Oid = Oid 701
{-# INLINE float8Oid #-}

unknownOid :: Oid
unknownOid = Oid 705
{-# INLINE unknownOid #-}

circleOid :: Oid
circleOid = Oid 718
{-# INLINE circleOid #-}

moneyOid :: Oid
moneyOid = Oid 790
{-# INLINE moneyOid #-}

macaddrOid :: Oid
macaddrOid = Oid 829
{-# INLINE macaddrOid #-}

inetOid :: Oid
inetOid = Oid 869
{-# INLINE inetOid #-}

bpcharOid :: Oid
bpcharOid = Oid 1042
{-# INLINE bpcharOid #-}

varcharOid :: Oid
varcharOid = Oid 1043
{-# INLINE varcharOid #-}

dateOid :: Oid
dateOid = Oid 1082
{-# INLINE dateOid #-}

timeOid :: Oid
timeOid = Oid 1083
{-# INLINE timeOid #-}

timestampOid :: Oid
timestampOid = Oid 1114
{-# INLINE timestampOid #-}

timestamptzOid :: Oid
timestamptzOid = Oid 1184
{-# INLINE timestamptzOid #-}

intervalOid :: Oid
intervalOid = Oid 1186
{-# INLINE intervalOid #-}

timetzOid :: Oid
timetzOid = Oid 1266
{-# INLINE timetzOid #-}

bitOid :: Oid
bitOid = Oid 1560
{-# INLINE bitOid #-}

varbitOid :: Oid
varbitOid = Oid 1562
{-# INLINE varbitOid #-}

numericOid :: Oid
numericOid = Oid 1700
{-# INLINE numericOid #-}

refcursorOid :: Oid
refcursorOid = Oid 1790
{-# INLINE refcursorOid #-}

recordOid :: Oid
recordOid = Oid 2249
{-# INLINE recordOid #-}

voidOid :: Oid
voidOid = Oid 2278
{-# INLINE voidOid #-}

array_recordOid :: Oid
array_recordOid = Oid 2287
{-# INLINE array_recordOid #-}

regprocedureOid :: Oid
regprocedureOid = Oid 2202
{-# INLINE regprocedureOid #-}

regoperOid :: Oid
regoperOid = Oid 2203
{-# INLINE regoperOid #-}

regoperatorOid :: Oid
regoperatorOid = Oid 2204
{-# INLINE regoperatorOid #-}

regclassOid :: Oid
regclassOid = Oid 2205
{-# INLINE regclassOid #-}

regtypeOid :: Oid
regtypeOid = Oid 2206
{-# INLINE regtypeOid #-}

uuidOid :: Oid
uuidOid = Oid 2950
{-# INLINE uuidOid #-}

jsonOid :: Oid
jsonOid = Oid 114
{-# INLINE jsonOid #-}

jsonbOid :: Oid
jsonbOid = Oid 3802
{-# INLINE jsonbOid #-}

int2vectorOid :: Oid
int2vectorOid = Oid 22
{-# INLINE int2vectorOid #-}

oidvectorOid :: Oid
oidvectorOid = Oid 30
{-# INLINE oidvectorOid #-}

array_xmlOid :: Oid
array_xmlOid = Oid 143
{-# INLINE array_xmlOid #-}

array_jsonOid :: Oid
array_jsonOid = Oid 199
{-# INLINE array_jsonOid #-}

array_lineOid :: Oid
array_lineOid = Oid 629
{-# INLINE array_lineOid #-}

array_cidrOid :: Oid
array_cidrOid = Oid 651
{-# INLINE array_cidrOid #-}

array_circleOid :: Oid
array_circleOid = Oid 719
{-# INLINE array_circleOid #-}

array_moneyOid :: Oid
array_moneyOid = Oid 791
{-# INLINE array_moneyOid #-}

array_boolOid :: Oid
array_boolOid = Oid 1000
{-# INLINE array_boolOid #-}

array_byteaOid :: Oid
array_byteaOid = Oid 1001
{-# INLINE array_byteaOid #-}

array_charOid :: Oid
array_charOid = Oid 1002
{-# INLINE array_charOid #-}

array_nameOid :: Oid
array_nameOid = Oid 1003
{-# INLINE array_nameOid #-}

array_int2Oid :: Oid
array_int2Oid = Oid 1005
{-# INLINE array_int2Oid #-}

array_int2vectorOid :: Oid
array_int2vectorOid = Oid 1006
{-# INLINE array_int2vectorOid #-}

array_int4Oid :: Oid
array_int4Oid = Oid 1007
{-# INLINE array_int4Oid #-}

array_regprocOid :: Oid
array_regprocOid = Oid 1008
{-# INLINE array_regprocOid #-}

array_textOid :: Oid
array_textOid = Oid 1009
{-# INLINE array_textOid #-}

array_tidOid :: Oid
array_tidOid = Oid 1010
{-# INLINE array_tidOid #-}

array_xidOid :: Oid
array_xidOid = Oid 1011
{-# INLINE array_xidOid #-}

array_cidOid :: Oid
array_cidOid = Oid 1012
{-# INLINE array_cidOid #-}

array_oidvectorOid :: Oid
array_oidvectorOid = Oid 1013
{-# INLINE array_oidvectorOid #-}

array_bpcharOid :: Oid
array_bpcharOid = Oid 1014
{-# INLINE array_bpcharOid #-}

array_varcharOid :: Oid
array_varcharOid = Oid 1015
{-# INLINE array_varcharOid #-}

array_int8Oid :: Oid
array_int8Oid = Oid 1016
{-# INLINE array_int8Oid #-}

array_pointOid :: Oid
array_pointOid = Oid 1017
{-# INLINE array_pointOid #-}

array_lsegOid :: Oid
array_lsegOid = Oid 1018
{-# INLINE array_lsegOid #-}

array_pathOid :: Oid
array_pathOid = Oid 1019
{-# INLINE array_pathOid #-}

array_boxOid :: Oid
array_boxOid = Oid 1020
{-# INLINE array_boxOid #-}

array_float4Oid :: Oid
array_float4Oid = Oid 1021
{-# INLINE array_float4Oid #-}

array_float8Oid :: Oid
array_float8Oid = Oid 1022
{-# INLINE array_float8Oid #-}

array_polygonOid :: Oid
array_polygonOid = Oid 1027
{-# INLINE array_polygonOid #-}

array_oidOid :: Oid
array_oidOid = Oid 1028
{-# INLINE array_oidOid #-}

array_macaddrOid :: Oid
array_macaddrOid = Oid 1040
{-# INLINE array_macaddrOid #-}

array_inetOid :: Oid
array_inetOid = Oid 1041
{-# INLINE array_inetOid #-}

array_timestampOid :: Oid
array_timestampOid = Oid 1115
{-# INLINE array_timestampOid #-}

array_dateOid :: Oid
array_dateOid = Oid 1182
{-# INLINE array_dateOid #-}

array_timeOid :: Oid
array_timeOid = Oid 1183
{-# INLINE array_timeOid #-}

array_timestamptzOid :: Oid
array_timestamptzOid = Oid 1185
{-# INLINE array_timestamptzOid #-}

array_intervalOid :: Oid
array_intervalOid = Oid 1187
{-# INLINE array_intervalOid #-}

array_numericOid :: Oid
array_numericOid = Oid 1231
{-# INLINE array_numericOid #-}

array_timetzOid :: Oid
array_timetzOid = Oid 1270
{-# INLINE array_timetzOid #-}

array_bitOid :: Oid
array_bitOid = Oid 1561
{-# INLINE array_bitOid #-}

array_varbitOid :: Oid
array_varbitOid = Oid 1563
{-# INLINE array_varbitOid #-}

array_refcursorOid :: Oid
array_refcursorOid = Oid 2201
{-# INLINE array_refcursorOid #-}

array_regprocedureOid :: Oid
array_regprocedureOid = Oid 2207
{-# INLINE array_regprocedureOid #-}

array_regoperOid :: Oid
array_regoperOid = Oid 2208
{-# INLINE array_regoperOid #-}

array_regoperatorOid :: Oid
array_regoperatorOid = Oid 2209
{-# INLINE array_regoperatorOid #-}

array_regclassOid :: Oid
array_regclassOid = Oid 2210
{-# INLINE array_regclassOid #-}

array_regtypeOid :: Oid
array_regtypeOid = Oid 2211
{-# INLINE array_regtypeOid #-}

array_uuidOid :: Oid
array_uuidOid = Oid 2951
{-# INLINE array_uuidOid #-}

array_jsonbOid :: Oid
array_jsonbOid = Oid 3807
{-# INLINE array_jsonbOid #-}

int4rangeOid :: Oid
int4rangeOid = Oid 3904
{-# INLINE int4rangeOid #-}

_int4rangeOid :: Oid
_int4rangeOid = Oid 3905
{-# INLINE _int4rangeOid #-}

numrangeOid :: Oid
numrangeOid = Oid 3906
{-# INLINE numrangeOid #-}

_numrangeOid :: Oid
_numrangeOid = Oid 3907
{-# INLINE _numrangeOid #-}

tsrangeOid :: Oid
tsrangeOid = Oid 3908
{-# INLINE tsrangeOid #-}

_tsrangeOid :: Oid
_tsrangeOid = Oid 3909
{-# INLINE _tsrangeOid #-}

tstzrangeOid :: Oid
tstzrangeOid = Oid 3910
{-# INLINE tstzrangeOid #-}

_tstzrangeOid :: Oid
_tstzrangeOid = Oid 3911
{-# INLINE _tstzrangeOid #-}

daterangeOid :: Oid
daterangeOid = Oid 3912
{-# INLINE daterangeOid #-}

_daterangeOid :: Oid
_daterangeOid = Oid 3913
{-# INLINE _daterangeOid #-}

int8rangeOid :: Oid
int8rangeOid = Oid 3926
{-# INLINE int8rangeOid #-}

_int8rangeOid :: Oid
_int8rangeOid = Oid 3927
{-# INLINE _int8rangeOid #-}
