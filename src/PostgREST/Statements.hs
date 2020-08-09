{-|
Module      : PostgREST.Statements
Description : PostgREST single SQL statements.

This module constructs single SQL statements that can be parametrized and prepared.

- It consumes the SqlQuery types generated by the QueryBuilder module.
- It generates the body format and some headers of the final HTTP response.

TODO: Currently, createReadStatement is not using prepared statements. See https://github.com/PostgREST/postgrest/issues/718.
-}
module PostgREST.Statements (
    createWriteStatement
  , createReadStatement
  , callProcStatement
  , createExplainStatement
) where


import           Control.Lens                    ((^?))
import           Data.Aeson                      as JSON
import qualified Data.Aeson.Lens                 as L
import qualified Data.ByteString.Char8           as BS
import           Data.Maybe
import           Data.Text.Read                  (decimal)
import qualified Hasql.Decoders                  as HD
import qualified Hasql.Encoders                  as HE
import qualified Hasql.Statement                 as H
import           Network.HTTP.Types.Status
import           PostgREST.Error
import           PostgREST.Private.Common
import           PostgREST.Private.QueryFragment
import           PostgREST.Types
import           Protolude                       hiding (cast,
                                                  replace, toS)
import           Protolude.Conv                  (toS)
import           Text.InterpolatedString.Perl6   (qc)

{-| The generic query result format used by API responses. The location header
    is represented as a list of strings containing variable bindings like
    @"k1=eq.42"@, or the empty list if there is no location header.
-}
type ResultsWithCount = (Maybe Int64, Int64, [BS.ByteString], BS.ByteString, Either SimpleError [GucHeader], Either SimpleError (Maybe Status))

createWriteStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool ->
                        PreferRepresentation -> [Text] -> PgVersion ->
                        H.Statement ByteString ResultsWithCount
createWriteStatement selectQuery mutateQuery wantSingle isInsert asCsv rep pKeys pgVer =
  H.Statement sql (param HE.unknown) decodeStandard True
 where
  sql = [qc|
      WITH
      {sourceCTEName} AS ({mutateQuery})
      SELECT
        '' AS total_result_set,
        pg_catalog.count(_postgrest_t) AS page_total,
        {locF} AS header,
        {bodyF} AS body,
        {responseHeadersF pgVer} AS response_headers,
        {responseStatusF pgVer} AS response_status
      FROM ({selectF}) _postgrest_t |]

  locF =
    if isInsert && rep `elem` [Full, HeadersOnly]
      then BS.unwords [
        "CASE WHEN pg_catalog.count(_postgrest_t) = 1",
          "THEN coalesce(" <> locationF pKeys <> ", " <> noLocationF <> ")",
          "ELSE " <> noLocationF,
        "END"]
      else noLocationF

  bodyF
    | rep `elem` [None, HeadersOnly] = "''"
    | asCsv = asCsvF
    | wantSingle = asJsonSingleF
    | otherwise = asJsonF

  selectF
    -- prevent using any of the column names in ?select= when no response is returned from the CTE
    | rep `elem` [None, HeadersOnly] = "SELECT * FROM " <> sourceCTEName
    | otherwise                      = selectQuery

  decodeStandard :: HD.Result ResultsWithCount
  decodeStandard =
   fromMaybe (Nothing, 0, [], mempty, Right [], Right Nothing) <$> HD.rowMaybe standardRow

createReadStatement :: SqlQuery -> SqlQuery -> Bool -> Bool -> Bool -> Maybe FieldName -> PgVersion ->
                       H.Statement () ResultsWithCount
createReadStatement selectQuery countQuery isSingle countTotal asCsv binaryField pgVer =
  H.Statement sql HE.noParams decodeStandard False
 where
  sql = [qc|
      WITH
      {sourceCTEName} AS ({selectQuery})
      {countCTEF}
      SELECT
        {countResultF} AS total_result_set,
        pg_catalog.count(_postgrest_t) AS page_total,
        {noLocationF} AS header,
        {bodyF} AS body,
        {responseHeadersF pgVer} AS response_headers,
        {responseStatusF pgVer} AS response_status
      FROM ( SELECT * FROM {sourceCTEName}) _postgrest_t |]

  (countCTEF, countResultF) = countF countQuery countTotal

  bodyF
    | asCsv = asCsvF
    | isSingle = asJsonSingleF
    | isJust binaryField = asBinaryF $ fromJust binaryField
    | otherwise = asJsonF

  decodeStandard :: HD.Result ResultsWithCount
  decodeStandard =
    HD.singleRow standardRow

{-| Read and Write api requests use a similar response format which includes
    various record counts and possible location header. This is the decoder
    for that common type of query.
-}
standardRow :: HD.Row ResultsWithCount
standardRow = (,,,,,) <$> nullableColumn HD.int8 <*> column HD.int8
                      <*> arrayColumn HD.bytea <*> column HD.bytea
                      <*> (fromMaybe (Right []) <$> nullableColumn decodeGucHeaders)
                      <*> (fromMaybe (Right Nothing) <$> nullableColumn decodeGucStatus)

type ProcResults = (Maybe Int64, Int64, ByteString, Either SimpleError [GucHeader], Either SimpleError (Maybe Status))

callProcStatement :: Bool -> SqlQuery -> SqlQuery -> SqlQuery -> Bool ->
                     Bool -> Bool -> Bool -> Bool -> Maybe FieldName -> PgVersion ->
                     H.Statement ByteString ProcResults
callProcStatement returnsScalar callProcQuery selectQuery countQuery countTotal isSingle asCsv asBinary multObjects binaryField pgVer =
  H.Statement sql (param HE.unknown) decodeProc True
  where
    sql = [qc|
      WITH {sourceCTEName} AS ({callProcQuery})
      {countCTEF}
      SELECT
        {countResultF} AS total_result_set,
        pg_catalog.count(_postgrest_t) AS page_total,
        {bodyF} AS body,
        {responseHeadersF pgVer} AS response_headers,
        {responseStatusF pgVer} AS response_status
      FROM ({selectQuery}) _postgrest_t;|]

    (countCTEF, countResultF) = countF countQuery countTotal

    bodyF
     | returnsScalar = scalarBodyF
     | isSingle     = asJsonSingleF
     | asCsv = asCsvF
     | isJust binaryField = asBinaryF $ fromJust binaryField
     | otherwise = asJsonF

    scalarBodyF
     | asBinary    = asBinaryF "pgrst_scalar"
     | multObjects = "json_agg(_postgrest_t.pgrst_scalar)::character varying"
     | otherwise   = "(json_agg(_postgrest_t.pgrst_scalar)->0)::character varying"

    decodeProc :: HD.Result ProcResults
    decodeProc =
      fromMaybe (Just 0, 0, mempty, defGucHeaders, defGucStatus) <$> HD.rowMaybe procRow
      where
        defGucHeaders = Right []
        defGucStatus  = Right Nothing
        procRow = (,,,,) <$> nullableColumn HD.int8 <*> column HD.int8
                         <*> column HD.bytea
                         <*> (fromMaybe defGucHeaders <$> nullableColumn decodeGucHeaders)
                         <*> (fromMaybe defGucStatus <$> nullableColumn decodeGucStatus)

createExplainStatement :: SqlQuery -> H.Statement () (Maybe Int64)
createExplainStatement countQuery =
  H.Statement sql HE.noParams decodeExplain False
  where
    sql = [qc| EXPLAIN (FORMAT JSON) {countQuery} |]
    -- |
    -- An `EXPLAIN (FORMAT JSON) select * from items;` output looks like this:
    -- [{
    --   "Plan": {
    --     "Node Type": "Seq Scan", "Parallel Aware": false, "Relation Name": "items",
    --     "Alias": "items", "Startup Cost": 0.00, "Total Cost": 32.60,
    --     "Plan Rows": 2260,"Plan Width": 8} }]
    -- We only obtain the Plan Rows here.
    decodeExplain :: HD.Result (Maybe Int64)
    decodeExplain =
      let row = HD.singleRow $ column HD.bytea in
      (^? L.nth 0 . L.key "Plan" .  L.key "Plan Rows" . L._Integral) <$> row

decodeGucHeaders :: HD.Value (Either SimpleError [GucHeader])
decodeGucHeaders = first (const GucHeadersError) . JSON.eitherDecode . toS <$> HD.bytea

decodeGucStatus :: HD.Value (Either SimpleError (Maybe Status))
decodeGucStatus = first (const GucStatusError) . fmap (Just . toEnum . fst) . decimal <$> HD.text
