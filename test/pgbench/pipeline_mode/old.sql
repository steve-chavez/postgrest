select
  set_config('search_path', 'test', true),
  set_config('role', 'postgrest_test_anonymous', true),
  set_config('request.jwt.claims', '{"role":"postgrest_test_anonymous"}', true),
  set_config('request.method', 'POST', true),
  set_config('request.path', '/complex_items', true),
  set_config('request.headers', '{"host":"localhost:3000","content-type":"application/json","accept":"application/json, */*;q=0.5","user-agent":"HTTPie/2.6.0","content-length":"151","accept-encoding":"gzip, deflate, br","connection":"keep-alive"}', true),
  set_config('request.cookies', '{}', true);

WITH pgrst_source AS (
  WITH
  pgrst_payload AS (SELECT '[{"id": 4, "name": "Vier"}, {"id": 5, "name": "Funf", "arr_data": null}, {"id": 6, "name": "Sechs", "arr_data": [1, 2, 3], "field-with_sep": 6}]'::json AS json_data),
  pgrst_body AS ( SELECT CASE WHEN json_typeof(json_data) = 'array' THEN json_data ELSE json_build_array(json_data) END AS val FROM pgrst_payload)
  INSERT INTO "test"."complex_items"("arr_data", "field-with_sep", "id", "name")
  SELECT "arr_data", "field-with_sep", "id", "name"
  FROM json_to_recordset ((SELECT val FROM pgrst_body)) AS _ ("arr_data" integer[], "field-with_sep" integer, "id" bigint, "name" text)
  RETURNING "test"."complex_items".*
)
SELECT
  '' AS total_result_set,
  pg_catalog.count(_postgrest_t) AS page_total,
  array[]::text[] AS header,
  coalesce(json_agg(_postgrest_t), '[]')::character varying AS body,
  nullif(current_setting('response.headers', true), '') AS response_headers,
  nullif(current_setting('response.status', true), '') AS response_status
FROM (SELECT "complex_items".* FROM "pgrst_source" AS "complex_items") _postgrest_t;
