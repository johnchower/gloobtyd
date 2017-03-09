WITH 
user_group AS(
  xyz_userGroupQuery_xyz
), 
run_date AS (
  xyz_runDateQuery_xyz
),
session_duration_data AS (
SELECT sdf.user_id
        , dd.calendar_week_start_date active_week_start_date
FROM public.session_duration_fact sdf
left join public.date_dim dd
ON dd.id = sdf.date_id
WHERE dd.id < (SELECT date_id FROM run_date) 
AND sdf.user_id IN (SELECT id FROM user_group)
GROUP BY sdf.user_id, dd.calendar_week_start_date
)
SELECT *
FROM session_duration_data
;
