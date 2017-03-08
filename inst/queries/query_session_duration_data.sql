WITH 
user_group AS(
	SELECT DISTINCT ud.id 
	FROM public.user_dimensions ud
        left join public.user_platform_action_facts upaf
        ON upaf.user_id=ud.id
	WHERE ud.email IS NOT NULL
        AND upaf.platform_action='Account Created'
), 
run_date AS (
  SELECT DISTINCT id AS date_id FROM public.date_dim WHERE id=20170201
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
