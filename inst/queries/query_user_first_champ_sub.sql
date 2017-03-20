WITH 
user_group AS(
  xyz_userGroupQuery_xyz
), 
run_date AS (
  xyz_runDateQuery_xyz
),
user_first_second_champ AS (
SELECT user_id
        , champion_id
        , sequence_number
FROM public.user_connected_to_champion_bridges uccb
WHERE sequence_number IN (1, 2)
),
user_first_second_champ_wide AS (
SELECT lr.user_id
        , sum((lr.sequence_number=1)::INTEGER * lr.champion_id) AS first_champ
        , sum((lr.sequence_number=2)::INTEGER * lr.champion_id) AS second_champ
FROM user_first_second_champ lr
GROUP BY lr.user_id
),
user_first_actual_champ AS (
SELECT user_id,
        case
        when first_champ=1 AND second_champ!=0
          THEN second_champ
        ELSE 
          first_champ
        end
        AS first_champ
FROM user_first_second_champ_wide
),
results AS (
SELECT * FROM user_first_actual_champ
ORDER BY user_id, first_champ
)
SELECT * FROM results
;
