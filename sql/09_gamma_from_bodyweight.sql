with
        first_bodyweight_table as (
            select icu_stay_id, min(body_weight) as first_bodyweight
            from `medicu-beta`.`snapshots_one_icu`.`body_weight_measurements_20250407`
            group by icu_stay_id
        ),
        gamma_table as (
            select
                icu_stay_id,
                first_bodyweight,
                first_noradrenaline_rate,
                round(first_noradrenaline_rate / (first_bodyweight * 0.06), 3) as gamma
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start`
            inner join first_bodyweight_table using (icu_stay_id)
        )
    select icu_stay_id, first_bodyweight, gamma
    from gamma_table
    where
        icu_stay_id in (
            select icu_stay_id
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`312_noradrenaline_inclusion_criteria`
        )