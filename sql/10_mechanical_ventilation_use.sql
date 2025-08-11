with
        -- Indicates whether mechanical ventilation (MV) is used.  
        -- on_vent = currently in use, past_vent = used in the past.  
        -- 0 = not used, 1 = used.
        mv_use_table as (
            select
                icu_stay_id,
                case
                    when
                        start_time <= noradrenaline_start_time
                        and noradrenaline_start_time <= end_time
                    then 1
                    else 0
                end as on_vent,
                case
                    when noradrenaline_start_time > end_time
                    then 1
                    when
                        start_time <= noradrenaline_start_time
                        and noradrenaline_start_time <= end_time
                    then 1
                    else 0
                end as past_vent
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start`
            left join
                `medicu-beta`.`snapshots_one_icu`.`mechanical_ventilations_20250407` using (icu_stay_id)
        ),
        mv_use_max as (
            select icu_stay_id, max(on_vent) as on_vent, max(past_vent) as past_vent,
            from mv_use_table
            group by icu_stay_id
        )
    select *
    from mv_use_max
    where
        icu_stay_id in (
            select icu_stay_id
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`312_noradrenaline_inclusion_criteria`
        )