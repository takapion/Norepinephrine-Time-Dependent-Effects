with  
    first_sofa_24hours as (
        select
            icu_stay_id,
            max(sofa_24hours) as first_sofa
        from `medicu-beta`.`snapshots_one_icu_derived`.`sofa_hourly_20250407`
        where
            icu_stay_id in (
                select icu_stay_id
                from `medicu-production`.`research_vasopressin_acidosis_2024`.`01_inclusion_criteria`
            )
            and 23 >= time_window_index
            and time_window_index >= 0
        group by icu_stay_id
    )
select *
from first_sofa_24hours