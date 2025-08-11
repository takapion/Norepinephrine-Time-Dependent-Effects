with
    static_table as (
        select icu_stay_id, female, age, in_time
        from `medicu-beta`.`snapshots_one_icu_derived`.`extended_icu_stays_20250407`
    )
select *
from
    `medicu-production`.`research_vasopressin_acidosis_2024`.`01_inclusion_criteria`
inner join static_table using (icu_stay_id)