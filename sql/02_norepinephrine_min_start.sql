with
    norad as (
        select
            icu_stay_id,
            min(start_time) as noradrenaline_start_time,
            min_by(unit_per_hour, start_time) as first_noradrenaline_rate,
            "noradrenaline" as active_ingredient_name
        from `medicu-beta.snapshots_one_icu_derived.infusion_injection_active_ingredient_rate_20250407`
        where active_ingredient_name = "noradrenaline" and source = "infusions"
        group by icu_stay_id
    )
select *
from norad
where
    icu_stay_id in (
        select icu_stay_id
        from `medicu-production.research_vasopressin_acidosis_2024.01_inclusion_criteria`
    )
order by icu_stay_id