with
    inclusion_criteria as (
        select icu_stay_id,
        from `medicu-beta.snapshots_one_icu.icu_stays_20250407`
        inner join
            `medicu-beta.snapshots_one_icu_derived.infusion_injection_active_ingredient_rate_20250407`
            using (icu_stay_id)
        inner join
            `medicu-beta.snapshots_one_icu.patients_20250407` using (subject_id)
        where
            active_ingredient_name = "noradrenaline"
            and female is not null
    ),
    weight_include as (
        select icu_stay_id
        from
            `medicu-beta.snapshots_one_icu_derived.extended_icu_stays_20250407`
        where body_weight is not null
    ),
    age_include as (
        select icu_stay_id
        from
            `medicu-beta.snapshots_one_icu_derived.extended_icu_stays_20250407`
        where age >= 15
    ),
    diagnosis_include as (
        select icu_stay_id
        from `medicu-beta.snapshots_one_icu_derived.aggregated_icu_diagnoses_20250407`
        where diagnosis_count > 0
    )
select distinct icu_stay_id
from inclusion_criteria
inner join weight_include using (icu_stay_id)
inner join age_include using (icu_stay_id)
inner join diagnosis_include using (icu_stay_id)