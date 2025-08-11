with
    -- Check if there is any MBP data ≤ 65 within 5 minutes before the norepinephrine start time
    hypot as (
        select distinct icu_stay_id
        from `medicu-beta`.`snapshots_one_icu`.`vital_measurements_20250407`
        inner join
            `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start`
            using (icu_stay_id)
        where
            time >= timestamp_sub(noradrenaline_start_time, interval 5 minute)
            and time < noradrenaline_start_time
            and invasive_mbp <= 65
    ),
    -- low-pass filter requires at least 20 measurements
    -- Check if there are 10 or more MBP measurements within 30 minutes before the norepinephrine start time
    bp_measured as (
        select distinct icu_stay_id
        from `medicu-beta`.`snapshots_one_icu`.`vital_measurements_20250407`
        inner join
            `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start`
            using (icu_stay_id)
        where
            time >= timestamp_sub(noradrenaline_start_time, interval 30 minute)
            and time < noradrenaline_start_time
            and invasive_mbp is not null
        group by icu_stay_id
        having (count(invasive_mbp) >= 10)
    ),
    -- Check if there are 10 or more MBP measurements after the norepinephrine start time  
    measure_count as (
        select distinct icu_stay_id
        from `medicu-beta`.`snapshots_one_icu`.`vital_measurements_20250407`
        inner join
            `medicu-production`.`research_vasopressin_acidosis_2024`.`302_noradrenaline_measure_stop`
            using (icu_stay_id)
        where
            noradrenaline_start_time <= time
            and time <= measurement_stop_time
            and invasive_mbp is not null
        group by icu_stay_id
        having (count(invasive_mbp) >= 10)
    ),
    -- Check if the patient was admitted to the ICU at least 30 minutes before the norepinephrine start time
    min_start as (
        select *
        from `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start`
        inner join
            `medicu-beta`.`snapshots_one_icu_derived`.`extended_icu_stays_20250407`
            using (icu_stay_id)
        where timestamp_add(in_time, interval 30 minute) <= noradrenaline_start_time
    )

select distinct icu_stay_id
from hypot
inner join bp_measured using (icu_stay_id)
inner join measure_count using (icu_stay_id)
inner join min_start using (icu_stay_id)
where
    icu_stay_id not in (
        select icu_stay_id
        from
            `medicu-production`.`research_vasopressin_acidosis_2024`.`308_noradrenaline_exclusion_criteria`
    )