with
        vital_sign_table as (
            select
                icu_stay_id,
                noradrenaline_start_time,
                measurement_stop_time,
                first_noradrenaline_rate,
                datetime_diff(time,noradrenaline_start_time, minute) as offset,
                time,
                invasive_mbp
            from
                `medicu-beta`.`snapshots_one_icu`.`vital_measurements_20250407`
            inner join
                `medicu-production`.`research_vasopressin_acidosis_2024`.`302_noradrenaline_measure_stop` using (icu_stay_id)
            where
                timestamp_sub(noradrenaline_start_time, interval 30 minute) <= time
                and time <= measurement_stop_time
                and icu_stay_id not in (
                    select icu_stay_id
                    from
                        `medicu-production`.`research_vasopressin_acidosis_2024`.`308_noradrenaline_exclusion_criteria`
                )
        ),
        final as (
            select
                icu_stay_id,
                noradrenaline_start_time,
                measurement_stop_time,
                offset,
                time,
                invasive_mbp
            from vital_sign_table
        )
select *
from final
where
    icu_stay_id in (
        select icu_stay_id
        from
            `medicu-production`.`research_vasopressin_acidosis_2024`.`312_noradrenaline_inclusion_criteria`
    )
order by icu_stay_id