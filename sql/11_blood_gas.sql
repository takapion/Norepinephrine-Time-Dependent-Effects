with
        bg_table as (
            select
                icu_stay_id,
                field_name,
                value,
                time as measurement_time,
                noradrenaline_start_time
            from
                `medicu-beta`.`snapshots_one_icu`.`blood_gas_20250407`
            inner join
                `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start` using (icu_stay_id)
            where
                field_name in ('ph', 'bicarbonate', 'base_excess', 'pco2', 'lactate')
        ),
        pivoted as (
            select
                icu_stay_id,
                noradrenaline_start_time,
                measurement_time,
                ph,
                bicarbonate,
                base_excess,
                pco2,
                lactate
            from
                bg_table pivot (
                    max(value) for field_name
                    in ('ph', 'bicarbonate', 'base_excess', 'pco2', 'lactate')
                )
        ),
        ph_pre_tab as (
            select
                icu_stay_id,
                max(measurement_time) as ph_measurement_time_pre,
                max_by(ph, measurement_time) as ph_pre
            from pivoted
            where  noradrenaline_start_time > measurement_time
            and ph is not null
            group by icu_stay_id
        ),
        ph_after_tab as (
            select
                icu_stay_id,
                min(measurement_time) as ph_measurement_time_after,
                min_by(ph, measurement_time) as ph_after
            from pivoted
            where measurement_time >= noradrenaline_start_time
            and ph is not null
            group by icu_stay_id
        ),
        bicarbonate_pre_tab as (
            select
                icu_stay_id,
                max(measurement_time) as bicarbonate_measurement_time_pre,
                max_by(bicarbonate, measurement_time) as bicarbonate_pre
            from pivoted
            where  noradrenaline_start_time > measurement_time
            and bicarbonate is not null
            group by icu_stay_id
        ),
        bicarbonate_after_tab as (
            select
                icu_stay_id,
                min(measurement_time) as bicarbonate_measurement_time_after,
                min_by(bicarbonate, measurement_time) as bicarbonate_after
            from pivoted
            where measurement_time >= noradrenaline_start_time
            and bicarbonate is not null
            group by icu_stay_id
        ),
        base_excess_pre_tab as (
            select
                icu_stay_id,
                max(measurement_time) as base_excess_measurement_time_pre,
                max_by(base_excess, measurement_time) as base_excess_pre
            from pivoted
            where  noradrenaline_start_time > measurement_time
            and base_excess is not null
            group by icu_stay_id
        ),
        base_excess_after_tab as (
            select
                icu_stay_id,
                min(measurement_time) as base_excess_measurement_time_after,
                min_by(base_excess, measurement_time) as base_excess_after
            from pivoted
            where measurement_time >= noradrenaline_start_time
            and base_excess is not null
            group by icu_stay_id
        ),
        pco2_pre_tab as (
            select
                icu_stay_id,
                max(measurement_time) as pco2_measurement_time_pre,
                max_by(pco2, measurement_time) as pco2_pre
            from pivoted
            where  noradrenaline_start_time > measurement_time
            and pco2 is not null
            group by icu_stay_id
        ),
        pco2_after_tab as (
            select
                icu_stay_id,
                min(measurement_time) as pco2_measurement_time_after,
                min_by(pco2, measurement_time) as pco2_after
            from pivoted
            where measurement_time >= noradrenaline_start_time
            and pco2 is not null
            group by icu_stay_id
        ),
        lactate_pre_tab as (
            select
                icu_stay_id,
                max(measurement_time) as lactate_measurement_time_pre,
                max_by(lactate, measurement_time) as lactate_pre
            from pivoted
            where  noradrenaline_start_time > measurement_time
            and lactate is not null
            group by icu_stay_id
        ),
        lactate_after_tab as (
            select
                icu_stay_id,
                min(measurement_time) as lactate_measurement_time_after,
                min_by(lactate, measurement_time) as lactate_after
            from pivoted
            where measurement_time >= noradrenaline_start_time
            and lactate is not null
            group by icu_stay_id
        ),
        bg_coalesce_tab as (
            select
                icu_stay_id,
                coalesce(ph_pre, ph_after) as ph,
                coalesce(bicarbonate_pre, bicarbonate_after) as bicarbonate,
                coalesce(base_excess_pre, base_excess_after) as base_excess,
                coalesce(pco2_pre, pco2_after) as pco2,
                coalesce(lactate_pre, lactate_after) as lactate
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`312_noradrenaline_inclusion_criteria`
            left join
                ph_pre_tab using (icu_stay_id)
            left join
                ph_after_tab using (icu_stay_id)
            left join
                bicarbonate_pre_tab using (icu_stay_id)
            left join
                bicarbonate_after_tab using (icu_stay_id)
            left join
                base_excess_pre_tab using (icu_stay_id)
            left join
                base_excess_after_tab using (icu_stay_id)
            left join
                pco2_pre_tab using (icu_stay_id)
            left join
                pco2_after_tab using (icu_stay_id)
            left join
                lactate_pre_tab using (icu_stay_id)
            left join   
                lactate_after_tab using (icu_stay_id)
        )
    select *
    from bg_coalesce_tab