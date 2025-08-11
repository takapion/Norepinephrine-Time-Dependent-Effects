with
        mey_rate as (
            select ms.*, mey.* except (icu_stay_id)
            from
                `medicu-beta`.`snapshots_one_icu_derived`.`infusion_injection_active_ingredient_rate_20250407`
                mey
            inner join
                `medicu-production`.`research_vasopressin_acidosis_2024`.`302_noradrenaline_measure_stop` ms
                on ms.icu_stay_id = mey.icu_stay_id
                and (
                    (
                        noradrenaline_start_time <= mey.start_time
                        and mey.start_time <= measurement_stop_time
                    )
                    or (
                        noradrenaline_start_time <= mey.end_time
                        and mey.end_time <= measurement_stop_time
                    )
                    or (
                        noradrenaline_start_time >= mey.start_time
                        and mey.end_time >= measurement_stop_time
                    )
                )
            where injection_product_name in ('meylon_7_percent', 'meylon_84_percent')
        ),
        -- Calculate the amount of Meylon administered, separating infusions and injections. 
        -- Extract only cases with a usage of 100 mL or more, and set meylon_use to 1.
        meylon_inf_amount as (
            select
                *,
                timestamp_diff(end_time, start_time, minute)
                / 60
                * ml_per_hour as amount
            from
                `medicu-beta`.`snapshots_one_icu`.`infusions_20250407`
            where
                infusion_id
                in (select source_id from mey_rate where source = 'infusions')
        ),
        meylon_inf_table as (
            select
                icu_stay_id,
                start_time as meylon_start,
                end_time as meylon_end,
                amount,
                1 as meylon_use
            from meylon_inf_amount
            where amount >= 100
        ),
        meylon_inj_table as (
            select *, cast(null as timestamp) as end_time, 1 as meylon_use
            from
                `medicu-beta`.`snapshots_one_icu`.`injections_20250407`
            where
                injection_id
                in (select source_id from mey_rate where source = "injections")
                and amount >= 100
        ),
        meylon_union_table as (
            select icu_stay_id, amount, meylon_use
            from meylon_inj_table
            union all
            select icu_stay_id, amount, meylon_use
            from meylon_inf_table
        )
    select distinct icu_stay_id
    from meylon_union_table