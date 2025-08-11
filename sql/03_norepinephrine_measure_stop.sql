with
        -- Extract initial continuous infusion start/end times and first infusion rate for norepinephrine
        main_drug_period as (
            select
                icu_stay_id,
                min(connect_start_time) as main_start_time,
                min_by(connect_end_time, connect_start_time) as main_end_time,
                min_by(
                    first_noradrenaline_rate, connect_start_time
                ) as main_first_rate
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`023_noradrenaline_time_continuous`
            group by icu_stay_id
        ),
        -- Candidate observation stop times due to injections of vasopressors; omitted if there is no injection (since min is taken)
        main_drug_injection as (
            select icu_stay_id, min(start_time) as main_inj_start
            from main_drug_period
            inner join
                (
                    select icu_stay_id, start_time
                    from
                        `medicu-beta`.`snapshots_one_icu_derived`.`infusion_injection_active_ingredient_rate_20250407`
                    where
                        active_ingredient_name in (
                            'noradrenaline',
                            'vasopressin',
                            'adrenaline'
                        )
                        and source = 'injections'
                ) using (icu_stay_id)
            where main_start_time <= start_time and start_time <= main_end_time
            group by icu_stay_id
        ),

        -- norepinephrine administration period (taking injection into account)
        main_drug_true_period as (
            select
                icu_stay_id,
                main_first_rate,
                main_start_time,
                coalesce(main_inj_start, main_end_time) as measurement_stop_by_inj
            from main_drug_period
            left join main_drug_injection using (icu_stay_id)
        ),
        -- Extract norepinephrine infusion rate transitions; currently all icu_stay_id have start_time within main_start_time and measurement_stop_by_inj
        main_drug_rate_log as (
            select
                icu_stay_id,
                start_time,
                end_time,
                noradrenaline_rate as main_rate,
                main_start_time,
                measurement_stop_by_inj
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`013_noradrenaline_cumulative_rate`
            inner join main_drug_true_period using (icu_stay_id)
            where
                main_start_time <= start_time
                and start_time <= measurement_stop_by_inj
                and source = 'infusions'
        ),

        -- Retrieve first and next infusion rates; calculate change percentages from the first and from the previous record
        main_drug_rate_change as (
            select
                icu_stay_id,
                main_start_time,
                measurement_stop_by_inj,
                first_value(main_rate) over (
                    partition by icu_stay_id order by start_time, end_time
                ) as first_main_rate,
                start_time,
                end_time,
                main_rate,
                lead(main_rate) over (
                    partition by icu_stay_id order by start_time, end_time
                ) as next_main_rate
            from main_drug_rate_log
        ),
        main_rate_change_eval as (
            select
                *,
                abs(first_main_rate - next_main_rate)
                * 100
                / first_main_rate as first_change_pct,
                abs(main_rate - next_main_rate) * 100 / main_rate as next_change_pct
            from main_drug_rate_change
        ),
        -- Candidate observation stop times due to ≥20% rate change or no change
        main_stop_candidates as (
            select
                icu_stay_id,
                min_by(main_start_time, end_time) as main_start_time,
                min_by(measurement_stop_by_inj, end_time) as measurement_stop_by_inj,
                min_by(first_main_rate, end_time) as first_main_rate,
                min(end_time) as measurement_stop_by_inf
            from main_rate_change_eval
            where
                next_change_pct is null
                or next_change_pct >= 20
                or first_change_pct >= 20
            group by icu_stay_id
        ),
        -- Since main_drug_rate_log filters by infusions start_time, it's possible that infusions end_time > measurement_stop_by_inj
        compere_inj_inf_time as (
            select
                icu_stay_id,
                main_start_time,
                first_main_rate,
                case
                    when measurement_stop_by_inf > measurement_stop_by_inj
                    then measurement_stop_by_inj
                    else measurement_stop_by_inf
                end as measurement_stop_time
            from main_stop_candidates
        ),
        main_measure_stop as (
            select
                icu_stay_id,
                main_start_time,
                first_main_rate,
                case
                    when
                        measurement_stop_time
                        < timestamp_add(main_start_time, interval 2 hour)
                    then measurement_stop_time
                    else timestamp_add(main_start_time, interval 2 hour)
                end as measurement_stop_time
            from compere_inj_inf_time
            where
                icu_stay_id in (
                    select icu_stay_id
                    from
                        `medicu-production`.`research_vasopressin_acidosis_2024`.`01_inclusion_criteria`
                )
        ),
        -- Consider other vasopressors as well
        subdrug_union_all as (
            select
                icu_stay_id,
                start_time,
                end_time,
                vasopressin_rate as sub_rate,
                "vasopressin" as subdrug_name
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`011_vasopressin_cumulative_rate`
            where source = 'infusions'
            union all
            select
                icu_stay_id,
                start_time,
                end_time,
                adrenaline_rate as sub_rate,
                "adrenaline" as subdrug_name
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`012_adrenaline_cumulative_rate`
            where source = 'infusions'
        ),
        -- Extract other vasopressors administrations within the norepinephrine period
        subdrug_within_main_period as (
            select
                icu_stay_id,
                main_start_time,
                measurement_stop_time,
                first_main_rate,
                subdrug_name,
                start_time,
                end_time,
                sub_rate
            from subdrug_union_all
            inner join main_measure_stop using (icu_stay_id)
            where
                (main_start_time <= start_time and start_time < measurement_stop_time)
                or (main_start_time < end_time and end_time <= measurement_stop_time)
                or (main_start_time >= start_time and end_time >= measurement_stop_time)
        ),
        -- Calculate rate change percentages for other vasopressors
        subdrug_rate_change as (
            select
                icu_stay_id,
                main_start_time,
                measurement_stop_time,
                first_main_rate,
                subdrug_name,
                first_value(sub_rate) over (
                    partition by icu_stay_id, subdrug_name order by start_time, end_time
                ) as first_sub_rate,
                start_time,
                end_time,
                sub_rate,
                lag(start_time) over (
                    partition by icu_stay_id, subdrug_name order by start_time, end_time
                ) as pre_start_time,
                lead(start_time) over (
                    partition by icu_stay_id, subdrug_name order by start_time, end_time
                ) as next_start_time,
                lead(sub_rate) over (
                    partition by icu_stay_id, subdrug_name order by start_time, end_time
                ) as next_rate
            from subdrug_within_main_period
        ),

        subdrug_rate_eval as (
            select
                *,
                abs(first_sub_rate - next_rate)
                * 100
                / first_sub_rate as first_change_pct,
                abs(sub_rate - next_rate) * 100 / sub_rate as next_change_pct
            from subdrug_rate_change
        ),

        subdrug_change_flagged as (
            select *
            from subdrug_rate_eval
            where
                -- Another vasopressor started after norepinephrine was initiated
                (main_start_time <= start_time and pre_start_time is null)
                -- Or a rate change occurred
                or (end_time != next_start_time)
                or (
                    next_change_pct is null
                    or next_change_pct >= 20
                    or first_change_pct >= 20
                )
        ),
        --- Get the earliest start time for each other vasopressor type
        subdrug_stop_candidates as (
            select
                icu_stay_id,
                subdrug_name,
                min(start_time) as sub_start_time,
                min_by(end_time, start_time) as sub_end_time,
                min_by(pre_start_time, start_time) as pre_start_time,
                min_by(next_change_pct, start_time) as next_change_pct,
                min_by(first_change_pct, start_time) as first_change_pct
            from subdrug_change_flagged
            group by icu_stay_id, subdrug_name
        ),
        final_stop_merge as (
            select
                icu_stay_id,
                main_start_time,
                sub_start_time,
                sub_end_time,
                case
                    -- Another vasopressor started after norepinephrine was initiated
                    when main_start_time <= sub_start_time and pre_start_time is null
                    then sub_start_time
                    -- Rate change occurred
                    when sub_end_time <= measurement_stop_time
                    then sub_end_time
                    else measurement_stop_time
                end as measurement_stop_time,
                first_main_rate
            from main_measure_stop
            left join subdrug_stop_candidates using (icu_stay_id)
        ),

        -- Finally, compare earliest stop time
        min_stop_time as (
            select
                icu_stay_id,
                any_value(first_main_rate) as first_noradrenaline_rate,
                any_value(main_start_time) as noradrenaline_start_time,
                min(measurement_stop_time) as measurement_stop_time
            from final_stop_merge
            group by icu_stay_id
        )

    select *
    from min_stop_time
    order by icu_stay_id