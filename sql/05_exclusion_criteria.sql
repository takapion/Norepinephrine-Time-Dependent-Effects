with
        meylon_exclude as (
            select icu_stay_id
            from
                `medicu-production`.`research_vasopressin_acidosis_2024`.`307_noradrenaline_meylon_use`
        ),
        ecmo_exclude as (
            select distinct dms.icu_stay_id
            from
                `medicu-beta`.`snapshots_one_icu`.`ecmo_20250407`
                ecm
            inner join
                `medicu-production`.`research_vasopressin_acidosis_2024`.`302_noradrenaline_measure_stop` dms
                on dms.icu_stay_id = ecm.icu_stay_id
                and (
                    (
                        noradrenaline_start_time <= ecm.start_time
                        and ecm.start_time <= measurement_stop_time
                    )
                    or (
                        noradrenaline_start_time <= ecm.end_time
                        and ecm.end_time <= measurement_stop_time
                    )
                    or (
                        noradrenaline_start_time >= ecm.start_time
                        and ecm.end_time >= measurement_stop_time
                    )
                )
        )
    select icu_stay_id
    from meylon_exclude
    union all
    select icu_stay_id
    from ecmo_exclude