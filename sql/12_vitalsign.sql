with
        vital_sign_table as (
            select
                icu_stay_id, noradrenaline_start_time, hr, time
            from
                `medicu-beta`.`snapshots_one_icu`.`vital_measurements_20250407`
            inner join
                `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start` using (icu_stay_id)

        ),
        -- Calculate the median HR during the 10 minutes before vasopressin administration
        hr_rank_table as (
            select
                icu_stay_id,
                row_number() over (partition by icu_stay_id order by hr asc) as hr_rank,
                count(*) over (partition by icu_stay_id) as total_count,
                time,
                hr
            from vital_sign_table
            where
                hr is not null
                and time >= timestamp_sub(noradrenaline_start_time, interval 9 minute)
                and time <= noradrenaline_start_time
        ),
        median_values_hr_pre as (
            select
                icu_stay_id,
                case
                    when mod(total_count, 2) = 1
                    then max(case when hr_rank = ceil(total_count / 2) then hr end)
                    else
                        (
                            max(case when hr_rank = total_count / 2 then hr end)
                            + max(case when hr_rank = total_count / 2 + 1 then hr end)
                        )
                        / 2.0
                end as hr_pre
            from hr_rank_table
            group by icu_stay_id, total_count
        )
    select icu_stay_id, hr_pre
    from
        `medicu-production`.`research_vasopressin_acidosis_2024`.`301_noradrenaline_min_start`
    left join median_values_hr_pre using (icu_stay_id)