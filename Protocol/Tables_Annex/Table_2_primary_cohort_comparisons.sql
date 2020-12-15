with
cte_prior_indication as (
  select 10000 as prior_indication_id, 'prior AFib' as prior_indication_name union all
  select 20000 as prior_indication_id, 'prior VTE' as prior_indication_name union all
  select 30000 as prior_indication_id, 'prior CAD or PAD' as prior_indication_name union all
  select 40000 as prior_indication_id, 'prior ortho. surgery' as prior_indication_name
),
cte_exposure as (
  select 1000 as exposure_id, 'Rivaroxaban' as exposure_name union all
  select 2000 as exposure_id, 'Apixaban' as exposure_name union all
  select 3000 as exposure_id, 'Dabigatran' as exposure_name union all
  select 4000 as exposure_id, 'Warfarin' exposure_name
),
cte_subgroup as (
  select 100 as subgroup_id, '18-49 years' as subgroup_name union all
  select 200 as subgroup_id, '>=50 years' as subgroup_name union all
  select 300 as subgroup_id, 'prior gynecological disorder' as subgroup_name union all
  select 400 as subgroup_id, 'no prior gynecological disorder' as subgroup_name union all
  select 500 as subgroup_id, 'prior anemia' as subgroup_name union all
  select 600 as subgroup_id, 'no prior anemia' as subgroup_name union all
  select 700 as subgroup_id, 'concurrent antiplatelet therapy' as subgroup_name union all
  select 800 as subgroup_id, 'no concurrent antiplatelet therapy' as subgroup_name
),
cte_outcome as (
  select 80000 as outcome_id, 'Severe uternine bleed with transfusion management' as outcome_name union all
  select 90000 as outcome_id, 'Severe uternine bleed with surgical management' as outcome_name
),
cte_analysis as (
  select '1:100 variable ratio matching with conditional outcome model' as analysis union all
  select '1:1 ratio matching with unconditional outcome model' as analysis
),
cte_tar as (
   select 'on-treatment +30 days' as tar union all
   select 'intent-to-treat' as tar
),
cte_cohort_list as (
  select 
      i.prior_indication_id + e.exposure_id as cohort_definition_id,
      e.exposure_name + ' new users with ' + i.prior_indication_name as cohort_name,
      i.prior_indication_id
  from cte_prior_indication i
  cross join cte_exposure e
),
cte_subgroup_cohort_list as (
  select
    i.prior_indication_id + e.exposure_id + s.subgroup_id as cohort_definition_id,
    e.exposure_name + ' new users with ' + i.prior_indication_name + ', ' + s.subgroup_name as cohort_name,
    i.prior_indication_id,
    e.exposure_id,
    s.subgroup_id
  from cte_prior_indication i 
  cross join cte_exposure e
  cross join cte_subgroup s
)
--primary cohort comparison list
select
  t.cohort_definition_id as t_id,
  t.cohort_name as t_cohort_name,  
  c.cohort_definition_id as c_id,
  c.cohort_name as c_cohort_name
from cte_cohort_list t
cross join cte_cohort_list c
where t.prior_indication_id = c.prior_indication_id
and t.cohort_definition_id <> c.cohort_definition_id
and t.cohort_definition_id < c.cohort_definition_id
