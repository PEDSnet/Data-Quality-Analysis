#Level 2 Queries for Remote Sites

## Query A1. Inclusion crieria check
```
select count(*) from dcc_pedsnet.person
where person_id not in
 (select person_id from dcc_pedsnet.visit_occurrence where visit_start_date >='01-01-2009'
    and visit_concept_id in (9201, 9202, 9203, 42898160,44814710,2000000048, 2000000088)
  intersect
  select person_id from dcc_pedsnet.condition_occurrence where condition_start_date>='01-01-2009'
  )
```
## Query A2. Observation Period Check
```
select person_id from dcc_pedsnet.person
where person_id not in
 (select person_id from dcc_pedsnet.observation_period
  )
  UNION
  select person_id from dcc_pedsnet.observation_period
where person_id not in
 (select person_id from dcc_pedsnet.person
  )
```

## Query B1. Top-15 Inpatient Observations
```
select observation_concept_id, concept_name, count(*)
from dcc_pedsnet.observation, vocabulary.concept
where observation_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by observation_concept_id, concept_name
order by 3 desc
limit 15
```

## Query B2. Top-15 Outpatient Observations
```
select observation_concept_id, concept_name, count(*)
from dcc_pedsnet.observation, vocabulary.concept
where observation_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by observation_concept_id, concept_name
order by 3 desc
limit 15
```

## Query C1. Top-100 Inpatient Conditions
```
select condition_concept_id, concept_name, count(*)
from dcc_pedsnet.condition_occurrence, vocabulary.concept
where condition_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by condition_concept_id, concept_name
order by 3 desc
limit 100
```

## Query C2. Top-100 Outpatient Conditions
```
select condition_concept_id, concept_name, count(*)
from dcc_pedsnet.condition_occurrence, vocabulary.concept
where condition_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by condition_concept_id, concept_name
order by 3 desc
limit 100
```

## Query C3. Top-100 No Matching Conditions
```
select condition_source_value, count(*)
from dcc_pedsnet.condition_occurrence
where condition_concept_id=0
group by condition_source_value
order by 2 desc
limit 100
```

## Query D1. Top-100 Inpatient Procedures
```
select procedure_concept_id, concept_name, count(*)
from dcc_pedsnet.procedure_occurrence, vocabulary.concept
where procedure_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by procedure_concept_id, concept_name
order by 3 desc
limit 100
```

## Query D2. Top-100 Outpatient Procedures
```
select procedure_concept_id, concept_name, count(*)
from dcc_pedsnet.procedure_occurrence, vocabulary.concept
where procedure_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by procedure_concept_id, concept_name
order by 3 desc
limit 100
```
## Query D3. Top-100 No Matching Procedures
```
select procedure_source_value, count(*)
from dcc_pedsnet.procedure_occurrence
where procedure_concept_id=0
group by procedure_source_value
order by 2 desc
limit 100
```
## Query E1. Top-100 Inpatient Drugs
```
select drug_concept_id, concept_name, count(*)
from dcc_pedsnet.drug_exposure, vocabulary.concept
where drug_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by drug_concept_id, concept_name
order by 3 desc
limit 100
```

## Query E2. Top-100 Outpatient Drugs
```
select drug_concept_id, concept_name, count(*)
from dcc_pedsnet.drug_exposure, vocabulary.concept
where drug_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by drug_concept_id, concept_name
order by 3 desc
limit 100
```
## Query E3. Top-100 No Matching Drugs
```
select drug_source_value, count(*)
from dcc_pedsnet.drug_exposure
where drug_concept_id=0
group by drug_source_value
order by 2 desc
limit 100
```

## Query F1. Top-100 Labs

```
select measurement_concept_id, concept_name, count(*)
from dcc_pedsnet.measurement, vocabulary.concept
where measurement_concept_id = concept_id
and measurement_type_concept_id = 44818702
group by measurement_concept_id, concept_name
order by 3 desc
limit 100
```
## Query F2. Top-100 Vitals
```
select measurement_concept_id, concept_name, count(*)
from dcc_pedsnet.measurement, vocabulary.concept
where measurement_concept_id = concept_id
and measurement_type_concept_id in ( 2000000033, 2000000032)
group by measurement_concept_id, concept_name
order by 3 desc
limit 100
```

## Query F3. Top-100 No Matching Measurements
```
select measurement_source_value, count(*)
from dcc_pedsnet.measurement
where measurement_concept_id=0
group by measurement_source_value
order by 2 desc
limit 100
```
## Query G1. Top-100 Organisms
```
select organism_concept_id, concept_name, count(*)
from dcc_pedsnet.measurement_organism, vocabulary.concept
where organism_concept_id = concept_id
group by organism_concept_id, concept_name
order by 3 desc
limit 100
```

## Query G2. Top-100 No Matching Organisms
```
select organism_source_value, count(*)
from dcc_pedsnet.measurement_organism
where organism_concept_id=0
group by organism_source_value
order by 2 desc
limit 100
```

## Query Set H1. Facts before birth
### Visits before birth
```
select count(*) from dcc_pedsnet.visit_occurrence a, dcc_pedsnet.person b
where visit_start_date < time_of_birth
 and a.person_id=b.person_id
```
### Pre-natal Visits
```
select count(*) from dcc_pedsnet.visit_occurrence a, dcc_pedsnet.person b
where floor(months_between(time_of_birth  - visit_start_date)) < 9
and floor(months_between(time_of_birth,visit_start_date)) > 0
 and a.person_id=b.person_id
```

### Conditions before birth
```
select count(*) from dcc_pedsnet.condition_occurrence a, dcc_pedsnet.person b
where condition_start_date < time_of_birth
 and a.person_id=b.person_id
```
### Pre-natal Condition
```
select count(*) from dcc_pedsnet.condition_occurrence a, dcc_pedsnet.person b
where floor(months_between(time_of_birth - condition_start_date )) < 9
and floor(months_between(time_of_birth,condition_start_date )) > 0
 and a.person_id=b.person_id
```
### Procedures before birth
```
select count(*) from dcc_pedsnet.procedure_occurrence a, dcc_pedsnet.person b
where procedure_date < time_of_birth
 and a.person_id=b.person_id
```
### Prenatal Procedures
```
select count(*) from dcc_pedsnet.procedure_occurrence a, dcc_pedsnet.person b
where floor(months_between(time_of_birth - procedure_date)) < 9
and floor(months_between(time_of_birth,procedure_date)) >0
 and a.person_id=b.person_id
```
### Measurement before birth
```
select count(*) from dcc_pedsnet.measurement a, dcc_pedsnet.person b
where measurement_date < time_of_birth
 and a.person_id=b.person_id
```
### Prenatal measurements
```
select count(*) from dcc_pedsnet.measurement a, dcc_pedsnet.person b
where floor(months_between(time_of_birth - measurement_date)) < 9
and floor(months_between(time_of_birth,measurement_date)) >0
 and a.person_id=b.person_id
```
### Drug Exposure before birth
```
select count(*) from dcc_pedsnet.drug_exposure a, dcc_pedsnet.person b
where drug_exposure_start_date < time_of_birth
 and a.person_id=b.person_id
```
### Prenatal drug exposures
```
select count(*) from dcc_pedsnet.drug_exposure a, dcc_pedsnet.person b
where floor(months_between(time_of_birth - drug_exposure_start_date)) < 9
and floor(months_between(time_of_birth,drug_exposure_start_date)) >0
 and a.person_id=b.person_id
```

## Query Set H2. Facts after death

### Visits after death
```
select count(*) from dcc_pedsnet.visit_occurrence a,  dcc_pedsnet.death b
where visit_start_date > death_date
 and a.person_id=b.person_id
```
### Conditions after death
```
select
count(co.condition_occurrence_id)
from pedsnet.death d
join pedsnet.condition_occurrence co on d.person_id = co.person_id
where co.condition_start_date > d.death_date
```
### Procedures after death
```
select
count(po.procedure_occurrence_id)
from pedsnet.death d
join pedsnet.procedure_occurrence po on d.person_id = po.person_id
where po.procedure_date > d.death_date
```
### Measurement after death
```
select count(*) from dcc_pedsnet.measurement a,  dcc_pedsnet.death b
where measurement_date > death_date
 and a.person_id=b.person_id
```
### Drug Exposure after death
```
select count(*) from dcc_pedsnet.drug_exposure a,  dcc_pedsnet.death b
where drug_exposure_start_date > death_date
 and a.person_id=b.person_id
```
## Query I: Visits with no associated facts
```
with total_visits as 
(select count(*) as total_count 
from dcc_pedsnet.visit_occurrence 
), 
 f2f_visits as ( select visit_occurrence_id from dcc_pedsnet.visit_occurrence  where visit_concept_id in (9201, 9202, 9203)), 
 no_fact_visit as 
 ( select v.visit_occurrence_id from 
f2f_visits v left outer join dcc_pedsnet.condition_occurrence c on v.visit_occurrence_id = c.visit_occurrence_id 
left outer join dcc_pedsnet.procedure_occurrence p on v.visit_occurrence_id = p.visit_occurrence_id
left outer join dcc_pedsnet.measurement m on v.visit_occurrence_id = m.visit_occurrence_id
left outer join dcc_pedsnet.drug_exposure d on v.visit_occurrence_id = d.visit_occurrence_id
where c.visit_occurrence_id is null and p.visit_occurrence_id is null
and  m.visit_occurrence_id is null and  d.visit_occurrence_id is null)
,no_fact_visits as (
select count(*) as nofact_count from no_fact_visit )
select  round(nofact_count*100/total_count,2) from total_visits, no_fact_visits 
```
