#Level 2 Queries for Remote Sites

## Query A1. Inclusion crieria check
```
select count(*) from dcc_pedsnet.person
where person_id not in
 (select person_id from dcc_pedsnet.visit_occurrence where visit_start_date >='01-01-2009'
    and visit_concept_id in (9201, 9202, 9203, 42898160,44814711,44814710,2000000048)
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

## Query C1. Top-15 Inpatient Conditions
```
select condition_concept_id, concept_name, count(*)
from dcc_pedsnet.condition_occurrence, vocabulary.concept
where condition_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by condition_concept_id, concept_name
order by 3 desc
limit 15
```

## Query C2. Top-15 Outpatient Conditions
```
select condition_concept_id, concept_name, count(*)
from dcc_pedsnet.condition_occurrence, vocabulary.concept
where condition_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by condition_concept_id, concept_name
order by 3 desc
limit 15
```

## Query D1. Top-15 Inpatient Procedures
```
select procedure_concept_id, concept_name, count(*)
from dcc_pedsnet.procedure_occurrence, vocabulary.concept
where procedure_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by procedure_concept_id, concept_name
order by 3 desc
limit 15
```

## Query D2. Top-15 Outpatient Procedures
```
select procedure_concept_id, concept_name, count(*)
from dcc_pedsnet.procedure_occurrence, vocabulary.concept
where procedure_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by procedure_concept_id, concept_name
order by 3 desc
limit 15
```
## Query E1. Top-15 Inpatient Drugs
```
select drug_concept_id, concept_name, count(*)
from dcc_pedsnet.drug_exposure, vocabulary.concept
where drug_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by drug_concept_id, concept_name
order by 3 desc
limit 15
```

## Query E2. Top-15 Outpatient Drugs
```
select drug_concept_id, concept_name, count(*)
from dcc_pedsnet.drug_exposure, vocabulary.concept
where drug_concept_id = concept_id
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by drug_concept_id, concept_name
order by 3 desc
limit 15
```

## Query F1. Top-15 Inpatient Labs

```
select measurement_concept_id, concept_name, count(*)
from dcc_pedsnet.measurement, vocabulary.concept
where measurement_concept_id = concept_id
and measurement_type_concept_id = 44818702
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9201)
group by measurement_concept_id, concept_name
order by 3 desc
limit 15
```
## Query F2. Top-15 Outpatient Labs
```
select measurement_concept_id, concept_name, count(*)
from dcc_pedsnet.measurement, vocabulary.concept
where measurement_concept_id = concept_id
and measurement_type_concept_id = 44818702
and visit_occurrence_id in (Select visit_occurrence_id from dcc_pedsnet.visit_occurrence where visit_concept_id = 9202)
group by measurement_concept_id, concept_name
order by 3 desc
limit 15
```

## Query Set G1. Facts before birth
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
 and a.person_id=b.person_id
```

## Query Set G2. Facts after death

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
## Query Set H
### % Visits with no associated conditions
```
with sub as
(select
sum(case when co.visit_occurrence_id is null then 1 else 0 end) as no_association,
count(distinct vo.visit_occurrence_id) as total
from pedsnet.visit_occurrence vo
left outer join condition_occurrence co on vo.visit_occurrence_id = co.visit_occurrence_id)
select
sub.no_association/sub.total
from sub
```


### % Visits with no associated procedures
```
with sub as
(select
sum(case when po.visit_occurrence_id is null then 1 else 0 end) as no_association,
count(distinct vo.visit_occurrence_id) as total
from pedsnet.visit_occurrence vo
left outer join procedure_occurrence po on vo.visit_occurrence_id = po.visit_occurrence_id)
select
sub.no_association/sub.total
from sub
```

### % Visits with no associated measurements
```
with sub as
(select
sum(case when m.visit_occurrence_id is null then 1 else 0 end) as no_association,
count(distinct vo.visit_occurrence_id) as total
from pedsnet.visit_occurrence vo
left outer join measurement m on vo.visit_occurrence_id = m.visit_occurrence_id)
select
sub.no_association/sub.total
from sub
```

### % Visits with no associated drugs
```
with sub as
(select
sum(case when de.visit_occurrence_id is null then 1 else 0 end) as no_association,
count(distinct vo.visit_occurrence_id) as total
from pedsnet.visit_occurrence vo
left outer join drug_exposure de on vo.visit_occurrence_id = de.visit_occurrence_id)
select
sub.no_association/sub.total
from sub
```
