create schema dqa; 


-- future work: resolve for multiple ingredients 

-- capturing IN -- SCDF relationships 
create table dqa.rxnorm_in_scdf 
as
select distinct a.concept_id as IN_concept_id, a.concept_name as IN_concept_name, b.concept_id as SCDF_concept_id, b.concept_name as SCDF_concept_name
from
vocabulary.concept a, vocabulary.concept b, vocabulary.concept_relationship c
where 
a.concept_class_id ='Ingredient'
and b.concept_class_id ='Clinical Drug Form'
and a.concept_id = c.concept_id_2 and b.concept_id = c.concept_id_1 
and c.relationship_id ='RxNorm has ing';

-- capturing IN -- SCD relationships 
create table dqa.rxnorm_in_scd 
as
select distinct c.IN_concept_id as IN_concept_id, c.IN_concept_name as IN_concept_name, a.concept_id as SCD_concept_id, a.concept_name as SCD_concept_name
from
vocabulary.concept a, vocabulary.concept_relationship b,  dqa.rxnorm_in_scdf  c
where 
a.concept_class_id in ('Clinical Drug', 'Quant Clinical Drug')
and a.concept_id = b.concept_id_1 and b.relationship_id ='RxNorm is a'
and b.concept_id_2 = c.SCDF_concept_id; 

-- capturing IN -- BCD relationships 
create table dqa.rxnorm_in_bcd 
as
select distinct c.IN_concept_id as IN_concept_id, c.IN_concept_name as IN_concept_name, a.concept_id as BCD_concept_id, a.concept_name as BCD_concept_name
from
vocabulary.concept a, vocabulary.concept_relationship b,  dqa.rxnorm_in_scd  c
where 
a.concept_class_id in ('Branded Drug', 'Quant Branded Drug')
and a.concept_id = b.concept_id_1 and b.relationship_id ='Tradename of'
and b.concept_id_2 = c.SCD_concept_id; 

-- capturing IN-BPCK relationships
create table dqa.rxnorm_in_bpck
as
select distinct c.IN_concept_id as IN_concept_id, c.IN_concept_name as IN_concept_name, a.concept_id as BPCK_concept_id,
 a.concept_name as BPCK_concept_name
from
vocabulary.concept a, vocabulary.concept_relationship b,  dqa.rxnorm_in_scd  c
where 
a.concept_class_id in ('Branded Pack', 'Clinical Pack')
and a.concept_id = b.concept_id_1 and b.relationship_id ='Contains'
and b.concept_id_2 = c.SCD_concept_id;


---- drug-ingredient map 
--drop table dqa.drug_in_concept_id_map; 
--- taking 5 min 
--- creating mapping table from drug concept id to in concept id
create table dqa.drug_in_concept_id_map as 
select distinct a.concept_id as drug_concept_id, c.concept_id as IN_concept_id, c.concept_name as IN_concept_name
 from vocabulary.concept a, vocabulary.concept_ancestor b , vocabulary.concept c 
where a.concept_id = b.descendant_concept_id and b.ancestor_concept_id = c.concept_id 
and c.concept_class_id ='Ingredient' and c.vocabulary_id='RxNorm' and a.vocabulary_id = 'RxNorm'
and a.domain_id='Drug'; 
