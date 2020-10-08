-- CTEs to assemble smaller bites for processing
-- sdc patient list
WITH sdcPatients AS (
SELECT DISTINCT
  PersonID
FROM
  [Cerner].[Encounter].[EncounterBASE]
WHERE
	NurseUnitLocationCVDSC IN ('Express Care Central',
  'Express Care Grand',
  'Express Care Heights',
  'Heights Same Day Care',
  'HTS Same Day Care',
  'Miles City Same Day Care',
  'Same Day Care',
  'Same Day Care Lab Schedule',
  'SDC Downtown Nurse',
  'SDC West Nurse',
  'Virtual Same Day Care Miles City',
  'WE Same Day Care',
  'West End SDC') AND
	BeginEffectiveDTS BETWEEN '2017-01-01' AND '2019-12-31'
),

-- appointment information
day AS (
SELECT DISTINCT
	BeginDTS
	,PersonID
	,EncounterID
	,ScheduleAppointmentID
	,ActiveIndicatorCD
FROM
	Cerner.Schedule.Appointment
WHERE
	BeginDTS BETWEEN '2017-01-01' AND '2019-12-31'
	AND RoleMeaningDSC = 'PATIENT'
	AND ActiveIndicatorCD = 1
),

-- demographic information
person AS (
SELECT
  DISTINCT PersonID
  ,BirthDTS
  ,EthnicGroupCVDisplayDSC AS Ethnicity
	,LanguageCVDisplayDSC AS Language
	,MaritalTypeCVDisplayDSC AS Marital_Status
	,RaceCVDisplayDSC AS Race
	,SexCVDisplayDSC as Sex
	,ReligionCVDisplayDSC AS Religion
FROM
  [Cerner].[Person].[PersonBASE]
WHERE PersonID IN (SELECT DISTINCT
						PersonID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
),

-- encounter information
encounter AS (
SELECT
  DISTINCT EncounterID
  ,BuildingLocationCVDSC
  ,NurseUnitLocationCVDSC
	,AdmitTypeCVDisplayDSC
	,EncounterTypeCVDSC
	,ReasonForVisitDSC
	,BeginEffectiveDTS
FROM
  [Cerner].[Encounter].[EncounterBASE]
WHERE
	BeginEffectiveDTS BETWEEN '2017-01-01' AND '2019-12-31'
	AND EncounterID IN (SELECT DISTINCT
						EncounterID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
	AND PersonID IN (SELECT
						PersonID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
),

-- diagnosis information
d1 AS (
SELECT DISTINCT
  PersonID
  ,EncounterID
  ,DiagnosisID
  ,DiagnosisTypeCVDSC
  ,DiagnosisPrioritySEQ
  ,BeginEffectiveDTS
FROM
  Cerner.Clinical.DiagnosisBASE
WHERE
	BeginEffectiveDTS BETWEEN '2017-01-01' AND '2019-12-31'
	AND PersonID IN (SELECT DISTINCT
						PersonID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
),

-- more dx information
d2 AS (
SELECT
  DISTINCT
  PatientID
  ,EncounterID
  ,DiagnosisID
  ,DiagnosisCD
  ,DiagnosisDSC
  ,DiagnosisNormDSC
FROM
  Shared.Clinical.DiagnosisBASE
WHERE EncounterID IN (SELECT DISTINCT
						EncounterID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
),

-- respiratory tables comorbidities
respiratoryFailure AS (
SELECT
  DISTINCT PatientID
  ,CASE WHEN PatientID IS NOT NULL THEN 1 ELSE 0 END AS RespiratoryFailure
  ,CharlsonDeyoRiskScoreNBR
FROM
SAM.Readmissions.SummaryIndex
),

zipcode AS (
SELECT
  DISTINCT ParentEntityID AS PersonID
  ,CONCAT(StreetAddress01TXT,' ',StreetAddress02TXT) AS street
  ,CityNM AS city
  ,StateCD AS state
  ,ZipCD AS zip
  ,CountryCVDSC AS country
FROM
  Cerner.Reference.Address
 WHERE ParentEntityNM = 'PERSON'
 AND AddressTypeCVDSC = 'home'
 AND ParentEntityID IN (SELECT DISTINCT
						PersonID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
)

-- Now tie them all together
-- Can I add LOS and vitals and other stuff?
SELECT DISTINCT
  day.BeginDTS AS DTS
  ,day.PersonID AS PersonID
  ,day.EncounterID AS EncounterID
  ,day.ScheduleAppointmentID AS AppointmentID
  ,day.ActiveIndicatorCD
  ,DATEDIFF(YEAR, person.BirthDTS, day.BeginDTS) AS AdmitAge
  ,person.Ethnicity AS Ethnicity
  ,person.Language AS Language
  ,person.Race AS Race
  ,person.Marital_Status AS Marital_Status
  ,person.Sex AS Sex
  ,person.Religion AS Religion
  ,zipcode.street
  ,zipcode.city
  ,zipcode.state
  ,zipcode.zip
  ,zipcode.country
  ,encounter.BuildingLocationCVDSC AS Building
  ,encounter.NurseUnitLocationCVDSC AS NurseUnit
  ,encounter.AdmitTypeCVDisplayDSC AS AdmitType
  ,encounter.EncounterTypeCVDSC AS EncounterType
  ,encounter.ReasonForVisitDSC AS ReasonForVisit
  ,d1.DiagnosisPrioritySEQ AS DiagnosisPrioritySEQ
  ,d2.DiagnosisCD AS ICD
  ,d1.DiagnosisTypeCVDSC
  ,d2.DiagnosisDSC
  ,respiratoryFailure.RespiratoryFailure AS RespiratoryFailure
  ,respiratoryFailure.CharlsonDeyoRiskScoreNBR AS CharlsonDeyoScore
FROM
  sdcPatients
  LEFT JOIN day ON sdcPatients.PersonID = day.PersonID
  LEFT JOIN person ON day.PersonID = person.PersonID
  LEFT JOIN zipcode ON day.PersonID = zipcode.PersonID
  AND day.PersonID = zipcode.PersonID
  LEFT JOIN encounter ON day.EncounterID = encounter.EncounterID
  LEFT JOIN d1 ON day.EncounterID = d1.EncounterID
	AND day.PersonID = d1.PersonID
  LEFT JOIN d2 ON day.EncounterID = d2.EncounterID
	AND day.PersonID = d2.PatientID
	AND d1.DiagnosisID = d2.DiagnosisID
  LEFT JOIN respiratoryFailure ON day.PersonID = respiratoryFailure.PatientID
WHERE day.BeginDTS BETWEEN '2017-01-01' AND '2019-12-31'
ORDER BY day.PersonID, day.BeginDTS, day.EncounterID
