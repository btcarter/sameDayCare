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
  ,DiagnosisFreeTXT
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
  ,DiagnosisTypeDSC
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

-- have they ever had an encounter with a heart failure diagnosis
heartFailure AS (
SELECT
  DISTINCT PatientID
  ,EncounterID
  ,CASE WHEN DiagnosisCD IS NOT NULL THEN 1 ELSE 0 END AS HeartFailure
FROM
  SAM.Cardiovascular.HeartFailureEventDiagnosisBASE
),


-- ever diagnosed with diabetes?
diabetes AS (
SELECT
  DISTINCT PatientID
  ,REPLACE(PatientEncounterID, 'EN', '') AS EncounterID
  ,EventSubTypeNM AS Diabetes
FROM
SAM.DiabetesBTC.EventDiabetes
),

zipcode AS (
SELECT
  DISTINCT NewPersonID AS PersonID
  ,NewEncounterID AS EncounterID
  ,NewPersonHomeAddressZipcodeNBR AS Zip
FROM
  Cerner.Person.ManagementTransaction
 WHERE NewEncounterID IN (SELECT DISTINCT
						EncounterID AS NewEncounterID
						FROM Cerner.Schedule.Appointment
						WHERE BeginDTS BETWEEN '2017-01-01'
						AND '2019-12-31'
						AND RoleMeaningDSC = 'PATIENT'
						AND ActiveIndicatorCD = 1 )
)

-- Now tie them all together

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
  ,zipcode.zip AS Person_ZipCode
  ,encounter.BuildingLocationCVDSC AS Building
  ,encounter.NurseUnitLocationCVDSC AS NurseUnit
  ,encounter.AdmitTypeCVDisplayDSC AS AdmitType
  ,encounter.EncounterTypeCVDSC AS EncounterType
  ,encounter.ReasonForVisitDSC AS ReasonForVisit
  ,d1.DiagnosisFreeTXT AS DiagnosisFreeTXT
  ,d1.DiagnosisPrioritySEQ AS DiagnosisPrioritySEQ
  ,d2.DiagnosisCD AS ICD
  ,d2.DiagnosisTypeDSC
  ,d2.DiagnosisDSC
  ,d2.DiagnosisNormDSC
  ,respiratoryFailure.RespiratoryFailure AS RespiratoryFailure
  ,respiratoryFailure.CharlsonDeyoRiskScoreNBR AS CharlsonDeyoScore
FROM
  sdcPatients
  LEFT JOIN day ON sdcPatients.PersonID = day.PersonID
  LEFT JOIN person ON day.PersonID = person.PersonID
  LEFT JOIN zipcode ON day.PersonID = zipcode.PersonID
  LEFT JOIN encounter ON day.EncounterID = encounter.EncounterID
  LEFT JOIN d1 ON day.EncounterID = d1.EncounterID
	AND day.PersonID = d1.PersonID
  LEFT JOIN d2 ON day.EncounterID = d2.EncounterID
	AND day.PersonID = d2.PatientID
	AND d1.DiagnosisID = d2.DiagnosisID
  LEFT JOIN respiratoryFailure ON day.PersonID = respiratoryFailure.PatientID
WHERE day.BeginDTS BETWEEN '2017-01-01' AND '2019-12-31' 
ORDER BY day.PersonID, day.BeginDTS, day.EncounterID
