-- CTEs to assemble smaller bites for processing
-- appointment information
WITH day AS (
SELECT
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
  ,EthnicGroupCVDisplayDSC AS Ethnicity
	,LanguageCVDisplayDSC AS Language
	,MaritalTypeCVDisplayDSC AS Marital_Status
	,RaceCVDisplayDSC AS Race
	,SexCVDisplayDSC as Sex
	,ReligionCVDisplayDSC AS Religion
FROM
  [Cerner].[Person].[PersonBASE]
),

-- encounter information
encounter AS (
SELECT
  DISTINCT EncounterID
  ,LocationCVDisplayDSC
	,FacilityLocationCVDSC
	,AdmitTypeCVDisplayDSC
	,EncounterTypeCVDSC
	,ReasonForVisitDSC
FROM
  [Cerner].[Encounter].[EncounterBASE]
),

-- diagnosis information
d1 AS (
SELECT
  PersonID
  ,EncounterID
	,DiagnosisID
  ,DiagnosisFreeTXT
  ,DiagnosisPrioritySEQ
FROM
  Cerner.Clinical.DiagnosisBASE
),

-- more dx information
d2 AS (
SELECT
  PatientID
  ,DiagnosisID
  , DISTINCT EncounterID
  ,DiagnosisCD
  ,DiagnosisTypeDSC
  ,DiagnosisDSC
  ,DiagnosisNormDSC
FROM
  Shared.Clinical.DiagnosisBASE
),

-- respiratory tables comorbidities
respiratoryFailure AS (
SELECT
  DISTINCT PatientID
  ,CASE WHEN PatientID IS NOT NULL THEN 1 ELSE 0 END AS RespiratoryFailure
  ,CharlsonDeyoRiskScoreNBR
  ,ReadmittedFLG_HW
  ,ReadmittedFLG_ED
  ,ReadmittedFLG_AV
  ,ReadmittedFLG_AMI
  ,ReadmittedFLG_CABG
  ,ReadmittedFLG_COPD
  ,ReadmittedFLG_HF
  ,ReadmittedFLG_InpPsych
  ,ReadmittedFLG_PN
  ,ReadmittedFLG_Stroke
  ,ReadmittedFLG_THTK
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
)

-- Now tie them all together

SELECT
  day.BeginDTS AS DTS
  ,day.PersonID AS PersonID
  ,day.EncounterID AS EncounterID
  ,day.ScheduleAppointmentID AS AppointmentID
  ,day.ActiveIndicatorCD
  ,person.Ethnicity AS Ethnicity
  ,person.Language AS Language
  ,person.Race AS Race
  ,person.Marital_Status AS Marital_Status
  ,person.Sex AS Sex
  ,person.Religion AS Religion
  ,encounter.LocationCVDisplayDSC AS Location
	,encounter.FacilityLocationCVDSC AS Facility
	,encounter.AdmitTypeCVDisplayDSC AS AdmitType
	,encounter.EncounterTypeCVDSC AS EncounterType
	,encounter.ReasonForVisitDSC AS ReasonForVisit
	,d1.DiagnosisID AS DiagnosisID
  ,d1.DiagnosisFreeTXT AS DiagnosisFreeTXT
  ,d1.DiagnosisPrioritySEQ AS DiagnosisPrioritySEQ
  ,d2.DiagnosisTypeDSC
  ,d2.DiagnosisDSC
  ,d2.DiagnosisNormDSC
  ,respiratoryFailure.RespiratoryFailure AS RespiratoryFailure
  ,respiratoryFailure.CharlsonDeyoRiskScoreNBR AS CharlsonDeyoScore
  ,respiratoryFailure.ReadmittedFLG_HW AS FLG_HW
  ,respiratoryFailure.ReadmittedFLG_ED AS FLG_ED
  ,respiratoryFailure.ReadmittedFLG_AV AS FLG_AV
  ,respiratoryFailure.ReadmittedFLG_AMI AS FLG_AMI
  ,respiratoryFailure.ReadmittedFLG_CABG AS FLG_CABG
  ,respiratoryFailure.ReadmittedFLG_COPD AS FLG_COPD
  ,respiratoryFailure.ReadmittedFLG_HF AS FLG_HF
  ,respiratoryFailure.ReadmittedFLG_InpPsych AS FLG_InpPsych
  ,respiratoryFailure.ReadmittedFLG_PN AS FLG_PN
  ,respiratoryFailure.ReadmittedFLG_Stroke AS FLG_Stroke
  ,respiratoryFailure.ReadmittedFLG_THTK AS FLG_THTK
FROM
  day
LEFT JOIN person ON day.PersonID = person.PersonID
LEFT JOIN encounter ON day.EncounterID = encounter.EncounterID
LEFT JOIN d1 ON day.EncounterID = d1.EncounterID
  AND day.PersonID = d1.PersonID
LEFT JOIN d2 ON day.EncounterID = d2.EncounterID
  AND day.PersonID = d2.PatientID
LEFT JOIN respiratoryFailure ON day.PersonID = respiratoryFailure.PatientID









-- original query is below, to be deleted once all above is working

SELECT DISTINCT
  --date info
	SA.BeginDTS
	,SA2.EncounterID AS EncounterID
	,DATEPART(MONTH, SA.BeginDTS) AS AdmitMonthNBR
	,DATEPART(DAY, SA.BeginDTS) AS AdmitDayNBR
	,DATEPART(HOUR, SA.BeginDTS) AS AdmitHourNBR
	,DATENAME(DW, SA.BeginDTS) AS AdmitDayOfWeek
	,DATEDIFF(YEAR, PER.BirthDTS, SA.BeginDTS) AS AdmitAgeNBR
	,SA.ResourceCVDSC
	,SA.ScheduleAppointmentID
	,SA.ActiveIndicatorCD
	,RES.ResourceTypeFLG
	,RESDEPT.Dept
	,EN.LocationCVDisplayDSC
	,EN.FacilityLocationCVDSC
	,EN.AdmitTypeCVDisplayDSC
	,EN.EncounterTypeCVDSC AS EncounterType
	--patient information
	,SA2.PersonID
	,PER.EthnicGroupCVDisplayDSC AS EthnicityDSC
	,PER.LanguageCVDisplayDSC AS LanguageDSC
	,PER.MaritalTypeCVDisplayDSC AS MaritalStatusDSC
	,PER.RaceCVDisplayDSC AS RaceDSC
	,PER.SexCVDisplayDSC as GenderDSC
	,PER.ReligionCVDisplayDSC AS ReligionDSC
	--dx
	,EN.ReasonForVisitDSC AS ReasonForVisitDSC
	,SH.DiagnosisCD AS ICD9CD
	,DXB.DiagnosisPrioritySEQ
	,SH.DiagnosisDSC AS DiagnosisDSC
	,DXB.DiagnosisFreeTXT AS DiagnosisFreeTXT
  -- prexisting coniditions
	,CASE WHEN SRF.FirstCOPDDiagnosisDT IS NOT NULL THEN 1 ELSE 0 END AS COPD
	,CASE WHEN CVS.DiagnosisDSC IS NOT NULL THEN 1 ELSE 0 END AS HeartFailure
	,CASE WHEN DBS.EventNM IS NOT NULL THEN 1 ELSE 0 END AS Diabetes

FROM
	[Cerner].[Schedule].[Appointment] SA
	LEFT JOIN [Cerner].[Person].[Personnel] (nolock)
		ON SA.PersonID = Personnel.PersonID
	LEFT JOIN [Cerner].[Schedule].[Resource] RES
		ON SA.ResourceCVCD = RES.ResourceCVCD
	LEFT JOIN [SAM].[PatientAccess].[ResourceToDeptCrosswalk] RESDEPT
		ON SA.ResourceCVCD = RESDEPT.ResourceCVCD
	LEFT JOIN [Cerner].[Person].[Person] PATIENT
		ON SA.PersonID = PATIENT.PersonID
	LEFT JOIN [Cerner].[Schedule].[Appointment] SA2
	  ON SA.ScheduleEventID = SA2.ScheduleID
		AND SA2.RoleMeaningDSC = 'PATIENT'
	LEFT JOIN [Cerner].[Person].[PersonBASE] PER
		ON SA2.PersonID = PER.PersonID
	LEFT JOIN [Cerner].[Encounter].[EncounterToPlanRelationshipBASE] EPR
		ON SA2.EncounterID = EPR.EncounterID
	LEFT JOIN [Cerner].[Encounter].[EncounterBASE] EN
		ON SA2.EncounterID = EN.EncounterID
	LEFT JOIN Shared.[Clinical].[DiagnosisBASE] SH
		ON SA2.EncounterID = SH.EncounterID
	LEFT JOIN Cerner.Clinical.DiagnosisBASE DXB
		ON SH.EncounterID = DXB.EncounterID AND SH.DiagnosisID = DXB.DiagnosisID
	LEFT JOIN [SAM].[RespiratoryFailure].[COPDSummaryPatientsBASE] SRF
		ON SA.PersonID = SRF.PatientID
	LEFT JOIN [SAM].[Cardiovascular].[HeartFailureEventDiagnosis] CVS
		ON SA2.EncounterID = CVS.EncounterID
	LEFT JOIN [SAM].[DiabetesBTC].[EventDiabetes] DBS
		ON SA2.EncounterID = REPLACE(DBS.PatientEncounterID, 'EN', '')


WHERE
	SA.BeginDTS BETWEEN '2016-01-01' AND '2019-12-31'
	-- Provider Side
	AND SA.ActiveIndicatorCD = 1
	AND SA.StateOfAppointmentMeaningDSC NOT IN ('PENDING')
	AND SA.EndEffectiveDTS > GETDATE()
	AND SA.RoleMeaningDSC != 'PATIENT'

	-- Patient Side
	AND SA2.ActiveIndicatorCD = 1
	AND SA2.EndEffectiveDTS > GETDATE()
	AND SA2.RoleMeaningDSC = 'PATIENT'
	AND SA2.ScheduleStateCVDisplayDSC NOT IN ('RESCHEDULED','CANCELED','DELETED')
	AND PER.ActiveIndicatorCD = 1
	AND EPR.ActiveIndicatorCD = 1

ORDER BY
  SA2.PersonID, SA2.EncounterID, SA.BeginDTS, ICD9CD
