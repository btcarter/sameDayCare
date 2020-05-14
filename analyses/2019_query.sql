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
	,SA2.AppointmentLocationCVDisplayDSC
	--patient information
	,SA2.PersonID	
	,HP.FinancialClassCVDSC
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
	,CASE WHEN	SRF.FirstCOPDDiagnosisDT IS NOT NULL THEN 1 ELSE 0 END AS COPD
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
		LEFT JOIN [Cerner].[Reference].[HealthPlanBASE] HP
			ON HP.HealthPlanID = EPR.HealthPlanID
			LEFT JOIN [Cerner].[Encounter].[EncounterBASE] EN
			ON SA2.EncounterID = EN.EncounterID
			LEFT JOIN Shared.[Clinical].[DiagnosisBASE] SH
			ON SA2.EncounterID = SH.EncounterID
			LEFT JOIN Cerner.Clinical.DiagnosisBASE DXB
			ON SA2.EncounterID = DXB.EncounterID
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
	AND HP.ActiveIndicatorCD = 1