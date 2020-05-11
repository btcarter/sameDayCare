SELECT DISTINCT				
	SA.BeginDTS
	,DATEPART(MONTH, SA.BeginDTS) AS AdmitMonthNBR
	,DATEPART(DAY, SA.BeginDTS) AS AdmitDayNBR
	,DATEPART(HOUR, SA.BeginDTS) AS AdmitHourNBR
	,DATENAME(DW, SA.BeginDTS) AS AdmitDayOfWeek
	,DATEDIFF(YEAR, PER.BirthDTS, SA.BeginDTS) AS AdmitAgeNBR	
	,SA.StateOfAppointmentMeaningDSC AS ApptStatusDSC		
	,SA.ResourceCVDSC
	,SA.ScheduleAppointmentID			
	,SA.ActiveIndicatorCD			
	,RES.ResourceTypeFLG			
	,RESDEPT.Dept			
	,SA2.AppointmentLocationCVDisplayDSC
	,SA2.PersonID	
	,HP.FinancialClassCVDSC
	,PER.EthnicGroupCVDisplayDSC AS EthnicityDSC
	,PER.LanguageCVDisplayDSC AS LanguageDSC
	,PER.MaritalTypeCVDisplayDSC AS MaritalStatusDSC
	,PER.RaceCVDisplayDSC AS RaceDSC
	,PER.SexCVDisplayDSC as GenderDSC
	,PER.ReligionCVDisplayDSC AS ReligionDSC
	,EN.ReasonForVisitDSC AS ReasonForVisitDSC
	,SH.DiagnosisCD AS ICD9CD
	,SH.DiagnosisDSC AS DiagnosisDSC
	,DXB.DiagnosisFreeTXT AS DiagnosisFreeTXT
				
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