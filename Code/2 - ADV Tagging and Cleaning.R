setwd("~/yourLocalDirectory")
#install.packages("purrr")

packages <- c("dplyr", "zoo", "tidyr", "data.table", "stringr", "ineq", "ggplot2")
purrr::walk(packages, library, character.only = TRUE)
rm( list=ls() ) #Removes any variables in the environment (wipe memory)


load("./Data/Generated/df.step1.Rda")
load("./Data/Generated/df.step2a.Rda")
load("./Data/Generated/df.step2b.Rda")
load("./Data/Generated/df.step3a.Rda")
load("./Data/Generated/df.step3b.Rda")
load("./Data/Generated/df.step4.Rda")
load("./Data/Generated/df.step5a.Rda")

Main <- function ()
{
  #Update names and types of base 
  df.Base <- BASE_A_UPDATENAMES(df.Base)
  df.Base <- BASE_A_TYPECLEAN(df.Base)
  df.Base <- TRIM_BASE_A(df.Base)
  
  saveList = ls(pattern = "df.Base")
  save(list = saveList, file="./Data/ReadyForMerging/df.Base.Rda")
  
  #Update names and types of 7b1
  df.Schedule_D_7B1_ <- SCHEDULE_D_7B1_TYPECLEAN(df.Schedule_D_7B1_) 
  df.Schedule_D_7B1_ <- SCHEDULE_D_7B1_UPDATENAMES(df.Schedule_D_7B1_)
  df.Schedule_D_7B1_ <- TRIM_7B1(df.Schedule_D_7B1_)
  
  saveList = ls(pattern = "df.Schedule_D_7B1_")
  save(list = saveList, file="./Data/ReadyForMerging/df.Schedule_D_7B1_.Rda")
  
  
  
}

CONVERT2BOOLEAN <- function(string) {
  #string <- ifelse(is.na(string), FALSE, string) ifelse(string=="Y", TRUE, FALSE))
  return(ifelse(string=="Y", TRUE, FALSE))
}

CONVERT2BOOLEAN2 <- function(string) {
  #string <- ifelse(is.na(string), FALSE, string) ifelse(string=="Y", TRUE, FALSE))
  return(ifelse(string=="Yes", TRUE, FALSE))
}

BASE_A_TYPECLEAN <- function(df) {
  #Format into boolean
  df <- df %>%
    mutate(ChangeLegalName                                          = CONVERT2BOOLEAN(ChangeLegalName                                          ),  
           ChangeBusinessName                                       = CONVERT2BOOLEAN(ChangeBusinessName                                       ),  
           OfficePrivateResidence                                   = CONVERT2BOOLEAN(OfficePrivateResidence                                   ),  
           Website                                                  = CONVERT2BOOLEAN(Website                                                  ),  
           MaintainedBooksAndRecord                                 = CONVERT2BOOLEAN(MaintainedBooksAndRecord                                 ),  
           ForeignRegulated                                         = CONVERT2BOOLEAN(ForeignRegulated                                         ),  
           PublicReportingCo                                        = CONVERT2BOOLEAN(PublicReportingCo                                        ),  
           isOneBillionFund                                         = CONVERT2BOOLEAN(isOneBillionFund                                         ),  
           
           isBrokerDealer                                           = CONVERT2BOOLEAN(isBrokerDealer                                           ),  
           isRepofBrokerDealer                                      = CONVERT2BOOLEAN(isRepofBrokerDealer                                      ),  
           isCommodityOperatorOrAdvisor                             = CONVERT2BOOLEAN(isCommodityOperatorOrAdvisor                             ),  
           isFuturesCommissionMerchant                              = CONVERT2BOOLEAN(isFuturesCommissionMerchant                              ),  
           isRealEstateBrokerDealerAgent                            = CONVERT2BOOLEAN(isRealEstateBrokerDealerAgent                            ),  
           
           isInsuranceBrokerAgent                                   = CONVERT2BOOLEAN(isInsuranceBrokerAgent                                   ),  
           isBank                                                   = CONVERT2BOOLEAN(isBank                                                   ),  
           isTrustCompany                                           = CONVERT2BOOLEAN(isTrustCompany                                           ),  
           isMunicipalAdvisor                                       = CONVERT2BOOLEAN(isMunicipalAdvisor                                       ),  
           isSecuritySwapDealer                                     = CONVERT2BOOLEAN(isSecuritySwapDealer                                     ),  
           isSecuritySwapParticipant                                = CONVERT2BOOLEAN(isSecuritySwapParticipant                                ),  
           
           isAccountantAccountingFirm                               = CONVERT2BOOLEAN(isAccountantAccountingFirm                               ),  
           isLawyerLawFirm                                          = CONVERT2BOOLEAN(isLawyerLawFirm                                          ),  
           isOther                                                  = CONVERT2BOOLEAN(isOther                                                  ),  
           OtherBusinessActivities                                  = CONVERT2BOOLEAN(OtherBusinessActivities                                  ),  
           isEngagedOtherBusinesses                                 = CONVERT2BOOLEAN(isEngagedOtherBusinesses                                 ),  
           isOtherBusinessPrimary                                   = CONVERT2BOOLEAN(isOtherBusinessPrimary                                   ),  
           
           SellNonInvestmentProducts                                = CONVERT2BOOLEAN(SellNonInvestmentProducts                                ),  
           isRelatedPersonBrokerDealer                              = CONVERT2BOOLEAN(isRelatedPersonBrokerDealer                              ),  
           isRelatedPersonInvestAdvisor                             = CONVERT2BOOLEAN(isRelatedPersonInvestAdvisor                             ),  
           iSRelatedPersonMuniAdvisor                               = CONVERT2BOOLEAN(iSRelatedPersonMuniAdvisor                               ),  
           isRelatedPersonSwapDealer                                = CONVERT2BOOLEAN(isRelatedPersonSwapDealer                                ),  
           isRelatedPersonSwapParticipant                           = CONVERT2BOOLEAN(isRelatedPersonSwapParticipant                           ),  
           
           isRelatedPersonCommodityOpAdv                            = CONVERT2BOOLEAN(isRelatedPersonCommodityOpAdv                            ),  
           isRelatedPersonFuturesMerchant                           = CONVERT2BOOLEAN(isRelatedPersonFuturesMerchant                           ),  
           isRelatedPersonBankorThriftCo                            = CONVERT2BOOLEAN(isRelatedPersonBankorThriftCo                            ),  
           isRelatedPersonTrustCo                                   = CONVERT2BOOLEAN(isRelatedPersonTrustCo                                   ),  
           isRelatedPersonAccountant                                = CONVERT2BOOLEAN(isRelatedPersonAccountant                                ),  
           isRelatedPersonLawyer                                    = CONVERT2BOOLEAN(isRelatedPersonLawyer                                    ),  
           
           isRelatedPersonInsuranceCo                               = CONVERT2BOOLEAN(isRelatedPersonInsuranceCo                               ),  
           isRelatedPersonPensionConsulta                           = CONVERT2BOOLEAN(isRelatedPersonPensionConsulta                           ),  
           isRelatedPersonRealEstateBroke                           = CONVERT2BOOLEAN(isRelatedPersonRealEstateBroke                           ),  
           isRelatedPersonLPSponsor                                 = CONVERT2BOOLEAN(isRelatedPersonLPSponsor                                 ),  
           isRelatedPersonGPSponsor                                 = CONVERT2BOOLEAN(isRelatedPersonGPSponsor                                 ),  
           isAdvisortoPrivateFund                                   = CONVERT2BOOLEAN(isAdvisortoPrivateFund                                   ),  
           
           isAnyOtherPersonInControl                                = CONVERT2BOOLEAN(isAnyOtherPersonInControl                                ),  
           CriminalRegulatoryInvolvement                            = CONVERT2BOOLEAN(CriminalRegulatoryInvolvement                            ),  
           isAnyConvictedFelony                                     = CONVERT2BOOLEAN(isAnyConvictedFelony                                     ),  
           isAnyChargedFelony                                       = CONVERT2BOOLEAN(isAnyChargedFelony                                       ),  
           isAnyConvictedMisdemeanor                                = CONVERT2BOOLEAN(isAnyConvictedMisdemeanor                                ),  
           isAnyChargedMisdemeanor                                  = CONVERT2BOOLEAN(isAnyChargedMisdemeanor                                  ),  
           
           isAnyFalseStatementSEC                                   = CONVERT2BOOLEAN(isAnyFalseStatementSEC                                   ),  
           isAnyViolationSEC                                        = CONVERT2BOOLEAN(isAnyViolationSEC                                        ),  
           isAnySuspendedRevokedSEC                                 = CONVERT2BOOLEAN(isAnySuspendedRevokedSEC                                 ),  
           isAnyOrderSEC                                            = CONVERT2BOOLEAN(isAnyOrderSEC                                            ),  
           isAnyCivilMoneyPenaltySEC                                = CONVERT2BOOLEAN(isAnyCivilMoneyPenaltySEC                                ),  
           isAnyFalseStatmentOther                                  = CONVERT2BOOLEAN(isAnyFalseStatmentOther                                  ),  
           
           isAnyViolationOther                                      = CONVERT2BOOLEAN(isAnyViolationOther                                      ),  
           isAnySuspendedRevokedOther                               = CONVERT2BOOLEAN(isAnySuspendedRevokedOther                               ),  
           isAnyOrderOther                                          = CONVERT2BOOLEAN(isAnyOrderOther                                          ),  
           isAnyRegistrationRestricted                              = CONVERT2BOOLEAN(isAnyRegistrationRestricted                              ),  
           isAnyFalseStatementRegOrg                                = CONVERT2BOOLEAN(isAnyFalseStatementRegOrg                                ),  
           isAnyViolationRegOrg                                     = CONVERT2BOOLEAN(isAnyViolationRegOrg                                     ),  
           
           isAnySupendedRevokedRegOrg                               = CONVERT2BOOLEAN(isAnySupendedRevokedRegOrg                               ),  
           isAnyDisciplinedRegOrg                                   = CONVERT2BOOLEAN(isAnyDisciplinedRegOrg                                   ),  
           AuthorizationRevoked                                     = CONVERT2BOOLEAN(AuthorizationRevoked                                     ),  
           SubjecttoRegProceeding                                   = CONVERT2BOOLEAN(SubjecttoRegProceeding                                   ),  
           isSucceedingRIABusiness                                  = CONVERT2BOOLEAN(isSucceedingRIABusiness                                  ),
           
           CourtEnjoined                                            = CONVERT2BOOLEAN(CourtEnjoined                                            ),
           CourtViolation                                           = CONVERT2BOOLEAN(CourtViolation                                           ),
           CourtDismissal                                           = CONVERT2BOOLEAN(CourtDismissal                                           ),
           SubjecttoCivilProceeding                                 = CONVERT2BOOLEAN(SubjecttoCivilProceeding                                 ),
           X1K1LBooksandRecords                                     = CONVERT2BOOLEAN(X1K1LBooksandRecords                                     ),
           
           ManagementFee                                            = CONVERT2BOOLEAN(ManagementFee                                            ),                      
           HourlyFee                                                = CONVERT2BOOLEAN(HourlyFee                                                ),                        
           SubscriptionFee                                          = CONVERT2BOOLEAN(SubscriptionFee                                          ),    
           FixedFee                                                 = CONVERT2BOOLEAN(FixedFee                                                 ),    
           CommissionFee                                            = CONVERT2BOOLEAN(CommissionFee                                            ),    
           PerformanceFee                                           = CONVERT2BOOLEAN(PerformanceFee                                           ),    
           OtherFee                                                 = CONVERT2BOOLEAN(OtherFee                                                 ),    
           SupervisoryRole                                          = CONVERT2BOOLEAN(SupervisoryRole                                          ),    
           
           ProvideFinPlanning                                       = CONVERT2BOOLEAN(ProvideFinPlanning                                       ),   
           ProvidePortfolioMgmtForIndv                              = CONVERT2BOOLEAN(ProvidePortfolioMgmtForIndv                              ),    
           ProvidePortfolioMgmtForInvCo                             = CONVERT2BOOLEAN(ProvidePortfolioMgmtForInvCo                             ),    
           
           ProvidePortfolioMgmtForPoolInv                           = CONVERT2BOOLEAN(ProvidePortfolioMgmtForPoolInv                           ),    
           ProvidePortfolioMgmtForInstClient                        = CONVERT2BOOLEAN(ProvidePortfolioMgmtForInstClient                        ),    
           ProvidePensionConsulting                                 = CONVERT2BOOLEAN(ProvidePensionConsulting                                 ),    
           ProvideSelectionOfAdvisors                               = CONVERT2BOOLEAN(ProvideSelectionOfAdvisors                               ),    
           ProvideNewslettersOrPeriodical                           = CONVERT2BOOLEAN(ProvideNewslettersOrPeriodical                           ),    
           ProvideSecurityRatingsPrices                             = CONVERT2BOOLEAN(ProvideSecurityRatingsPrices                             ),    
           
           ProvideMarketTiming                                      = CONVERT2BOOLEAN(ProvideMarketTiming                                      ),   
           ProvideEducSeminars                                      = CONVERT2BOOLEAN(ProvideEducSeminars                                      ),   
           ProvideOtherServices                                     = CONVERT2BOOLEAN(ProvideOtherServices                                     ),   
           SponsorWrapFeeProgram                                    = CONVERT2BOOLEAN(SponsorWrapFeeProgram                                    ),   
           IsPortfolioManagerForWrapProg                            = CONVERT2BOOLEAN(IsPortfolioManagerForWrapProg                            ),   
           ProvideAdviceToLimitedInvsType                           = CONVERT2BOOLEAN(ProvideAdviceToLimitedInvsType                           ),   
           
           BuySellInPrincipalTransactions                           = CONVERT2BOOLEAN(BuySellInPrincipalTransactions                           ),                    
           BuySellForSelfAndRecommendToClients                      = CONVERT2BOOLEAN(BuySellForSelfAndRecommendToClients                      ),                    
           
           RecommendOwnedSecuritiesToClients                        = CONVERT2BOOLEAN(RecommendOwnedSecuritiesToClients                        ),   
           BuySellInAgencyCrossTransactions                         = CONVERT2BOOLEAN(BuySellInAgencyCrossTransactions                         ),   
           RecommendUnderwrittenSecuritiesToClients                 = CONVERT2BOOLEAN(RecommendUnderwrittenSecuritiesToClients                 ),   
           RecommendSecuritiesOfSalesInterest                       = CONVERT2BOOLEAN(RecommendSecuritiesOfSalesInterest                       ),   
           RelatedWithAuthorityToDetermineBuySellForClients         = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineBuySellForClients         ),   
           RelatedWithAuthorityToDetermineAmountBuySellForClients   = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineAmountBuySellForClients   ),   
           RelatedWithAuthorityToDetermineBrokerOrDealer            = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineBrokerOrDealer            ),   
           RelatedWithAuthorityToDetermineCommissionRates           = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineCommissionRates           ),   
           isRelatedPersonBrokerOrDealer                            = CONVERT2BOOLEAN(isRelatedPersonBrokerOrDealer                            ),   
           doRelatedPersonRecommendBrokerOrDealer                   = CONVERT2BOOLEAN(doRelatedPersonRecommendBrokerOrDealer                   ),   
           isRelatedPersonBrokerOrDealerForQuestionsAbove           = CONVERT2BOOLEAN(isRelatedPersonBrokerOrDealerForQuestionsAbove           ),   
           doRelatedPersonReceiveResearchFromThirdParty             = CONVERT2BOOLEAN(doRelatedPersonReceiveResearchFromThirdParty             ),   
           
           areSoftDollarBenefitsResearchOrBrokerageServices         = CONVERT2BOOLEAN(areSoftDollarBenefitsResearchOrBrokerageServices         ),       
           CompensateForClientReferrals                             = CONVERT2BOOLEAN(CompensateForClientReferrals                             ),       
           ReceiveCompensationForClientReferrals                    = CONVERT2BOOLEAN(ReceiveCompensationForClientReferrals                    ),       
           HaveCustodyOfClientAccounts                              = CONVERT2BOOLEAN(HaveCustodyOfClientAccounts                              ),       
           HaveCustodyOfClientSecurities                            = CONVERT2BOOLEAN(HaveCustodyOfClientSecurities                            ),
           HaveCustodyOfAdvisoryClientAccounts                      = CONVERT2BOOLEAN(HaveCustodyOfAdvisoryClientAccounts                      ),
           HaveCustodyOfAdvisoryClientSecurities                    = CONVERT2BOOLEAN(HaveCustodyOfAdvisoryClientSecurities                    ),
           HaveCustodyOfAdvisoryClientSecuritiesFunds               = CONVERT2BOOLEAN(HaveCustodyOfAdvisoryClientSecuritiesFunds               ),
           
           BuySellInPrincipalTransactions                           = CONVERT2BOOLEAN(BuySellInPrincipalTransactions                           ),                  
           BuySellForSelfAndRecommendToClients                      = CONVERT2BOOLEAN(BuySellForSelfAndRecommendToClients                      ),                  
           
           RecommendOwnedSecuritiesToClients                        = CONVERT2BOOLEAN(RecommendOwnedSecuritiesToClients                        ),    
           BuySellInAgencyCrossTransactions                         = CONVERT2BOOLEAN(BuySellInAgencyCrossTransactions                         ),    
           RecommendUnderwrittenSecuritiesToClients                 = CONVERT2BOOLEAN(RecommendUnderwrittenSecuritiesToClients                 ),    
           RecommendSecuritiesOfSalesInterest                       = CONVERT2BOOLEAN(RecommendSecuritiesOfSalesInterest                       ),    
           RelatedWithAuthorityToDetermineBuySellForClients         = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineBuySellForClients         ),    
           RelatedWithAuthorityToDetermineAmountBuySellForClients   = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineAmountBuySellForClients   ),    
           RelatedWithAuthorityToDetermineBrokerOrDealer            = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineBrokerOrDealer            ),    
           RelatedWithAuthorityToDetermineCommissionRates           = CONVERT2BOOLEAN(RelatedWithAuthorityToDetermineCommissionRates           ),    
           isRelatedPersonBrokerOrDealer                            = CONVERT2BOOLEAN(isRelatedPersonBrokerOrDealer                            ),    
           doRelatedPersonRecommendBrokerOrDealer                   = CONVERT2BOOLEAN(doRelatedPersonRecommendBrokerOrDealer                   ),    
           isRelatedPersonBrokerOrDealerForQuestionsAbove           = CONVERT2BOOLEAN(isRelatedPersonBrokerOrDealerForQuestionsAbove           ),    
           doRelatedPersonReceiveResearchFromThirdParty             = CONVERT2BOOLEAN(doRelatedPersonReceiveResearchFromThirdParty             ),    
           
           areSoftDollarBenefitsResearchOrBrokerageServices         = CONVERT2BOOLEAN(areSoftDollarBenefitsResearchOrBrokerageServices         ),    
           CompensateForClientReferrals                             = CONVERT2BOOLEAN(CompensateForClientReferrals                             ),    
           ReceiveCompensationForClientReferrals                    = CONVERT2BOOLEAN(ReceiveCompensationForClientReferrals                    ),
           CustodianProvidesQuarterlyStatementsToClients            = CONVERT2BOOLEAN(CustodianProvidesQuarterlyStatementsToClients            ),
           AccountantAuditsAnnually                                 = CONVERT2BOOLEAN(AccountantAuditsAnnually                                 ),
           AccountantConductsAnnualSurpriseExamination              = CONVERT2BOOLEAN(AccountantConductsAnnualSurpriseExamination              ),
           AccountantPreparesInternalControlReport                  = CONVERT2BOOLEAN(AccountantPreparesInternalControlReport                  ),
           doYouActAsQualifiedCustodianForClients                   = CONVERT2BOOLEAN(doYouActAsQualifiedCustodianForClients                   ),
           doRelatedPersonsActAsQualifiedCustodianForClients        = CONVERT2BOOLEAN(doRelatedPersonsActAsQualifiedCustodianForClients        ),
           
           AnyOtherPersonControlManagementOrPolicies                = CONVERT2BOOLEAN(AnyOtherPersonControlManagementOrPolicies                ),    
           Assets5MillionOrMoreLastFY                               = CONVERT2BOOLEAN(Assets5MillionOrMoreLastFY                               ),    
           ControlAnotherAdvisorWithAUM25MillionOrMoreLastFY        = CONVERT2BOOLEAN(ControlAnotherAdvisorWithAUM25MillionOrMoreLastFY        ),    
           ControlAnotherPersonWithAssets5MillionOrMoreLastFY       = CONVERT2BOOLEAN(ControlAnotherPersonWithAssets5MillionOrMoreLastFY       ),    
           ControlledByAnotherAdvisorWithAUM25MillionOrMoreLastFY   = CONVERT2BOOLEAN(ControlledByAnotherAdvisorWithAUM25MillionOrMoreLastFY   ),    
           ControlledByAnotherPersonWithAssets5MillionOrMoreLastFY  = CONVERT2BOOLEAN(ControlledByAnotherPersonWithAssets5MillionOrMoreLastFY  )) 
  
  #Convert to numeric     
  cols.names <- c("NumEmployees","NumEmployeesAdvisors","NumEmployeesBrokers","NumEmployeesInvestAdvisors","NumEmployeesInvestAdvisorsOtherFirm",
                  "NumEmployeesInsurers","NumClientsProvideServicesExact","PctNonUSClients",
                  "DiscretionaryAUM","NonDiscretionaryAUM","TotalAUM","DiscretionaryAccounts",
                  "NonDiscretionaryAccounts","TotalAccounts","NumClientsFinPlanningOther","DollarAmtOfClientAccountsInCustody","NumClientsInCustody",
                  "DollarAmtOfAdvisoryClientAccountsInCustody","NumClientsInAdvisoryCustody","NumClientsUnderCustodianRole")
  
  df[cols.names] <- sapply(df[cols.names], as.numeric)
  
  #Clean dates
  df <- df %>%
    mutate(ExecutionDate      = as.Date(ExecutionDate,          "%m/%d/%Y"),
           DateSubmittedTime  = format(strptime(DateSubmitted,  "%m/%d/%Y %I:%M:%S %p"), "%I:%M:%S"),
           DateSubmitted      = as.Date(DateSubmitted,          "%m/%d/%Y"),
           Year               = year(DateSubmitted),
           SuccessionDate     = as.Date(SuccessionDate,         "%m/%d/%Y"),
           DateSubjectToSurpriseExaminationByAccountantLastFY = as.Date(DateSubjectToSurpriseExaminationByAccountantLastFY,"%m/%d/%Y"))
  
  return(df)
}

BASE_A_UPDATENAMES <- function(df) {
  
  names(df) <- gsub("\\.", "",names(df))
  
  df = df %>% 
    rename( 
      LegalName                                                = X1A,
      FirmName                                                 = X1B,
      ChangeLegalName                                          = X1CLegal,
      
      ChangeBusinessName                                       = X1CBusiness,
      NewName                                                  = X1CNewName,
      SECFileNumber                                            = X1D,
      CRDNumber                                                = X1E,
      OfficeStreet                                             = X1F1Street1,
      OfficeApt                                                = X1F1Street2,
      
      OfficeCity                                               = X1F1City, 
      OfficeState                                              = X1F1State,
      OfficeCountry                                            = X1F1Country, 
      OfficeZipcode                                            = X1F1Postal,
      OfficePrivateResidence                                   = X1F1Private,
      OfficeBusinessDays                                       = X1F2MF,
      
      OfficeBusinessDaysOther                                  = X1F2Other,
      OfficeBusinessHours                                      = X1F2Hours,
      OfficeMainPhone                                          = X1F3,
      OfficemainFax                                            = X1F4,
      MailStreet                                               = X1GStreet1,
      MailApt                                                  = X1GStreet2,
      
      MailCity                                                 = X1GCity,
      MailState                                                = X1GState,
      MailCountry                                              = X1GCountry,
      MailZipcode                                              = X1GPostal,
      MailPrivateResidence                                     = X1GPrivate,
      Website                                                  = X1I,
      
      ComplianceContactName                                    = X1JName,
      ComplianceContactTitles                                  = X1JTitle,
      ComplianceContactPhone                                   = X1JPhone,
      ComplianceContactFax                                     = X1JFax,
      ComplianceContactStreet1                                 = X1JStreet1,
      ComplianceContactStreet2                                 = X1JStreet2,
      
      ComplianceContactCity                                    = X1JCity,
      ComplianceContactState                                   = X1JState,
      ComplianceContactCountry                                 = X1JCountry,
      ComplianceContactZipcode                                 = X1JPostal,
      ComplianceContactEmail                                   = X1JEmail,
      AdditionalContactName                                    = X1KName,
      
      AdditionalContactTitle                                   = X1KTitle,
      AdditionalContactPhone                                   = X1KPhone,
      AdditionalContactFax                                     = X1KFax,
      AdditionalContactStreet1                                 = X1KStreet1,
      AdditionalContactStreet2                                 = X1KStreet2,
      AdditionalContactCity                                    = X1KCity,
      
      AdditionalContactState                                   = X1KState,
      AdditionalContactCountry                                 = X1KCountry,
      AdditionalContactPostal                                  = X1KPostal,
      AdditionalContactEmail                                   = X1KEmail,
      MaintainedBooksAndRecord                                 = X1L,
      ForeignRegulated                                         = X1M,
      
      PublicReportingCo                                        = X1N,
      CIKNumber                                                = X1NCIK,
      isOneBillionFund                                         = X1O,
      LegalEntityId                                            = X1P,
      X2B1                                                     = X2B1, #To do
      X2B2                                                     = X2B2, #To do
      
      X2B3                                                     = X2B3, #To do                   
      X2BAssets                                                = X2BAssets, #To do
      OrganizationForm                                         = X3A,
      OrganizationFormOther                                    = X3AOther,
      MonthofFiscalYearEnd                                     = X3B,
      StateIncorp                                              = X3CState,
      
      CountryIncorp                                            = X3CCountry,
      isBrokerDealer                                           = X6A1,
      isRepofBrokerDealer                                      = X6A2,
      isCommodityOperatorOrAdvisor                             = X6A3,
      isFuturesCommissionMerchant                              = X6A4,
      isRealEstateBrokerDealerAgent                            = X6A5,
      
      isInsuranceBrokerAgent                                   = X6A6,
      isBank                                                   = X6A7,
      isTrustCompany                                           = X6A8,
      isMunicipalAdvisor                                       = X6A9,
      isSecuritySwapDealer                                     = X6A10,
      isSecuritySwapParticipant                                = X6A11,
      
      isAccountantAccountingFirm                               = X6A12,
      isLawyerLawFirm                                          = X6A13,
      isOther                                                  = X6A14,
      OtherBusinessActivities                                  = X6A14Other,
      isEngagedOtherBusinesses                                 = X6B1,
      isOtherBusinessPrimary                                   = X6B2,
      
      SellNonInvestmentProducts                                = X6B3,
      isRelatedPersonBrokerDealer                              = X7A1,
      isRelatedPersonInvestAdvisor                             = X7A2,
      iSRelatedPersonMuniAdvisor                               = X7A3,
      isRelatedPersonSwapDealer                                = X7A4,
      isRelatedPersonSwapParticipant                           = X7A5,
      
      isRelatedPersonCommodityOpAdv                            = X7A6,
      isRelatedPersonFuturesMerchant                           = X7A7,
      isRelatedPersonBankorThriftCo                            = X7A8,
      isRelatedPersonTrustCo                                   = X7A9,
      isRelatedPersonAccountant                                = X7A10,
      isRelatedPersonLawyer                                    = X7A11,
      
      isRelatedPersonInsuranceCo                               = X7A12,
      isRelatedPersonPensionConsulta                           = X7A13,
      isRelatedPersonRealEstateBroke                           = X7A14,
      isRelatedPersonLPSponsor                                 = X7A15,
      isRelatedPersonGPSponsor                                 = X7A16,
      isAdvisortoPrivateFund                                   = X7B,
      
      isAnyOtherPersonInControl                                = X10A,
      CriminalRegulatoryInvolvement                            = X11,
      isAnyConvictedFelony                                     = X11A1,
      isAnyChargedFelony                                       = X11A2,
      isAnyConvictedMisdemeanor                                = X11B1,
      isAnyChargedMisdemeanor                                  = X11B2,
      
      isAnyFalseStatementSEC                                   = X11C1,
      isAnyViolationSEC                                        = X11C2,
      isAnySuspendedRevokedSEC                                 = X11C3,
      isAnyOrderSEC                                            = X11C4,
      isAnyCivilMoneyPenaltySEC                                = X11C5,
      isAnyFalseStatmentOther                                  = X11D1,
      
      isAnyViolationOther                                      = X11D2,
      isAnySuspendedRevokedOther                               = X11D3,
      isAnyOrderOther                                          = X11D4,
      isAnyRegistrationRestricted                              = X11D5,
      isAnyFalseStatementRegOrg                                = X11E1,
      isAnyViolationRegOrg                                     = X11E2,
      
      isAnySupendedRevokedRegOrg                               = X11E3,
      isAnyDisciplinedRegOrg                                   = X11E4,
      AuthorizationRevoked                                     = X11F,
      SubjecttoRegProceeding                                   = X11G,
      CourtEnjoined                                            = X11H1a,
      CourtViolation                                           = X11H1b,
      
      CourtDismissal                                           = X11H1c,
      SubjecttoCivilProceeding                                 = X11H2,
      ExecutionType                                            = ExecutionType,
      Signatory                                                = Signatory,
      ExecutionDate                                            = ExecutionDate,
      Title                                                    = Title,
      
      X110E02                                                  = X110E02, #To do
      X110E03                                                  = X110E03, #To do
      X110E04                                                  = X110E04, #To do
      X110E05                                                  = X110E05, #To do
      RegContactName                                           = RegContactName,
      RegContactTitle                                          = RegContactTitle,
      
      RegContactPhone                                          = RegContactPhone,
      RegContactFax                                            = RegContactFax,
      RegContactStreet1                                        = RegContactStreet1,
      RegContactStreet2                                        = RegContactStreet2,
      RegContactCity                                           = RegContactCity,
      RegContactState                                          = RegContactState,
      
      RegContactCountry                                        = RegContactCountry,
      RegContactPostal                                         = RegContactPostal,
      RegContactEmail                                          = RegContactEmail,
      X1K1LBooksandRecords                                     = X1K1LBooksandRecords,
      isSucceedingRIABusiness                                  = X4A,
      SuccessionDate                                           = X4B,
      
      X5ARange                                                 = X5ARange, #To do
      NumEmployees                                             = X5ANumber,
      X5B1Range                                                = X5B1Range, #To do
      NumEmployeesAdvisors                                     = X5B1Number,
      X5B2Range                                                = X5B2Range, #To do
      NumEmployeesBrokers                                      = X5B2Number,
      
      X5B3Range                                                = X5B3Range, #To do
      NumEmployeesInvestAdvisors                               = X5B3Number,
      NumEmployeesInvestAdvisorsOtherFirm                      = X5B4,
      NumEmployeesInsurers                                     = X5B5,
      NumPeopleSolicitClients                                  = X5B6,
      NumClientsProvideServices                                = X5CRange,
      
      NumClientsProvideServicesExact                           = X5CNumber,
      PctNonUSClients                                          = X5C2,
      PctClientPoorIndividuals                                 = X5D15D1a,
      PctClientHNWIndividuals                                  = X5D25D1b,
      PctClientBankingThrift                                   = X5D35D1c,
      PctClientInvestmentCompanies                             = X5D45D1d,
      
      PctClientBDC                                             = X5D1e,
      PctClientPooled                                          = X5D65D1f,
      PctClientPensions                                        = X5D55D1g,
      PctClientCharities                                       = X5D75D1h,
      PctClientCorporations                                    = X5D85D1i,
      PctClientStateMuniGov                                    = X5D95D1j,
      
      PctClientOtherIAs                                        = X5D1k,
      PctClientInsuranceCompanies                              = X5D1l,
      PctClientOtherClient                                     = X5D105D1m,
      X5D10Other5D1mOther                                      = X5D10Other5D1mOther, #To do
      PctAssetPoorIndividuals                                  = X5D2a,
      PctAssetHNWIndividuals                                   = X5D2b,
      
      PctAssetBankingThrift                                    = X5D2c,
      PctAssetInvestmentCompanies                              = X5D2d,
      PctAssetBDC                                              = X5D2e,
      PctAssetPooled                                           = X5D2f,
      PctAssetPensions                                         = X5D2g,
      PctAssetCharities                                        = X5D2h,
      
      PctAssetCorporations                                     = X5D2i,
      PctAssetStateMuniGov                                     = X5D2j,
      PctAssetOtherIAs                                         = X5D2k,
      PctAssetInsuranceCompanies                               = X5D2l,
      PctAssetOtherClient                                      = X5D2m,
      X5D2mOther                                               = X5D2mOther, #To do
      
      ManagementFee                                            = X5E1,
      HourlyFee                                                = X5E2,
      SubscriptionFee                                          = X5E3,
      FixedFee                                                 = X5E4,
      CommissionFee                                            = X5E5,
      PerformanceFee                                           = X5E6,
      
      OtherFee                                                 = X5E7,
      OtherFeeType                                             = X5E7Other,
      SupervisoryRole                                          = X5F1,
      DiscretionaryAUM                                         = X5F2a,
      NonDiscretionaryAUM                                      = X5F2b,
      TotalAUM                                                 = X5F2c,
      
      DiscretionaryAccounts                                    = X5F2d,
      NonDiscretionaryAccounts                                 = X5F2e,
      TotalAccounts                                            = X5F2f,
      ProvideFinPlanning                                       = X5G1,
      ProvidePortfolioMgmtForIndv                              = X5G2,
      ProvidePortfolioMgmtForInvCo                             = X5G3,
      
      ProvidePortfolioMgmtForPoolInv                           = X5G4,
      ProvidePortfolioMgmtForInstClient                        = X5G5,
      ProvidePensionConsulting                                 = X5G6,
      ProvideSelectionOfAdvisors                               = X5G7,
      ProvideNewslettersOrPeriodical                           = X5G8,
      ProvideSecurityRatingsPrices                             = X5G9,
      
      ProvideMarketTiming                                      = X5G10,
      ProvideEducSeminars                                      = X5G11,
      ProvideOtherServices                                     = X5G12,
      ProvideOtherServicesType                                 = X5G105G12Other,
      NumClientsProvideFinPlanning                             = X5H,
      NumClientsFinPlanningOther                               = X5HOther,
      
      SponsorWrapFeeProgram                                    = X5I1,
      IsPortfolioManagerForWrapProg                            = X5I2,
      ProvideAdviceToLimitedInvsType                           = X5J,
      X6A76A14Other                                            = X6A76A14Other, #To do
      BuySellInPrincipalTransactions                           = X8A1,
      BuySellForSelfAndRecommendToClients                      = X8A2,
      
      RecommendOwnedSecuritiesToClients                        = X8A3,
      BuySellInAgencyCrossTransactions                         = X8B1,
      RecommendUnderwrittenSecuritiesToClients                 = X8B2,
      RecommendSecuritiesOfSalesInterest                       = X8B3,
      RelatedWithAuthorityToDetermineBuySellForClients         = X8C1,
      RelatedWithAuthorityToDetermineAmountBuySellForClients   = X8C2,
      
      RelatedWithAuthorityToDetermineBrokerOrDealer            = X8C3,
      RelatedWithAuthorityToDetermineCommissionRates           = X8C4,
      isRelatedPersonBrokerOrDealer                            = X8D,
      doRelatedPersonRecommendBrokerOrDealer                   = X8E,
      isRelatedPersonBrokerOrDealerForQuestionsAbove           = X8F,
      doRelatedPersonReceiveResearchFromThirdParty             = X8G1,
      
      areSoftDollarBenefitsResearchOrBrokerageServices         = X8G2,
      CompensateForClientReferrals                             = X8H,
      ReceiveCompensationForClientReferrals                    = X8I,
      HaveCustodyOfClientAccounts                              = X9A19A1a,
      HaveCustodyOfClientSecurities                            = X9A29A1b,
      DollarAmtOfClientAccountsInCustody                       = X9A2a,
      
      NumClientsInCustody                                      = X9A2b,
      HaveCustodyOfAdvisoryClientAccounts                      = X9B19B1a,
      HaveCustodyOfAdvisoryClientSecurities                    = X9B29B1b,
      DollarAmtOfAdvisoryClientAccountsInCustody               = X9B2a,
      NumClientsInAdvisoryCustody                              = X9B2b,
      HaveCustodyOfAdvisoryClientSecuritiesFunds               = X9C,
      
      CustodianProvidesQuarterlyStatementsToClients            = X9C1,
      AccountantAuditsAnnually                                 = X9C2,
      AccountantConductsAnnualSurpriseExamination              = X9C3,
      AccountantPreparesInternalControlReport                  = X9C4,
      doYouActAsQualifiedCustodianForClients                   = X9D1,
      doRelatedPersonsActAsQualifiedCustodianForClients        = X9D2,
      
      DateSubjectToSurpriseExaminationByAccountantLastFY       = X9E,
      NumClientsUnderCustodianRole                             = X9F,
      AnyOtherPersonControlManagementOrPolicies                = X1010A,
      Assets5MillionOrMoreLastFY                               = X12A,
      ControlAnotherAdvisorWithAUM25MillionOrMoreLastFY        = X12B1,
      ControlAnotherPersonWithAssets5MillionOrMoreLastFY       = X12B2,
      
      ControlledByAnotherAdvisorWithAUM25MillionOrMoreLastFY   = X12C1,
      ControlledByAnotherPersonWithAssets5MillionOrMoreLastFY  = X12C2)
  
  return(df)
  
}

SCHEDULE_D_7B1_TYPECLEAN <- function(df) {
  #Format into boolean
  df <- df %>%
    mutate(X3c1Exclusion                  = CONVERT2BOOLEAN(X3c1Exclusion             ),
           X3c7Exclusion                  = CONVERT2BOOLEAN(X3c7Exclusion             ),
           MasterFund                     = CONVERT2BOOLEAN(MasterFund                ),
           FeederFund                     = CONVERT2BOOLEAN(FeederFund                ),
           FundofFunds                    = CONVERT2BOOLEAN(FundofFunds               ),
           FundInvestedSelforRelated      = CONVERT2BOOLEAN(FundInvestedSelforRelated ),
           FundInvestedinSecurities       = CONVERT2BOOLEAN(FundInvestedinSecurities  ),
           Subadviser                     = CONVERT2BOOLEAN(Subadviser                ),
           OtherIAsAdvise                 = CONVERT2BOOLEAN(OtherIAsAdvise            ),
           ClientsSolicited               = CONVERT2BOOLEAN(ClientsSolicited          ),
           ExemptfromRegistration         = CONVERT2BOOLEAN(ExemptfromRegistration    ),
           AnnualAudit                    = CONVERT2BOOLEAN(AnnualAudit               ),
           GAAP                           = CONVERT2BOOLEAN(GAAP                      ),
           FSDistributed                  = CONVERT2BOOLEAN(FSDistributed             ),
           UnqualifiedOpinion             = CONVERT2BOOLEAN2(UnqualifiedOpinion       ),
           PrimeBrokers                   = CONVERT2BOOLEAN(PrimeBrokers              ),
           Custodians                     = CONVERT2BOOLEAN(Custodians                ),
           Administrator                  = CONVERT2BOOLEAN(Administrator             ),
           XAssetsValued                  = CONVERT2BOOLEAN(XAssetsValued             ),
           Marketing                      = CONVERT2BOOLEAN(Marketing                 ))
  
  
  #Convert to numeric     
  cols.names <- c("GrossAssetValue","MinimumInvestment","Owners","XOwnedYouorRelated","XOwnedFunds","XOwnedNonUS","PercentageInvested")
  
  df[cols.names] <- sapply(df[cols.names], as.numeric)
  
  return(df)
  
}

SCHEDULE_D_7B1_UPDATENAMES <- function(df) {
  names(df) <- gsub("\\.", "",names(df))
  return(df)
}

TRIM_BASE_A <- function(df) {
  
  df <- df %>% filter(!is.na(SECFileNumber))
  df <- df %>% filter(SECFileNumber!="" & SECFileNumber != "801-")
  
  df <- df %>%
    group_by(SECFileNumber, Year) %>% # Groups data by X1D and year
    arrange(desc(DateSubmitted), desc(DateSubmittedTime) )%>% # Arranges each group by date and time submitted
    filter(row_number() ==1) %>%  # Drop obs in each group that are not the latest in date and time
    as.data.frame %>% # Coerces object to a dataframe
    unique 
  
  return(df)
}

TRIM_7B1 <- function(df) {
  
  df <- df %>% filter(FundType !="")
  return(df) 
}






#BASE_A_CLEANER <- function() {
#  
#  # Load up base a!
#  ADV_BASE_A <- df.Base
#  
#  # Drops any "801-" (malformed) SEC IDs that cannot be recovered
#  df.clean   <- ADV_BASE_A %>% filter(X1D != "801-" & X1D != "")  
#  df.toClean <- ADV_BASE_A %>% filter(X1D == "801-" | X1D == "") %>% select(-X1D) # Drops column X1D
#  cleanMap   <- ADV_BASE_A %>% select(X1D, X1A) %>% filter(X1D != "801-")  %>% unique
#  df.toClean <- inner_join(df.toClean, cleanMap, by="X1A")
#  ADV_BASE_A <- bind_rows(df.clean, df.toClean)
#  
#}
