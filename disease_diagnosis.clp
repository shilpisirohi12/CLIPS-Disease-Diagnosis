;------------------------------------------
; CREATED BY: GAUTAM SAINI and FNU SHILPI
;------------------------------------------

;------------------------------------------------------------------------------------------------------------------------------------------------------------
;INSTRUCTIONS TO EXECUTE
;------------------------
; From the CLIPS menu, File --> Load --> Select the file
; From the CLIPS menu, Execution --> Reset
; From the CLIPS menu, Execution --> Run
; Please note that for the proper execution of the program, it is required to execute (reset) command after loading the .clp file. Then excute (run) command.
; Also, for every execution, first execute (reset) and then execute (run) command
;-------------------------------------------------------------------------------------------------------------------------------------------------------------

;-------------------------------
;Title: DISEASE EXPERT SYSTEM
;-------------------------------


;--------------------------------------------------------------------------------------------------------------------------------------------
;DESCRIPTION 
;------------
This is a Diagnosis Expert System to tell the user if there is a chance that the user might be infected with the given disease
;User will be asked certain questions about the symptoms. Based on the user answers the system will diagnose.
;--------------------------------------------------------------------------------------------------------------------------------------------


;--------------------
; CLASSES
;--------------------
(defclass PERSON
	(is-a USER)
	(role concrete)
	(slot analysis)
	(slot diseasetype))

(defclass DISEASE
	(is-a USER)
	(slot type1)
	(slot type2)	
	(slot analysis)
	(slot suggestion ))
	
(defclass GENERALHEALTH
	(is-a USER)	
	(slot analysis)
	(slot suggestion ))	

;-------------------------------
; DEFAULT INSTANCES
;-------------------------------

(definstances PERSON-INSTANCES
	(client of PERSON))

(definstances DISEASE-INSTANCES
	(which_disease of DISEASE))
	
(definstances GENERALHEALTH-INSTANCES
	(which_suggestion of GENERALHEALTH))	

;--------------------------------------------
; INITIAL USER INPUTS AND VALIDATIONS
;--------------------------------------------

(deffunction user-input-validation (?question $?valid-input)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?valid-input)) do
      (printout t "Please enter a valid input as mentioned in the question!" crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)
   
; RULE TO GET THE USER INPUT
(defrule GetDIAGNOSED(declare (salience 10))
    =>
    (printout t crlf)
    (printout t "--------------------------------------------------------------------------------------------------------" crlf)
    (printout t "------------------------ WELCOME TO THE DISEASE EXPERT ------------------------" crlf)
    (printout t "--------------------------------------------------------------------------------------------------------" crlf)
    (printout t crlf)    
    (send [client] put-diseasetype
    (user-input-validation "Did you want diagnosis for a disease or looking for Healthy Lifestyle Advice? (cancer/diabetes/flu/pandemic/advice):  "cancer diabetes flu pandemic advice)))
   

;---------------------------------------------------------------------
; RULES OF THE EXPERT SYSTEM TO PROVIDE GENERAL HEALTHY ADVICE
;---------------------------------------------------------------------

; RULE FOR advice
(defrule give_advice
	?ins <- (object (is-a PERSON) (diseasetype advice))
	=> 
	(printout t crlf crlf crlf)
	(printout t "Are you looking for the suggestion to set a healthy lifestyle? " crlf crlf)
   	(send [which_suggestion] put-analysis
    (user-input-validation "Enter your response(yes/ no): " yes no)))

;---------------------------------------------------------
; RULES OF THE EXPERT SYSTEM TO DIAGNOSE THE DISEASE
;---------------------------------------------------------

; RULE FOR cancer
(defrule diagnose_cancer
	?ins <- (object (is-a PERSON) (diseasetype cancer))
	=> 
	(printout t crlf crlf crlf)
	(printout t "Please think if you have these symptoms (Feeling tired, Having pain, Losing weight for no reason that you know of)." crlf crlf)
   	(send [which_disease] put-analysis
    (user-input-validation "Enter your response(yes/ no): " yes no)))
   	 
; RULE FOR Diabetes
(defrule diagnose_diabetes
	?ins <- (object (is-a PERSON) (diseasetype diabetes))
	=> 
	(printout t crlf crlf crlf)
	(printout t "Please think if you have these symptoms (Fatigue, Weight Loss or Gain, Excessive thirst and hunger, Frequent urination,Slow-healing wounds)." crlf crlf)
    (send [which_disease] put-analysis
  	(user-input-validation "Enter your response(yes/ no): " yes no)))
   	 
; RULE FOR Flu
(defrule diagnose_flu
	?ins <- (object (is-a PERSON) (diseasetype flu))
	=> 
	(printout t crlf crlf crlf)
	(printout t "Please think if you have these symptoms (Fever* or feeling feverish/chills, cough, sore throat)." crlf crlf)
	(send [which_disease] put-analysis
  	(user-input-validation "Enter your response(yes/ no): " yes no)))
   	 
; RULE FOR Pandemic
(defrule diagnose_pandemic
	?ins <- (object (is-a PERSON) (diseasetype pandemic))
	=> 
	(printout t crlf crlf crlf)
	(printout t "Please think if you have these symptoms (Fever* or feeling feverish/chills, cough, sore throat)." crlf crlf)
	(send [which_disease] put-analysis
  	(user-input-validation "Enter your response(yes/ no): " yes no)))
	
; RULE TO Diabetes analysis is YES
(defrule diabetes_type1
	(and ?ins <- (object (is-a DISEASE) (analysis yes))
	(object (is-a PERSON)(diseasetype diabetes)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " Type 1 diabetes --> is an autoimmune condition in which the body does not produce insulin " crlf)
	(printout t " because the body’s immune system attacks insulin-producing cells from the pancreas called beta cells" crlf crlf)
	(send [which_disease] put-type1
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))

; RULE TO Diabetes analysis is YES and type1 is NO
(defrule diabetes_type2
	(and ?ins <- (object (is-a DISEASE) (analysis yes) (type1 no))
	(object (is-a PERSON)(diseasetype diabetes)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " Type 2 diabetes --> is a condition in which cells cannot use blood sugar (glucose) efficiently for energy." crlf)
	(printout t " This occurs when blood sugar levels get too high over time, and the cells become insensitive or resistant to insulin " crlf crlf)
	(send [which_disease] put-type2
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))
	
; RULE TO Cancer analysis is YES
(defrule kidneyCancer_type1
	(and ?ins <- (object (is-a DISEASE) (analysis yes))
	(object (is-a PERSON)(diseasetype cancer)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " Kidney Cancer --> patients can see specific symptoms like a lump or mass in the kidney area or abdomen, " crlf)
	(printout t " blood in urine, lower back pain or pain in the side that does not go away." crlf crlf)
	(send [which_disease] put-type1
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))

; RULE TO Cancer analysis is YES and type1 is NO
(defrule lungCancer_type2
	(and ?ins <- (object (is-a DISEASE) (analysis yes) (type1 no))
	(object (is-a PERSON)(diseasetype cancer)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " Lung Cancer --> patients can see specific symptoms like coughing that gets worse or does not go away," crlf)
	(printout t " chest pain, Shortness of breath or coughing up blood." crlf crlf)
	(send [which_disease] put-type2
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))	
	
; RULE TO Flu analysis is YES
(defrule flu_type1
	(and ?ins <- (object (is-a DISEASE) (analysis yes))
	(object (is-a PERSON)(diseasetype flu)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " Seasonal Flu --> patients can see specific symptoms like runny or stuffy nose, headaches. " crlf)
	(printout t " Some people may have vomiting and diarrhea, though this is more common in children than adults." crlf crlf)
	(send [which_disease] put-type1
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))

; RULE TO Flu analysis is YES and type1 is NO
(defrule flu_type2
	(and ?ins <- (object (is-a DISEASE) (analysis yes) (type1 no))
	(object (is-a PERSON)(diseasetype flu)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " Swine Flu --> Have you been tested positive with any of these viruses(swine triple reassortant (tr) H1N1 influenza virus, trH3N2 virus, and trH1N2 virus)." crlf)
	(send [which_disease] put-type2
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))	
	


; RULE TO pandemic analysis is YES
(defrule pandemic_coronavirus
	(and ?ins <- (object (is-a DISEASE) (analysis yes))
	(object (is-a PERSON)(diseasetype pandemic)))
	=> 
	(printout t crlf crlf crlf)
	(printout t " CoronaVirus --> patients can see specific symptoms like shortness of breath, fever more than 100F, cough " crlf crlf)
	(send [which_disease] put-type1
    (user-input-validation "Have you ever been diagnose with this (yes/ no):  " yes no)))	
	
	
;------------------------------------
; SET THE VALUE FOR THE SUGGESSION	
;------------------------------------	

; RULE TO Suggest for Healthy Advice
(defrule suggest_healthy_advice
	(and ?ins <- (object (is-a GENERALHEALTH) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "Eat green vegetables, exercise daily for 30 minutes, sleep 8 hours, drink 3 liters of water"))


; RULE TO Suggest for Type 1 Diabetes
(defrule suggest_type1_diabetes
	(and ?ins <- (object (is-a PERSON) (diseasetype diabetes))
	(object (is-a DISEASE) (type1 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Type 1 Diabetes. Please visit doctor and take only doctor's prescribe medication"))

; RULE TO Suggest for Type 2 Diabetes
(defrule suggest_type2_diabetes
	(and ?ins <- (object (is-a PERSON) (diseasetype diabetes))
	(object (is-a DISEASE) (type2 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Type 2 Diabetes. Please visit doctor and take only doctor's prescribe medication"))		
	

	
	
; RULE TO Suggest for Kidney Cancer
(defrule suggest_type1_cancer
	(and ?ins <- (object (is-a PERSON) (diseasetype cancer))
	(object (is-a DISEASE) (type1 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Kidney Cancer. Please visit your doctor and take only doctor's prescribe medication"))

	

; RULE TO Suggest for Lung Cancer
(defrule suggest_type2_cancer
	(and ?ins <- (object (is-a PERSON) (diseasetype cancer))
	(object (is-a DISEASE) (type2 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Lung Cancer. Please visit doctor and take only doctor's prescribe medication"))		

; RULE TO Suggest for Type 1 Flu
(defrule suggest_type1_flu
	(and ?ins <- (object (is-a PERSON) (diseasetype flu))
	(object (is-a DISEASE) (type1 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Seasonal Flu. Please visit doctor and take only doctor's prescribe medication"))	

; RULE TO Suggest for Type 2 Flu
(defrule suggest_type2_flu
	(and ?ins <- (object (is-a PERSON) (diseasetype flu))
	(object (is-a DISEASE) (type2 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Swine Flu. Please visit doctor and take only doctor's prescribe medication"))	
	

; RULE TO Suggest for CoronaVirus
(defrule suggest_pandemic
	(and ?ins <- (object (is-a PERSON) (diseasetype pandemic))
	(object (is-a DISEASE) (type1 yes) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "There are high chances that you have Novel Coronavirus. Please visit your doctor and take only doctor's prescribe medication"))	


;--------------------------------------------------------------------------------------------------
; SET THE VALUE FOR THE SUGGESSION	WHEN USER IS ENTERING "NO" FOR ALL THE SUGGESTED SYMPTOMS
;--------------------------------------------------------------------------------------------------	

; RULE TO Suggest Not Known when analysis get user input as NO
(defrule suggest_not_analysis_advice
	(and ?ins <- (object (is-a PERSON) (diseasetype advice))
	(object (is-a GENERALHEALTH) (analysis no)))
	=> 
	(send [which_disease] put-suggestion "Have a great day!!"))	


; RULE TO Suggest Not Known when analysis get user input as NO
(defrule suggest_not_analysis_pandemic
	(and ?ins <- (object (is-a PERSON) (diseasetype pandemic))
	(object (is-a DISEASE) (type1 no) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "We don't have any suggestion for you currently. Please check back later"))	

	
; RULE TO Suggest Not Known when analysis get user input as NO
(defrule suggest_not_analysis
	(and ?ins <- (object (is-a PERSON))
	(object (is-a DISEASE) (analysis no)))
	=> 
	(send [which_disease] put-suggestion "We don't have any suggestion for you currently. Please check back later"))		

; RULE TO Suggest Not Known when Type 1 and 2 get user input as NO
(defrule suggest_not_known2
	(and ?ins <- (object (is-a PERSON))
	(object (is-a DISEASE) (type1 no) (type2 no) (analysis yes)))
	=> 
	(send [which_disease] put-suggestion "We don't have any suggestion for you currently. Please check back later"))		
	
;--------------------------------
; PRINTS THE FINAL SUGGESSION	
;--------------------------------

; RULE TO PRINT THE FINAL SUGGESTION
(defrule confirm_disease (declare (salience -1))
	(object (is-a DISEASE) (suggestion ?mov))
	=>
	(printout t crlf)
	(printout t "---------------------------------------------------------------------------------------------------------------------------------------------------" crlf)
    (printout t "The recommended Suggestion is: " ?mov crlf)
    (printout t "---------------------------------------------------------------------------------------------------------------------------------------------------" crlf crlf crlf crlf)
	(printout t "------------------------------------------------------THANK YOU--------------------------------------------------------" crlf crlf crlf crlf))
	 
	 

 
	 
