#load "load-project-release.fsx"


module GenTreat =

    type Patient = Patient

    type Treatment = Treatment

    type Target = Target

    type PatientTreatment = PatientTreatment

    type Condition = Condition

    type Protocol =  Target * (Condition * Treatment list) list

    type Treat = Treatment * Target * PatientTreatment -> Treatment

    type Evaluate = Patient * PatientTreatment * Target list * Protocol list -> Treatment list
