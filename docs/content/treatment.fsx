
(**
Top Down Design of Treatment
============================

This literate script defines the top down design of a library
that facilitates treatment of patients.

Patient treatment is a continuous loop of the following steps:

1. Get the patient (and patient data)
2. Get decision support (drug repositories, protocols, etc...)
3. Determine the treatment targets
4. Add or remove treatment from existing patient treatment
5. Plan the treatment
6. Administer treatment
7. Evaluate the treatment (is essentially step 1 - 4)

** The Medication Cycle **
<img src="https://docs.google.com/drawings/d/1NP8mq1Pu6UYbBp51DUmkQoo8tz3769VDbiEoDoPQ2IE/pub?w=744&amp;h=520">

*)

(**
The domain model
----------------
*)

(**
A patient type is needed to model the patient that is
receiving treatment.
*)
/// The patient who is receiving treatment
type Patient = Patient of string

/// A Sign is an patient sympton or measurement
type Sign =
    | BloodPressure of int
    | PainScore of int
    | LiverFailure of bool
    | CentralVenousLine of bool

/// The Sign list a Patient has
type PatientSigns = PatientSigns of Patient * Sign list

/// The target to achieve by Treatment
/// For example: </br>
/// blood pressure > 60 mmHg </br>
/// pain score < 1
type Target  = Target of (Sign -> bool)

/// A Condition is a whether a Target
/// is met or not.
type Condition = Condition of Target * bool

/// The order by which the Target can be achieved
type Order = Order of string

/// An Order to achieve a Target
type Treatment = Treatment of Target * Order

/// The list of treatment for a patient
type PatientTreatment = PatientTreatment of Patient * Treatment list

/// Add a treatment to the patient treatment
type Treat = Treat of (PatientTreatment -> Treatment -> PatientTreatment)

/// A protocol is a list of Sign and a Treatment
/// ordered by precedence. If all Sign in the
/// Sign list is met and the Target is not met
/// then the Treatment can be applied.
type Protocol =  Protocol of (Condition list * Treatment) list

/// Evaluate a Patient whether all Targets
/// in Protocol are met and add Treatment if
/// Target is not met or remove if
/// Target is not met
type EvaluateProtocol = Evaluate of (Protocol -> PatientSigns -> PatientTreatment -> PatientTreatment)

/// Evaluate a Patient whether a Target
/// is met and add Treatment if
/// Target is not met or remove if
/// Target is not met
type EvaluateTarget = Evaluate of (PatientSigns -> Target -> PatientTreatment -> PatientTreatment)

// Create patient
let createPatient s = s |> Patient

// Create signs
let createSign c s = s |> c
let createBloodPressure = createSign BloodPressure
let createPainScore         = createSign PainScore
let createLiverFailure      = createSign LiverFailure
let createCentralVenousLine = createSign CentralVenousLine

// Create and add to PatientSigns
let createPatienSigns p = (p, []) |> PatientSigns
let addPatientSign sgn (PatientSigns(p, sgns)) = (p, sgn::sgns) |> PatientSigns

// Create a patient
let pat = "Test Patient" |> createPatient
// Create patient signs for that patient
let patsigns = pat |> createPatienSigns

printfn "%A" patsigns

// Create targets
let createTarget f = f |> Target
// Create treatment targets
let createBloodPressureTarget pred = createTarget (fun sgn -> match sgn with | BloodPressure bp -> bp |> pred | _ -> true)
let createPainScoreTarget pred     = createTarget (fun sgn -> match sgn with | PainScore ps     -> ps |> pred | _ -> true)
// Create conditions
let createCentralVenousLineCondition b =
    (createTarget (fun sgn -> match sgn with | CentralVenousLine cvl -> cvl | _ -> false), b) |> Condition
let createLiverFailureCondition b =
    (createTarget (fun sgn -> match sgn with | LiverFailure lvf -> lvf | _ -> false), b) |> Condition
let createBloodPressureCondition pred b =
    (createTarget (fun sgn -> match sgn with | BloodPressure bpl -> bpl |> pred | _ -> true), b) |> Condition

// Bloodpressure targets
let bpTargetLt60  = createBloodPressureTarget (fun bp -> bp > 60)

// Pain target
let noPain = createPainScoreTarget (fun sc -> sc <= 1)

// Condition targets
let hasCvlCond = createCentralVenousLineCondition true
let noLvfCond = createLiverFailureCondition false
let bpCondSt160 = createBloodPressureCondition (fun bp -> bp < 160) true

// Signs
let bp40 = createBloodPressure 40
let pain = createPainScore 3
let cvl = createCentralVenousLine true
let lvf = createLiverFailure true

// Add patient signs
patsigns
|> addPatientSign bp40
|> addPatientSign pain
|> addPatientSign cvl
|> addPatientSign lvf

// Check if target is met
let isTargetMet (Target(f)) sgn = sgn |> f

// Check if condition is met
let isConditionMet (Condition(Target(f), b)) sgn = sgn |> f = b

printfn "Target bp > 60 with bp: %A is met: %A"  bp40 (bp40 |> isTargetMet bpTargetLt60)
printfn "Target bp < 160 with bp: %A is met: %A" bp40 (bp40 |> isConditionMet bpCondSt160)
printfn "Condition central venous line with cvl: %A is met: %A"      cvl (cvl |> isConditionMet hasCvlCond)
printfn "Condition liver failure with no liverfailure: %A is met %A" lvf (lvf |> isConditionMet noLvfCond)

// Create an Order
let createOrder ord = ord |> Order

// Create some test orders
let pcm  = "paracetamol"   |> createOrder
let morf = "morfine"       |> createOrder
let dopa = "dopamine"      |> createOrder
let nor  = "noradrenaline" |> createOrder

// Create a Treatment
let createTreatment targ ord = (targ, ord) |> Treatment

let eqsTreatment (Treatment(_, ord1)) (Treatment(_, ord2)) = ord1 = ord2

// Create treatement examples
let pcmTreat  = pcm  |> createTreatment noPain
let morfTreat = morf |> createTreatment noPain
let dopaTreat = dopa |> createTreatment bpTargetLt60
let norTreat  = nor  |> createTreatment bpTargetLt60

// Create and add to a Protocol
let createProtocol cnds trt = [(cnds, trt)] |> Protocol
let nextStepInProtocol cnds trt (Protocol(trts)) =
    [(cnds, trt)] |> List.append trts |> Protocol

// Create a pain protocol
let painProtocol =
    pcmTreat
    |> createProtocol [noLvfCond]      // Give pcm if no liver failure and pain
    |> nextStepInProtocol [] morfTreat // Give morfine if still pain

// Create a blood pressure protocol
let bpProtocol =
    dopaTreat
    |> createProtocol [bpCondSt160]                          // Start with dopa if bp < 60 but no bp > 160
    |> nextStepInProtocol [hasCvlCond; bpCondSt160] norTreat // Add nor if cvl and no bp > 160

// Create and add to patient treatment
let createPatientTreatment pat trts = (pat, trts) |> PatientTreatment
let addPatientTreatment trt (PatientTreatment(pat, trts)) =
    if trts |> List.exists (eqsTreatment trt) then (pat, trts) |> PatientTreatment
    else (pat, trt::trts) |> PatientTreatment

// Test patient treatment
let patTreatm = createPatientTreatment pat []

// Evaluate a protocols
let evaluateProtocol prot (PatientSigns(_, sgns)) patTr =
    let rec eval (Protocol(xs)) acc =
        match xs with
        | [] -> acc
        | h::rest ->
            let cnds = h |> fst
            let trt  = h |> snd
            let trg = let (Treatment(trg, _)) = trt in trg
            let condMet = sgns |> List.forall (fun sgn ->
                cnds |> List.forall (fun cnd ->
                    sgn |> isConditionMet cnd))
            let targetNotMet = sgns |> List.forall (fun sgn ->
                sgn |> isTargetMet trg) |> not
            let hasNotTrt =
                let trts = let (PatientTreatment(_, trts)) = acc in trts
                trts |> List.exists(eqsTreatment trt) |> not
            if condMet && targetNotMet && hasNotTrt then
                acc
                |> addPatientTreatment trt
                |> eval ([] |> Protocol)
            else
                acc |> eval (rest |> Protocol)

    eval prot patTr

// Evaluate pain protocol for patient without signs
patTreatm
|> evaluateProtocol painProtocol patsigns

// Evaluate pain protocol for patient with pain
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)

// Evaluate pain protocol for patient with pain and allready pcm
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> evaluateProtocol painProtocol (patsigns |> addPatientSign pain)

// Evaluate pain protocol for patient with pain but also liver failure
patsigns
|> addPatientSign pain
|> addPatientSign lvf
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
