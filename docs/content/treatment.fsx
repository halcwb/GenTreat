
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

</br>
</br>

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

(**
Next a patient exhibits signs, these signs can be
used to define targets and/or conditions.

For example:

* Bloodpressure: target is keeping the bloodpressure
above 60, or as a condition, a bloodpressure of above
160 signifies high bloodpressure
* Painscore: whether or not and how much pain a patient
experiences
* Liverfailure: whether or not a patient has liverfailure
* Central Venous Line: whether or not a patient has a
central venous line.

*)

/// A Sign is an patient sympton or measurement
type Sign =
    | BloodPressure of int
    | PainScore of int
    | LiverFailure
    | CentralVenousLine

(**
So, a patient can have zero or more signs.
*)

/// The Sign list a Patient has
type PatientSigns = PatientSigns of Patient * Sign list

(**
A target is defigned by whether a target is reached
or not. Given a patient sign this is determined.
*)

/// The target to achieve by Treatment
/// For example: </br>
/// blood pressure > 60 mmHg </br>
/// pain score < 1
type Target  = Target of (Sign -> bool)

(**
A condition is defined as a state the patient is in.

For example, the condition liverfailure is defined by
the sign liverfailure is true, while the absence is
defined by liverfailure is false. Absence or presence
of the condition is checked by the combination of a target
and a boolean
*)

/// A Condition is a whether a Target
/// is met or not.
type Condition =
    | Has of Target
    | HasNot of Target

(**
The order is the actual treatment order that is used
to achieve a target. This can be a medication order but
also a order concerning nutrition, ventilation etc..
*)

/// The order by which the Target can be achieved
type Order = Order of string

(**
So, a treatment is the combination of a target and an order.
In which the order is used to achieve the target. In medical terms
the target can also be viewed as the indication. Only, the
indication can be low blood pressure, while a target can be
more specific, a blood pressure above 60.
*)

/// An Order to achieve a Target
type Treatment = Treatment of Target * Order

(**
The patient treatment is a patient with a list of treatments.
*)

/// The list of treatment for a patient
type PatientTreatment = PatientTreatment of Patient * Treatment list

(**
Using a combination of conditions and a treatment it can be
determined if a treatment should be applied. When all the
conditions in the condition list are met, and the treatment
target is not achieved, there is an indication to start the
treatment.

For example, a patien is in pain, has no liverfaiure, then
treatment with paracetamol can be commenced. However, when the
patient has liver failure, paracetamol might be the wrong choice.

Then next item in the protocol list can be morfine. So, when
the first step, paracetamol is not feasible, the next step,
morfine can be applied.
*)

/// A protocol is a list of Sign and a Treatment
/// ordered by precedence. If all Sign in the
/// Sign list is met and the Target is not met
/// then the Treatment can be applied.
type Protocol =  Protocol of (Condition list * Treatment) list

(**
In order to evaluate a patient using a protocol, the protocol
is needed, the patient signs and the current patient treatment.

The result of the evaluation is a new patient treatment, in
which according to patient signs and the protocol, treatment is
added or removed, or the patient treatment is unchanged.
*)

/// Evaluate a Patient whether all Targets
/// in Protocol are met and add Treatment if
/// Target is not met or remove if
/// Target is not met
type EvaluateProtocol =
    Evaluate of
        (Protocol -> PatientSigns -> PatientTreatment -> PatientTreatment)

(**
Implementation of the model
---------------------------
*)

(**
First we need a patient so we create one with a simple string as
the patient id.
*)

// Create patient
let createPatient s = s |> Patient

(**
Next we can create some patient signs.
*)

// Create signs
let createSign c s = s |> c
let createBloodPressure     = createSign BloodPressure
let createPainScore         = createSign PainScore

(**
And then we can connect those signs to a specific patient.
*)

// Create and add to PatientSigns
let createPatienSigns p = (p, []) |> PatientSigns
let addPatientSign sgn (PatientSigns(p, sgns)) = (p, sgn::sgns) |> PatientSigns

(**
For example a 'Test Patient', with no current signs.
*)

// Create a patient
let testPat = "Test Patient" |> createPatient
// Create patient signs for that patient
let patsigns = testPat |> createPatienSigns

(*** hide ***)
printfn "%A" patsigns

(**
Next we define some targets and conditions to construct protocols.
*)

// Create targets
let createTarget f = f |> Target
// Create treatment targets
let createBloodPressureTarget pred = createTarget (fun sgn ->
        match sgn with
        | BloodPressure bp -> bp |> pred
        | _ -> true)
let createPainScoreTarget pred = createTarget (fun sgn ->
    match sgn with
    | PainScore ps -> ps |> pred
    | _ -> true)

// Create conditions
let createCentralVenousLineCondition c : Condition =
    (createTarget (fun sgn ->
            match sgn with
            | CentralVenousLine -> true
            | _ -> false)) |> c
let createLiverFailureCondition c : Condition =
    (createTarget (fun sgn ->
            match sgn with
            | LiverFailure -> true
            | _ -> false)) |> c
let createBloodPressureCondition pred c : Condition =
    (createTarget (fun sgn ->
            match sgn with
            | BloodPressure bpl -> bpl |> pred
            | _ -> true)) |> c

(**
A blood pressure target could be the intention to keep the
blood pressure above 60.
*)

// Bloodpressure targets
let bpTargetLt60  = createBloodPressureTarget (fun bp -> bp > 60)

(**
Also, we want the patient to experience minimal or no pain as indicated
by a painscore of 1 or lower.
*)

// Pain target
let noPain = createPainScoreTarget (fun sc -> sc <= 1)

(**
There are also some conditions that can determine whether or not
a treatment should be applied.
*)

// Condition targets
let hasCvlCond  = createCentralVenousLineCondition Has
let noLvfCond   = createLiverFailureCondition HasNot
let bpCondSt160 = createBloodPressureCondition (fun bp -> bp < 160) Has

(**
Lets create a set of possible patient signs.
*)

// Signs
let bp40 = createBloodPressure 40
let pain = createPainScore 3
let cvl  = CentralVenousLine
let lvf  = LiverFailure

(**
We the can add these signs to the test patient.
*)

// Add patient signs
patsigns
|> addPatientSign bp40
|> addPatientSign pain
|> addPatientSign cvl
|> addPatientSign lvf

(**
We need to determine whether a target is met or a condition exists.
*)

// Check if target is met according to signs
let targetIsMet (Target(f)) sgns =
    sgns |> List.forall f

// Check if condition is met according to signs
let conditionIsMet cond sgns =
    match cond with
    | Has (Target(f))    -> sgns |> List.exists f
    | HasNot (Target(f)) ->
        if sgns |> List.isEmpty then true
        else sgns |> List.exists f |> not

(*** hide ***)
printfn "Target bp > 60 with bp: %A is met: %A"  [bp40] ([bp40] |> targetIsMet bpTargetLt60)
printfn "Target bp < 160 with bp: %A is met: %A" [bp40] ([bp40] |> conditionIsMet bpCondSt160)
printfn "Condition central venous line with cvl: %A is met: %A"      [cvl] ([cvl] |> conditionIsMet hasCvlCond)
printfn "Condition liver failure with no liverfailure: %A is met %A" [lvf] ([lvf] |> conditionIsMet noLvfCond)
printfn "Condition liver failure with no liverfailure: %A is met %A" [] ([] |> conditionIsMet noLvfCond)

(**
To define treatmet, orders are needed.
*)

// Create an Order
let createOrder ord = ord |> Order

// Create some test orders
let pcm  = "paracetamol"   |> createOrder
let morf = "morfine"       |> createOrder
let dopa = "dopamine"      |> createOrder
let nor  = "noradrenaline" |> createOrder

(**
Treatment is created by combining a target with an orderd.
*)

// Create a Treatment
let createTreatment targ ord = (targ, ord) |> Treatment

(**
A treatment is the same as some other treatment when the orders
are the same (targets could differ).
*)

let eqsTreatment (Treatment(_, ord1)) (Treatment(_, ord2)) = ord1 = ord2

(**
Here, treatment with paracetamol, morfine, dopamine and noradrenalin
are created.
*)

// Create treatement examples
let pcmTreat  = pcm  |> createTreatment noPain
let morfTreat = morf |> createTreatment noPain
let dopaTreat = dopa |> createTreatment bpTargetLt60
let norTreat  = nor  |> createTreatment bpTargetLt60

(**
To create protocol, a list of conditions is needed along
with a treatment.
*)

// Create and add to a Protocol
let createProtocol cnds trt = [(cnds, trt)] |> Protocol
let nextStepInProtocol cnds trt (Protocol(trts)) =
    [(cnds, trt)] |> List.append trts |> Protocol

(**

A pain protocol could look like this:

1. The first step is paracetamol, if no liver failure present
and the patient is in pain (the target)
2. The next step is to add or replace with morfine.

*)

// Create a pain protocol
let painProtocol =
    pcmTreat
    // Give pcm if no liver failure and pain
    |> createProtocol [noLvfCond]
    // Give morfine if still pain
    |> nextStepInProtocol [] morfTreat

(**
A blood pressure protocol looks like:

1. First start with dopammine when the blood pressure
is lower than 60, a contraindication is a bloodpressure above 160
2. Next add noradrenalin if the patient has a central venous line

*)

// Create a blood pressure protocol
let bpProtocol =
    dopaTreat
    // Start with dopa if bp < 60 but no bp > 160
    |> createProtocol [bpCondSt160]
    // Add nor if cvl and no bp > 160
    |> nextStepInProtocol [hasCvlCond; bpCondSt160] norTreat

(**
A patient can be associated with a list of treatment. Treatment
can be added an removed from that list.
*)

// Create and add to patient treatment
let createPatientTreatment testPat trts =
    (testPat, trts) |> PatientTreatment
// Add the treatment if not allready there
let addPatientTreatment trt (PatientTreatment(testPat, trts)) =
    if trts |> List.exists (eqsTreatment trt) then
        (testPat, trts) |> PatientTreatment
    else (testPat, trt::trts) |> PatientTreatment
// Remove the treatment if there
let removePatientTreatment trt (PatientTreatment(testPat, trts)) =
    (testPat, trts |> List.filter((eqsTreatment trt) >> not))
    |> PatientTreatment

// Test patient treatment
let patTreatm = createPatientTreatment testPat []

(*** hidden ***)
let printTreatment (PatientTreatment(pat, trts)) =
    let n = let (Patient n) = pat in n
    printfn "------------------------"
    printfn ""
    printfn "Treatment for: %s" n
    for trt in trts do
        let ord = let (Treatment(_, ord)) = trt in ord
        let s = let (Order(s)) = ord in s
        printfn "- %s" s
    printfn ""
    printfn "------------------------"


(**
Finally, the algorythm to evaluate the patient treatment
according to a protocol.
*)

// Evaluate a protocols
let evaluateProtocol prot (PatientSigns(_, sgns)) patTr =
    let rec eval (Protocol(xs)) acc =
        match xs with
        | [] -> acc
        | h::rest ->
            // The list of conditions for the treatment
            let cnds = h |> fst
            // The treatment to consider
            let trt  = h |> snd
            // The treatment target
            let trg = let (Treatment(trg, _)) = trt in trg
            // Check whether all conditions are met
            // according to the patient signals
            let condMet =
                cnds
                |> List.forall (fun cnd ->
                    sgns |> conditionIsMet cnd)
            // Check whether target is met according
            // to the patient signals
            let targetMet = sgns |> targetIsMet trg

            printfn "Conditions met: %b, TargetMet: %b" condMet targetMet
            printfn "For treatment: %A" (let (Treatment(_, ord)) = trt in ord)

            match condMet, targetMet with
            // condition met, target not met
            | true, false ->
                // First check if treatment is not allready there
                let hasNotTrt =
                    let trts = let (PatientTreatment(_, trts)) = acc in trts
                    trts |> List.exists(eqsTreatment trt) |> not
                if hasNotTrt then
                    // Add the treatment
                    acc
                    |> addPatientTreatment trt
                    // And stop the evaluation
                    |> eval ([] |> Protocol)
                // Otherwise continue with the rest
                else acc |> eval (rest |> Protocol)
            // conditions met, but target also met
            | true, true
            // or conditions not met
            | false, _ ->
                // Remove the treatment if there
                acc
                |> removePatientTreatment trt
                // And continue the evaluation
                |> eval (rest |> Protocol)

    eval prot patTr

(**
Test the implementation
-----------------------
*)


(**
The implementation is tested with some examples

The first example tests what happens when the pain
protocol is applied to a patient without any signs.
*)

// Evaluate pain protocol for patient without signs
// Expect: no treatment
patTreatm
|> evaluateProtocol painProtocol patsigns
|> printTreatment

// Evaluate pain protocol for patient with pain
// Expect: paracetamol
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> printTreatment

// Evaluate pain protocol for patient with pain and allready paracetemol
// Expect: paracetamol and morfine
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> evaluateProtocol painProtocol (patsigns |> addPatientSign pain)
|> printTreatment

// Evaluate pain protocol for patient with pain but also liver failure
// Expect: morfine
patsigns
|> addPatientSign pain
|> addPatientSign lvf
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> printTreatment

// First the patient has pain, then also liver failure
// Evaluate pain protocol for patient with pain but also liver failure
// Expect: first paracatamol, then only morfine
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> (fun pt -> pt |> printTreatment; pt)
|> evaluateProtocol painProtocol (patsigns
        |> addPatientSign pain
        |> addPatientSign lvf)
|> printTreatment

// Evaluate pain protocol for patient with low blood pressure
// Expect: No treatment
patsigns
|> addPatientSign bp40
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> printTreatment

// Evaluate pain protocol for patient with pain and low blood pressure
// Expect: paracetamol
patsigns
|> addPatientSign bp40
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluateProtocol painProtocol signs)
|> printTreatment

// Evaluate pain protocol and blood pressure protocol
// for patient with pain and low blood pressure
// Expect: paracetamol and dopamine
patsigns
|> addPatientSign bp40
|> addPatientSign pain
|> (fun signs ->
        patTreatm
        |> evaluateProtocol painProtocol signs
        |> evaluateProtocol bpProtocol signs)
|> printTreatment

// Evaluate a blood pressure protocol for a patient
// with low blood pressure, which still has low blood
// pressure during the second evaluation but has no
// central venous line
// Expect: dopamine and then still only dopamine
patsigns
|> addPatientSign bp40
|> (fun signs ->
        patTreatm
        |> evaluateProtocol bpProtocol signs
        |> (fun pt -> pt |> printTreatment; pt)
        |> evaluateProtocol bpProtocol signs
        |> printTreatment)

// Now the patient has a central venous line
// Expect: dopamine, then dopamine and noradrenalin
patsigns
|> addPatientSign bp40
|> addPatientSign cvl
|> (fun signs ->
        patTreatm
        |> evaluateProtocol bpProtocol signs
        |> (fun pt -> pt |> printTreatment; pt)
        |> evaluateProtocol bpProtocol signs
        |> printTreatment)
