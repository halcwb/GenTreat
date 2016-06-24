
(**
Top Down Design of Treatment
============================

This literate script defines the top down design of a library
that facilitates treatment of patients.

Patient treatment is a continuous loop of the following processes:

1. Get the patient (and patient data)
2. Get decision support (drug repositories, protocols, etc...)
3. Determine the treatment targets
4. Add or remove treatment from existing patient treatment plan
5. Plan the treatment
6. Administer treatment
7. Evaluate the treatment (is essentially step 1 - 4)

</br>
</br>

** The Medication Cycle **
<img src="https://docs.google.com/drawings/d/1NP8mq1Pu6UYbBp51DUmkQoo8tz3769VDbiEoDoPQ2IE/pub?w=744&amp;h=520">

</br>
</br>

GenTreat combines in the *medication cycle* decision support with treatment
evaluation and order generation.

</br>
</br>

**Contents:**

* [The domain model](#The-domain-model)
* [Implementation of the Model](#Implementation-of-the-model)
* [Test the implementation](#Test-the-implementation)

</br>
</br>
*)

(**
The domain model
----------------
*)

(**
A patient type is needed to model the patient that is
receiving treatment. The patient type needs to contain
data relevant to identify a patient for treatment. For
now a simple string is used.
*)
/// The patient who is receiving treatment
type Patient = Patient of string

(**
Next a patient exhibits signs, these signs can be
used to define targets and/or conditions.
</br>
For example:

* Bloodpressure: target is keeping the bloodpressure
above 60, or as a condition, a bloodpressure of above
160 signifies high bloodpressure
* Painscore: whether or not and how much pain a patient
experiences
* Liverfailure: whether or not a patient has liverfailure
* Central Venous Line: whether or not a patient has a
central venous line.

But signs can also be generalized as any patient data that
could influence treatment, like age, weight, past medical
history, etc..
</br>
*)

/// A Sign is any patient sympton or measurement
/// relevant to treatment
type Sign =
    | BloodPressure of int
    | PainScore of int
    | LiverFailure
    | CentralVenousLine

(**
A patient is associated with a list of signs.
A patient can have zero or more signs.
*)

/// The list of signs a patient exhibits
type PatientSigns = PatientSigns of Patient * Sign list

(**
A target is defigned by whether a target is reached,
or not. Given a patient sign this is determined by a
function that returnes a true or false. When the target
is a blood pressure above 60, then the function will
evaluate the blood pressure sign an return true if the
blood pressure is above 60. If the sign is any other sign
it will also return true.

So, a treatment indication is explicitly a target that is not met.
*)

/// The Target to achieve by treatment
/// For example:
/// blood pressure > 60 mmHg
/// pain score < 1
type Target  = Target of (Sign -> bool)

(**
A condition is defined as a state the patient should
have or not have. This can be modelled by the presence or
absence of a target.

For example, the condition liverfailure is defined by
the sign Liverfailure. Absence or presence of a condition
is determined by matching a sign with the target.

In the case of liverfailure presence of a sign is enough.
But to describe for example low blood pressure, as defined
by the sign blood pressure, which is a number, a target is
needed.
*)

/// A Condition is determined by
/// the result of a Target
type Condition =
    | Has of Target
    | HasNot of Target

(**
The order is the actual treatment order that is used
to achieve a target. This can be a medication order but
can also be an order concerning nutrition, ventilation etc..
*)

/// The Order by which the Target can be achieved
type Order = Order of string

(**
So, a treatment is the combination of a target and an order,
in which the order is used to achieve the target. In medical terms
the target can also be viewed as the indication. Only, the
indication can be low blood pressure, while a target can be
more specific, a blood pressure above 60, that can be evaluated.
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
The protocol for paracetamol can be defined by the condition *has
not liverfailure* and the target *pain score > 1*

The next item in the protocol list could, for example, be
morfine. So, when the first step, paracetamol is not feasible,
the next step, morfine can be applied.
*)

/// A protocol is a list of Conditions and a Treatment
/// ordered by precedence. If all Conditions in the
/// Condition list are met and the Target is not met
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
/// Target is not met and Conditions are met
/// or remove the Treatment if the Target is
/// met or Conditions are not met.
type EvaluateProtocol =
        (Protocol -> PatientSigns -> PatientTreatment -> PatientTreatment)

(**
Implementation of the model
---------------------------
The following describes a sample implementation of the domain model.
*)

(**
First we need a patient so we create one with a simple string as
the patient id.
*)

// Create patient
let createPatient s = s |> Patient

(**
Next we have to create some patient signs.
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
Targets and conditions are needed to construct protocols. In this
case we create a blood pressure and pain target. And we create a
liver failure condition and a central venous line condition.
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
A specific blood pressure target could be the intention to keep the
blood pressure above 60. So the *target function* checks whether a
blood pressure sign is above 60. When the blood pressure falls below
60 the target is not met and the function returns false.
*)

// Bloodpressure targets
let bpTargetLt60  = createBloodPressureTarget (fun bp -> bp > 60)

(**
Also, we want the patient to experience minimal or no pain as indicated
by a painscore of 1 or lower. Note that the target can be modified by
setting a threshold.
*)

// Pain target
let noPain = createPainScoreTarget (fun sc -> sc <= 1)

(**
There are also some conditions that can determine whether or not
a treatment should be applied.
For example:

*  When the patient has a central venous line, noradrenaline can
be given.
* When the patient has no liverfailure, paracetamol can be given.
* When the patient has a blood pressure below 160, inotropics are
not contra-indicated.

Note, that these are just examples, not real medical constructs.
*)

// Condition targets
let hasCvlCond  = createCentralVenousLineCondition Has
let noLvfCond   = createLiverFailureCondition HasNot
let bpCondSt160 = createBloodPressureCondition (fun bp -> bp < 160) Has

(**
Lets create a set of possible patient signs a patient can show.
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
We now have patient signs, targets and conditions. But we need to
determine whether a target is met or a condition exists. The below
functions evaluate a list of signs and return whether a target or
condition is met.
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
To define treatment, orders are needed to achieve the treatment
target. In this case we create parecetamol, morfine, dopamine and
noradrenaline orders.
*)

// Create an Order
let createOrder ord = ord |> Order

// Create some test orders
let pcm  = "paracetamol"   |> createOrder
let morf = "morfine"       |> createOrder
let dopa = "dopamine"      |> createOrder
let nor  = "noradrenaline" |> createOrder

(**
Treatment is created simply by combining a target with an order.
*)

// Create a Treatment
let createTreatment targ ord = (targ, ord) |> Treatment

(**
A treatment is the same as some other treatment when the orders
are the same (targets could differ). But the comparison is needed
to determine if the patient allready recieves treatment, i.e. order.
*)

let eqsTreatment (Treatment(_, ord1)) (Treatment(_, ord2)) = ord1 = ord2

(**
Here, treatment with paracetamol, morfine, dopamine and noradrenalin
are created. Paracetamol and morfine are used to relieve pain.
Dopamine and noradrenalin can be used to maintain the blood pressure
above 60.
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
2. The next step is to add morfine or replace paracetamol with morfine.

Note that in this case morfine doesn't have a condition other than
that first paracetamol should be evaluated.
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
and, again, the bloodpressure is not above 160

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
can be added an removed from that list. The patient treatment is
the subject and result of an evaluation with a protocol.
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

// The treatment of the test patient example.
let patTreatm = createPatientTreatment testPat []

(*** hide ***)
let printTreatment f (PatientTreatment(pat, trts)) =
    let n = let (Patient n) = pat in n
    sprintf "------------------------" |> f
    sprintf "" |> f
    sprintf "Treatment for: %s" n |> f
    for trt in trts do
        let ord = let (Treatment(_, ord)) = trt in ord
        let s = let (Order(s)) = ord in s
        sprintf "- %s" s |> f
    sprintf "" |> f
    sprintf "------------------------" |> f


(**
Finally, patient treatment can be evaluated with a protocol
that uses patient signs to determine wheter conditions and
targets are met.

The algorythm will:

* Add a treatment when conditions *are met* and the target
is *not met*
* Remove a treatment when either conditions *are not met*
and/or the target *is met*

When a treatment allready is applied, the next item in the
protocol is used.
*)

// Evaluate a protocols
let evaluateProtocol f prot (PatientSigns(_, sgns)) patTr =
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

            sprintf "Conditions met: %b, TargetMet: %b" condMet targetMet
            |> f
            sprintf "For treatment: %A" (let (Treatment(_, ord)) = trt in ord)
            |> f

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
The following code blocks test the implementation by examples.
In each code block the result of the evaluation is shown
as it appears in the *fsi* (code output window).
*)


(*** hide ***)
let f = printfn "%s" // printfn "// [fsi: %s ]"
let evaluate = evaluateProtocol f
let print    = "" |> f; printTreatment f

(**
The first example tests what happens when the pain
protocol is applied to a patient without any signs.

The output reads that conditions are met, but
targets also are met, so, there is no indication
for treatment.
*)

// Evaluate pain protocol for patient without signs
// Expect: no treatment
patTreatm
|> evaluate painProtocol patsigns
|> print
//
// [fsi: Conditions met: true, TargetMet: true ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: true ]
// [fsi: For treatment: Order "morfine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi:  ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
However, if a patient is in pain, given a painscore
of 3, then paracetamol is added to the treatment in
absence of liverfailure.
*)

// Evaluate pain protocol for patient with pain
// Expect: paracetamol
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluate painProtocol signs)
|> print
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - paracetamol ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
When the patient still has pain, and also has paracetamol,
the next step is to add morfine.
*)

// Evaluate pain protocol for patient with pain and allready paracetemol
// Expect: paracetamol and morfine
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluate painProtocol signs)
|> evaluate painProtocol (patsigns |> addPatientSign pain)
|> print
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "morfine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - morfine ]
// [fsi: - paracetamol ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
A patient with pain but also liverfailure will skip the
paracetamol step but continue with morfine.
*)

// Evaluate pain protocol for patient with pain but also liver failure
// Expect: morfine
patsigns
|> addPatientSign pain
|> addPatientSign lvf
|> (fun signs -> patTreatm |> evaluate painProtocol signs)
|> print
//
// [fsi: Conditions met: false, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "morfine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - morfine ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
When the patient signs evolve from first pain and then pain,
but also liverfailure, at first the treatment is paracetamol,
but given liverfailure paracetamol is removed to be replaced
with morfine.
*)

// First the patient has pain, then also liver failure
// Evaluate pain protocol for patient with pain but also liver failure
// Expect: first paracatamol, then only morfine
patsigns
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluate painProtocol signs)
|> (fun pt -> pt |> print; pt)
|> evaluate painProtocol (patsigns
        |> addPatientSign pain
        |> addPatientSign lvf)
|> print
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - paracetamol ]
// [fsi:  ]
// [fsi: ------------------------ ]
// [fsi: Conditions met: false, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "morfine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - morfine ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
A patient that has a low blood pressure, but is evaluated by
the pain protocol, will not receive treatment.
*)

// Evaluate pain protocol for patient with low blood pressure
// Expect: No treatment
patsigns
|> addPatientSign bp40
|> (fun signs -> patTreatm |> evaluate painProtocol signs)
|> print
//
// [fsi: Conditions met: true, TargetMet: true ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: true ]
// [fsi: For treatment: Order "morfine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
If that patient not only has blood pressure, but also has
pain, then, given the pain protocol, paracetamol is added
to the treatment, but blood pressure remains untreated.
*)

// Evaluate pain protocol for patient with pain and low blood pressure
// Expect: paracetamol
patsigns
|> addPatientSign bp40
|> addPatientSign pain
|> (fun signs -> patTreatm |> evaluate painProtocol signs)
|> print
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - paracetamol ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
When both the pain and the blood pressure protocol are
evaluated, then the patient will receive both paracetamol
and dopamine.
*)

// Evaluate pain protocol and blood pressure protocol
// for patient with pain and low blood pressure
// Expect: paracetamol and dopamine
patsigns
|> addPatientSign bp40
|> addPatientSign pain
|> (fun signs ->
        patTreatm
        |> evaluate painProtocol signs
        |> evaluate bpProtocol signs)
|> print
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "paracetamol" ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "dopamine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - dopamine ]
// [fsi: - paracetamol ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
The patient with low blood pressure but no central venous
line will not proceed to noradrenaline in the next evaluation.
*)

// Evaluate a blood pressure protocol for a patient
// with low blood pressure, which still has low blood
// pressure during the second evaluation but has no
// central venous line
// Expect: dopamine and then still only dopamine
patsigns
|> addPatientSign bp40
|> (fun signs ->
        patTreatm
        |> evaluate bpProtocol signs
        |> (fun pt -> pt |> print; pt)
        |> evaluate bpProtocol signs
        |> print)
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "dopamine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - dopamine ]
// [fsi:  ]
// [fsi: ------------------------ ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "dopamine" ]
// [fsi: Conditions met: false, TargetMet: false ]
// [fsi: For treatment: Order "noradrenaline" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - dopamine ]
// [fsi:  ]
// [fsi: ------------------------ ]

(**
Once the patient has a central venous line, noradrenaline
can be added to the treatment.
*)

// Now the patient has a central venous line
// Expect: dopamine, then dopamine and noradrenalin
patsigns
|> addPatientSign bp40
|> addPatientSign cvl
|> (fun signs ->
        patTreatm
        |> evaluate bpProtocol signs
        |> (fun pt -> pt |> print; pt)
        |> evaluate bpProtocol signs
        |> print)
//
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "dopamine" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - dopamine ]
// [fsi:  ]
// [fsi: ------------------------ ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "dopamine" ]
// [fsi: Conditions met: true, TargetMet: false ]
// [fsi: For treatment: Order "noradrenaline" ]
// [fsi: ------------------------ ]
// [fsi:  ]
// [fsi: Treatment for: Test Patient ]
// [fsi: - noradrenaline ]
// [fsi: - dopamine ]
// [fsi:  ]
// [fsi: ------------------------ ]
