// ----------------------------------------------------------------------------
// 05 - Arguments and sequencing of expressions
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeDataObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject slots special = 
  { Code = None; Special = Some special; Slots = slots }

let makeSlot n contents = 
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot n contents = 
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

// NOTE: Implemented in step #2
let addSlot n contents obj = 
  let slot = makeSlot n contents
  obj.Slots <- slot :: obj.Slots 

let addParentSlot n contents obj = 
  let parentSlot = makeParentSlot n contents
  obj.Slots <- parentSlot :: obj.Slots

let cloneObject obj =  
  let clone: Objekt = { Code = obj.Code; Special = obj.Special; Slots = obj.Slots }
  clone

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup msg obj : list<Objekt * Slot> = 
  let slot = List.tryFind (fun s -> s.Name = msg) obj.Slots
  match slot with
  | Some s -> [(obj, s)]
  | None -> obj.Slots 
          |> List.filter (fun s -> s.IsParent) 
          |> List.collect (fun ps -> lookup msg ps.Contents)

and parentLookup msg obj : list<Objekt * Slot> = failwith "implemented in step 2"

let rec eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
  | Some code ->
    let activationRecord = cloneObject code
    activationRecord |> addParentSlot "self*" instance
    activationRecord |> addParentSlot "__args*" args

    match code.Special with
    | Some special ->
      match special with
      | Native fn -> 
        fn(activationRecord)
      | _ -> failwith "unsupported special"
    
    | None -> 
      let args = makeDataObject [makeSlot "activation" activationRecord]
      code |> send "eval" args
  | None -> slotValue

and send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  let matchedSlots = lookup msg instance |> List.map (fun (obj, slot) -> slot)
  //System.Console.WriteLine(msg + " - " + matchedSlots.Length.ToString())
  match matchedSlots with 
  | slot::[] -> eval slot.Contents args instance
  | _ -> failwith "message not understood"

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

let empty = {Code = None; Special = None; Slots = []}
let printCode = makeNativeMethod (fun arcd ->
  let s = send "value" empty arcd
  match s.Special with
  | Some special ->
    match special with
    | String str -> 
      System.Console.WriteLine(str)
      empty
    | _ -> failwith "expected special to be string"
  | None -> failwith "expected special"
)

let stringPrototype = makeDataObject [
  makeSlot "print" printCode  
]
let makeString s = 
  makeDataObject [ 
    makeSlot "value" (makeSpecialObject [] (String s)) 
    makeParentSlot "__string*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Cloning and assignments
// ----------------------------------------------------------------------------

let cloneMethod = makeNativeMethod (fun arcd -> 
  // NOTE: The activation record contains a slot 'self*' which is the
  // target object. Use lookup to get it, clone it & return it!
  // (If the lookup returns a wrong thing, fail - that's wrong.)
  let self = lookup "self*" arcd
  match self with
  | [(selfo, selfslot)] ->
    let selfClone = cloneObject selfo
    selfClone
  | _ -> failwith "wrong thing - that's wrong!"
  )

let clonablePrototype = makeDataObject [
  makeSlot "clone" cloneMethod  
]

let assignmentMethod n = makeNativeMethod (fun arcd -> 
  // NOTE: The activation record has a slot named 'n' somewhere in its
  // inheritance graph and a slot 'new' which is a method argument.
  // Find those two using 'lookup' and modify the slot value (in the 
  // that contained it - as returned from lookup). (Tiny)Self assignment 
  // should return the object that has been modified.
  let nSlot = lookup n arcd
  let newSlot = lookup "new" arcd
  match nSlot, newSlot with
  | [nn], [neww] -> 
    let (originalObj, originalSlot) = nn
    let (_, newSlot) = neww

    let withoutOldSlot = originalObj.Slots |> List.filter (fun s -> s.Name <> originalSlot.Name)
    let banan = cloneObject newSlot.Contents
    let newNSlot = makeSlot n banan
    
    originalObj.Slots <- newNSlot::withoutOldSlot
    banan
  | _ -> failwith "cant find slot n or new")

let makeAssignmentSlot n = { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// TinySelf code representation & interpreter
// ----------------------------------------------------------------------------

let exprSelf = makeDataObject [
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    let activation = lookupSlotValue "activation" arcd
    lookupSlotValue "self*" activation
  ))
]
let exprString (s:string) = makeDataObject [ 
  makeSlot "string" (makeString s) 
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    lookupSlotValue "string" arcd
  )) ]

let exprSend msg rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    // NOTE: To evalaute 'send' expression, we need to:
    // * Get 'activation' (activation record of the method call we are 
    //   interpreting) from the activation record 'arcd' and create
    //   a new data object with this as the 'activation' to be used
    //   as an argument of recursive 'eval' call(s) later
    // * Get the string value of 'msg' slot (lookup from the 'acrd')
    // * Get the receiver expression (from the 'acrd')
    //   and evaluate it by send it 'eval' with the data object 
    //   (containing 'activation') as argument
    // * Send 'msg' to the recursively evaluated receiver object!
    let activation = lookupSlotValue "activation" arcd
    let activationDO = makeDataObject [makeSlot "activation" activation]
    
    let messageObj = lookupSlotValue "msg" arcd
    let message = getStringValue messageObj

    let receiverExpr = lookupSlotValue "receiver" arcd
    let receiver = receiverExpr |> send "eval" activationDO
    
    send message empty receiver
  )) ]


// TODO: This one is done for you. 'exprSelf' gives you access to the
// object on which a method is called, but if we want to get method
// arguments, those will be stored in the activation record. We get them
// by sending message (with the argument name) to 'exprImplicit'
let exprImplicit = makeDataObject [
  makeSlot "eval" (makeNativeMethod (fun msg ->
    msg |> lookupSlotValue "activation" 
  )) ]


let exprSeq e1 e2 = makeDataObject [ 
  makeSlot "e1" e1
  makeSlot "e2" e2
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    // TODO: Construct the activation record to be passed to recursive
    // 'eval' calls (as in 'exprSend'), recursively evaluate 'e1',
    // ignore the result, then recursively evaluate 'e2' & return the result
    let activation = lookupSlotValue "activation" arcd
    let activationDO = makeDataObject [makeSlot "activation" activation]

    e1 |> send "eval" activationDO |> ignore
    e2 |> send "eval" activationDO
  )) ]
  
let exprSendWith msg args rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "args" (makeDataObject [ for k, v in args -> makeSlot k v ])
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    // TODO: This is like 'exprSend' but the method now optionally can 
    // take arguments. Do the same as in 'exprSend' - but before sending,
    // retrieve 'args' and create a new data object that contains the results
    // of recursively evaluating all the argument expressions in 'args'
    let activation = lookupSlotValue "activation" arcd
    let activationDO = makeDataObject [makeSlot "activation" activation]
    
    let messageObj = lookupSlotValue "msg" arcd
    let message = getStringValue messageObj

    let receiverExpr = lookupSlotValue "receiver" arcd
    let receiver = receiverExpr |> send "eval" activationDO

    let args = lookupSlotValue "args" arcd
    let argsEvaluated = args.Slots |> List.map (fun x -> 
      x.Contents |> send "eval" activationDO |> makeSlot x.Name  
    )
    let argsDO = makeDataObject argsEvaluated
    
    send message argsDO receiver
  )) ]
  
// ----------------------------------------------------------------------------
// Tests - Greetings
// ----------------------------------------------------------------------------

let (++) e1 e2 = exprSeq e1 e2

// Object with 'greet' method that prints "Hello" followed by the
// name specified as method argument and then "!!" string. In Self:
//
//   (| greet = ( 'Hello' print. name print. '!!' print ) |)
//
let greeterObj = makeDataObject [ 
  makeSlot "greet" (makeObject [] (
    ( exprString "Hello " |> exprSend "print" ) ++
    ( exprImplicit |> exprSend "name" |> exprSend "print" ) ++
    ( exprString "!!" |> exprSend "print" )
  )) 
]

// Send the 'greet' method directly. 
// This tests 'exprImplicit' and 'exprSeq'
greeterObj 
|> send "greet" 
  (makeDataObject [makeSlot "name" (makeString "Prague")])

// Object that has 'greeter' as a slot and a 'main' method that 
// calls the 'greet' method with a string as argument.
//
//   (| greeter = g. main = ( self greeter greet: 'NPRG077' ) |)
//
let mainObj = makeDataObject [
  makeSlot "greeter" greeterObj
  makeSlot "main" (makeObject [] (
    exprSelf 
    |> exprSend "greeter"
    |> exprSendWith "greet" ["name", exprString "NPRG077"]
  ))
]

// Send the 'main' message - this tests 'exprSendWith'
mainObj |> send "main" empty
