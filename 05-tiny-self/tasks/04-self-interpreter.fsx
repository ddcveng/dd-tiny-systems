// ----------------------------------------------------------------------------
// 04 - Representing & interpreting TinySelf expressions
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

// TODO: If 'slotValue' has some non-native 'Code', we want to evaluate it.
// But it will be easier to add this later, so copy code from step 3 and then
// return to it later (there is a TODO below telling you to do this).
//
// If the slot contains 'Some(code)', we run it by sending the 'eval' message
// to the 'code'. The method takes the activation record (the same as in the
// case of native code) as the argument, so create data object with 'activation' 
// slot and pass the as argument when calling 'eval'.
// (to call 'send', you will need to use 'let rec .. and ..' here)
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
  // NOTE: We ignore the object returned by 'lookup' here.
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

let empty = {Code = None; Special = None; Slots = []}
let printCode = makeNativeMethod (fun arcd ->
  // TODO: Print the string value! To get the string, you can send 'value' 
  // to the activation record (because this has the receiver string as a 
  // parent). The returned object will be 'Special' with 'String' in it.
  // The function needs to return 'Objekt' - you can return 'empty'.
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
  // TODO: The activation record contains a slot 'self*' which is the
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
  // TODO: The activation record has a slot named 'n' somewhere in its
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

let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }
  

// ----------------------------------------------------------------------------
// TinySelf code representation & interpreter
// ----------------------------------------------------------------------------

// NOTE: All code is represented as objects with 'eval' method. 
// The 'eval' method receives argument 'evalTarget' which represents 
// the object of the method containing the code that is being invoked.
// The 'eval' method is a native method implemented as F# function. We
// store the parameters of the expressions in the objects themselves 
// (and not in F# closures) to keep more things in the TinySelf world.


// TODO: 'self' expression has no arguments and needs no state. When 
// evaluated, it needs to get the original 'activation' we constructed
// when calling 'eval' - this is done by looking up 'activation' slot
// in the activation record with which the method is called - and then 
// from that, we can get the 'self*' slot.
// 
// NOTE: This is a bit confusing - the F# function we write gets 'arcd'
// which is the activation record for the 'eval' call. But this in turn 
// contains activation record 'activation' which is the activation record
// for the message send that our interpreter is handling!
let exprSelf = makeDataObject [
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    let activation = lookupSlotValue "activation" arcd
    lookupSlotValue "self*" activation
  ))
]
  
// DEMO: 'string' expression stores the string value in a slot 'str'. When 
// evaluated, it fetches 'str' (slot value) from the activation record.
let exprString (s:string) = makeDataObject [ 
  makeSlot "string" (makeString s) 
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    lookupSlotValue "string" arcd
  )) ]

let exprSend msg rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    // TODO: To evalaute 'send' expression, we need to:
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
    
    // TODO: what should the args be here?
    send message empty receiver
  )) ]

// ----------------------------------------------------------------------------
// Tests - hello world (finally!)
// ----------------------------------------------------------------------------

// TinySelf code that sends "print" to a Hello world string
let helloCode =
  exprString "Hello world!!" |> exprSend "print" 

// Run it! We need to create arguments for 'eval' with 'activation'
// but since we are not using it (no exprSelf), it can be empty.
let emptySelf = makeDataObject [makeSlot "activation" empty]
helloCode |> send "eval" emptySelf |> ignore

// TODO: Now go back to the missing case in 'eval'. 
// We now add code as methods to a TinySelf object.



// Object with 'hello' method that prints hello world!
let helloObj = makeDataObject [ 
  makeSlot "hello" (makeObject [] helloCode) 
]
helloObj |> send "hello" empty |> ignore

// TODO: A more interesting example! Create an object with 
// string slot 'sound' (Meow! if you like cats) and a 'speak'
// method that sends 'sound' to self (use exprSelf) and then
// sends 'print' to the result to print it.
let speakCode = exprSelf |> exprSend "sound" |> exprSend "print"
let animalObj = makeDataObject [
  makeSlot "sound" (makeString "meow meow!")
  makeSlot "speak" (makeObject [] speakCode)
]

animalObj |> send "speak" empty |> ignore
