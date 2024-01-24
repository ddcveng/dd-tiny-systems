// ----------------------------------------------------------------------------
// 03 - Cloning and mutating TinySelf objects
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
let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  // TODO: Make a new non-parent slot and add it to 'Slots' 
  // (create a new list and assign it to the 'Slots' field.)
  let slot = makeSlot n contents
  obj.Slots <- slot :: obj.Slots

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  // TODO: Make a new parent slot and add it to 'Slots' 
  // (create a new list and assign it to the 'Slots' field.)
  let parentSlot = makeParentSlot n contents
  obj.Slots <- parentSlot :: obj.Slots

let cloneObject (obj:Objekt) : Objekt = 
  // TODO: Return a new 'Objekt' with exactly the same slots, code & special
  // as 'obj' (we are not doing "deep copy" - the two clones will share
  // references to the same objects in after their slots are copied)
  let clone: Objekt = { Code = obj.Code; Special = obj.Special; Slots = obj.Slots }
  clone

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

// TODO: To implement assignment, we need to know what object a slot
// comes from. Modify 'lookup' so that it returns not just the slot,
// but also the object that the slot comes from.
let rec lookup msg obj : list<Objekt * Slot> =   
  let slot = List.tryFind (fun s -> s.Name = msg) obj.Slots
  match slot with
  | Some s -> [(obj, s)]
  | None -> obj.Slots 
          |> List.filter (fun s -> s.IsParent) 
          |> List.collect (fun ps -> lookup msg ps.Contents)

// TODO: Modify 'send' and 'eval' to also take message send arguments.
// In Self, the arguments are copied into the activation record. 
// In TinySelf, we use simpler trick - just make the 'args' object 
// another parent of the activation record! Lookup for argument name 
// in the activation record will then give us the value.
// NOTE: The object newly returned from 'lookup' should be ignored.
// BEWARE: All arguments are 'Objekt' so it is easy to swap them!! 
let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue.Code with
  | Some code ->
    match code.Special with
    | Some special ->
      match special with
      | Native fn -> 
        let codeClone = cloneObject code
        addParentSlot "self*" instance codeClone
        addParentSlot "__args*" args codeClone
        fn(codeClone)
      | _ -> failwith "unsupported special"
    | None -> failwith "code is not special"
  | None -> slotValue

let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
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

let empty : Objekt = {Code = None; Special = None; Slots = []}

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

// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }
  
// ----------------------------------------------------------------------------
// Tests - cloning and modifying cats
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let mogscats = makeDataObject [
  makeSlot "book" (makeString "Mog's Family of Cats")
  // NOTE: This allows us to rename the book (probably not
  // something you'd want to do, but for illustration purposes...)
  makeAssignmentSlot "book"
]
let mog = makeDataObject [
  // NOTE: Mog is now also clonable and has assignment slot "name:"
  makeParentSlot "parent*" cat
  makeParentSlot "clonable*" clonablePrototype
  makeParentSlot "fictional*" mogscats
  makeSlot "name" (makeString "Mog")
  makeAssignmentSlot "name"
]

// NOTE: We now pass empty arguments to all of the message sends
mog |> send "name" empty |> send "print" empty
mog |> send "sound" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty

// NOTE: Clone Ginger and print its name & book
let ginger = mog |> send "clone" empty

ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty

// TODO: Write code to change the name of 'ginger' to "Ginger"!
// (send message "name:" with arument containing slot 'new' with the new value)
ginger 
|> send "name:" (makeDataObject [makeSlot "new" (makeString "Ginger!")])
|> send "print" empty

// TODO: Write code to change the book of 'ginger' to "Goodbye, Mog"!
ginger 
|> send "book:" (makeDataObject [makeSlot "new" (makeString "Goodbye, Mog!")])
|> send "print" empty

// TODO: What do we get if we run the following now?
mog |> send "name" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty
ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty
