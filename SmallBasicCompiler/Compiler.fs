module Compiler

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open Microsoft.SmallBasic.Library

/// Generates fields for set operations defined in instructions
let generateFields (typeBuilder:TypeBuilder) (instructions:instruction[]) =
    let generateField name = 
        typeBuilder.DefineField(name, typeof<Primitive>, FieldAttributes.Static)
    let (|Bound|) pattern =
        let rec bound = function
            | Bind name -> [name]
            | Clause _ -> []
            | Tuple(xs) -> [for x in xs do yield! bound x]
        bound pattern
    [for instruction in instructions do
        match instruction with
        | Assign(Set(name,_)) -> yield name
        | SetAt(Location(name,_),_) -> yield name
        | Deconstruct(Bound names,_) -> yield! names
        | For(Set(name,_),_,_) -> yield name
        | _ -> ()
    ]
    |> Seq.distinct
    |> Seq.map (fun name -> name, generateField name)
    |> dict

/// Generates methods for named subroutines defined in instructions
let generateMethods (typeBuilder:TypeBuilder) (instructions:instruction[]) =
    let generateMethod name returnType (ps:string list) =
        let mi =
            typeBuilder.DefineMethod(
                name, 
                MethodAttributes.Static ||| MethodAttributes.Public,
                returnType,
                [|for p in ps -> typeof<Primitive>|])
        for i = 0 to ps.Length - 1 do 
            mi.DefineParameter(i+1,ParameterAttributes.None, ps.[i]) |> ignore
        mi
    [for instruction in instructions do
        match instruction with
        | Sub(name, ps) -> yield name, typeof<Void>, ps
        | Function(name, ps) -> yield name, typeof<Primitive>, ps
        | _ -> ()
    ]
    |> Seq.map (fun (name,ty,ps) -> name, (generateMethod name ty ps ,ps))
    |> dict

/// Emits IL for the specified instructions
let emitInstructions 
        (mainIL:ILGenerator)
        doc
        (methods:IDictionary<identifier,MethodBuilder * string list>) 
        (fields:IDictionary<identifier,FieldBuilder>)
        (program:(position * instruction)[]) =
    let fieldLookup name = fields.[name]
    /// IL generator for current method
    let methodIL = ref mainIL
    /// Name of current method
    let methodName = ref "Main"
    /// Loop stack for for & while statements
    let loopStack = Stack<Label * Label>()
    /// If stack
    let ifStack = Stack<Label * Label>()
    /// Case stack
    let caseStack = Stack<(Label * Label) option>()
    /// Labels for goto statements
    let labels = Dictionary<string, Label>()
    /// Gets an existing label or creates a new label
    let obtainLabel (il:ILGenerator) name =
        match labels.TryGetValue(name) with
        | true, label -> label
        | false, _ ->
            let label = il.DefineLabel()
            labels.Add(name, label)
            label
    /// Get fully qualified type name for SmallBasicLibrary type
    let getLibraryTypeName name =
        sprintf "Microsoft.SmallBasic.Library.%s, SmallBasicLibrary" name
    let emitNewPrimitive (il:ILGenerator) t =
        let ci = typeof<Primitive>.GetConstructor([|t|])
        il.Emit(OpCodes.Newobj, ci) 
    let emitLiteral (il:ILGenerator) = function
        | Bool(true) -> 
            il.Emit(OpCodes.Ldc_I4_1)
            emitNewPrimitive il typeof<bool>
        | Bool(false) -> 
            il.Emit(OpCodes.Ldc_I4_0)
            emitNewPrimitive il typeof<bool>
        | Int(n) -> 
            il.Emit(OpCodes.Ldc_I4, n) 
            emitNewPrimitive il typeof<int>
        | Double(n) -> 
            il.Emit(OpCodes.Ldc_R8, n) 
            emitNewPrimitive il typeof<double>
        | String(s) -> 
            il.Emit(OpCodes.Ldstr, s); 
            emitNewPrimitive il typeof<string>
        | Array(_) -> raise (NotImplementedException())
    let emitNewCallback (il:ILGenerator) name =
        il.Emit(OpCodes.Ldnull)
        il.Emit(OpCodes.Ldftn, fst methods.[name])
        let cb = typeof<SmallBasicCallback>
        let ci = cb.GetConstructor([|typeof<obj>;typeof<IntPtr>|])
        il.Emit(OpCodes.Newobj, ci)
    let emitLdArg (il:ILGenerator) = function
        | 0 -> il.Emit(OpCodes.Ldarg_0)
        | 1 -> il.Emit(OpCodes.Ldarg_1)
        | 2 -> il.Emit(OpCodes.Ldarg_2)
        | 3 -> il.Emit(OpCodes.Ldarg_3)
        | i -> il.Emit(OpCodes.Ldarg,i)
    let rec emitExpression (il:ILGenerator) = function
        | Literal(x) -> emitLiteral il x
        | Identifier(name) ->
            let paramIndex =
                match methods.TryGetValue(!methodName) with
                | true, (_,ps) -> ps |> List.tryFindIndex ((=) name) 
                | false, _ -> None
            match paramIndex with
            | Some i -> emitLdArg il i
            | None ->
                match fields.TryGetValue(name) with
                | true, field -> il.Emit(OpCodes.Ldsfld, field)
                | false, _ -> emitNewCallback il name
        | GetAt(Location(name,indices)) ->
            il.Emit(OpCodes.Ldsfld, fieldLookup name)
            for index in indices do
                emitExpression il index
                let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                il.EmitCall(OpCodes.Call, mi, null)
        | Func(invoke) -> emitInvoke il invoke
        | Neg(e) ->
            emitExpression il e; 
            let mi = typeof<Primitive>.GetMethod("op_UnaryNegation")
            il.EmitCall(OpCodes.Call, mi, null)
        | Arithmetic(lhs,Add,rhs) -> emitOp il lhs rhs "op_Addition" 
        | Arithmetic(lhs,Subtract,rhs) -> emitOp il lhs rhs "op_Subtraction"
        | Arithmetic(lhs,Multiply,rhs) -> emitOp il lhs rhs "op_Multiply" 
        | Arithmetic(lhs,Divide,rhs) -> emitOp il lhs rhs "op_Division"
        | Comparison(lhs,op,rhs) -> op |> toOp |> emitOp il lhs rhs 
        | Logical(lhs,And,rhs) -> emitOp il lhs rhs "op_And"
        | Logical(lhs,Or,rhs) -> emitOp il lhs rhs "op_Or"
        | NewTuple(xs) -> newTuple il xs
    and newTuple il xs =
        let array = il.DeclareLocal(typeof<Primitive>)
        xs |> List.iteri (fun i x ->
            emitExpression il x
            il.Emit(OpCodes.Ldloc, array.LocalIndex)
            emitExpression il (Literal(Int(i)))
            let mi = typeof<Primitive>.GetMethod("SetArrayValue")
            il.EmitCall(OpCodes.Call, mi, null)
            il.Emit(OpCodes.Stloc, array.LocalIndex)
        )
        il.Emit(OpCodes.Ldloc, array.LocalIndex)
    and toOp = function
        | Eq -> "op_Equality" | Ne -> "op_Inequality"
        | Lt -> "op_LessThan" | Gt -> "op_GreaterThan"
        | Le -> "op_LessThanOrEqual" | Ge -> "op_GreaterThanOrEqual"
    and emitOp (il:ILGenerator) lhs rhs op =
        emitExpression il lhs;
        emitExpression il rhs;
        let mi = typeof<Primitive>.GetMethod(op)
        il.EmitCall(OpCodes.Call, mi, null)
    and emitInvoke (il:ILGenerator) = function
        | Call(name, args) ->
            emitArgs il args
            let mi, _ = methods.[name]
            il.EmitCall(OpCodes.Call, mi, [||])
        | Method(typeName, methodName, args) ->
            emitArgs il args
            let types = [|for arg in args -> typeof<Primitive>|]
            let typeName = getLibraryTypeName typeName
            let mi = Type.GetType(typeName).GetMethod(methodName, types)
            il.EmitCall(OpCodes.Call, mi, null)
        | PropertyGet(typeName, propertyName) ->
            let typeName = getLibraryTypeName typeName 
            let pi = Type.GetType(typeName).GetProperty(propertyName)
            il.EmitCall(OpCodes.Call, pi.GetGetMethod(), null)
    and emitArgs (il:ILGenerator) args =
        for arg in args do emitExpression il arg
    let emitSet (il:ILGenerator) (Set(name,e)) =
        emitExpression il e
        il.Emit(OpCodes.Stsfld, fieldLookup name)
    let emitSetAt (il:ILGenerator) (Location(name,indices),e) =
        emitExpression il e
        let lastIndex = indices.Length - 1
        for lastIndex = indices.Length - 1 downto 0 do
            il.Emit(OpCodes.Ldsfld, fieldLookup name)
            for index = 0 to lastIndex - 1 do
                emitExpression il indices.[index]
                let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                il.EmitCall(OpCodes.Call, mi, null)
            emitExpression il indices.[lastIndex]
            let mi = typeof<Primitive>.GetMethod("SetArrayValue")
            il.EmitCall(OpCodes.Call, mi, null)
        il.Emit(OpCodes.Stsfld, fieldLookup name)
    let emitPropertySet (il:ILGenerator) typeName name e =
        emitExpression il e
        let typeName = getLibraryTypeName typeName 
        let ty = Type.GetType(typeName)
        let pi = ty.GetProperty(name)
        if pi <> null
        then
            il.EmitCall(OpCodes.Call, pi.GetSetMethod(), null)
        else
            let ei = ty.GetEvent(name)
            il.EmitCall(OpCodes.Call, ei.GetAddMethod(), null)
    let emitConvertToBool (il:ILGenerator) =
        let mi = typeof<Primitive>.GetMethod("ConvertToBoolean")
        il.EmitCall(OpCodes.Call, mi, null)
    let emitInstruction (il:ILGenerator) = function
        | Assign(set) -> emitSet il set
        | Deconstruct(pattern, e) ->
            emitExpression il e
            let rec deconstruct = function
                | Bind("_") -> il.Emit(OpCodes.Pop)
                | Bind(name) -> il.Emit(OpCodes.Stsfld, fieldLookup name)
                | Clause(_) -> raise (NotImplementedException())
                | Tuple(xs) ->
                    xs |> List.iteri (fun i x ->
                        il.Emit(OpCodes.Dup)
                        emitExpression il (Literal(Int(i)))
                        let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                        il.EmitCall(OpCodes.Call, mi, null)
                        deconstruct x
                    )
            deconstruct pattern
            il.Emit(OpCodes.Pop)
        | SetAt(location,e) -> emitSetAt il (location,e)
        | Action(invoke) -> emitInvoke il invoke
        | PropertySet(typeName,name,e) -> emitPropertySet il typeName name e
        | If(condition) ->
            let elseLabel = il.DefineLabel()
            let endLabel = il.DefineLabel()
            ifStack.Push(elseLabel, endLabel)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, elseLabel)
        | ElseIf(condition) ->
            let elseLabel, endLabel = ifStack.Pop()
            il.Emit(OpCodes.Br, endLabel)
            il.MarkLabel(elseLabel)
            let elseLabel = il.DefineLabel()
            ifStack.Push(elseLabel, endLabel)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, elseLabel)
        | Else ->
            let elseLabel, endLabel = ifStack.Pop()
            il.Emit(OpCodes.Br, endLabel)
            il.MarkLabel(elseLabel)
            ifStack.Push(endLabel,endLabel)
        | EndIf ->
            let elseLabel, endLabel = ifStack.Pop()
            il.MarkLabel(elseLabel)
            if elseLabel <> endLabel then il.MarkLabel(endLabel)
        | For((Set(name,x)) as set, until, step) ->
            let beginFor = il.DefineLabel()
            let endFor = il.DefineLabel()
            loopStack.Push(beginFor,endFor)
            // Initialize counter
            emitSet il set
            // Skip step on first iteration
            let compare = il.DefineLabel()
            il.Emit(OpCodes.Br, compare)
            // Begin for loop
            il.MarkLabel(beginFor)
            // Step
            emitExpression il (Arithmetic(Identifier(name),Add,step))
            il.Emit(OpCodes.Stsfld, fieldLookup name)
            // Compare
            il.MarkLabel(compare)
            // Is step +ve or -ve
            let positiveStep = il.DefineLabel()
            let cont = il.DefineLabel()
            emitExpression il (Comparison(step,Le,Literal(Int(0))))
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, positiveStep)
            // -ve step
            emitExpression il (Comparison(Identifier(name),Ge,until))
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, endFor)
            il.Emit(OpCodes.Br, cont)
            // +ve step
            il.MarkLabel(positiveStep)
            emitExpression il (Comparison(Identifier(name),Le,until))
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, endFor)
            il.MarkLabel(cont)
        | While(condition) ->
            let beginWhile = il.DefineLabel()
            let endWhile = il.DefineLabel()
            loopStack.Push(beginWhile,endWhile)
            il.MarkLabel(beginWhile)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, endWhile)
        | EndFor | EndWhile -> 
            let beginLoop, endLoop = loopStack.Pop()
            il.Emit(OpCodes.Br, beginLoop)
            il.MarkLabel(endLoop)
        | Goto(name) ->
            let label = obtainLabel il name
            il.Emit(OpCodes.Br, label)
        | Label(name) ->
            let label = obtainLabel il name
            il.MarkLabel(label)
        | Sub(name,_) | Function(name,_) ->
            let builder, _ = methods.[name]
            methodName := name
            methodIL := builder.GetILGenerator()
        | EndSub ->
            il.Emit(OpCodes.Ret)
            methodName := "Main"
            methodIL := mainIL
        | EndFunction ->
            il.Emit(OpCodes.Ldsfld, fieldLookup !methodName)
            il.Emit(OpCodes.Ret)
            methodName := "Main"
            methodIL := mainIL
        | Select(e) ->
            emitExpression il e
            let endLabel = il.DefineLabel()
            caseStack.Push(None)
        | Case(clauses) ->
            let endLabel =
                match caseStack.Pop() with
                | Some(caseLabel, endLabel) ->
                    il.Emit(OpCodes.Br, endLabel)
                    il.MarkLabel(caseLabel)
                    endLabel
                | None ->
                    il.DefineLabel()
            let caseLabel = il.DefineLabel()
            caseStack.Push(Some (caseLabel, endLabel))
            let emitCompare op value =
                il.Emit(OpCodes.Dup)
                emitLiteral il value
                let mi = typeof<Primitive>.GetMethod(toOp op)
                il.EmitCall(OpCodes.Call, mi, null)
                emitConvertToBool il
            let rec emitClause (matchLabel:Label) = function
                | Any ->
                    il.Emit(OpCodes.Br, matchLabel)
                | Is(op,value) ->
                    emitCompare op value
                    il.Emit(OpCodes.Brtrue, matchLabel)
                | Range(from,until) ->
                    let below = il.DefineLabel()
                    emitCompare Lt from
                    il.Emit(OpCodes.Brtrue, below)
                    emitCompare Le until
                    il.Emit(OpCodes.Brtrue, matchLabel)
                    il.MarkLabel(below)
                | Pattern(Tuple(patterns)) ->
                    let failLabel = il.DefineLabel()
                    // Check IsArray
                    il.Emit(OpCodes.Dup)
                    let mi = typeof<Microsoft.SmallBasic.Library.Array>.GetMethod("IsArray")
                    il.EmitCall(OpCodes.Call, mi, null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brfalse,failLabel)
                    // Check Item Count
                    il.Emit(OpCodes.Dup)
                    let mi = typeof<Microsoft.SmallBasic.Library.Array>.GetMethod("GetItemCount")
                    il.EmitCall(OpCodes.Call, mi, null)
                    emitLiteral il (Int(patterns.Length))
                    let mi = typeof<Primitive>.GetMethod(toOp Eq)
                    il.EmitCall(OpCodes.Call, mi, null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brfalse,failLabel)
                    // Check Items
                    patterns |> List.iteri (fun i pattern ->
                        match pattern with
                        | Bind("_") -> ()
                        | Clause(clause) ->
                            let itemLabel = il.DefineLabel()
                            il.Emit(OpCodes.Dup)
                            emitExpression il (Literal(Int(i)))
                            let mi = typeof<Primitive>.GetMethod("GetArrayValue")
                            il.EmitCall(OpCodes.Call, mi, null)
                            emitClause itemLabel clause
                            il.Emit(OpCodes.Pop)
                            il.Emit(OpCodes.Br, failLabel)
                            il.MarkLabel(itemLabel)
                            il.Emit(OpCodes.Pop)
                        | _ -> raise (NotImplementedException())
                    )
                    il.Emit(OpCodes.Br, matchLabel)
                    il.MarkLabel(failLabel)
                | Pattern(_) -> raise (NotImplementedException())
            let clauseLabel = il.DefineLabel()
            for clause in clauses do emitClause clauseLabel clause
            il.Emit(OpCodes.Br, caseLabel)
            il.MarkLabel(clauseLabel)
        | EndSelect ->
            match caseStack.Pop() with
            | Some(caseLabel,endLabel) ->
                il.MarkLabel(caseLabel)
                il.MarkLabel(endLabel)
            | None -> ()
            il.Emit(OpCodes.Pop)
    // Iterate over instructions
    for (pos, instruction) in program do
        let il = !methodIL
        il.MarkSequencePoint(doc,pos.StartLn,pos.StartCol,pos.EndLn,pos.EndCol)
        emitInstruction il instruction 

/// Compiles program instructions to a .Net assembly
let compileTo name program =
    let _, instructions = program |> Array.unzip
    /// Builder for assembly
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            AssemblyName(name),
            AssemblyBuilderAccess.RunAndSave)
    /// Builder for module
    let moduleBuilder = 
        assemblyBuilder.DefineDynamicModule(name+".exe", true)
    /// Writer for source links
    let doc = moduleBuilder.DefineDocument(name+".sb", Guid.Empty, Guid.Empty, Guid.Empty)
    /// Builder for type
    let typeBuilder =
        moduleBuilder.DefineType("Program", TypeAttributes.Public)
    /// Fields representing program's variables
    let fields = generateFields typeBuilder instructions
    /// Methods representing program's subroutine
    let methods = generateMethods typeBuilder instructions
    /// Main method representing main routine
    let mainBuilder =
        typeBuilder.DefineMethod(
            "_Main", 
            MethodAttributes.Static ||| MethodAttributes.Public,
            typeof<Void>,
            [|typeof<string[]>|])
    // Set name of main method arguments
    let args = mainBuilder.DefineParameter(1, ParameterAttributes.None, "args")
    // IL generator for main method
    let il = mainBuilder.GetILGenerator()
    // Emit program instructions
    emitInstructions il doc methods fields program
    il.Emit(OpCodes.Ret)
    // Set main method as entry point
    assemblyBuilder.SetEntryPoint(mainBuilder)
    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(name+".exe")

