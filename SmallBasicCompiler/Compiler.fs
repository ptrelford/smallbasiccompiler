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
    [for instruction in instructions do
        match instruction with
        | Assign(Set(name,_)) -> yield name
        | SetAt(Location(name,_),_) -> yield name
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
        (methods:IDictionary<identifier,MethodBuilder * string list>) 
        (fields:IDictionary<identifier,FieldBuilder>)
        (instructions:instruction[]) =
    let fieldLookup name = fields.[name]
    /// IL generator for current method
    let methodIL = ref mainIL
    /// Name of current method
    let methodName = ref "Main"
    let loopStack = Stack<Label * Label>()
    let ifStack = Stack<Label * Label>()
    let caseStack = Stack<(Label * Label) option>()
    let labels = Dictionary<string, Label>()
    let obtainLabel (il:ILGenerator) name =
        match labels.TryGetValue(name) with
        | true, label -> label
        | false, _ ->
            let label = il.DefineLabel()
            labels.Add(name, label)
            label
    let getLibraryTypeName name =
        sprintf "Microsoft.SmallBasic.Library.%s, SmallBasicLibrary" name
    let emitPrimitive (il:ILGenerator) t =
        let ci = typeof<Primitive>.GetConstructor([|t|])
        il.Emit(OpCodes.Newobj, ci) 
    let emitLiteral (il:ILGenerator) = function
        | Bool(true) -> 
            il.Emit(OpCodes.Ldc_I4_1)
            emitPrimitive il typeof<bool>
        | Bool(false) -> 
            il.Emit(OpCodes.Ldc_I4_0)
            emitPrimitive il typeof<bool>
        | Int(n) -> 
            il.Emit(OpCodes.Ldc_I4, n) 
            emitPrimitive il typeof<int>
        | Double(n) -> 
            il.Emit(OpCodes.Ldc_R8, n) 
            emitPrimitive il typeof<double>
        | String(s) -> 
            il.Emit(OpCodes.Ldstr, s); 
            emitPrimitive il typeof<string>
        | Array(_) -> raise (NotImplementedException())
    let rec emitExpression (il:ILGenerator) = function
        | Literal(x) -> emitLiteral il x
        | Identifier(name) ->
            let getParamIndex =
                match methods.TryGetValue(!methodName) with
                | true, (_,ps) -> ps |> List.tryFindIndex ((=) name) 
                | false, _ -> None
            match getParamIndex with
            | Some 0 -> il.Emit(OpCodes.Ldarg_0)
            | Some 1 -> il.Emit(OpCodes.Ldarg_1)
            | Some 2 -> il.Emit(OpCodes.Ldarg_2)
            | Some 3 -> il.Emit(OpCodes.Ldarg_3)
            | Some i -> il.Emit(OpCodes.Ldarg, i)
            | None ->
                match fields.TryGetValue(name) with
                | true, field -> il.Emit(OpCodes.Ldsfld, field)
                | false, _ ->
                    il.Emit(OpCodes.Ldnull)
                    il.Emit(OpCodes.Ldftn, fst methods.[name])
                    let ci = typeof<SmallBasicCallback>.GetConstructor([|typeof<obj>;typeof<IntPtr>|])
                    il.Emit(OpCodes.Newobj, ci) 
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
    let emitSet (il:ILGenerator) = function
        | Set(name,e) ->
            emitExpression il e
            il.Emit(OpCodes.Stsfld, fieldLookup name)
    let emitConvertToBool (il:ILGenerator) =
        let mi = typeof<Primitive>.GetMethod("ConvertToBoolean")
        il.EmitCall(OpCodes.Call, mi, null)
    let emitInstruction (il:ILGenerator) = function
        | Assign(set) -> emitSet il set
        | SetAt(Location(name,indices),e) ->
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
        | Action(invoke) -> emitInvoke il invoke
        | PropertySet(typeName,name,e) ->
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
            emitSet il set
            let beginFor = il.DefineLabel()
            let endFor = il.DefineLabel()
            let compare = il.DefineLabel()
            loopStack.Push(beginFor,endFor)
            il.Emit(OpCodes.Br, compare)
            il.MarkLabel(beginFor)
            emitExpression il (Identifier(name))
            emitExpression il step
            let mi = typeof<Primitive>.GetMethod("op_Addition")
            il.EmitCall(OpCodes.Call, mi, null)
            il.Emit(OpCodes.Stsfld, fieldLookup name)
            il.MarkLabel(compare)
            let positiveStep = il.DefineLabel()
            let cont = il.DefineLabel()
            emitExpression il step
            emitExpression il (Literal(Int(0)))
            let mi = typeof<Primitive>.GetMethod("op_LessThanOrEqual")
            il.EmitCall(OpCodes.Call, mi, null)
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, positiveStep)
            emitExpression il (Identifier(name))
            emitExpression il until
            let mi = typeof<Primitive>.GetMethod("op_GreaterThanOrEqual")
            il.EmitCall(OpCodes.Call, mi, null)
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, endFor)
            il.Emit(OpCodes.Br, cont)
            il.MarkLabel(positiveStep)
            emitExpression il (Identifier(name))
            emitExpression il until
            let mi = typeof<Primitive>.GetMethod("op_LessThanOrEqual")
            il.EmitCall(OpCodes.Call, mi, null)
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
            let matchLabel = il.DefineLabel()
            let emitClause = function
                | Any ->
                    il.Emit(OpCodes.Br, matchLabel)
                | Is(op,value) ->
                    il.Emit(OpCodes.Dup)
                    emitLiteral il value
                    let mi = typeof<Primitive>.GetMethod(toOp op)
                    il.EmitCall(OpCodes.Call, mi, null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brtrue, matchLabel)
                | Range(from,until) ->
                    il.Emit(OpCodes.Dup)
                    emitLiteral il from
                    let below = il.DefineLabel()
                    let mi = typeof<Primitive>.GetMethod(toOp Lt)
                    il.EmitCall(OpCodes.Call, mi, null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brtrue, below)
                    il.Emit(OpCodes.Dup)
                    emitLiteral il until
                    let mi = typeof<Primitive>.GetMethod(toOp Le)
                    il.EmitCall(OpCodes.Call, mi, null)
                    emitConvertToBool il
                    il.Emit(OpCodes.Brtrue, matchLabel)
                    il.MarkLabel(below)
            for clause in clauses do emitClause clause
            il.Emit(OpCodes.Br, caseLabel)
            il.MarkLabel(matchLabel)
        | EndSelect ->
            match caseStack.Pop() with
            | Some(caseLabel,endLabel) ->
                il.MarkLabel(caseLabel)
                il.MarkLabel(endLabel)
            | None -> ()
            il.Emit(OpCodes.Pop)
    // Iterate over instructions
    for instruction in instructions do 
        emitInstruction !methodIL instruction 

/// Compiles program instructions to a .Net assembly
let compileTo name (program:instruction[]) =
    /// Builder for assembly
    let assemblyBuilder =
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            AssemblyName(name),
            AssemblyBuilderAccess.RunAndSave)
    /// Builder for module
    let moduleBuilder = 
        assemblyBuilder.DefineDynamicModule(name+".exe")
    /// Builder for type
    let typeBuilder =
        moduleBuilder.DefineType("Program", TypeAttributes.Public)
    /// Fields representing program's variables
    let fields = generateFields typeBuilder program
    /// Methods representing program's subroutine
    let methods = generateMethods typeBuilder program
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
    emitInstructions il methods fields program
    il.Emit(OpCodes.Ret)
    // Set main method as entry point
    assemblyBuilder.SetEntryPoint(mainBuilder)
    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(name+".exe")

