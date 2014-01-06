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
    let generateMethod name = 
        typeBuilder.DefineMethod(
            name, 
            MethodAttributes.Static ||| MethodAttributes.Public,
            typeof<Void>,
            [||])
    [for instruction in instructions do
        match instruction with
        | Sub(name) -> yield name
        | _ -> ()
    ]
    |> Seq.map (fun name -> name, generateMethod name)
    |> dict

/// Emits IL for the specified instructions
let emitInstructions 
        (mainIL:ILGenerator, methods:IDictionary<identifier,MethodBuilder>) 
        (fieldLookup:string -> FieldBuilder) 
        (instructions:instruction[]) =
    let methodIL = ref mainIL
    let labels = Dictionary<string, Label>()
    let loopStack = Stack<Label * Label>()
    let ifStack = Stack<Label>()
    let obtainLabel (il:ILGenerator) name =
        match labels.TryGetValue(name) with
        | true, label -> label
        | false, _ ->
            let label = il.DefineLabel()
            labels.Add(name, label)
            label
    let emitPrimitive (il:ILGenerator) t =
        let ci = typeof<Primitive>.GetConstructor([|t|])
        il.Emit(OpCodes.Newobj, ci) 
    let emitLiteral (il:ILGenerator) = function
        | Bool(true) -> 
            il.Emit(OpCodes.Ldc_I4_1);
            emitPrimitive il typeof<bool>          
        | Bool(false) -> 
            il.Emit(OpCodes.Ldc_I4_0); 
            emitPrimitive il typeof<bool>
        | Int(n) -> 
            il.Emit(OpCodes.Ldc_I4, n); 
            emitPrimitive il typeof<int>
        | Double(n) -> 
            il.Emit(OpCodes.Ldc_R8, n); 
            emitPrimitive il typeof<double>
        | String(s) -> 
            il.Emit(OpCodes.Ldstr, s); 
            emitPrimitive il typeof<string>
        | Array(_) -> raise (NotImplementedException())    
    let rec emitExpression (il:ILGenerator) = function
        | Literal(x) -> emitLiteral il x
        | Var(name) -> il.Emit(OpCodes.Ldsfld, fieldLookup name)
        | GetAt(_) -> raise (NotImplementedException())
        | Func(_) -> raise (NotImplementedException())
        | Neg(e) -> 
            emitExpression il e; 
            let mi = typeof<Primitive>.GetMethod("op_UnaryNegation")
            il.EmitCall(OpCodes.Call, mi, null)
        | Arithmetic(lhs,Add,rhs) -> emitOp il lhs rhs "op_Addition" 
        | Arithmetic(lhs,Subtract,rhs) -> emitOp il lhs rhs "op_Subtraction"
        | Arithmetic(lhs,Multiply,rhs) -> emitOp il lhs rhs "op_Multiply" 
        | Arithmetic(lhs,Divide,rhs) -> emitOp il lhs rhs "op_Divide"
        | Comparison(lhs,Eq,rhs) -> emitOp il lhs rhs "op_Equality"
        | Comparison(lhs,Ne,rhs) ->     
            emitOp il lhs rhs "op_Equality"
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ceq)
        | Comparison(lhs,Gt,rhs) -> emitOp il lhs rhs "op_GreaterThan"
        | Comparison(lhs,Lt,rhs) -> emitOp il lhs rhs "op_LessThan"
        | Comparison(lhs,Ge,rhs) -> emitOp il lhs rhs "op_GreaterThanOrEqual"
        | Comparison(lhs,Le,rhs) -> emitOp il lhs rhs "op_LessThanOrEqual"
        | Logical(lhs,And,rhs) -> emitOp il lhs rhs "op_And"
        | Logical(lhs,Or,rhs) -> emitOp il lhs rhs "op_And"
    and emitOp (il:ILGenerator) lhs rhs op =
        emitExpression il lhs; 
        emitExpression il rhs;
        let mi = typeof<Primitive>.GetMethod(op)
        il.EmitCall(OpCodes.Call, mi, null)
    let emitInvoke (il:ILGenerator) = function
        | Method(typeName, methodName, args) ->
            for arg in args do emitExpression il arg
            let types = [|for arg in args -> typeof<Primitive>|]
            let typeName = 
                sprintf "Microsoft.SmallBasic.Library.%s, SmallBasicLibrary" typeName
            let mi = Type.GetType(typeName).GetMethod(methodName, types)
            il.EmitCall(OpCodes.Call, mi, null)
        | PropertyGet(typeName, propertyName) -> raise (NotImplementedException())
    let emitSet (il:ILGenerator) = function
        | Set(name,e) ->           
            let ty = emitExpression il e     
            il.Emit(OpCodes.Stsfld, fieldLookup name)
    let emitConvertToBool (il:ILGenerator) =
        let mi = typeof<Primitive>.GetMethod("ConvertToBoolean")
        il.EmitCall(OpCodes.Call, mi, null)
    let emitInstruction (il:ILGenerator) = function       
        | Assign(set) -> emitSet il set
        | SetAt(location,e) -> raise (NotImplementedException())
        | Action(invoke) -> emitInvoke il invoke
        | PropertySet(typeName,propertyName,e) -> raise (NotImplementedException())
        | If(condition) ->
            let label = il.DefineLabel()
            ifStack.Push(label)
            emitExpression il condition
            emitConvertToBool il
            il.Emit(OpCodes.Brfalse, label)
        | ElseIf(condition) ->
            raise (NotImplementedException())
        | Else ->
            let newLabel = il.DefineLabel()
            il.Emit(OpCodes.Br, newLabel)
            let label = ifStack.Pop()
            il.MarkLabel(label)           
            ifStack.Push(newLabel)
        | EndIf ->
            let label = ifStack.Pop()
            il.MarkLabel(label)          
        | For((Set(name,x)) as set, until, step) ->
            emitSet il set
            let beginFor = il.DefineLabel()
            let endFor = il.DefineLabel()
            let compare = il.DefineLabel()
            loopStack.Push(beginFor,endFor)
            il.Emit(OpCodes.Br, compare)
            il.MarkLabel(beginFor)
            emitExpression il (Var(name))
            emitExpression il step
            let mi = typeof<Primitive>.GetMethod("op_Addition")
            il.EmitCall(OpCodes.Call, mi, null)
            il.Emit(OpCodes.Stsfld, fieldLookup name)
            il.MarkLabel(compare)
            emitExpression il (Var(name))
            emitExpression il until
            let mi = typeof<Primitive>.GetMethod("op_LessThanOrEqual")
            il.EmitCall(OpCodes.Call, mi, null)
            emitConvertToBool il             
            il.Emit(OpCodes.Brfalse, endFor)
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
        | Sub(name) ->
            let builder = methods.[name]
            methodIL := builder.GetILGenerator()
        | EndSub ->
            il.Emit(OpCodes.Ret)
            methodIL := mainIL
        | GoSub(name) ->
            let mi = methods.[name]
            il.EmitCall(OpCodes.Call, mi, [||])
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
    let fields = generateFields typeBuilder program
    let methods = generateMethods typeBuilder program
    let mainBuilder =
        typeBuilder.DefineMethod(
            "Main", 
            MethodAttributes.Static ||| MethodAttributes.Public,
            typeof<Void>,
            [|typeof<string[]>|])
    let args = mainBuilder.DefineParameter(1, ParameterAttributes.None, "args")
    let il = mainBuilder.GetILGenerator() 
    let fieldLookup name = fields.[name]
    emitInstructions (il,methods) fieldLookup program
    il.Emit(OpCodes.Ret)
    assemblyBuilder.SetEntryPoint(mainBuilder)
    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(name+".exe")

