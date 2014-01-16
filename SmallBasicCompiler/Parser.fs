module Parser

// [snippet:Parser]
open FParsec

let pnumvalue: Parser<value, unit> =
    let numberFormat = NumberLiteralOptions.AllowFraction
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Int (int nl.String)
            else Double(float nl.String)

let ws = skipManySatisfy (fun c -> c = ' ' || c = '\t' || c='\r') // spaces
let str_ws s = pstring s .>> ws
let str_ws1 s = pstring s .>> spaces1

let pstringvalue = 
    between (pstring "\"") (pstring "\"") (manySatisfy (fun x -> x <> '"')) 
    |>> (fun s -> String(s))

let pvalue = pnumvalue <|> pstringvalue

let pidentifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
let pidentifier_ws = pidentifier .>> ws

let pinvoke, pinvokeimpl = createParserForwardedToRef ()
let pfunc = pinvoke |>> (fun x -> Func(x))

let plocation, plocationimpl = createParserForwardedToRef ()
let pgetat = plocation |>> (fun loc -> GetAt(loc))

let patom = 
    choice [
        pvalue |>> (fun x -> Literal(x))
        attempt pgetat;attempt pfunc
        attempt (pidentifier |>> (fun x -> Identifier(x)))
    ]

type Assoc = Associativity

let opp = new OperatorPrecedenceParser<expr,unit,unit>()
let pterm = opp.ExpressionParser
let term = (patom .>> ws) <|> between (str_ws "(") (str_ws ")") pterm
opp.TermParser <- term
opp.AddOperator(InfixOperator("And", ws, 1, Assoc.Left, fun x y -> Logical(x,And,y)))
opp.AddOperator(InfixOperator("Or", ws, 1, Assoc.Left, fun x y -> Logical(x,Or,y)))
opp.AddOperator(InfixOperator("+", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Add, y)))
opp.AddOperator(InfixOperator("-", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Subtract, y)))
opp.AddOperator(InfixOperator("*", ws, 3, Assoc.Left, fun x y -> Arithmetic(x, Multiply, y)))
opp.AddOperator(InfixOperator("/", ws, 3, Assoc.Left, fun x y -> Arithmetic(x, Divide, y)))
opp.AddOperator(PrefixOperator("-", ws, 2, true, fun x -> Neg(x)))
let comparisons = ["=",Eq; "<>",Ne; "<=",Le; ">=",Ge; "<",Lt; ">",Gt]
for s,op in comparisons do
    opp.AddOperator(InfixOperator(s, ws, 2, Assoc.Left, fun x y -> Comparison(x, op, y)))

let pexpr =  pterm

let pmember = pipe3 (pidentifier_ws) (pchar '.') (pidentifier_ws) (fun tn _ mn -> tn,mn) 
let pparamtuple = between (str_ws "(") (str_ws ")") (sepBy pexpr (str_ws ","))
let pmemberinvoke =
    pipe2 pmember (opt pparamtuple)
        (fun (tn,mn) args -> 
        match args with
        | Some args -> Method(tn, mn, args)
        | None -> PropertyGet(tn,mn)
        )
let pcall = pidentifier_ws .>>. pparamtuple |>> (fun (name,args) -> Call(name, args))

pinvokeimpl := attempt pcall <|> attempt pmemberinvoke 

let paction = pinvoke |>> (fun x -> Action(x))
let pset = pipe3 pidentifier_ws (str_ws "=") pexpr (fun id _ e -> Set(id, e))
let passign = pipe3 pidentifier_ws (str_ws "=") pexpr (fun id _ e -> Assign(Set(id, e)))
let ppropertyset = pipe3 pmember (str_ws "=") pexpr (fun (tn,pn) _ e -> PropertySet(tn,pn,e))

let pindex = str_ws "[" >>. pexpr .>> str_ws "]"
let pindices = many1 pindex
plocationimpl := pipe2 pidentifier_ws pindices (fun id xs -> Location(id,xs))
let psetat = pipe3 plocation (str_ws "=") pexpr (fun loc _ e -> SetAt(loc, e))

let pfor =
    let pfrom = str_ws1 "For" >>. pset
    let pto = str_ws1 "To" >>. pexpr
    let pstep = str_ws1 "Step" >>. pexpr
    let toStep = function None -> Literal(Int(1)) | Some s -> s
    pipe3 pfrom pto (opt pstep) (fun f t s -> For(f, t, toStep s))
let pendfor = str_ws "EndFor" |>> (fun _ -> EndFor)

let pwhile = str_ws1 "While" >>. pexpr |>> (fun e -> While(e))
let pendwhile = str_ws "EndWhile" |>> (fun _ -> EndWhile)

let pif = str_ws1 "If" >>. pexpr .>> str_ws "Then" |>> (fun e -> If(e))
let pelseif = str_ws1 "ElseIf" >>. pexpr .>> str_ws "Then" |>> (fun e -> ElseIf(e))
let pelse = str_ws "Else" |>> (fun _ -> Else)
let pendif = str_ws "EndIf" |>> (fun _ -> EndIf)

let pparams = between (str_ws "(") (str_ws ")") (sepBy pidentifier_ws (str_ws ","))
let pmethod = pidentifier_ws .>>. opt pparams
              |>> (fun (name,ps) -> name, match ps with Some ps -> ps | None -> [])

let psub = str_ws1 "Sub" >>. pmethod |>> (fun (name,ps) -> Sub(name,ps))
let pendsub = str_ws "EndSub" |>> (fun _ -> EndSub)

let plabel = pidentifier_ws .>> str_ws ":" |>> (fun label -> Label(label))
let pgoto = str_ws1 "Goto" >>. pidentifier |>> (fun label -> Goto(label))

let pfunction = str_ws1 "Function" >>. pmethod |>> (fun (name,ps) -> Function(name,ps))
let pendfunction = str_ws "EndFunction" |>> (fun _ -> EndFunction)

let pselect = str_ws1 "Select" >>. str_ws1 "Case" >>. pexpr
              |>> (fun e -> Select(e))

let prange = pvalue .>> ws .>> str_ws1 "To" .>>. pvalue |>> (fun (a,b) -> Range(a,b))
let pcomparison = choice [ for s,op in comparisons -> str_ws1 s |>> fun _ -> op]
let pis = str_ws1 "Is" >>. pcomparison .>>. pvalue |>> (fun (op,x) -> Is(op,x))
let pisequal = pvalue |>> (fun x -> Is(Eq,x))
let pany = str_ws "Else" |>> (fun _ -> Any)
let pclause = attempt prange <|> attempt pis <|> attempt pisequal <|> attempt pany
let pcase =
    str_ws1 "Case" >>.
    sepBy pclause (str_ws ",") 
    |>> (fun xs -> Case(xs))
let pendselect = str_ws "EndSelect" |>> (fun _ -> EndSelect)

let pinstruct = 
    [
        pfor;pendfor
        pwhile;pendwhile
        pif; pelseif; pelse; pendif
        pselect; pcase; pendselect
        psub; pendsub
        pfunction; pendfunction
        ppropertyset; passign; psetat
        paction
        plabel; pgoto
    ]
    |> List.map attempt
    |> choice

type Line = Blank | Instruction of instruction
let pcomment = pchar '\'' >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'
let peol = pcomment <|> (pchar '\n')
let pinstruction = ws >>. pinstruct .>> peol |>> (fun i -> Instruction i)
let pblank = ws >>. peol |>> (fun _ -> Blank)
let plines = many (attempt pinstruction <|> attempt pblank) .>> eof
let parse (program:string) =    
    match run plines program with
    | Success(result, _, _)   -> 
        result 
        |> List.choose (function Instruction i -> Some i | Blank -> None) 
        |> List.toArray
    | Failure(errorMsg, e, s) -> failwith errorMsg
// [/snippet]