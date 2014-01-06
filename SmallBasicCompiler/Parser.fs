module Parser

// [snippet:Parser]
open FParsec

let pnumliteral: Parser<expr, unit> =
    let numberFormat = NumberLiteralOptions.AllowFraction
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Literal(Int (int nl.String))
            else Literal(Double (float nl.String))

let ws = skipManySatisfy (fun c -> c = ' ' || c = '\t' || c='\r') // spaces
let str_ws s = pstring s .>> ws
let str_ws1 s = pstring s .>> spaces1

let pstringliteral = 
    between (pstring "\"") (pstring "\"") (manySatisfy (fun x -> x <> '"')) 
    |>> (fun s -> Literal(String(s)))

let pidentifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
let pidentifier_ws = pidentifier .>> ws
let pvar = pidentifier |>> (fun x -> Var(x))

let pinvoke, pinvokeimpl = createParserForwardedToRef ()
let pfunc = pinvoke |>> (fun x -> Func(x))

let plocation, plocationimpl = createParserForwardedToRef ()
let pgetat = plocation |>> (fun loc -> GetAt(loc))

let pvalue = 
    choice [
        pnumliteral; pstringliteral
        attempt pgetat <|> attempt pfunc <|> attempt pvar
    ]

type Assoc = Associativity

let oppa = new OperatorPrecedenceParser<expr,unit,unit>()
let parithmetic = oppa.ExpressionParser
let terma = (pvalue .>> ws) <|> between (str_ws "(") (str_ws ")") parithmetic
oppa.TermParser <- terma
oppa.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Add, y)))
oppa.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Subtract, y)))
oppa.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Multiply, y)))
oppa.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Divide, y)))
oppa.AddOperator(PrefixOperator("-", ws, 2, true, fun x -> Neg(x)))

let oppc = new OperatorPrecedenceParser<expr,unit,unit>()
let pcomparison = oppc.ExpressionParser
let termc = (parithmetic .>> ws) <|> between (str_ws "(") (str_ws ")") pcomparison
oppc.TermParser <- termc
oppc.AddOperator(InfixOperator("=", ws, 1, Assoc.Left, fun x y -> Comparison(x, Eq, y)))
oppc.AddOperator(InfixOperator("<>", ws, 1, Assoc.Left, fun x y -> Comparison(x, Ne, y)))
oppc.AddOperator(InfixOperator("<=", ws, 2, Assoc.Left, fun x y -> Comparison(x, Le, y)))
oppc.AddOperator(InfixOperator(">=", ws, 2, Assoc.Left, fun x y -> Comparison(x, Ge, y)))
oppc.AddOperator(InfixOperator("<", ws, 2, Assoc.Left, fun x y -> Comparison(x, Lt, y)))
oppc.AddOperator(InfixOperator(">", ws, 2, Assoc.Left, fun x y -> Comparison(x, Gt, y)))

let oppl = new OperatorPrecedenceParser<expr,unit,unit>()
let plogical = oppl.ExpressionParser
let terml = (pcomparison .>> ws) <|> between (str_ws "(") (str_ws ")") plogical
oppl.TermParser <- terml
oppl.AddOperator(InfixOperator("And", ws, 1, Assoc.Left, fun x y -> Logical(x,And,y)))
oppl.AddOperator(InfixOperator("Or", ws, 1, Assoc.Left, fun x y -> Logical(x,Or,y)))

let pmember = pipe3 (pidentifier_ws) (pchar '.') (pidentifier_ws) (fun tn _ mn -> tn,mn) 
let ptuple = between (str_ws "(") (str_ws ")") (sepBy parithmetic (str_ws ","))
pinvokeimpl := 
    pipe2 pmember (opt ptuple)
        (fun (tn,mn) args -> 
        match args with
        | Some args -> Method(tn, mn, args |> List.toArray)
        | None -> PropertyGet(tn,mn)
        )

let paction = pinvoke |>> (fun x -> Action(x))
let pset = pipe3 pidentifier_ws (str_ws "=") parithmetic (fun id _ e -> Set(id, e))
let passign = pipe3 pidentifier_ws (str_ws "=") parithmetic (fun id _ e -> Assign(Set(id, e)))
let ppropertyset = pipe3 pmember (str_ws "=") parithmetic (fun (tn,pn) _ e -> PropertySet(tn,pn,e))

let pindex = str_ws "[" >>. parithmetic .>> str_ws "]"
let pindices = many1 pindex
plocationimpl := pipe2 pidentifier_ws pindices (fun id xs -> Location(id,xs))
let psetat = pipe3 plocation (str_ws "=") parithmetic (fun loc _ e -> SetAt(loc, e))

let pfor =
    let pfrom = str_ws1 "For" >>. pset
    let pto = str_ws1 "To" >>. parithmetic
    let pstep = str_ws1 "Step" >>. parithmetic
    let toStep = function None -> Literal(Int(1)) | Some s -> s
    pipe3 pfrom pto (opt pstep) (fun f t s -> For(f, t, toStep s))
let pendfor = str_ws "EndFor" |>> (fun _ -> EndFor)

let pwhile = str_ws1 "While" >>. plogical |>> (fun e -> While(e))
let pendwhile = str_ws "EndWhile" |>> (fun _ -> EndWhile)

let pif = str_ws1 "If" >>. plogical .>> str_ws "Then" |>> (fun e -> If(e))
let pelseif = str_ws1 "ElseIf" >>. pcomparison .>> str_ws "Then" |>> (fun e -> ElseIf(e))
let pelse = str_ws "Else" |>> (fun _ -> Else)
let pendif = str_ws "EndIf" |>> (fun _ -> EndIf)

let psub = str_ws1 "Sub" >>. pidentifier |>> (fun name -> Sub(name))
let pendsub = str_ws "EndSub" |>> (fun _ -> EndSub)
let pgosub = pidentifier_ws .>> str_ws "()" |>> (fun routine -> GoSub(routine))

let plabel = pidentifier_ws .>> str_ws ":" |>> (fun label -> Label(label))
let pgoto = str_ws1 "Goto" >>. pidentifier |>> (fun label -> Goto(label))

let pinstruct = 
    [
        pfor;pendfor
        pwhile;pendwhile
        pif; pelseif; pelse; pendif
        psub; pendsub; pgosub                 
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
let plines = many (pinstruction <|> pblank) .>> eof
let parse (program:string) =    
    match run plines program with
    | Success(result, _, _)   -> 
        result 
        |> List.choose (function Instruction i -> Some i | Blank -> None) 
        |> List.toArray
    | Failure(errorMsg, e, s) -> failwith errorMsg
// [/snippet]

type Color = System.ConsoleColor
// [snippet:Library]
type TextWindow private () =
    static member WriteLine (o:obj) = System.Console.WriteLine(o)
    static member ForegroundColor
        with get () = System.Console.ForegroundColor.ToString()
        and set color =   
            let color = Color.Parse(typeof<Color>, color, true)
            System.Console.ForegroundColor <- color :?> Color
type Clock private () =
    static let now() = System.DateTime.Now
    static member Year = now().Year
    static member Month = now().Month
    static member Day = now().Day

type IMarker = interface end
let getLibraryType name = typeof<IMarker>.DeclaringType.GetNestedType(name) 
// [/snippet]