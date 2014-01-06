let source = """
' Sets Result to Modulus
Sub Modulus
  Result = Dividend
  While Result >= Divisor
    Result = Result - Divisor
  EndWhile
EndSub

For A = 1 To 100 ' Print from 1 to 100
  Dividend = A
  Divisor = 3
  Modulus()
  Mod3 = Result ' A % 3
  Divisor = 5
  Modulus()
  Mod5 = Result ' A % 5
  If Mod3 = 0 And Mod5 = 0 Then
    TextWindow.WriteLine("FizzBuzz")  
  Else
    If Mod3 = 0 Then
        TextWindow.WriteLine("Fizz")
    Else
        If Mod5 = 0 Then
            TextWindow.WriteLine("Buzz")
        Else
            TextWindow.WriteLine(A)
        EndIf
    EndIf
  EndIf
EndFor
"""

[<EntryPoint>]
let main argv = 
    let program = Parser.parse source
    Compiler.compileTo "MyProgram" program
    0 // return an integer exit code
