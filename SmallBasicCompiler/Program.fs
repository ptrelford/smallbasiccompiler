let source = """
' Returns Modulus
Function Modulus(Dividend,Divisor)
  Modulus = Dividend
  While Modulus >= Divisor
    Modulus = Modulus - Divisor
  EndWhile
EndFunction

For A = 1 To 100 ' Iterate from 1 to 100
  Mod3 = Modulus(A,3) ' A % 3
  Mod5 = Modulus(A,5) ' A % 5
  If Mod3 = 0 And Mod5 = 0 Then
    TextWindow.WriteLine("FizzBuzz")
  ElseIf Mod3 = 0 Then
    TextWindow.WriteLine("Fizz")
  ElseIf Mod5 = 0 Then
    TextWindow.WriteLine("Buzz")
  Else
    TextWindow.WriteLine(A)
  EndIf
EndFor
"""

[<EntryPoint>]
let main argv = 
    let program = Parser.parse source
    Compiler.compileTo "MyProgram" program
    0 // return an integer exit code
