module Core exposing (..)


type Identifier = Identifier String

type Expr = 
    Var Identifier 
    | Lit Literal 
    | App Expr Expr 
    | Lambda Identifier Expr 
    | Let Identifier Expr Expr 
    | Case Expr (List (Pattern, Expr))

type Pattern =
    ConstructorPattern Identifier (List Identifier)
    | LiteralPattern Literal 
    | DefaultPattern 

type Literal =
    StrLit String 
    | WebglLit String 
    | NumberLit Float
    | CharLit Char 
    


identifierToStr : Identifier -> String
identifierToStr id =
    case id of 
        Identifier n -> n


exprToStr : Expr -> String
exprToStr expression =
    case expression of 
        Var id -> identifierToStr id 
        Lit lit -> litToStr lit 
        Lambda param expr -> "(\\" ++ lambdaToStr  param expr ++ ")"
        App expr1 expr2 -> exprToStr expr1 ++ " " ++ exprToStr expr2
        Let binding expr body ->
            "let\n" ++ identifierToStr binding ++ " = " ++ exprToStr expr ++ "\nin\n" ++ exprToStr  body 
        Case expr patterns ->
            "case " ++ exprToStr  expr ++ " of\n    " ++ (String.join "\n\n    " (List.map (\(pattern, body) ->
                patternToString pattern ++ " ->\n        " ++ exprToStr body
                 
            ) patterns ))

litToStr : Literal -> String 
litToStr lit =
    case lit of 
        StrLit str -> "\"" ++ str ++ "\""
        WebglLit code -> "[glsl|" ++ code ++ "|]"
        NumberLit num -> String.fromFloat num


lambdaToStr : Identifier -> Expr -> String
lambdaToStr param body =
    case body of 
        Lambda param2 newBody ->
            (identifierToStr param) ++ " " ++ lambdaToStr (param2) newBody
        
        Case (Var id1) [(ConstructorPattern c args, newBody)] ->
            if id1 == param then
                "(" ++ patternToString (ConstructorPattern c args) ++ ")" ++ case newBody of
                    Lambda param2 newerBody ->
                         " " ++ lambdaToStr (param2) newerBody
                     
                    _ ->
                        " -> " ++ exprToStr newBody
            
            else 
                (identifierToStr param) ++ " = " ++ exprToStr body 

        _ ->
            (identifierToStr param) ++ " -> " ++ exprToStr body 
    


patternToString : Pattern -> String 
patternToString pattern =
    case pattern of 
        ConstructorPattern constructor args ->
            List.map identifierToStr (constructor :: args) |> String.join " "
        LiteralPattern lit ->
            litToStr lit
        DefaultPattern ->
            "_"
