open Core

type lexeme =
  | Number of int
  | Id of string
  | Let
  | In
  | Equals
  | Plus
  | Minus
  | Star
  | Slash
  | RParen
  | LParen
[@@deriving show]

let lex (text:string) : lexeme list =
  let rec lex (lexemes: lexeme list) (input: string): lexeme list =
    if String.length input = 0 then
      lexemes
    else
      let first_char = input.[0] in
      if first_char = ' ' || first_char = '\n' then
        lex lexemes (String.drop_prefix input 1)
      else
        let (new_lexeme, len) = match input.[0] with
          | '+' -> Plus,1
          | '-' -> Minus,1
          | '*' -> Star,1
          | '/' -> Slash,1
          | '(' -> LParen,1
          | ')' -> RParen,1
          | '=' -> Equals,1
          | _ ->
            let r = Str.regexp "[0-9]+" in
            let match_pos = try Str.search_forward r input 0 with
                _ -> 999
            in
            if match_pos <> 0 then
              let r = Str.regexp "[a-z]+" in
              let match_pos = try Str.search_forward r input 0 with
                  _ -> 999
              in
              if match_pos <> 0 then
                failwith "Counldn't lex"
              else
                let s = Str.matched_string input in
                match s with
                | "let" -> Let, 3
                | "in" -> In, 2
                | _ ->
                  Id s, String.length s
            else
              let s = Str.matched_string input in
              Number (s |> int_of_string), String.length s

        in
        lex (new_lexeme::lexemes) (String.drop_prefix input len)
  in

  text
  |> lex []
  |> List.rev
