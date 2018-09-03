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

  | If
  | Then
  | Else

  | Greater
  | Lesser

  | Fun
  | Arrow
  | Colon

  | Unit

  | Comma
  | Dot
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
          | '-' -> (
              if input.[1] = '>' then
                Arrow, 2
              else Minus,1
            )
          | '*' -> Star,1
          | '/' -> Slash,1
          | '(' -> (
            if input.[1] = ')' then
              Unit, 2
            else LParen,1
            )
          | ')' -> RParen,1
          | '=' -> Equals,1
          | '>' -> Greater,1
          | '<' -> Lesser,1
          | ':' -> Colon,1
          | ',' -> Comma,1
          | '.' -> Dot,1
          | _ ->
            (* Parsing of number *)
            let r = Str.regexp "[0-9]+" in
            let match_pos = try Str.search_forward r input 0 with
                _ -> 999
              (* Str.search_forward returns an exception if the regex
                 doesnt matches so we catch it and consider it a match in a
                 position different that start: 0 since that's the only thing we
                 are interested in.
              *)
            in
            if match_pos = 0 then
              let s = Str.matched_string input in
              Number (s |> int_of_string), String.length s
            else
              let r = Str.regexp "[a-z]+" in
              let match_pos = try Str.search_forward r input 0 with
                  _ -> 999
              in
              if match_pos = 0 then
                let s = Str.matched_string input in
                match s with
                | "let" -> Let, 3
                | "in" -> In, 2
                | "if" -> If, 2
                | "then" -> Then, 4
                | "else" -> Else, 4
                | "fun" -> Fun, 3
                | _ ->
                  Id s, String.length s
              else
                failwith @@ "Counldn't lex: '" ^ Char.to_string input.[0] ^ "'"

        in
        lex (new_lexeme::lexemes) (String.drop_prefix input len)
  in

  text
  |> lex []
  |> List.rev
