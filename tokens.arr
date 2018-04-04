provide *

provide-types *

data Token:
  | AngleBracketOpen
  | AngleBracketClosed
  | Plus
  | Minus 
  | Dot
  | Comma 
  | BracketOpen
  | BracketClosed
end

data Result:
  | Ok(result)
  | InvalidToken(index)
end

fun tokenize(input):
  length = string-length(input)
  
  for map(index from range(0, length)):
    character = string-char-at(input, index)

    if character == "<":
      some(AngleBracketOpen)
    else if character == ">":
      some(AngleBracketClosed)
    else if character == "+":
      some(Plus)
    else if character == "-":
      some(Minus)
    else if character == ".":
      some(Dot)
    else if character == ",":
      some(Comma)
    else if character == "[":
      some(BracketOpen)
    else if character == "]":
      some(BracketClosed)
    else:
      none
    end
  end
where:
  tokenize("N") is [list: none]
  tokenize("<>,.A") is [list:
                        some(AngleBracketOpen), 
                        some(AngleBracketClosed), 
                        some(Comma),
                        some(Dot),
                        none]
  tokenize("]D") is [list: some(BracketClosed), none]
end
