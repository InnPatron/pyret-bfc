import file("tokens.arr") as T

data VM:
  | state(tape-left, current, tape-right)
end

fun handle-instruction(s, t):
  cases(T.Tokens) t:
    | AngleBracketOpen => shift-l(s)
    | AngleBracketClosed => shift-r(s)
    | else => s
  end
end

fun step(s):
  cases(option) s:
    | state(tl, c, tr) =>
    cases(option) c:
      | some(t) => handle-instruction(s, t)
      | none => raise("Attempting to execute bad character")
    end
  end
end

fun shift-l(old-state):
  cases(VM) old-state:
    | state(tape-left, current, tape-right) =>
      cases(List) tape-left:
        | link(f, r) => 
          some(state(r, f, link(current, tape-right)))
        | empty => none
      end
  end
end

fun shift-r(old-state):
  cases(VM) old-state:
    | state(tape-left, current, tape-right) =>
      cases(List) tape-right:
        | link(f, r) => 
          some(state(link(current, tape-left), f, r))
        | empty => none
      end
  end
end

check:
  base = state([list: some(2), some(1)], some(3), [list: some(4), some(5)])

  first = shift-r(base)
  first is some(state([list: some(3), some(2), some(1)], 
                         some(4), 
                         [list: some(5)]))
  # shift-r(shift-r(shift-r(base))) is none
end
