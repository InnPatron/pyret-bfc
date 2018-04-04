import file("tokens.arr") as T

data VM:
  | vm-state(instr, cells)
end

data State:
  | state(tape-left, current, tape-right)
end

fun get-current(s :: State):
  cases(State) s:
    | state(_, c, _) => c
  end
end

fun handle-instruction(vm, t):
  cases(VM) vm:
    | vm-state(instr, cells) =>
      new-cells = cases(T.Tokens) t:
        | AngleBracketOpen => shift-l(cells)
        | AngleBracketClosed => shift-r(cells)
        | Plus => apply-to-current(cells, lam(c): c + 1 end)
        | Minus => apply-to-current(cells, lam(c): c - 1 end)
        | Dot =>
          block:
            print(get-current(cells))
            cells
          end
        | Comma => raise("no i/o")
        | else => cells 
      end

    vm-state(shift-l(instr), new-cells)
  end
end

fun apply-to-current(s :: State, f) -> State:
  cases(State) s:
    | state(tl, c, tr) => state(tl, f(c), tr)
  end
end

fun shift-l(old-state :: State) -> State:
  cases(State) old-state:
    | state(tape-left, current, tape-right) =>
      cases(List) tape-left:
        | link(f, r) => 
          state(r, f, link(current, tape-right))
        | empty => raise("shift left beyond tape")
      end
  end
end

fun shift-r(old-state :: State) -> State:
  cases(State) old-state:
    | state(tape-left, current, tape-right) =>
      cases(List) tape-right:
        | link(f, r) => 
          state(link(current, tape-left), f, r)
        | empty => raise("shift right beyond tape")
      end
  end
end

check:
  base = state([list: some(2), some(1)], some(3), [list: some(4), some(5)])

  first = shift-r(base)
  first is state([list: some(3), some(2), some(1)], 
                         some(4), 
                         [list: some(5)])
  shift-r(shift-r(shift-r(base))) raises "shift right beyond tape"
end
