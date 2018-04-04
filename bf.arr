import file("tokens.arr") as T

data VM:
  | state(tape-left, current, tape-right)
end

fun get-current(s :: VM):
  cases(VM) s:
    | state(_, c, _) => c
  end
end

fun handle-instruction(s, t):
  cases(T.Tokens) t:
    | AngleBracketOpen => shift-l(s)
    | AngleBracketClosed => shift-r(s)
    | Plus => apply-to-current(lam(c): c + 1 end)
    | Minus => apply-to-current(lam(c): c - 1 end)
    | else => s
  end
end

fun apply-to-current(s :: VM, f) -> VM:
  cases(VM) s:
    | state(tl, c, tr) => state(tl, f(c), tr)
  end
end

fun shift-l(old-state :: VM) -> VM:
  cases(VM) old-state:
    | state(tape-left, current, tape-right) =>
      cases(List<option<T.Tokens>>) tape-left:
        | link(f, r) => 
          state(r, f, link(current, tape-right))
        | empty => raise("shift left beyond tape")
      end
  end
end

fun shift-r(old-state :: VM) -> VM:
  cases(VM) old-state:
    | state(tape-left, current, tape-right) =>
      cases(List<option<T.Tokens>>) tape-right:
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
