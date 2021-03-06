import file("tokens.arr") as T

data VM:
  | vm-exec(instr :: State<option<T.Token>>, cells :: State<Number>)
  | vm-end(cells :: State<Number>)
end

data State<a>:
  | state(tape-left :: List<a>, current :: a, tape-right :: List<a>)
end

fun step(vm :: VM) -> VM:
  cases(VM) vm:
    | vm-exec(instr :: State<option<T.Token>>, cells :: State<Number>) => 
      curr-instr :: option<T.Token> = get-current<option<T.Token>>(instr)
      cases(option<T.Token>) curr-instr:
        | some(t :: T.Token) => handle-instruction(vm, t)

        | none =>
          if is-end(instr) == false:
            vm-exec(shift-r(instr), cells)
          else:
            vm-end(cells)
          end
      end
    | vm-end(cells) => vm-end(cells)
  end
where:
  initial = vm-exec(
              state(empty, T.Plus, [list: T.Plus]),
              state(empty, 0, repeat(0, 99))
            )

  next = step(initial)

  current-cell(next) is 1
end

fun handle-instruction(vm :: VM, t :: T.Token) -> VM:
  cases(VM) vm:
    | vm-exec(instr, cells) =>
      new-cells :: State<Number> = cases(T.Token) t:
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

      if is-end(instr):
        vm-end(new-cells)
      else:

        new-instr = cases(T.Token) t:
          | BracketOpen => shift-r(jump-forward-to-closed(instr))
          | BracketClosed => shift-r(jump-backward-to-open(instr))
          | else => shift-r(instr)
        end

        vm-exec(new-instr, new-cells)
      end

    | vm-end(_) => vm
  end
where:
  initial = vm-exec(
              state(empty, T.Plus, [list: T.Plus]),
              state(empty, 0, repeat(0, 99))
            )

  next = handle-instruction(initial, T.Plus)

  current-cell(next) is 1

end

fun current-cell(vm):
  cases(VM) vm:
    | vm-exec(_, cells) => get-current(cells)
    | vm-end(cells) => get-current(cells)
  end
end

fun is-end(s :: State):
  cases(State) s:
    | state(_, _, tr) => tr.length() == 0
  end

where:
  first = state(empty, 0, empty)
  is-end(first) is true

  second = state([list: 1], 0, empty)
  is-end(second) is true

  third = state(empty, 0, [list: 1])
  is-end(third) is false
end

fun get-current<a>(s :: State<a>) -> a:
  cases(State) s:
    | state(_, c, _) => c
  end

where:
  base = state(empty, 100, empty)
  get-current(base) is 100
end

fun jump-forward-to-closed(s :: State<T.Token>) -> State<T.Token>:
  next-state = shift-r(s)
  if get-current(next-state) == some(T.BracketClosed):
    next-state
  else:
    jump-forward-to-closed(next-state)
  end
end

fun jump-backward-to-open(s :: State<T.Token>) -> State<T.Token>:
  next-state = shift-l(s)
  if get-current(next-state) == some(T.BracketOpen):
    next-state
  else:
    jump-backward-to-open(next-state)
  end
end

fun apply-to-current(s :: State, f) -> State:
  cases(State) s:
    | state(tl, c, tr) => state(tl, f(c), tr)
  end

where:
  base = state(empty, 5, empty)

  first = apply-to-current(base, lam(x): x + 1 end)
  get-current(first) is 6
  get-current(base) is 5
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
