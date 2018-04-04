import file("tokens.arr") as T

data VM:
  | vm-exec(instr :: State<T.Token>, cells :: State<Number>)
  | vm-end(cells :: State<Number>)
end

data State<a>:
  | state(tape-left :: List<a>, current :: a, tape-right :: List<a>)
end

fun step(vm) -> VM:
  cases(VM) vm:
    | vm-exec(instr, cells) => 
      curr-instr = get-current(instr)
      cases(option) curr-instr:
        | some(t) => handle-instruction(vm, t)

        | none =>
          if is-end(instr) == false:
            vm-exec(shift-r(instr), cells)
          else:
            vm-end(cells)
          end
      end
    | vm-end(_) => vm
  end
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

        new-instr = cases(T.Tokens) t:
          | BracketOpen => shift-r(jump-forward-to-closed(instr))
          | BracketClosed => shift-r(jump-backward-to-open(instr))
          | else => shift-r(instr)
        end

        vm-exec(new-instr, new-cells)
      end

    | vm-end(_) => vm
  end
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
end

fun get-current(s :: State):
  cases(State) s:
    | state(_, c, _) => c
  end
end

fun jump-forward-to-closed(s):
  next-state = shift-r(s)
  if get-current(next-state) == some(T.BracketClosed):
    next-state
  else:
    jump-forward-to-closed(next-state)
  end
end

fun jump-backward-to-open(s):
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

check:
  initial = vm-exec(
              state(empty, T.Plus, [list: T.Plus]),
              state(empty, 0, repeat(0, 99))
            )

  next = step(initial)

  current-cell(next) is 1
end
