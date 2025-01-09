use crate::expr::*;
use crate::linkedlist::*;
use crate::value::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use EnvEntryM::*;
use Expr::*;
use ValM::*;

impl Expr {
    fn eval_memo_env<'a, 'b>(&'a self, env_rc: &'b Rc<EnvM<'a>>) -> ValM<'a> {
        fn eval_arith<'a, 'b>(
            op: fn(i64, i64) -> i64,
            e1: &'a Expr,
            e2: &'a Expr,
            env_rc: &'b Rc<EnvM<'a>>,
        ) -> ValM<'a> {
            let e1_eval = e1.eval_memo_env(env_rc);
            let e2_eval = e2.eval_memo_env(env_rc);
            match (e1_eval, e2_eval) {
                (VNum(n1), VNum(n2)) => VNum(op(n1, n2)),
                _ => panic!("error: arithmetic operation with non-number"),
            }
        }

        fn eval_if<'a, 'b>(
            e_cond: &'a Expr,
            e_then: &'a Expr,
            e_else: &'a Expr,
            env_rc: &'b Rc<EnvM<'a>>,
        ) -> ValM<'a> {
            let cond_result = e_cond.eval_memo_env(env_rc);
            match cond_result {
                VNum(n) => {
                    if n == 0 {
                        e_then.eval_memo_env(env_rc)
                    } else {
                        e_else.eval_memo_env(env_rc)
                    }
                }
                _ => panic!("error: if condition is not a number"),
            }
        }

        fn eval_name<'a, 'b>(x: &'static str, env_rc: &'b Rc<EnvM<'a>>) -> ValM<'a> {
            match &env_rc.entry {
                EEVal(name, val) => {
                    if *name == x {
                        return val.clone();
                    }
                }
                EEFuns(list) => {
                    for (name, func, memo) in list.iter() {
                        if *name == x {
                            return VClo(*func, env_rc.clone(), memo.clone());
                        }
                    }
                }
            }
            match &env_rc.parent {
                Some(parent_env) => eval_name(x, parent_env),
                None => panic!("error: name not found: {}", x),
            }
        }

        fn eval_app<'a, 'b>(f: &'a Expr, arg: &'a Expr, env_rc: &'b Rc<EnvM<'a>>) -> ValM<'a> {
            let f_eval = f.eval_memo_env(env_rc);
            let arg_eval = arg.eval_memo_env(env_rc);

            match f_eval {
                VNum(_) => panic!("error: function not found"),
                VClo(function, fun_rc, memo_option) => {
                    let (var_name, func_body) = function;

                    if let Some(memo) = memo_option {
                        if let VNum(n) = arg_eval {
                            if let Some(result) = memo.borrow().get(&n) {
                                return VNum(*result);
                            }

                            let new_env = Rc::new(EnvM::new(
                                Some(fun_rc.clone()),
                                EEVal(&var_name, arg_eval),
                            ));
                            let func_result = func_body.eval_memo_env(&new_env);

                            if let VNum(n2) = func_result {
                                memo.borrow_mut().insert(n, n2);
                            }
                            return func_result;
                        }
                    }

                    let new_env =
                        Rc::new(EnvM::new(Some(fun_rc.clone()), EEVal(&var_name, arg_eval)));
                    return func_body.eval_memo_env(&new_env);
                }
            }
        }

        fn eval_letval<'a, 'b>(
            x: &'static str,
            e1: &'a Expr,
            e2: &'a Expr,
            env_rc: &'b Rc<EnvM<'a>>,
        ) -> ValM<'a> {
            let new_env = Rc::new(EnvM::new(
                Some(env_rc.clone()),
                EEVal(x, e1.eval_memo_env(env_rc)),
            ));
            return e2.eval_memo_env(&new_env);
        }

        fn eval_letfuns<'a, 'b>(
            binds: &'a List<Bind>,
            e: &'a Expr,
            env_rc: &'b Rc<EnvM<'a>>,
        ) -> ValM<'a> {
            let mut fun_defs_map: HashMap<&'static str, (&'a FunDef, bool)> = HashMap::new();

            for bind in binds.iter() {
                let (is_memo, name, func_def) = bind;

                if fun_defs_map.contains_key(name) {
                    panic!("error: duplicated function definition: {}", name);
                }

                fun_defs_map.insert(name, (func_def, *is_memo));
            }

            let fun_defs_list: List<(&'static str, &'a FunDef, Option<Rc<RefCell<Memo>>>)> =
                fun_defs_map
                    .iter()
                    .fold(List::new(), |list, (name, (func_def, is_memo))| {
                        if *is_memo {
                            List::cons(
                                (name, func_def, Some(Rc::new(RefCell::new(Memo::new())))),
                                list,
                            )
                        } else {
                            List::cons((name, func_def, None), list)
                        }
                    });

            let new_env_rc = Rc::new(EnvM::new(Some(env_rc.clone()), EEFuns(fun_defs_list)));
            e.eval_memo_env(&new_env_rc)
        }

        match self {
            ENum(n) => VNum(*n),
            EAdd(e1, e2) => eval_arith(|x, y| x + y, e1, e2, env_rc),
            ESub(e1, e2) => eval_arith(|x, y| x - y, e1, e2, env_rc),
            EMul(e1, e2) => eval_arith(|x, y| x * y, e1, e2, env_rc),
            EDiv(e1, e2) => eval_arith(|x, y| x / y, e1, e2, env_rc),
            EIf0(e_cond, e_then, e_else) => eval_if(e_cond, e_then, e_else, env_rc),
            EName(x) => eval_name(x, env_rc),
            EApp(f, arg) => eval_app(f, arg, env_rc),
            ELetVal(x, e1, e2) => eval_letval(x, e1, e2, env_rc),
            ELetFuns(binds, e) => eval_letfuns(binds, e, env_rc),
        }
    }

    pub fn eval_memo(&self) -> ValM<'_> {
        self.eval_memo_env(&Rc::new(EnvM::new(None, EEFuns(List::new()))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tests_memo_leak_check() {
        // ((3 + 5) / 2) = 4
        let expr1 = Expr::ediv(Expr::eadd(Expr::enumb(3), Expr::enumb(5)), Expr::enumb(2));
        test_eval_memo_leak_check(expr1, 4);

        // letfun memo:(☆ x := (x - 1)) in
        // ☆(5)
        // = 4
        let expr2 = Expr::eapp(
            Expr::elam_memo("x", Expr::esub(Expr::ename("x"), Expr::enumb(1))),
            Expr::enumb(5),
        );
        test_eval_memo_leak_check(expr2, 4);

        // letfun (f x := (x - 1)) in
        // let n := f(42) in
        // let m := (n * 2) in
        // f(m)
        // = 81
        let expr3 = Expr::eletrec(
            vec![Expr::efun(
                "f",
                "x",
                Expr::esub(Expr::ename("x"), Expr::enumb(1)),
            )],
            Expr::eletval(
                "n",
                Expr::eapp(Expr::ename("f"), Expr::enumb(42)),
                Expr::eletval(
                    "m",
                    Expr::emul(Expr::ename("n"), Expr::enumb(2)),
                    Expr::eapp(Expr::ename("f"), Expr::ename("m")),
                ),
            ),
        );
        test_eval_memo_leak_check(expr3, 81);

        // letfun (emul x := letfun (☆ y := (x * y)) in ☆)
        //      (fact op :=
        //          letfun (☆ i0 :=
        //              letfun memo:(self x :=
        //                  if x = 0 then i0
        //                  else op(x)(self((x - 1)))
        //              ) in self
        //          ) in ☆
        //      ) in
        //      fact(emul)(1)(10)
        // = 3628800
        let expr_fact = Expr::eletrec(
            vec![
                Expr::efun(
                    "fact",
                    "op",
                    Expr::elam(
                        "i0",
                        Expr::eletrec(
                            vec![Expr::efun_memo(
                                "self",
                                "x",
                                Expr::eif0(
                                    Expr::ename("x"),
                                    Expr::ename("i0"),
                                    Expr::eapp(
                                        Expr::eapp(Expr::ename("op"), Expr::ename("x")),
                                        Expr::eapp(
                                            Expr::ename("self"),
                                            Expr::esub(Expr::ename("x"), Expr::enumb(1)),
                                        ),
                                    ),
                                ),
                            )],
                            Expr::ename("self"),
                        ),
                    ),
                ),
                Expr::efun(
                    "emul",
                    "x",
                    Expr::elam("y", Expr::emul(Expr::ename("x"), Expr::ename("y"))),
                ),
            ],
            Expr::eapp(
                Expr::eapp(
                    Expr::eapp(Expr::ename("fact"), Expr::ename("emul")),
                    Expr::enumb(1),
                ),
                Expr::enumb(10),
            ),
        );
        test_eval_memo_leak_check(expr_fact, 3628800);

        // letfun (eadd x := letfun (☆ y := (x + y)) in ☆)
        //      memo:(fib op :=
        //          letfun (☆ i0 :=
        //              letfun (☆ i1 :=
        //                  letfun memo:(self x :=
        //                      if x = 0 then i0
        //                      else if (x - 1) = 0 then i1
        //                      else op(self((x - 1)))(self((x - 2))) )
        //                  in self
        //              ) in ☆
        //          ) in ☆
        //      ) in
        //      fib(eadd)(0)(1)(50)
        // = 12586269025
        let expr_fib = Expr::eletrec(
            vec![
                Expr::efun_memo(
                    "fib",
                    "op",
                    Expr::elam(
                        "i0",
                        Expr::elam(
                            "i1",
                            Expr::eletrec(
                                vec![Expr::efun_memo(
                                    "self",
                                    "x",
                                    Expr::eif0(
                                        Expr::ename("x"),
                                        Expr::ename("i0"),
                                        Expr::eif0(
                                            Expr::esub(Expr::ename("x"), Expr::enumb(1)),
                                            Expr::ename("i1"),
                                            Expr::eapp(
                                                Expr::eapp(
                                                    Expr::ename("op"),
                                                    Expr::eapp(
                                                        Expr::ename("self"),
                                                        Expr::esub(
                                                            Expr::ename("x"),
                                                            Expr::enumb(1),
                                                        ),
                                                    ),
                                                ),
                                                Expr::eapp(
                                                    Expr::ename("self"),
                                                    Expr::esub(Expr::ename("x"), Expr::enumb(2)),
                                                ),
                                            ),
                                        ),
                                    ),
                                )],
                                Expr::ename("self"),
                            ),
                        ),
                    ),
                ),
                Expr::efun(
                    "eadd",
                    "x",
                    Expr::elam("y", Expr::eadd(Expr::ename("x"), Expr::ename("y"))),
                ),
            ],
            Expr::eapp(
                Expr::eapp(
                    Expr::eapp(
                        Expr::eapp(Expr::ename("fib"), Expr::ename("eadd")),
                        Expr::enumb(0),
                    ),
                    Expr::enumb(1),
                ),
                Expr::enumb(50),
            ),
        );
        test_eval_memo_leak_check(expr_fib, 12586269025);
    }
}
