fn eval_app<'a, 'b>(f: &'a Expr, arg: &'a Expr, env_rc: &'b Rc<EnvM<'a>>) -> ValM<'a> {
    let f_val = f.eval_memo_env(env_rc);
    let arg_val = arg.eval_memo_env(env_rc);
    println!("f_val: {}", f_val); // debug
    println!("arg_val: {}", arg_val); // debug
    match f_val {
        VClo((p_name, body), fun_env, Some(memo)) => {
            if let VNum(arg_num) = &arg_val.clone() {
                if let Some(&cached) = memo.borrow().get(arg_num) {
                    VNum(cached)
                } else {
                    let result = body
                        .eval_memo_env(&Rc::new(EnvM::new(Some(fun_env), EEVal(*p_name, arg_val))));
                    if let VNum(result_num) = result {
                        memo.borrow_mut().insert(*arg_num, result_num);
                    }
                    result
                }
            } else {
                body.eval_memo_env(&Rc::new(EnvM::new(Some(fun_env), EEVal(*p_name, arg_val))))
            }
        }
        _ => panic!("f must be function"),
    }
}
