use std::collections::HashMap;

use astro_float::BigFloat;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

use crate::ast::{BuiltinFunc, CalcFuncRef, UserFunc};
use crate::{CalcError, CalcResult, Number, PREC, RM};

lazy_static! {
    pub static ref BUILTINS: HashMap<String, BuiltinFunc> = {
        let mut m = HashMap::new();

        // Square root function
        m.insert(
            "sqrt".to_string(),
            BuiltinFunc::new(1, |args, _ctx| {
                Ok(args[0].clone().sqrt(PREC, RM))
            }),
        );

        // Natural logarithm
        m.insert(
            "ln".to_string(),
            BuiltinFunc::new(1, |args, _ctx| {
                let mut consts = astro_float::Consts::new().unwrap();
                Ok(args[0].ln(PREC, RM, &mut consts))
            })
        );

        // Logarithm of any base
        m.insert(
            "log".to_string(),
            BuiltinFunc::new(1, |args, _ctx| {
                let mut consts = astro_float::Consts::new().unwrap();
                Ok(args[0].log10(PREC, RM, &mut consts))
            })
        );

        // Trig functions
        m.insert(
            "sin".to_string(),
            BuiltinFunc::new(1, |args, _ctx| {
                let mut consts = astro_float::Consts::new().unwrap();
                if args[0] == consts.pi(PREC, RM) {
                    // return 0
                    Ok(BigFloat::from_f32(0.0, PREC))
                } else {
                    Ok(args[0].clone().sin(PREC, RM, &mut consts))
                }
            })
        );

        m.insert(
            "cos".to_string(),
            BuiltinFunc::new(1, |args, _ctx| {
                let mut consts = astro_float::Consts::new().unwrap();
                Ok(args[0].clone().cos(PREC, RM, &mut consts))
            })
        );

        m.insert(
            "tan".to_string(),
            BuiltinFunc::new(1, |args, _ctx| {
               let mut consts = astro_float::Consts::new().unwrap();
                Ok(args[0].clone().tan(PREC, RM, &mut consts))
            })
        );

        m
    };

    pub static ref BUILTIN_VALUES: HashMap<String, BigFloat> = {
        let mut m = HashMap::new();
        let mut consts = astro_float::Consts::new().unwrap();
        m.insert("pi".to_string(), consts.pi(PREC, RM));
        m.insert("e".to_string(), consts.e(PREC, RM));
        m
    };
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Context {
    functions: Vec<HashMap<String, UserFunc>>,
    values: Vec<HashMap<String, Number>>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            functions: vec![HashMap::new()],
            values: vec![HashMap::new()],
        }
    }

    pub fn add_scope(&mut self, values: HashMap<String, Number>) {
        self.values.push(values);
    }

    pub fn lookup_value(&self, name: &str) -> CalcResult {
        if let Some(value) = self
            .values
            .iter()
            .rev()
            .find(|s| s.contains_key(name))
            .and_then(|s| s.get(name).cloned())
        {
            Ok(value)
        } else if let Some(value) = BUILTIN_VALUES.get(name) {
            Ok(value.clone())
        } else {
            Err(CalcError::NameNotFound(name.to_owned()))
        }
    }

    pub fn lookup_fn(&self, name: &str) -> Result<CalcFuncRef, CalcError> {
        if let Some(func) = self
            .functions
            .iter()
            .rev()
            .find(|s| s.contains_key(name))
            .and_then(|s| s.get(name))
        {
            Ok(CalcFuncRef::UserDef(func))
        } else if let Some(func) = BUILTINS.get(name) {
            Ok(CalcFuncRef::Builtin(func))
        } else {
            Err(CalcError::NameNotFound(name.to_owned()))
        }
    }

    pub fn bind_value(&mut self, name: String, value: Number) -> CalcResult {
        // Make sure you don't overwrite a builtin
        if BUILTIN_VALUES.contains_key(&name) {
            Err(CalcError::NameNotFound(name))
        } else {
            self.values.last_mut().unwrap().insert(name, value.clone());
            Ok(value)
        }
    }

    pub fn bind_fn(&mut self, name: String, func: UserFunc) -> Result<(), CalcError> {
        // Make sure you don't overwrite a builtin
        if BUILTINS.contains_key(&name) {
            Err(CalcError::NameAlreadyBound(name))
        } else {
            self.functions.last_mut().unwrap().insert(name, func);
            Ok(())
        }
    }
}
