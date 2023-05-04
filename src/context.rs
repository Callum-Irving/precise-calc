//! Contains the [Context] struct used to store functions and values currently defined.

use std::collections::HashMap;

use astro_float::BigFloat;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

use crate::ast::{BuiltinFunc, CalcFuncRef, UserFunc};
use crate::builtins;
use crate::{CalcError, CalcResult, Number, PREC, RM};

lazy_static! {
    /// HashMap of all builtin functions (name mapped to function).
    pub static ref BUILTINS: HashMap<String, BuiltinFunc> = {
        let mut m = HashMap::new();

        // Square root function
        m.insert(
            "sqrt".to_string(),
            BuiltinFunc::new(1, builtins::sqrt)
        );

        // Natural logarithm
        m.insert(
            "ln".to_string(),
            BuiltinFunc::new(1, builtins::ln)
        );

        // Logarithm of any base
        m.insert(
            "log".to_string(),
            BuiltinFunc::new(1, builtins::log)
        );

        // Trig functions
        m.insert(
            "sin".to_string(),
            BuiltinFunc::new(1, builtins::sin)
        );

        m.insert(
            "cos".to_string(),
            BuiltinFunc::new(1, builtins::cos)
        );

        m.insert(
            "tan".to_string(),
            BuiltinFunc::new(1, builtins::tan)
        );

        m
    };

    /// Builtin values such as e or pi.
    pub static ref BUILTIN_VALUES: HashMap<String, BigFloat> = {
        let mut m = HashMap::new();
        let mut consts = astro_float::Consts::new().unwrap();
        m.insert("pi".to_string(), consts.pi(PREC, RM));
        m.insert("e".to_string(), consts.e(PREC, RM));
        m
    };
}

/// The context for evaluating expressions and statements in.
///
/// Maps names to functions and values.
#[derive(Clone, Serialize, Deserialize)]
pub struct Context {
    functions: Vec<HashMap<String, UserFunc>>,
    values: Vec<HashMap<String, Number>>,
}

impl Context {
    /// Create a new empty context.
    pub fn new() -> Context {
        Context {
            functions: vec![HashMap::new()],
            values: vec![HashMap::new()],
        }
    }

    /// Add a new scope of values to the context.
    pub fn add_scope(&mut self, values: HashMap<String, Number>) {
        self.values.push(values);
    }

    /// Lookup a value in the context. Returns the [CalcError::NameNotFound] if the name is not
    /// mapped in the context (in any scope). If the name is in multiple scopes it returns the
    /// value from the last scope.
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

    /// Lookup a function in the context. Returns the [CalcError::NameNotFound] if the name is not
    /// mapped in the context (in any scope). If the name is in multiple scopes it returns the
    /// value from the last scope.
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

    /// Bind `value` to `name` in the top scope. If `name` is the name of a builtin value nothing
    /// will be bound and the function will return [CalcError::NameAlreadyBound].
    pub fn bind_value(&mut self, name: String, value: Number) -> CalcResult {
        // Make sure you don't overwrite a builtin
        if BUILTIN_VALUES.contains_key(&name) {
            Err(CalcError::NameAlreadyBound(name))
        } else {
            self.values.last_mut().unwrap().insert(name, value.clone());
            Ok(value)
        }
    }

    /// Bind `func` to `name` in the top scope. If `name` is the name of a builtin function nothing
    /// will be bound and the function will return [CalcError::NameAlreadyBound].
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
