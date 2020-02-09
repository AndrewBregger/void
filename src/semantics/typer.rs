use super::{
    Diagnostics,
    scope::{Scope, ScopeManager, ScopeKind, ScopeId},
    item_info::{ItemId, ItemInfo},
    types::*,
    Error,
    Result,
};
use crate::syntax::{ast::*, token::Op};

use std::rc::Rc;
use std::collections::HashSet;



pub struct Environment<'env> {
    /// state of everything processed
    pub scope_manager: &'env mut ScopeManager,
    /// all the names of currently unresolved toplevel items
    pub unresolved_top_level_names: &'env HashSet<String>,
    /// true if this scope item is in the top level of a module.
    pub is_top_level: bool,
}

impl<'env> Environment<'env> {
    pub fn new(scope_manager: &'env mut ScopeManager, unresolved_top_level_names: &'env HashSet<String>, is_top_level: bool) -> Self {
        Self {
            scope_manager,
            unresolved_top_level_names,
            is_top_level
        }
    }

    pub fn check_name(&self, name: &String) -> bool {
        let current = self.scope_manager.get_current();
        self.scope_manager.scope_contains(current, name)
    }

    /// checks if the the names defined by this item already exist
    pub fn check_item_names(&self, item: &Ptr<Item>) -> Option<&ItemInfo> {
        use crate::syntax::ast::ItemKind::*;

        match item.kind() {
            Funct(_) |
            Struct(_) => {
                let name = item.name();
                self.scope_manager.lookup_current(name)
            },
            LocalVar(Local {
                local,
                ..
            }) => {
                for name in local.collect_names() {
                    // i only care about the current scope. Not its parents.
                    let current = self.scope_manager.get_current();
                    match current.lookup(name) {
                       Some(item) => return Some(item),
                       None => {},
                    }
                }
                None
            }
           e => {
                println!("Chekcing for names of invalid items: {:?}", e);
                None
            }
        }
    }
}


pub struct Typer<'diag> {
    diagnostics: &'diag mut Diagnostics
}

impl<'diag> Typer<'diag> {
    pub fn new(diagnostics: &'diag mut Diagnostics) -> Self {
        Self {
            diagnostics
        }
    }

    fn resolve_unary_expr<'scope>(&mut self, env: &Environment<'scope>, op: Op, expr: &mut Ptr<Expr>) -> Result<Type> {
        let ty = self.resolve_expr(env, expr, None)?;
        unimplemented!()
        // match op {
        //     Op::Minus => {
                
        //     },
        //     Op::Tilde => {},
        //     Op::Ampersand => {},
        //     Op::Astrick => {},
        //     _ => {
        //         self.diagnostics.fatal(format!("Invalid unary operand: {}", op.to_string()).as_str());
        //         return Err(Error::FatalError);
        //     },
        // }

    }

    fn resolve_expr<'scope>(&mut self, env: &Environment<'scope>, expr: &mut Ptr<Expr>, expected_type: Option<&Type>) -> Result<Type> {
        use ExprKind::*;
        
        let ty = match &mut expr.kind {
            Name(..) |
            NameTyped(..) |
            Field(..) => {
                let info = self.resolve_named_expr(env, expr, expected_type)?;
                info.item_type().clone()
            },
            IntegerLiteral(_) => Type::new(TypeKind::U32),
            FloatLiteral(_) => Type::new(TypeKind::F32),
            StringLiteral(_) => Type::new(TypeKind::Str),
            CharLiteral(_) => Type::new(TypeKind::Char),
            Unary(op, ref mut operand) => self.resolve_unary_expr(env, op.clone(), operand)?,
            Binary(op, lhs, rhs) => { unimplemented!() },
            FunctionCall(operand, actuals) => { unimplemented!() },
            MethodCall(operand, actuals) => { unimplemented!() },
            Block(stmts) => { unimplemented!() },
            If(cond, body) => { unimplemented!() },
            While(cond, body) => { unimplemented!() },
            For(element, operand, body) => { unimplemented!() },
            Loop(body) => { unimplemented!() },
        };
        // ty.resolved_type = ty.clone()
        Ok(ty)
    }

    fn resolve_named_expr<'env>(&mut self, env: &'env Environment<'env>, expr: &mut Ptr<Expr>, expected_type: Option<&Type>) -> Result<&'env ItemInfo> {
        use ExprKind::*;
        match &mut expr.kind {
            Name(n) => {
                let current = env.scope_manager.get_current();
                match env.scope_manager.lookup_current(n.value_string()) {
                    Some(item) => Ok(item),
                    None => {
                        self.diagnostics.type_error(format!("use of undeclared name '{}'", n.value()).as_str(), n.pos().clone());
                        Err(Error::TypeError)
                    }
                }
            },
            NameTyped(n, type_params) => {
                unimplemented!("Type parameters are not implemented")
            },
            Field(ref mut operand, ref mut field) => {
                let operand_type = self.resolve_expr(env, operand, None)?;
                if let TypeKind::Struct(ref structure) = operand_type.kind() {
                    let structure_scope = env.scope_manager.get_scope(structure.field_scope());

                    match structure_scope.lookup(field.value_string()) {
                        Some(item) => { Ok(item) }
                        None => {
                            self.diagnostics.type_error(format!("'{}' is not a field of structure '{:?}'", field.value(), operand_type).as_str(), field.pos().clone());
                            Err(Error::TypeError)
                        }
                    }
                }
                else {
                    // @TODO: change the debug print to a formated output of the type.
                    self.diagnostics.type_error(format!("attempting to access field of non-structure type: {:?}", operand_type).as_str(), operand.pos().clone());
                    Err(Error::TypeError)
                }
            },
            _ => {
                unreachable!("{:#?}", expr.kind);
            }
        }
    }

    fn resolve_type_spec<'env>(&mut self, env: &mut Environment<'env>, type_spec: &mut Ptr<TypeSpec>) -> Result<Type> {
        use TypeSpecKind::*;
        match &mut type_spec.kind {
            ExprType(ref mut expr) => {
                let info = self.resolve_named_expr(env, expr, None)?;
                if info.is_type() {
                    Ok(info.item_type().clone())
                }
                else {
                    self.diagnostics.type_error(format!("'{}' must refer to a type", info.name()).as_str(), expr.pos().clone());
                    Err(Error::TypeError)
                }
            },
            MutType(ref mut spec) => {
                let ty = self.resolve_type_spec(env, spec)?;
                Ok(Type::new(TypeKind::MutType(Rc::new(ty))))
            },
            TupleType(ref mut tuple) => {
                let mut sub_types = Vec::new();
                for ty in tuple {
                    let t = self.resolve_type_spec(env, ty)?;
                    sub_types.push(t);
                }

                Ok(Type::new(TypeKind::Tuple(sub_types)))
            },
            PtrType(ref mut spec) => {
                let ty = self.resolve_type_spec(env, spec)?;
                Ok(Type::new(TypeKind::PtrType(Rc::new(ty))))
            },
            UnitType => {
                let prelude = env.scope_manager.get_prelude();
                match prelude.lookup("__UNIT__") {
                    Some(item) => Ok(item.item_type().clone()),
                    None => {
                        self.diagnostics.fatal("Undeclared langauge defined type '__UNIT__'");
                        Err(Error::FatalError)
                    }
                }
            },
        }
    }

    fn resovle_function<'env>(&mut self, env: &mut Environment<'env>, function: &mut Function) -> Result<ItemInfo> {
        unimplemented!()
    }

    fn resolve_struct_field<'env>(&mut self, env: &mut Environment<'env>, item: &mut Ptr<Item>) -> Result<ItemInfo> {
        use VariableKind::*;

        println!("{}:{}|resolve_struct_field({})", file!(), line!(), item.name());
        
        let name = item.name().clone();
        match &mut item.kind {
            ItemKind::StructField(field) => {
                match &env.scope_manager.lookup_current(&name.to_string()) {
                    Some(ref other_field) => {
                        self.diagnostics.type_error(format!("previously declared member '{}'", name).as_str(), field.name().pos().clone());
                        self.diagnostics.info_with_line("previosly defined:", other_field.pos().clone());
                    }
                    None => {}
                }

                let expr_type = match &mut field.kind {
                    Init(ref mut init) => self.resolve_expr(env, init, None),
                    Typed(ref mut type_spec) => self.resolve_type_spec(env, type_spec),
                    Full(ref mut type_spec, ref mut init) => {
                        let type_spec = self.resolve_type_spec(env, type_spec)?;
                        self.resolve_expr(env, init, Some(&type_spec))?;
                        Ok(type_spec)
                    }
                }?;

                println!("Field '{}': {}", name, expr_type);

                let current = env.scope_manager.get_current();
                Ok(ItemInfo::resolved(field.name().clone(), item.clone(), expr_type, Some(current.id())).with_item(item.clone()))
            }
            _ => {
                self.diagnostics.fatal("Compiler Error: invalid structure field item");
                Err(Error::TypeError)
            }   
        }
    }

    fn resolve_structure<'env>(&mut self, env: &mut Environment<'env>, structure: &mut Structure) -> Result<ItemInfo> {
        let type_params_scope: Option<ScopeId> = if let Some(type_params) = structure.type_params.as_ref() {
            // resolve type parameters
            None
        }
        else { None };

        env.scope_manager.push_scope(ScopeKind::Struct);
        
        for mut field in structure.fields.as_mut_slice() {
            let field_item = self.resolve_struct_field(env, &mut field)?;
            let current = env.scope_manager.get_current_mut();

            current.entry(field_item.name().to_string())
                .or_insert(field_item);
        }

        let field_scope = env.scope_manager.get_current().id();

        env.scope_manager.pop_scope();

        if type_params_scope.is_some() {
            env.scope_manager.pop_scope();
        }

        let name = structure.name.clone();

        // in this case, the info needs to be created so the item id can be created.
        let info = ItemInfo::unresolved(name.clone(), Item::invalid_ptr(), Some(env.scope_manager.get_current().id()));
        let ty = StructureType::new(name, field_scope, type_params_scope, info.id());

        Ok(info.resolve(Type::new(TypeKind::Struct(ty))))
    }

    fn resolve_local<'env>(&mut self, env: &mut Environment<'env>, variable: &Local) -> Result<ItemInfo> {
        unimplemented!()
    }

    /// Resolves an item using the given environment
    pub fn resolve_item<'env>(&mut self, env: &mut Environment<'env>, item: &mut Ptr<Item>) -> Result<ItemInfo> {
        if env.is_top_level {
            if item.is_variable() {
                self.diagnostics.type_error("invalid variable declaration in toplevel scope", item.pos().clone());
                return Err(Error::TypeError);
            }
        }

        match env.check_item_names(&item) {
            Some(other) => {
                self.diagnostics.type_error(format!("redefinition of name '{}'", other.name()).as_str(), item.pos().clone());
                return Err(Error::TypeError);
            },
            None => {},
        }

        let info = match item.kind {
            ItemKind::Funct(ref mut funct) => self.resovle_function(env, funct),
            ItemKind::Struct(ref mut structure) => self.resolve_structure(env, structure),
            ItemKind::LocalVar(ref mut variable) => self.resolve_local(env, variable),
            _ => {
                Err(Error::TypeError)
            },
        }?;

        Ok(info.with_item(item.clone()))
    }
}