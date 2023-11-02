/*!
A builder for `isotope` IR functions from syntax
*/
use super::*;
use isotope_syntax as syn;

/// A builder for `isotope` IR functions from syntax
#[derive(Debug)]
pub struct SyntaxBuilder {}

/// An error when building `isotope` syntax
#[derive(Debug)]
pub enum SyntaxError {
    BitvectorError,
}

impl SyntaxBuilder {
    /// Build a type into a function
    pub fn ty(&mut self, ty: &syn::Type, func: &mut Function) -> Result<TypeId, SyntaxError> {
        match ty {
            syn::Type::Ident(_) => todo!(),
            syn::Type::Tuple(_) => todo!(),
        }
    }

    /// Build an expression into a function
    pub fn expr(&mut self, expr: &syn::Expr, func: &mut Function) -> Result<ExprId, SyntaxError> {
        match expr {
            syn::Expr::Ident(_) => todo!(),
            syn::Expr::App(_) => todo!(),
            syn::Expr::Tuple(_) => todo!(),
            syn::Expr::Let(_) => todo!(),
            syn::Expr::Splice(_) => todo!(),
            syn::Expr::Bitvector(b) => Ok(func.insert_bitvector(b.try_into().unwrap())),
        }
    }

    /// Build a definition into a function, returning its associated expression
    pub fn def(
        &mut self,
        pattern: &syn::Pattern,
        expr: &syn::Expr,
        func: &mut Function,
    ) -> Result<ExprId, SyntaxError> {
        todo!()
    }

    /// Build a basic block into a function
    pub fn block(
        &mut self,
        block: &syn::Block,
        func: &mut Function,
    ) -> Result<BlockId, SyntaxError> {
        todo!()
    }
}
