/*!
A builder for `isotope` IR functions from syntax
*/
use super::*;
use isotope_syntax as syn;

/// A builder for `isotope` IR functions from syntax
#[derive(Debug, Default)]
pub struct FunctionBuilder {}

/// An error when building `isotope` syntax
#[derive(Debug)]
pub enum SyntaxError {
    BitvectorError,
    NotImplemented,
}

impl FunctionBuilder {
    /// Build a type into a function
    pub fn ty(&mut self, ty: &syn::Type, func: &mut Function) -> Result<TypeId, SyntaxError> {
        match ty {
            syn::Type::Ident(_) => Err(SyntaxError::NotImplemented),
            syn::Type::Tuple(t) => {
                let t: Result<Vec<_>, _> = t.iter().map(|ty| self.ty(ty, func)).collect();
                Ok(func.tuple_ty(t?))
            }
        }
    }

    /// Build an expression into a function
    pub fn expr(&mut self, expr: &syn::Expr, func: &mut Function) -> Result<ValId, SyntaxError> {
        match expr {
            // syn::Expr::Ident(_) => todo!(),
            // syn::Expr::App(_) => todo!(),
            // syn::Expr::Tuple(_) => todo!(),
            // syn::Expr::Let(_) => todo!(),
            // syn::Expr::Splice(_) => todo!(),
            syn::Expr::Bitvector(b) => {
                Ok(func.insert_bitvector(b.try_into().map_err(|_| SyntaxError::BitvectorError)?))
            }
            _ => Err(SyntaxError::NotImplemented),
        }
    }

    /// Build a definition into the current basic block, returning and binding its associated expression
    pub fn bind_def(
        &mut self,
        _pattern: &syn::Pattern,
        expr: &syn::Expr,
        func: &mut Function,
    ) -> Result<ValId, SyntaxError> {
        let _value = self.def(expr, func)?;
        //TODO: bind variables in symbol table
        Err(SyntaxError::NotImplemented)
    }

    /// Build a definition into the current basic block, returning its associated value
    pub fn def(&mut self, expr: &syn::Expr, func: &mut Function) -> Result<InstId, SyntaxError> {
        let _value = self.expr(expr, func)?;
        //TODO: bind value to instruction if not central
        Err(SyntaxError::NotImplemented)
    }

    // /// Build a basic block into a function
    // pub fn block(
    //     &mut self,
    //     block: &syn::Block,
    //     func: &mut Function,
    // ) -> Result<BlockId, SyntaxError> {
    //     Err(SyntaxError::NotImplemented)
    // }
}
