use std::sync::atomic::AtomicU32;

use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemFn, parse_macro_input};

static DEPTH: AtomicU32 = AtomicU32::new(0);

#[proc_macro_attribute]
pub fn log_call(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);

    let vis = &input.vis;
    let sig = &input.sig;
    let block = &input.block;
    let name = &sig.ident;

    let expanded = quote! {
        #vis #sig {
            println!("BEGIN {}", stringify!(#name));
            let __result = (|| #block)();
            println!("END {}", stringify!(#name));
            __result
        }
    };

    expanded.into()
}
