extern crate proc_macro;

use ::proc_macro::TokenStream;
use ::proc_macro2::{Span, TokenStream as TokenStream2, Group, TokenTree};
use ::quote::quote;
use ::syn::{*, Result};

#[proc_macro_derive(Reflect, attributes(reflect))]
pub fn reflect_derive(input: TokenStream) -> TokenStream 
{
  let ast = syn::parse(input).unwrap();
  
  TokenStream::from(match impl_derive(&ast) 
  {
    Ok(it) => it,
    Err(err) => err.to_compile_error(),
  })
}

enum ReflectAttributeType
{
  Skip,
  Rename(String),
  With(String),
  None,
}

struct ReflectAttribute
{
  skip : bool,
  rename : Option<String>,
  with : Option<String>,
}

fn get_reflect_attribute(group : Group) -> ReflectAttributeType
{
    let mut found_ident : Option<String> = None;
    let mut found_literal : Option<String> = None;

    let tokens = group.stream();
    for token in tokens.into_iter()
    {
      match token 
      {
         TokenTree::Ident(ident) => {  found_ident = Some(ident.to_string()); },
         //literal is quoted as we pass it quoted, so we must remove quote
         TokenTree::Literal(literal) => { let lit_str = literal.to_string(); 
                                          found_literal = Some(lit_str[1..lit_str.len() - 1].to_string())},
         _ => (),
      }
    }

    if let Some(ident) = found_ident
    {
      match ident.as_ref() 
      {
        "skip" => return ReflectAttributeType::Skip,
        "rename" => { if let Some(found_literal) = found_literal 
                         { return ReflectAttributeType::Rename(found_literal) } 
                    },
        "with" => { if let Some(found_literal) = found_literal 
                         { return ReflectAttributeType::With(found_literal) } 
                    },
        _ => (),
      }
    }
    ReflectAttributeType::None
}

fn get_attribute(field: &Field) -> ReflectAttribute
{
    let mut reflect_attribute = ReflectAttribute{ skip : false, rename : None, with : None};
    
    for attribute in &field.attrs
    {
      if attribute.path.is_ident("reflect")
      {
        let group : Group = syn::parse2(attribute.tokens.clone()).unwrap(); //use into_iter rather than reparse
        match  get_reflect_attribute(group)
        {
          ReflectAttributeType::Skip => {reflect_attribute.skip = true},
          ReflectAttributeType::Rename(name) => { reflect_attribute.rename = Some(name)  },
          ReflectAttributeType::With(name) => { reflect_attribute.with = Some(name)  },
          _ => (),
        }
      }
    }
    reflect_attribute
}

fn impl_derive(ast: &syn::DeriveInput) -> Result<TokenStream2>
{
    let name = &ast.ident;
    let data = &ast.data;

    let fields = match data {
        | Data::Enum(DataEnum { enum_token: token::Enum { span }, .. })
        //=> it, //XXX must handle enum but use 0,1,2 ... to access fields ...
        | Data::Union(DataUnion { union_token: token::Union { span }, .. })
        => {
            return Err(Error::new(
                *span,
                "Expected a `struct`",
            ));
        },

        | Data::Struct(DataStruct { fields: Fields::Named(it), .. })
        => it,
        
        | Data::Struct(_)
        => {
            return Err(Error::new(
                Span::call_site(),
                "Expected a `struct` with named fields",
            ));
        },
    };

    let named = &fields.named;

    let mut infos_data : Vec<TokenStream2> = Vec::new();
    let mut get_value_data : Vec<TokenStream2> = Vec::new();

for field in named.into_iter()
    {
        let ident = &field.ident;
        let reflect_attribute = get_attribute(field); 

        if !reflect_attribute.skip
        {
          let field_name = ident.as_ref().expect("Unreachable");
          let span = field_name.span();
          let field_name_stringified =  LitStr::new(&field_name.to_string(), span);

          match reflect_attribute.rename
          {
            Some(name) =>
            {
              infos_data.push(quote!{ (#name, None) });
              match reflect_attribute.with
              {
                None => get_value_data.push(quote!{ #name => Some(Value::from(self.#field_name.clone()))}),
                //transform func to token not string ...
                Some(func) => {
                        let func_ident = Ident::new(&func, Span::call_site());
                        get_value_data.push(quote!{#name => #func_ident(&self.#field_name)});
                    },
              }
            }, 
            None => 
            {
              infos_data.push(quote!{ (#field_name_stringified, None) });
              match reflect_attribute.with
              {
                None => get_value_data.push(quote!{ #field_name_stringified => Some(Value::from(self.#field_name.clone()))}),
                Some(func) => { 
                        //transform func string to ident  
                        let func_ident = Ident::new(&func, Span::call_site());
                        get_value_data.push(quote!{#field_name_stringified => #func_ident(&self.#field_name)});
                    },
              }
            }
          }
        }
    }

    let gen = quote! 
    {
      impl ReflectStruct for #name 
      {
        fn name(&self) -> &'static str
        {
          stringify!(#name)
        }

        fn infos(&self) -> Vec<(&'static str, Option<&'static str>) >
        {
          vec![#(#infos_data,)*]
        }

        fn get_value(&self, name : &str) -> Option<Value>
        {
          match name
          {
           #(#get_value_data,)*
           _ => None,
          }
        }
      }
    };
    Ok(gen)
}
