use std::iter::Iterator;
use std::clone::Clone;
use std::sync::Arc;

#[derive(Clone)]
enum QRE<D> {
    Bot,
    Eps{c: f64},
    Sat{phi: fn(&D) -> bool, op: fn(&D) -> f64},
    Choice{v: Vec<QRE<D>>},
    Split{f: Box<QRE<D>>, g: Box<QRE<D>>, op: fn(f64,f64) -> f64},
    Iter{init: Box<QRE<D>>, body: Box<QRE<D>>, op: fn(f64,f64) -> f64},
    App{f: Box<QRE<D>>, op: Arc<Fn(f64) -> f64>},
    Combine{f: Box<QRE<D>>, g: Box<QRE<D>>, op: fn(f64,f64) -> f64},    
}

use self::QRE::*;

fn epsilon<D>(q: &QRE<D>) -> Vec<f64> {
    match q {
        Bot => vec![],
        Eps{c} => vec![*c],
        Sat{phi, op} => vec![],
        Choice{v} => {
            let mut vnew = Vec::new();
            for q in v {
                vnew.append(&mut epsilon(&*q))
            };
            vnew
        },
        Split{f, g, op} => {
            let mut acc = vec![];
            for x in &epsilon(&*f)[..] {
                for y in &epsilon(&*f)[..] {
                    acc.push(op(*x, *y))
                }
            };
            acc
        },
        Iter{init, body, op} => epsilon(&*init),
        App{f, op} => {
            let mut acc = vec![];
            for x in &epsilon(&*f)[..] {
                acc.push(op(*x))
            };
            acc
        },
        Combine{f, g, op} => {
            let mut acc = vec![];
            for x in &epsilon(&*f)[..] {
                for y in &epsilon(&*f)[..] {
                    acc.push(op(*x, *y))
                }
            };
            acc
        }
    }
}

fn deriv<D>(q: QRE<D>, d: &D) -> Vec<QRE<D>> where D: Clone {
    match q {
        Bot => vec![Bot],
        Eps{..} => vec![Bot],
        Sat{phi, op} if phi(d) => vec![Eps{c: op(d)}],
        Sat{phi, ..} => vec![Bot],        
        Choice{v} => {
            let mut vnew = Vec::new();
            for q in v {
                vnew.append(&mut deriv(q, d))
            };
            vnew
        },
        Split{f, g, op} => {
            let mut vnew = Vec::new();
            for a in epsilon(&*f) {
                vnew.push(App{f: Box::new(Choice{v: deriv((*g).clone(), d)}),
                              op: Arc::new(move |x| op(a, x))})
            };
            vnew.push(
                Split{f: Box::new(Choice{v: deriv(*f, d)}),
                      g: g,
                      op: op});
            vnew
        },
        Iter{init, body, op} => {
            let mut vnew = Vec::new();
            for b in epsilon(&*init) {
                vnew.push(Iter{
                    init: Box::new(App{f: Box::new(Choice{v: deriv((*body).clone(), d)}),
                                       op: Arc::new(move |x| op(b, x))}),
                    body: body.clone(),
                    op: op})
            };
            vnew.push(
                Iter{init: Box::new(Choice{v: deriv(*init, d)}),
                     body: body,
                     op: op});
            vnew
        },
        App{f, op} => vec![App{f: Box::new(Choice{v: deriv(*f, d)}), op: op}], 
        Combine{f, g, op} =>
            vec![Combine{f: Box::new(Choice{v: deriv(*f, d)}),
                         g: Box::new(Choice{v: deriv(*g, d)}),
                         op: op}],
    }
}

fn main() {
}
