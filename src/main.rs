use std::iter::Iterator;
use std::clone::Clone;
use std::sync::Arc;
use std::time::{Instant};

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
                for y in &epsilon(&*g)[..] {
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
                for y in &epsilon(&*g)[..] {
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

struct Solve<D> {
    pub state: Vec<QRE<D>>,
}

impl <D> Solve<D> where D: Clone {
    pub fn new(q: QRE<D>) -> Self {
        Self {state: vec![q]}
    }

    pub fn update(&mut self, d: D) -> () {
        let mut vnew = Vec::new();
        for q in &self.state[..] {
            vnew.append(&mut deriv(q.clone(), &d))
        };
        self.state = vnew
    }

    pub fn output(&self) -> Result<f64, String> {
        let mut cnew = Vec::new();
        for q in &self.state[..] {
            cnew.append(&mut epsilon(&q.clone()))
        };
        if cnew.len() == 1 { Ok(cnew[0]) }
        else {
            println!("err: {:?}", cnew);
            Err("undefined".to_string())
        }
    }
}

fn true_f64(_x: &f64) -> bool { true }
fn id_f64(x: &f64) -> f64 { *x }
fn zero_f64(_x: &f64) -> f64 { 0.0 }
fn one_f64(_x: &f64) -> f64 { 1.0 }
fn sum_f64(x: f64, y: f64) -> f64 { x + y }
fn div_f64(x: f64, y: f64) -> f64 { x / y }
fn min_f64(x: f64, y: f64) -> f64 { x.min(y) }
fn max_f64(x: f64, y: f64) -> f64 { x.max(y) }
fn pi2(x: f64, y: f64) -> f64 { y }
fn avg(x: f64, y: f64) -> f64 { (x + y) / 2.0 }

fn example14() {
    let f = Sat{phi: true_f64, op: id_f64};
    let h1 = Split{
        f: Box::new(f.clone()),
        g: Box::new(f.clone()),
        op: max_f64
    };
    let h2 = Split{
        f: Box::new(f.clone()),
        g: Box::new(f.clone()),
        op: min_f64
    };
    let gbody = Sat{phi: true_f64, op: zero_f64};
    let g = Iter{
        init: Box::new(Eps{c: 0.0}),
        body: Box::new(gbody),
        op: pi2
    };
    let k1 = Split{
        f: Box::new(g.clone()),
        g: Box::new(h1.clone()),
        op: pi2
    };
    let k2 = Split{
        f: Box::new(g),
        g: Box::new(h2),
        op: pi2
    };
    let r = Combine{
        f: Box::new(k1),
        g: Box::new(k2),
        op: avg
    };
    let mut s = Solve::new(r);
    s.update(5.0);
    s.update(4.0);    
    s.update(3.0);
    s.update(2.0);    
    s.update(1.0);    
    println!("{:?}", s.output())
}

fn running_avg() {
    let zero = Sat{phi: true_f64, op: zero_f64};
    let f = Sat{phi: true_f64, op: id_f64};
    let g = Sat{phi: true_f64, op: one_f64};
    let sum = Iter{
        init: Box::new(zero.clone()),
        body: Box::new(f),
        op: sum_f64
    };
    let len = Iter{
        init: Box::new(zero.clone()),
        body: Box::new(g),
        op: sum_f64
    };
    let avg = Combine{
        f: Box::new(sum),
        g: Box::new(len),
        op: div_f64
    };
    let mut s = Solve::new(avg);

    for x in 0..101 {
        s.update(x as f64);
    }
    println!("{:?}", s.output())            
}

fn main() {
    //Example 14 from https://www.cis.upenn.edu/~alur/KimFest17.pdf
    example14();

    //Compute a running average of the numbers from 0 to 100
    running_avg();
    
    let f = Sat{phi: true_f64, op: id_f64};
    let r = Iter{init: Box::new(f.clone()),
                 body: Box::new(f),
                 op: sum_f64};
    let mut s = Solve::new(r);

    //Compute T(1000) using QREs
    let now = Instant::now();
    for x in 0..1001 { s.update(x as f64) }
    println!("{:?}", s.output());
    let elapsed = now.elapsed();
    println!("QRE time = {}s, {}ms", elapsed.as_secs(), elapsed.subsec_millis());

    //Compute T(1000) by iteration
    let mut t = 0.0;
    let now2 = Instant::now();    
    for x in 0..1001 { t = t + (x as f64) }
    println!("{:?}", t);
    let elapsed2 = now.elapsed();
    println!("time = {}s, {}ms", elapsed2.as_secs(), elapsed2.subsec_millis());
}
