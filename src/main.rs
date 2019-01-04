use std::iter::Iterator;
use std::clone::Clone;
use std::sync::Arc;
use std::time::{Instant};

#[derive(Clone)]
enum QRE<D,C> {
    Bot,
    Eps{c: C},
    Sat{phi: fn(&D) -> bool, op: fn(&D) -> C}, 
    Choice{v: Vec<QRE<D,C>>},
    Split{f: Box<QRE<D,C>>, g: Box<QRE<D,C>>, op: fn(C,C) -> C},
    Iter{init: Box<QRE<D,C>>, body: Box<QRE<D,C>>, op: fn(C,C) -> C},
    App{f: Box<QRE<D,C>>, op: Arc<Fn(C) -> C>},
    Combine{f: Box<QRE<D,C>>, g: Box<QRE<D,C>>, op: fn(C,C) -> C},    
}

use self::QRE::*;

fn epsilon<D,C>(q: &QRE<D,C>) -> Vec<C> where C: Clone {
    match q {
        Bot => vec![],
        Eps{c} => vec![c.clone()],
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
                    acc.push(op(x.clone(), y.clone()))
                }
            };
            acc
        },
        Iter{init, body, op} => epsilon(&*init),
        App{f, op} => {
            let mut acc = vec![];
            for x in &epsilon(&*f)[..] {
                acc.push(op(x.clone()))
            };
            acc
        },
        Combine{f, g, op} => {
            let mut acc = vec![];
            for x in &epsilon(&*f)[..] {
                for y in &epsilon(&*g)[..] {
                    acc.push(op(x.clone(), y.clone()))
                }
            };
            acc
        }
    }
}

fn deriv<D,C: 'static>(q: QRE<D,C>, d: &D) -> Vec<QRE<D,C>> where D: Clone, C: Clone {
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
                              op: Arc::new(move |x| op(a.clone(), x))})
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
                                       op: Arc::new(move |x| op(b.clone(), x))}),
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

struct Solve<D,C: 'static> {
    pub state: Vec<QRE<D,C>>,
    max_workingset: u64,
}

impl <D,C> Solve<D,C> where D: Clone, C: Clone {
    pub fn new(q: QRE<D,C>) -> Self {
        Self {
            state: vec![q],
            max_workingset: 0
        }
    }

    pub fn update(&mut self, d: D) -> () {
        let mut vnew = Vec::new();
        for q in &self.state[..] {
            vnew.append(&mut deriv(q.clone(), &d))
        };
        let len = vnew.len() as u64;
        self.state = vnew;
        if len > self.max_workingset {
            self.max_workingset = len
        }
    }

    pub fn output(&self) -> Result<C, String> {
        let mut cnew = Vec::new();
        for q in &self.state[..] {
            cnew.append(&mut epsilon(&q.clone()))
        };
        if cnew.len() == 1 {
            println!("max_workingset = {}", self.max_workingset);
            Ok(cnew[0].clone())
        }
        else {
            Err("undefined".to_string())
        }
    }
}

fn true_f64(_x: &f64) -> bool { true }
fn id_f64(x: &f64) -> f64 { *x }
fn zero<D>(_x: &D) -> f64 { 0.0 }
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
    let gbody = Sat{phi: true_f64, op: zero};
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
    let zero = Sat{phi: true_f64, op: zero};
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

#[derive(Clone)]
struct Record {
    name: String,
    amount: f64
}

fn match_pred(r: &Record) -> bool { r.name == "Gordon".to_string() }
fn notmatch_pred(r: &Record) -> bool { r.name != "Gordon".to_string() }
fn proj_amount(r: &Record) -> f64 { r.amount }

fn aggregate() {
    let f =
        Choice{
            v: vec![Sat{phi: match_pred, op: proj_amount},
                    Sat{phi: notmatch_pred, op: zero}]
        };
    let agg_gordon = Iter{
        init: Box::new(f.clone()),
        body: Box::new(f),
        op: sum_f64
    };
    let mut s = Solve::new(agg_gordon);

    for x in 0..10 {
        if x % 2 == 0 {
            s.update(Record{
                name: "NotGordon".to_string(),
                amount: 3.0
            })
        } else { 
            s.update(Record{
                name: "Gordon".to_string(),
                amount: 10.0
            })
        }
    }
    println!("{:?}", s.output())
}

fn main() {
    //Example 14 from https://www.cis.upenn.edu/~alur/KimFest17.pdf
    example14();

    //Compute a running average of the numbers from 0 to 100
    running_avg();

    aggregate();
    
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
    let elapsed2 = now2.elapsed();
    println!("time = {}s, {}ms", elapsed2.as_secs(), elapsed2.subsec_millis());
}
