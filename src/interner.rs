use std::{collections::HashMap, fmt::Debug, ops::Index};

pub type Intern = usize;

pub struct Interner {
    vec: Vec<String>,
    map: HashMap<String, Intern>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, symbol: &str) -> Intern {
        match self.map.entry(symbol.to_string()) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                let sym = self.vec.len();
                self.vec.push(symbol.to_string());
                entry.insert(sym);
                sym
            }
        }
    }
}

impl Index<Intern> for Interner {
    type Output = str;

    fn index(&self, idx: Intern) -> &str {
        &self.vec[idx]
    }
}

impl Debug for Interner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.vec)
    }
}
