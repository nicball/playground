use std::ops::{Index, IndexMut};
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub struct Board {
    size: usize,
    blocks: Vec<Vec<BlockState>>
}

impl Index<Coord> for Board {
    type Output = BlockState;
    fn index(&self, c: Coord) -> &BlockState {
        &self.blocks[c.x][c.y]
    }
}

impl IndexMut<Coord> for Board {
    fn index_mut(&mut self, c: Coord) -> &mut BlockState {
        &mut self.blocks[c.x][c.y]
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Color {
    Black,
    White
}

#[derive(Clone)]
pub enum BlockState {
    Empty,
    Occupied { color: Color, parent: Option<Coord> }
}

impl BlockState {
    fn get_parent(&self) -> &Option<Coord> {
        match self {
            BlockState::Empty => unreachable!(),
            BlockState::Occupied { parent: p, .. } => p
        }
    }
    fn get_parent_mut(&mut self) -> &mut Option<Coord> {
        match self {
            BlockState::Empty => unreachable!(),
            BlockState::Occupied { parent: p, .. } => p
        }
    }
    fn is_empty(&self) -> bool {
        match self {
            BlockState::Empty => true,
            BlockState::Occupied { .. } => false
        }
    }
    fn get_color(&self) -> Option<Color> {
        match self {
            BlockState::Empty => None,
            BlockState::Occupied { color, .. } => Some(*color)
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Coord {
    pub x: usize,
    pub y: usize
}

impl Coord {
    pub fn new(x: usize, y: usize) -> Coord {
        Coord { x, y }
    }
}

impl Board {
    pub fn new(size: usize) -> Board {
        let mut blocks = Vec::with_capacity(size);
        for _ in 0 .. size {
            let mut row = Vec::with_capacity(size);
            for _ in 0 .. size { row.push(BlockState::Empty); }
            blocks.push(row);
        }
        Board { size, blocks }
    }
    pub fn size(&self) -> usize { self.size }
    fn neighbors(&self, c: Coord) -> Vec<Coord> {
        let (x, y): (i32, i32) = (c.x.try_into().unwrap(), c.y.try_into().unwrap());
        let cs = vec![
                        (x, y - 1),
            (x - 1, y),             (x + 1, y),
                        (x, y + 1)
        ];
        cs.iter()
            .filter(|x| x.0 < self.size as i32 && x.0 >= 0 && x.1 < self.size as i32 && x.1 >= 0)
            .copied()
            .map(|(x, y)| Coord { x: x.try_into().unwrap(), y: y.try_into().unwrap() })
            .collect()
    }
    fn repr_mut(&mut self, c: Coord) -> Coord {
        let mut to_be_changed = Vec::new();
        let mut r = c;
        while let Some(p) = self[r].get_parent() {
            to_be_changed.push(r);
            r = *p;
        }
        while let Some(c) = to_be_changed.pop() {
            match &mut self[c] {
                BlockState::Empty => unreachable!(),
                BlockState::Occupied { parent, .. } => *parent = Some(r)
            }
        }
        r
    }
    pub fn repr(&self, c: Coord) -> Coord {
        match self[c].get_parent() {
            None => c,
            Some(p) => self.repr(*p)
        }
    }
    fn union(&mut self, cs: &Vec<Coord>) -> Coord {
        let r = cs[0];
        for &c in cs.iter().skip(1) {
            *self[c].get_parent_mut() = Some(r);
        }
        r
    }
    pub fn add_piece(&mut self, color: Color, pos: Coord) -> bool {
        if !self[pos].is_empty() { return false; }
        let friendlies = self
            .neighbors(pos)
            .iter()
            .filter(|&&p| self[p].get_color() == Some(color))
            .copied()
            .collect::<Vec<Coord>>();
        let friendlies = friendlies
            .iter()
            .map(|&p| self.repr_mut(p))
            .collect::<Vec<Coord>>();
        let mut r = None;
        let mut other = self.clone();
        if !friendlies.is_empty() {
            r = Some(other.union(&friendlies));
        }
        other[pos] = BlockState::Occupied { color, parent: r };
        let r = r.unwrap_or(pos);
        let deads = other
            .breath()
            .iter()
            .filter(|&(_, &n)| n == 0).map(|(&p, _)| p)
            .collect::<Vec<Coord>>();
        if deads.len() == 1 && deads[0] == r {
            false
        }
        else {
            for c in deads {
                if c != r {
                    other.remove(c);
                }
            }
            *self = other;
            true
        }
    }
    fn breath(&mut self) -> HashMap<Coord, usize> {
        let mut free_space = HashMap::<Coord, HashSet<Coord>>::new();
        for x in 0 .. self.size {
            for y in 0 .. self.size {
                let c = Coord::new(x, y);
                if !self[c].is_empty() {
                    let r = self.repr_mut(Coord::new(x, y));
                    free_space.insert(r, HashSet::new());
                }
            }
        }
        for x in 0 .. self.size {
            for y in 0 .. self.size {
                let c = Coord::new(x, y);
                match &self[c] {
                    BlockState::Empty => {
                        let adjs = self
                            .neighbors(c)
                            .iter()
                            .filter(|&&p| !self[p].is_empty()).copied()
                            .collect::<Vec<Coord>>();
                        for g in adjs {
                            let r = self.repr(g);
                            match free_space.get_mut(&r) {
                                Some(s) => { s.insert(c); },
                                None => unreachable!()
                            }
                        }
                    },
                    BlockState::Occupied { .. } => ()
                }
            }
        }
        free_space.iter().map(|(&k, v)| (k, v.len())).collect()
    }
    fn remove(&mut self, r: Coord) {
        let mut to_be_removed = Vec::new();
        for x in 0 .. self.size {
            for y in 0 .. self.size {
                let c = Coord::new(x, y);
                if !self[c].is_empty() && self.repr_mut(c) == r {
                    to_be_removed.push(c);
                }
            }
        }
        for c in to_be_removed {
            self[c] = BlockState::Empty;
        }
    }
}
