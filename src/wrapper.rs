use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Deref, DerefMut},
};

pub const FLOAT_COMP_ERROR_MARGIN: f64 = 0.01;

#[derive(Clone)]
pub struct F64(f64);

impl F64 {
    pub fn new(value: f64) -> Self {
        Self(value)
    }
}

impl Display for F64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for F64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

impl From<f64> for F64 {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl Deref for F64 {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for F64 {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        (self.0 - other.0).abs() < FLOAT_COMP_ERROR_MARGIN
    }
}

impl Eq for F64 {}

impl Hash for F64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

#[derive(Clone)]
pub struct HashableHashMap<K, V>(HashMap<K, V>);

impl<K, V> HashableHashMap<K, V> {
    pub fn unwrap(self) -> HashMap<K, V> {
        self.0
    }
}

impl<K, V> Debug for HashableHashMap<K, V>
where
    K: Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl<K, V> From<HashMap<K, V>> for HashableHashMap<K, V> {
    fn from(map: HashMap<K, V>) -> Self {
        Self(map)
    }
}

impl<K, V> Deref for HashableHashMap<K, V> {
    type Target = HashMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> DerefMut for HashableHashMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K, V> PartialEq for HashableHashMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<K, V> Eq for HashableHashMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
}

impl<K, V> Hash for HashableHashMap<K, V>
where
    K: Hash + Eq,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.iter().for_each(|(k, _v)| k.hash(state));
    }
}
