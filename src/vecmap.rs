use std::{marker::PhantomData, ops::{Index, IndexMut}};

pub struct VecMap<K, V> {
    items: Vec<V>,
    id: PhantomData<K>,
}

impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self { items: Vec::new(), id: PhantomData }
    }
}

impl<K, V> VecMap<K, V> {
    pub fn push(&mut self, item: V) -> K where K: From<usize> {
        let key = K::from(self.items.len());
        self.items.push(item);
        key
    }
}

impl<K, V> Index<K> for VecMap<K, V> where K: Into<usize> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.items[index.into()]
    }
}

impl<K, V> IndexMut<K> for VecMap<K, V> where K: Into<usize> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.items[index.into()]
    }
}
