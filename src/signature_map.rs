use std::cmp::Ordering;

use crate::types::{FnBody, Params};

#[derive(Debug, Default)]
pub struct FnSignatureMap {
    roots: Vec<MapNode>,
}

#[derive(Debug)]
struct MapNode {
    params: Params,
    body: FnBody,

    // invariant: only one of these will be comparable to the current params
    greater_nodes: Vec<MapNode>,
}

fn insert_impl(nodes: &mut Vec<MapNode>, new_node: MapNode) {
    for i in 0..nodes.len() {
        match new_node.params.partial_cmp(&nodes[i].params) {
            Some(Ordering::Equal) => {
                nodes[i] = new_node;
                return;
            }
            Some(Ordering::Greater) => {
                insert_impl(&mut nodes[i].greater_nodes, new_node);
                return;
            }
            Some(Ordering::Less) => {
                // -> should be spliced in between current and greater node
                let curr = nodes.splice(i..i + 1, [new_node]).next().unwrap();
                nodes[i].greater_nodes.push(curr);
                return;
            }
            None => {
                continue;
            }
        }
    }

    // // could not find any comparable greater node -> add new
    nodes.push(new_node);
}

fn find_impl(nodes: &Vec<MapNode>, params: Params) -> Option<FnBody> {
    for node in nodes {
        match params.partial_cmp(&node.params) {
            Some(Ordering::Equal) => {
                return Some(node.body.clone());
            }
            Some(Ordering::Greater) => {
                return find_impl(&node.greater_nodes, params).or_else(|| Some(node.body.clone()));
            }
            Some(Ordering::Less) | None => {
                continue;
            }
        }
    }

    None
}

impl FnSignatureMap {
    pub fn insert(&mut self, params: Params, body: FnBody) {
        insert_impl(
            &mut self.roots,
            MapNode {
                params,
                body,
                greater_nodes: vec![],
            },
        );
    }

    pub fn find_best_match(&self, params: Params) -> Option<FnBody> {
        find_impl(&self.roots, params)
    }
}
