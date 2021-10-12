use hecs::{Entity, World};
use hecs_hierarchy::{Child, Hierarchy};
use itertools::{EitherOrBoth, Itertools};

/// Marker component for reactive nodes
#[derive(Clone, Copy, Debug)]
pub struct Node;

/// `hecs-hierarchy` marker for the tree relation, between either reactive
/// nodes, or property entities
#[derive(Clone, Copy, Debug)]
pub struct NodeTree;

/// Component that links [`Node`]s and their properties to each other. Each one
/// has this component, specifying the other's [`Entity`].
///
/// This is separate from [`hecs_hierarchy::Child`], since it only supports a
/// single entity in each direction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodePropertiesLink(pub Entity);

/// Component attached to [`Node`]s during [`update`], specifying how this
/// node's lifecycle was affected. Note that this is attached to the [`Node`]s,
/// *not* to their properties!
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UpdateKind {
    New,
    Updated,
}

fn query_singleton<T: hecs::Component>(world: &mut World) -> Option<Entity> {
    world
        .query_mut::<()>()
        .with::<T>()
        .into_iter()
        .next()
        .map(|(entity, ())| entity)
}

pub fn get_props(world: &World, node: Entity) -> Entity {
    world.get::<NodePropertiesLink>(node).unwrap().0
}

fn attach_props(world: &mut World, node: Entity, props: Entity) {
    world.insert_one(node, NodePropertiesLink(props)).unwrap();
    world.insert_one(props, NodePropertiesLink(node)).unwrap();
}

fn despawn_props(world: &mut World, props: Entity) {
    if world.get::<Child<NodeTree>>(props).is_ok() {
        world.detach::<NodeTree>(props).unwrap();
    }

    // No need to clean up the `NodePropertiesLink` here - either the node is
    // about to be despawned, in which case it doesn't matter, or a new link
    // will be set, in which case it also doesn't matter. No need to fix up any
    // linked list like `hecs-hierarchy` has, either.

    world.despawn(props).unwrap();
}

fn despawn_node_props(world: &mut World, node: Entity) {
    let props = get_props(world, node);
    despawn_props(world, props);
}

fn spawn_node(world: &mut World, props_tree: Entity) -> Entity {
    let node = world.spawn((Node, UpdateKind::New));

    update_children(world, node, props_tree);

    attach_props(world, node, props_tree);

    node
}

fn update_node(world: &mut World, node: Entity, props_tree: Entity) {
    world.insert_one(node, UpdateKind::Updated).unwrap();

    // This has to happen before we despawn the parent props, because we don't
    // call `detach_children` before despawning, and that messes up the call to
    // `detach` for the child props. This way, it works out without us having to
    // call `detach_children`.
    update_children(world, node, props_tree);

    despawn_node_props(world, node);
    attach_props(world, node, props_tree);
}

fn despawn_node(world: &mut World, node: Entity) {
    let children = world.children::<NodeTree>(node).collect::<Vec<_>>();
    for child in children {
        despawn_node(world, child);
    }

    despawn_node_props(world, node);

    if world.get::<Child<NodeTree>>(node).is_ok() {
        world.detach::<NodeTree>(node).unwrap();
    }

    world.despawn(node).unwrap();
}

fn update_children(world: &mut World, node: Entity, props: Entity) {
    let existing_children = world.children::<NodeTree>(node);
    let props_children = world.children::<NodeTree>(props);

    let child_pairs = existing_children
        .zip_longest(props_children)
        .collect::<Vec<_>>();

    for child_pair in child_pairs {
        match child_pair {
            EitherOrBoth::Both(node_child, props_child) => {
                update_node(world, node_child, props_child);
            }
            EitherOrBoth::Left(node_child) => {
                despawn_node(world, node_child);
            }
            EitherOrBoth::Right(props_child) => {
                let node_child = spawn_node(world, props_child);
                world.attach::<NodeTree>(node_child, node).unwrap();
            }
        }
    }
}

pub fn update<RootMarker: hecs::Component + Default>(
    world: &mut World,
    new_props_root: Option<Entity>,
) -> Option<Entity> {
    let old_root = query_singleton::<RootMarker>(world);

    match (old_root, new_props_root) {
        (None, Some(new_props_root)) => {
            let root = spawn_node(world, new_props_root);
            world.insert_one(root, RootMarker::default()).unwrap();
            Some(root)
        }

        (Some(old_root), Some(new_props_root)) => {
            update_node(world, old_root, new_props_root);
            Some(old_root)
        }

        (Some(old_root), None) => {
            despawn_node(world, old_root);
            None
        }

        (None, None) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct MyTree;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Props(u32);

    #[track_caller]
    fn check_update_kind(world: &World, node: Entity, kind: UpdateKind) {
        assert_eq!(*world.get::<UpdateKind>(node).unwrap(), kind);
    }

    fn check_props(
        world: &World,
        node: Entity,
        expected_props_entity: Entity,
        expected_props_value: Option<Props>,
    ) {
        let props = get_props(world, node);
        assert_eq!(props, expected_props_entity);

        assert_eq!(
            world.get::<Props>(props).ok().as_deref(),
            expected_props_value.as_ref(),
        );
    }

    #[test]
    fn it_works() {
        let mut world = World::new();

        let root_props = world.spawn((Props(1),));
        let child_props1 = world
            .attach_new::<NodeTree, _>(root_props, (Props(2),))
            .unwrap();
        let child_props2 = world
            .attach_new::<NodeTree, _>(root_props, (Props(3),))
            .unwrap();

        let root = update::<MyTree>(&mut world, Some(root_props)).unwrap();
        let children = world.children::<NodeTree>(root).collect::<Vec<_>>();
        assert_eq!(children.len(), 2);
        check_props(&world, root, root_props, Some(Props(1)));
        check_props(&world, children[0], child_props1, Some(Props(2)));
        check_props(&world, children[1], child_props2, Some(Props(3)));
        check_update_kind(&world, root, UpdateKind::New);
        check_update_kind(&world, children[0], UpdateKind::New);
        check_update_kind(&world, children[1], UpdateKind::New);

        // Now update, removing a child
        let update_root_props = world.spawn((Props(100),));
        let update_child_props1 = world
            .attach_new::<NodeTree, _>(update_root_props, (Props(200),))
            .unwrap();

        let updated_root = update::<MyTree>(&mut world, Some(update_root_props)).unwrap();
        assert_eq!(updated_root, root);
        let updated_children = world.children::<NodeTree>(updated_root).collect::<Vec<_>>();
        assert_eq!(updated_children.len(), 1);
        assert_eq!(updated_children[0], children[0]);
        check_props(&world, updated_root, update_root_props, Some(Props(100)));
        check_props(
            &world,
            updated_children[0],
            update_child_props1,
            Some(Props(200)),
        );
        check_update_kind(&world, updated_root, UpdateKind::Updated);
        check_update_kind(&world, updated_children[0], UpdateKind::Updated);

        // Now add a few children
        let update2_root_props = world.spawn((Props(10),));
        let update2_child_props1 = world
            .attach_new::<NodeTree, _>(update2_root_props, (Props(20),))
            .unwrap();
        let update2_child_props2 = world
            .attach_new::<NodeTree, _>(update2_root_props, (Props(30),))
            .unwrap();
        let update2_child_props3 = world
            .attach_new::<NodeTree, _>(update2_root_props, (Props(40),))
            .unwrap();

        let updated2_root = update::<MyTree>(&mut world, Some(update2_root_props)).unwrap();
        assert_eq!(updated2_root, updated_root);
        let updated2_children = world
            .children::<NodeTree>(updated2_root)
            .collect::<Vec<_>>();
        assert_eq!(updated2_children.len(), 3);
        assert_eq!(updated2_children[0], updated_children[0]);
        check_props(&world, updated2_root, update2_root_props, Some(Props(10)));
        check_props(
            &world,
            updated2_children[0],
            update2_child_props1,
            Some(Props(20)),
        );
        check_props(
            &world,
            updated2_children[1],
            update2_child_props2,
            Some(Props(30)),
        );
        check_props(
            &world,
            updated2_children[2],
            update2_child_props3,
            Some(Props(40)),
        );
        check_update_kind(&world, updated2_root, UpdateKind::Updated);
        check_update_kind(&world, updated2_children[0], UpdateKind::Updated);
        check_update_kind(&world, updated2_children[1], UpdateKind::New);
        check_update_kind(&world, updated2_children[2], UpdateKind::New);

        // Check that we have no dangling entities
        assert_eq!(
            world
                .iter()
                .map(|eref| eref.entity())
                .sorted()
                .collect::<Vec<_>>(),
            vec![
                updated2_root,
                updated2_children[0],
                updated2_children[1],
                updated2_children[2],
                update2_root_props,
                update2_child_props1,
                update2_child_props2,
                update2_child_props3
            ]
            .iter()
            .copied()
            .sorted()
            .collect::<Vec<_>>()
        );
    }
}
