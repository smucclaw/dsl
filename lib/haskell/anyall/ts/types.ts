interface withDefault<T> {
  byDefault?: T,
  fromUser?: T
}
export default interface StdinSchema {
  marking: {
    [nodeLabel: `${string}`]: withDefault<boolean>;
  },
  andOrTree: LeafNode | AnyNode | AllNode
}
export class LeafNode {
  leaf: string
}
abstract class SubTree {
  children: (LeafNode | AnyNode | AllNode)[];
  pre: string;
  prepost?: string
}
export class AnyNode extends SubTree { nodetype: "any" }
export class AllNode extends SubTree { nodetype: "all" }
