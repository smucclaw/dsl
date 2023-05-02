interface myEither<T> {
  Right?: T,
  Left?: T
}
interface withDefault<T> {
  getDefault: myEither<T>,
}
export default interface StdinSchema {
  marking: {
    [nodeLabel: `${string}`]: myEither<boolean>;
  },
  andOrTree: LeafNode | AnyNode | AllNode | NotNode
}
export class LeafNode    { contents: string; tag: "Leaf"; }
export class PreNode     { contents: string; tag: "Pre"; }
export class PrePostNode { contents: string; tag: "PrePost"; }

abstract class SubTree {
    contents: (PreNode | (LeafNode | AnyNode | AllNode | NotNode)[])[];
}
export class AnyNode extends SubTree { tag: "Any"  }
export class AllNode extends SubTree { tag: "All"  }
export class NotNode { contents: (LeafNode | AnyNode | AllNode | NotNode); tag: "Not" }
