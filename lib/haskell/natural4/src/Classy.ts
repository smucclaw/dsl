class SuperParent { }

export class SubChild extends SuperParent {
  newSubAttribute9: number = 42;
  constructor(
    public newSubAttribute7: number,
    public newSubAttribute8: string
  ) { super() }
}
