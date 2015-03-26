import expect from 'expect.js';
import Type from '../src/type';
import unifyType from '../src/unify_type';

describe('unifyType', () => {
  it('unifies a variable with a concrete type', () => {
    let t1 = Type.Var();
    let t2 = Type.create('int');

    unifyType(t1, t2);

    expect(t1.instance).to.equal(t2);
    expect(t2.instance).to.be(undefined);
  });

  it('unifies a concrete type with a variable', () => {
    let t1 = Type.create('int');
    let t2 = Type.Var();

    unifyType(t1, t2);

    expect(t1.instance).to.be(undefined);
    expect(t2.instance).to.equal(t1);
  });

  it('unifies an arrow type with a variable', () => {
    let a = Type.Var();
    let b = Type.Var();
    let t1 = Type.arrow('int', 'bool');
    let t2 = Type.arrow(a, b);

    unifyType(t1, t2);

    expect(a.instance.toString()).to.equal("int");
    expect(b.instance.toString()).to.equal("bool");
  });

  it('unifies in two directions at once', () => {
    let a = Type.Var();
    let b = Type.Var();
    let t1 = Type.arrow(a, 'bool');
    let t2 = Type.arrow('int', b);

    unifyType(t1, t2);

    expect(a.instance.toString()).to.equal("int");
    expect(b.instance.toString()).to.equal("bool");
  });
});
