import parse from '../src/parse';
import expect from 'expect.js';

describe('parser', () => {
  it('parses a number', () => {
    expect(parse('0').toString()).to.equal('(ide 0)');
  });

  it('parses an atom', () => {
    expect(parse('a').toString()).to.equal('(ide a)');
  });

  it('parses a lambda', () => {
    expect(parse('fun(x) 0').toString()).
      to.equal('(lambda (ide x) (ide 0))');
  });

  it('parses a cond', () => {
    expect(parse('if a then b else c').toString()).
      to.equal('(cond (ide a) (ide b) (ide c))');
  });

  it('parses a function application', () => {
    expect(parse('f(x)').toString()).
      to.equal('(apply (ide f) (ide x))');
  });

  it('parses a double function application', () => {
    expect(parse('max(x)(y)').toString()).
      to.equal('(apply (apply (ide max) (ide x)) (ide y))');
  });

  it ('parses a double function application inside a cond', () => {
    // This resolves an ambiguity: does the expression mean
    // (1)  (if a then b else max(x))(y) or
    // (2)  if a then b else (max(x)(y)) ?
    // Answer: (2)

    let expr  = parse('if a then b else max(x)(y)').toString();
    let right = parse('if a then b else (max(x)(y))').toString();
    let wrong = parse('(if a then b else max(x))(y)').toString();

    expect(expr).to.equal(right);
    expect(expr).not.to.equal(wrong);

    expect(expr).to.equal('(cond (ide a) (ide b) (apply (apply (ide max) (ide x)) (ide y)))');
  });

  it('parses a let expression', () => {
    expect(parse('let n = 3 in n').toString()).
      to.equal('(block (def n (ide 3)) (ide n))');
  });

  it ('parses a recursive let expression', () => {
    expect(parse('let rec f = fun(x) f(x) in f(0)').toString()).
      to.equal('(block (rec (def f (lambda (ide x) (apply (ide f) (ide x))))) (apply (ide f) (ide 0)))');
  });


});
